from typing import Optional, Callable
from parsed_ast import Node, Program, ProgramBlock, InstrBlock, Instr, ArrayDecl, VarAssign, AddrInstr, \
    AddrExpr, AddrAssign, Identifier, AddrMember, IndexingSuffix, ExprList, Expr, Term, Factor, SimpleExpr, \
    CallableSuffix, IntLiteral, AddrIdOrCall, AddOp, MulOp, UnaryMinus, UnaryNot, PowOp, IfElse, ElseIf, \
    InnerInstrBlock, GoToInstr, SwitchCase, ForLoop, WhileLoop, DoUntil, StrLiteral, NumLiteral, PrintInstr, \
    FunDecl, ParamList, Param, ReturnInstr, ProcDecl, FunExpr, CastStr, CastInt, CastFloat, Input, Length, StrSubstring, ClassDecl, ClassBlock, ClassMember, AttrDecl, \
    EndOfFile, ReadLine, WriteLine, FileClose, OpenRead, OpenWrite, NewExpr, BoolLiteral, CompOp, Disjunction, Comparison, ArithmExpr
from lexer import Lexer
from parsed_token import TokenVals, ParsedToken, TokenContents, KNOWN_TOKEN_VALS


class Parser:
    __lexer: Lexer

    def __init__(self, lexer: Lexer, on_error: Optional[Callable] = None, on_parse_begin: Optional[Callable] = None, on_parse_finish: Optional[Callable] = None):
        self.__lexer = lexer
        self.curr_line_index: Optional[int] = None
        self.on_error = on_error
        self.on_parse_finish = on_parse_finish
        self.on_parse_begin = on_parse_begin

    def parse(self) -> Optional[Node]:
        result: Optional[Node] = self.__program({})
        if self.on_parse_begin is not None:
            self.on_parse_begin()
        if (next_token := self.__lexer.next()) is not None:
            self.curr_line_index = next_token.line_index
            self.__raise_error(SyntaxError(f"Unexpected {next_token}"))
        if self.on_parse_finish is not None:
            self.on_parse_finish(result)
        return result

    def __program(self, ctx: dict) -> Optional[Program]:
        result: Optional[Program] = None
        while True:
            block: ProgramBlock = self.__program_block(ctx)
            if block:
                if result is None:
                    result = Program(0)
                result.add_block(block)
            else:
                break
        return result

    def __program_block(self, ctx: dict) -> Optional[ProgramBlock]:
        procs: list[Callable] = [self.__instr_block, self.__fun_decl, self.__proc_decl, self.__class_decl]
        for proc in procs:
            result: ProgramBlock = proc(ctx)
            if result:
                return result
        return None

    def __instr_block(self, ctx: dict) -> Optional[InstrBlock]:
        result = InstrBlock(self.curr_line_index)
        while True:
            instr: Optional[Instr] = self.__instr(ctx)
            if instr:
                self.__expect_new_line()
                result.add_sub_node(instr)
                # flag indicating a function call should not be retained between instructions
                ctx.pop(AddrIdOrCall.IS_INSTR, False)
            else:
                break
        return result if result.sub_nodes else None

    def __instr(self, ctx: dict) -> Optional[Instr]:
        procs: list[Callable] = [self.__glob_decl, self.__arr_decl, self.__addr_instr, self.__if_else, self.__switch_case,
                                 self.__for_loop, self.__while_loop, self.__do_until, self.__print_instr,
                                 self.__return_instr]
        for proc in procs:
            result: Instr = proc(ctx)
            if result:
                return result
        return None

    def __glob_decl(self, ctx: dict) -> Optional[Instr]:
        result: Optional[Instr] = None
        if self.__token_is(TokenVals.GLOBAL):
            ctx[TokenVals.GLOBAL] = True
            self.__expect_no_newline()
            result = self.__tree_expect(ctx,
                                        self.__arr_or_var,
                                        "Syntax error: array or variable declaration expected")
        return result

    def __arr_or_var(self, ctx: dict) -> Optional[Instr]:
        procs: list[Callable] = [self.__arr_decl, self.__var_assign]
        for proc in procs:
            result: Instr = proc(ctx)
            if result:
                return result
        return None

    def __arr_decl(self, ctx: dict) -> Optional[ArrayDecl]:
        result: Optional[ArrayDecl] = None
        if self.__token_is(TokenVals.ARRAY):
            is_global: bool = ctx.pop(TokenVals.GLOBAL, False)
            name: str = self.__token_expect(TokenVals.ID, same_line=True)
            result = ArrayDecl(self.curr_line_index, is_global).set_name(name)
            self.__token_must_be(TokenContents.OPEN_BRACKET, TokenVals.OPEN_BRACKET, same_line=True)
            result.set_dims(self.__tree_expect(ctx,
                                               self.__expr_list,
                                               "Syntax error: list of expressions expected"))
            self.__token_must_be(TokenContents.CLOSED_BRACKET, TokenVals.CLOSED_BRACKET)
        return result

    def __var_assign(self, ctx: dict) -> Optional[VarAssign | AddrInstr]:
        addr_instr: Optional[AddrInstr] = self.__addr_instr(ctx)
        if addr_instr is not None and isinstance(addr_instr, VarAssign):
            return addr_instr
        return None

    def __addr_instr(self, ctx: dict) -> Optional[AddrInstr]:
        addr_expr = self.__addr_expr(ctx)
        result: Optional[AddrInstr] = None
        is_global = ctx.pop(TokenVals.GLOBAL, False)
        if addr_expr:
            result = AddrInstr(addr_expr.line_index).add_sub_node(addr_expr)
            if ctx.pop(AddrIdOrCall.IS_INSTR, False):
                #
                # The address expression is of the form '<member>.<member>. ... <id>(...)'
                # which constitutes a complete instruction
                #
                return result
            result.add_sub_node(
                self.__tree_expect(ctx,
                                   self.__addr_assign,
                                   "Expected variable assignment"))
            # In case parsing the assignment expression enabled the IS_INSTR flag
            ctx.pop(AddrIdOrCall.IS_INSTR, False)
            return VarAssign(result.line_index, is_global).add_sub_node(result, children=True)
        return result

    def __addr_expr(self, ctx: dict) -> Optional[AddrExpr]:
        result: Optional[AddrExpr] = None
        if (addr_member := self.__addr_member(ctx)) is not None:
            result = AddrExpr(addr_member.line_index).add_sub_node(addr_member)
            while self.__token_is(TokenVals.DOT, same_line=True):
                ctx.pop(AddrIdOrCall.IS_INSTR, False)
                if (builtin_attribute := self.__builtin_attribute(ctx)) is not None:
                    return builtin_attribute.add_sub_node(result)
                result.add_sub_node(self.__tree_expect(ctx,
                                                       self.__addr_member,
                                                       "Syntax error: '.' detected but no identifier found"))
        return result

    def __builtin_attribute(self, ctx: dict) -> Optional[Node]:
        procs: list[Callable] = [self.__str_substring, self.__str_len, self.__end_of_file, self.__read_line, self.__write_line, self.__close]
        for proc in procs:
            if (result := proc(ctx)) is not None:
                return result

    def __str_len(self, ctx: dict) -> Optional[Length]:
        if self.__token_is(TokenVals.LENGTH):
            return Length(self.curr_line_index)

    def __str_substring(self, ctx) -> Optional[StrSubstring]:
        if self.__token_is(TokenVals.SUBSTRING):
            self.__token_must_be(TokenContents.OPEN_PAREN, TokenVals.OPEN_PAREN, same_line=True)
            result = StrSubstring(self.curr_line_index)
            result.add_sub_node(self.__tree_expect(ctx,
                                                   self.__expr,
                                                   "Syntax error: 2 integer parameters required for substring() call"))
            self.__token_must_be(TokenContents.COMMA, TokenVals.COMMA)
            result.add_sub_node(self.__tree_expect(ctx,
                                                   self.__expr,
                                                   "Syntax error: 2 integer parameters required for substring() call"))
            self.__token_must_be(TokenContents.CLOSED_PAREN, TokenVals.CLOSED_PAREN)
            return result

    def __end_of_file(self, ctx: dict) -> Optional[EndOfFile]:
        if self.__token_is(TokenVals.ENDOFFILE):
            self.__token_must_be(TokenContents.OPEN_PAREN, TokenVals.OPEN_PAREN, same_line=True)
            self.__token_must_be(TokenContents.CLOSED_PAREN, TokenVals.CLOSED_PAREN, same_line=True)
            return EndOfFile(self.curr_line_index)

    def __read_line(self, ctx: dict) -> Optional[ReadLine]:
        if self.__token_is(TokenVals.READLINE):
            self.__token_must_be(TokenContents.OPEN_PAREN, TokenVals.OPEN_PAREN, same_line=True)
            self.__token_must_be(TokenContents.CLOSED_PAREN, TokenVals.CLOSED_PAREN, same_line=True)
            ctx[AddrIdOrCall.IS_INSTR] = True
            return ReadLine(self.curr_line_index)

    def __write_line(self, ctx: dict) -> Optional[WriteLine]:
        if self.__token_is(TokenVals.WRITELINE):
            self.__token_must_be(TokenContents.OPEN_PAREN, TokenVals.OPEN_PAREN, same_line=True)
            result = WriteLine(self.curr_line_index).add_sub_node(self.__tree_expect(ctx,
                                                                                     self.__expr,
                                                                                     "Argument expected for writeLine() function call"))
            self.__token_must_be(TokenContents.CLOSED_PAREN, TokenVals.CLOSED_PAREN)
            ctx[AddrIdOrCall.IS_INSTR] = True
            return result

    def __close(self, ctx: dict) -> Optional[FileClose]:
        if self.__token_is(TokenVals.CLOSE):
            self.__token_must_be(TokenContents.OPEN_PAREN, TokenVals.OPEN_PAREN, same_line=True)
            self.__token_must_be(TokenContents.CLOSED_PAREN, TokenVals.CLOSED_PAREN, same_line=True)
            ctx[AddrIdOrCall.IS_INSTR] = True
            return FileClose(self.curr_line_index)

    def __addr_member(self, ctx: dict) -> Optional[AddrMember]:
        result: Optional[AddrMember] = None
        if (addr_id_or_call := self.__addr_id_or_call(ctx)) is not None:
            result = AddrMember(addr_id_or_call.line_index).add_sub_node(addr_id_or_call)
            if (indexing_suffix := self.__indexing_suffix(ctx)) is not None:
                result.add_sub_node(indexing_suffix)
        return result

    def __addr_id_or_call(self, ctx: dict) -> Optional[AddrIdOrCall]:
        result: Optional[AddrIdOrCall] = None
        possible_method_vals: list[TokenVals] = [TokenVals.ID, TokenVals.NEW, TokenVals.SUPER]
        for val in possible_method_vals:
            if (name := self.__token_str(val)) is not None:
                result = AddrIdOrCall(self.curr_line_index).add_sub_node(Identifier(self.curr_line_index, name))
                if (callable_suffix := self.__callable_suffix(ctx)) is not None:
                    result.add_sub_node(callable_suffix)
                    ctx[AddrIdOrCall.IS_INSTR] = True
        return result

    def __callable_suffix(self, ctx: dict) -> Optional[CallableSuffix]:
        result: Optional[CallableSuffix] = None
        if self.__token_is(TokenVals.OPEN_PAREN, same_line=True):
            expr_list = self.__expr_list(ctx)
            result = CallableSuffix(self.curr_line_index)
            if expr_list is not None:
                result.add_sub_node(expr_list)
            self.__token_must_be(TokenContents.CLOSED_PAREN.value, TokenVals.CLOSED_PAREN)
        return result

    def __indexing_suffix(self, ctx: dict) -> Optional[IndexingSuffix]:
        result: Optional[IndexingSuffix] = None
        if self.__token_is(TokenVals.OPEN_BRACKET, same_line=True):
            result = IndexingSuffix(self.curr_line_index)
            result.add_sub_node(self.__tree_expect(ctx,
                                                   self.__expr_list,
                                                   f"Syntax error: '{TokenContents.OPEN_BRACKET.value}' detected but no indices provided"))
            self.__token_must_be(TokenContents.CLOSED_BRACKET.value, TokenVals.CLOSED_BRACKET)
        return result

    def __addr_assign(self, ctx: dict) -> Optional[AddrAssign]:
        if self.__token_is(TokenVals.EQUALS):
            result = AddrAssign(self.curr_line_index).add_sub_node(self.__tree_expect(ctx,
                                                                                      self.__expr,
                                                                                      "No assignment value specified"))
            return result

    def __expr_list(self, ctx: dict) -> Optional[ExprList]:
        result: Optional[ExprList] = None
        if (expr := self.__expr(ctx)) is not None:
            result = ExprList(expr.line_index).add_sub_node(expr)
            self.__expect_no_newline()
            while self.__token_is(TokenVals.COMMA):
                result.add_sub_node(self.__tree_expect(ctx,
                                                       self.__expr,
                                                       f"'{TokenContents.COMMA.value}' detected but no expression found"))
        return result

    def __expr(self, ctx: dict) -> Optional[Expr]:
        #
        # Keeping record of the previous ID_CALL flag prevents multiple AddrExpr nodes overwriting each other
        #
        prev_id_or_call = ctx.pop(AddrIdOrCall.IS_INSTR, False)
        result: Optional[Expr] = None
        if (disjunction := self.__disjunction(ctx)) is not None:
            result = Expr(disjunction.line_index).add_sub_node(disjunction)
            while self.__token_is(TokenVals.AND):
                result.add_sub_node(self.__tree_expect(ctx,
                                                       self.__disjunction,
                                                       f"'{TokenContents.AND.value}' detected but no subsequent expression found"))
        ctx[AddrIdOrCall.IS_INSTR] = prev_id_or_call
        return result

    def __disjunction(self, ctx: dict) -> Optional[Disjunction]:
        result: Optional[Disjunction] = None
        if (inversion := self.__inversion(ctx)) is not None:
            result = Disjunction(inversion.line_index).add_sub_node(inversion)
            while self.__token_is(TokenVals.OR):
                result.add_sub_node(self.__tree_expect(ctx,
                                                       self.__inversion,
                                                       f"'{TokenContents.OR.value}' detected but no subsequent expression found"))
        return result

    def __inversion(self, ctx: dict) -> Optional[UnaryNot | Comparison]:
        if self.__token_is(TokenVals.NOT):
            self.__expect_no_newline()
            return (UnaryNot(self.curr_line_index)
                    .add_sub_node(self.__tree_expect(ctx,
                                                     self.__comparison,
                                                     f"'{TokenContents.NOT.value}' detected but no subsequent expression found")))
        return self.__comparison(ctx)

    def __comparison(self, ctx: dict) -> Optional[Comparison]:
        result: Optional[Comparison] = None
        if (arithm_expr := self.__arithm_expr(ctx)) is not None:
            result = Comparison(arithm_expr.line_index).add_sub_node(arithm_expr)
            while (comp_op := self.__token_comp_op()) is not None:
                result.add_sub_node(comp_op)
                result.add_sub_node(self.__tree_expect(ctx,
                                                       self.__arithm_expr,
                                                       f"'{KNOWN_TOKEN_VALS[comp_op.val].value}' detected but no subsequent expression found"))
        return result

    def __arithm_expr(self, ctx: dict) -> Optional[ArithmExpr]:
        result: Optional[ArithmExpr] = None
        if (term := self.__term(ctx)) is not None:
            result = ArithmExpr(term.line_index).add_sub_node(term)
            while (add_op := self.__token_add_op()) is not None:
                result.add_sub_node(add_op)
                result.add_sub_node(self.__tree_expect(ctx,
                                                       self.__term,
                                                       f"'{KNOWN_TOKEN_VALS[add_op.val].value}' detected but no subsequent expression found"))
        return result

    def __term(self, ctx: dict) -> Optional[Term]:
        result: Optional[Term] = None
        factor: Optional[Factor] = self.__factor(ctx)
        if factor is not None:
            result = Term(factor.line_index).add_sub_node(factor)
            while (op_node := self.__token_mul_op()) is not None:
                result.add_sub_node(op_node)
                result.add_sub_node(self.__tree_expect(ctx,
                                                       self.__factor,
                                                       f"'{KNOWN_TOKEN_VALS[op_node.val].value}' detected but no operand found"))
        return result

    def __factor(self, ctx: dict) -> Optional[Factor]:
        result: Optional[Factor] = None
        simple_expr: Optional[SimpleExpr] = self.__simple_expr(ctx)
        if simple_expr is not None:
            result = Factor(simple_expr.line_index).add_sub_node(simple_expr)
            while (op_node := self.__token_pow_op()) is not None:
                result.add_sub_node(op_node)
                result.add_sub_node(self.__tree_expect(ctx,
                                                       self.__simple_expr,
                                                       f"'{KNOWN_TOKEN_VALS[op_node.val].value}' detected but no operand found"))
        return result

    def __simple_expr(self, ctx: dict) -> Optional[Node]:
        #
        # Set aside highest priority single tokens that trigger recursion
        #
        if self.__token_is(TokenVals.MINUS):
            self.__expect_no_newline()
            return UnaryMinus(self.curr_line_index).add_sub_node(self.__tree_expect(ctx,
                                                                                    self.__simple_expr,
                                                                                    f"'{TokenContents.MINUS.value}' detected but no subsequent expression found"))

        if self.__token_is(TokenVals.OPEN_PAREN):
            expr: Node = self.__tree_expect(ctx,
                                            self.__expr,
                                            f"'{TokenContents.OPEN_PAREN.value}' detected but no subsequent expression found")
            self.__token_must_be(TokenContents.CLOSED_PAREN, TokenVals.CLOSED_PAREN)
            expr.line_index = self.curr_line_index
            return expr
        prev_curr_line_index: int = self.curr_line_index
        #
        # Literal-like cases
        #
        procs: list[Callable] = [self.__new_expr, self.__addr_expr, self.__fun_expr, self.__int_literal, self.__str_literal, self.__num_literal, self.__bool_literal]
        for proc in procs:
            sub_node = proc(ctx)
            if sub_node is not None:
                return SimpleExpr(prev_curr_line_index).add_sub_node(sub_node)
        return None

    def __int_literal(self, ctx: dict) -> Optional[IntLiteral]:
        next_int: Optional[int] = self.__int_token()
        return IntLiteral(self.curr_line_index, next_int) if next_int is not None else None

    def __str_literal(self, ctx: dict) -> Optional[StrLiteral]:
        next_str: Optional[str] = self.__str_token()
        return StrLiteral(self.curr_line_index, next_str) if next_str is not None else None

    def __num_literal(self, ctx: dict) -> Optional[NumLiteral]:
        next_float: Optional[float] = self.__num_token()
        return NumLiteral(self.curr_line_index, next_float) if next_float is not None else None

    def __bool_literal(self, ctx: dict) -> Optional[BoolLiteral]:
        next_bool: Optional[bool] = self.__bool_token()
        return BoolLiteral(self.curr_line_index, next_bool) if next_bool is not None else None

    def __new_expr(self, ctx: dict) -> Optional[NewExpr]:
        if self.__token_is(TokenVals.NEW):
            result = NewExpr(self.curr_line_index)
            class_name: Optional[str] = self.__token_str(TokenVals.ID, same_line=True)
            if class_name is None:
                raise SyntaxError("Name of class to instantiate not specified")
            result.add_sub_node(Identifier(self.curr_line_index, class_name))
            self.__token_must_be(TokenContents.OPEN_PAREN, TokenVals.OPEN_PAREN, same_line=True)
            if (expr_list := self.__expr_list(ctx)) is not None:
                result.add_sub_node(expr_list, children=True)
            self.__token_must_be(TokenContents.CLOSED_PAREN, TokenVals.CLOSED_PAREN)
            return result

    def __fun_expr(self, ctx: dict) -> Optional[FunExpr]:
        def __get_fun_expr_type() -> Optional[type]:
            token_val_to_type: dict[TokenVals, type] = {
                TokenVals.INT_CAST: CastInt,
                TokenVals.FLOAT_CAST: CastFloat,
                TokenVals.STR_CAST: CastStr,
                TokenVals.INPUT: Input,
                TokenVals.OPENREAD: OpenRead,
                TokenVals.OPENWRITE: OpenWrite,
            }
            for token_val, node_class in token_val_to_type.items():
                if self.__token_is(token_val):
                    return node_class
            return None

        if (fun_node_type := __get_fun_expr_type()) is not None:
            self.__token_must_be(TokenContents.OPEN_PAREN, TokenVals.OPEN_PAREN, same_line=True)
            fun_node_line_index: int = self.curr_line_index
            result = fun_node_type(fun_node_line_index).add_sub_node(self.__tree_expect(ctx,
                                                                                        self.__expr,
                                                                                        err_msg="built-in function call requires an expression argument"))
            self.__token_must_be(TokenContents.CLOSED_PAREN, TokenVals.CLOSED_PAREN)
            return result
        return None

    def __if_else(self, ctx: dict) -> Optional[IfElse]:
        if self.__token_str(TokenVals.IF):
            self.__expect_no_newline()
            result = IfElse(self.curr_line_index).add_sub_node(self.__tree_expect(ctx,
                                                                                  self.__expr,
                                                                                  "No expression found in if statement declaration"))
            self.__token_must_be(TokenContents.THEN, TokenVals.THEN, same_line=True)
            result.add_sub_node(self.__tree_expect(ctx,
                                                   self.__inner_instr_block,
                                                   "Non-empty if statement block required"))
            self.__expect_new_line()
            if (elseif := self.__else_if(ctx)) is not None:
                result.add_sub_node(elseif, children=True)
            self.__token_must_be(TokenContents.ENDIF, TokenVals.ENDIF)
            return result
        return None

    def __else_if(self, ctx: dict) -> Optional[ElseIf]:
        if self.__token_str(TokenVals.ELSEIF):
            self.__expect_no_newline()
            result = ElseIf(self.curr_line_index).add_sub_node(self.__tree_expect(ctx,
                                                                                  self.__expr,
                                                                                  err_msg="No expression found in if statement declaration"))
            self.__expect_no_newline()
            self.__token_must_be(TokenContents.THEN, TokenVals.THEN)
            self.__expect_new_line()
            result.add_sub_node(self.__tree_expect(ctx,
                                                   self.__inner_instr_block,
                                                   "Non-empty elseif statement block required"))
            self.__expect_new_line()
            next_else_if: Optional[ElseIf] = self.__else_if(ctx)
            return result.add_sub_node(next_else_if, children=True) if next_else_if else result
        if self.__token_str(TokenVals.ELSE):
            self.__expect_new_line()
            return ElseIf(self.curr_line_index).add_sub_node(self.__tree_expect(ctx,
                                                                                self.__inner_instr_block,
                                                                                err_msg="Non-empty else statement block required"))
        return None

    def __inner_instr_block(self, ctx: dict) -> Optional[InnerInstrBlock]:
        result = None
        while (instr := self.__inner_instr(ctx)) is not None:
            if not result:
                result = InnerInstrBlock(instr.line_index)
            result.add_sub_node(instr)
            self.__expect_new_line()
        return result

    def __go_to_instr(self, ctx: dict) -> Optional[GoToInstr]:
        vals: list[TokenVals] = [TokenVals.BREAK, TokenVals.CONTINUE]
        for val in vals:
            if self.__token_str(val):
                return GoToInstr(self.curr_line_index, val)

    def __switch_case(self, ctx: dict) -> Optional[SwitchCase]:
        if self.__token_str(TokenVals.SWITCH):
            self.__expect_no_newline()
            result = SwitchCase(self.curr_line_index).add_sub_node(self.__tree_expect(ctx,
                                                                                      self.__expr,
                                                                                      "Expression missing from switch-case statement declaration"))
            self.__token_must_be(TokenContents.COLON, TokenVals.COLON, same_line=True)
            self.__expect_new_line()
            while self.__token_str(TokenVals.CASE):
                self.__expect_no_newline()
                result.add_sub_node(self.__tree_expect(ctx,
                                                       self.__expr,
                                                       "Expression missing from case statement"))
                self.__token_must_be(TokenContents.COLON, TokenVals.COLON, same_line=True)
                self.__expect_new_line()
                result.add_sub_node(self.__tree_expect(ctx,
                                                       self.__inner_instr_block,
                                                       "Block missing from case statement"))
                self.__expect_new_line()
            if self.__token_str(TokenVals.DEFAULT):
                self.__token_must_be(TokenContents.COLON, TokenVals.COLON, same_line=True)
                self.__expect_new_line()
                result.add_sub_node(self.__tree_expect(ctx,
                                                       self.__inner_instr_block,
                                                       "Block missing from default case statement"))
                self.__expect_new_line()
            self.__token_must_be(TokenContents.ENDSWITCH, TokenVals.ENDSWITCH)
            return result

    def __for_loop(self, ctx: dict) -> Optional[ForLoop]:
        if self.__token_str(TokenVals.FOR):
            result = ForLoop(self.curr_line_index)
            var_id: str = self.__token_expect(TokenVals.ID, same_line=True)
            result.add_sub_node(Identifier(self.curr_line_index, var_id))
            self.__token_must_be(TokenContents.EQUALS, TokenVals.EQUALS, same_line=True)
            self.__expect_no_newline()
            result.add_sub_node(self.__tree_expect(ctx,
                                                   self.__expr,
                                                   "Expression for lower bound of for loop missing"))
            self.__token_must_be(TokenContents.TO, TokenVals.TO, same_line=True)
            self.__expect_no_newline()
            result.add_sub_node(self.__tree_expect(ctx,
                                                   self.__expr,
                                                   "Expression for upper bound of for loop missing"))
            self.__expect_new_line()
            result.add_sub_node(self.__tree_expect(ctx,
                                                   self.__inner_instr_block,
                                                   "For loop instruction block missing"))
            self.__expect_new_line()
            self.__token_must_be(TokenContents.NEXT, TokenVals.NEXT)
            #
            # Identifier in 'next <ID>' statement must be the same as that in the for loop declaration
            #
            self.__token_must_be(var_id, TokenVals.ID, same_line=True)
            return result

    def __while_loop(self, ctx: dict) -> Optional[WhileLoop]:
        if self.__token_str(TokenVals.WHILE):
            self.__expect_no_newline()
            result = WhileLoop(self.curr_line_index).add_sub_node(self.__tree_expect(ctx,
                                                                                     self.__expr,
                                                                                     "Expression missing from while loop declaration"))
            self.__expect_new_line()
            result.add_sub_node(self.__tree_expect(ctx,
                                                   self.__inner_instr_block,
                                                   "Instruction block missing from while loop"))
            self.__expect_new_line()
            self.__token_must_be(TokenContents.ENDWHILE, TokenVals.ENDWHILE)
            return result

    def __do_until(self, ctx: dict) -> Optional[DoUntil]:
        if self.__token_str(TokenVals.DO):
            result = DoUntil(self.curr_line_index).add_sub_node(self.__tree_expect(ctx,
                                                                                   self.__inner_instr_block,
                                                                                   "Instruction block missing from do-until loop"))
            self.__token_must_be(TokenContents.UNTIL, TokenVals.UNTIL)
            result.add_sub_node(self.__tree_expect(ctx,
                                                   self.__expr,
                                                   "Expression missing from do-until loop declaration"))
            return result

    def __print_instr(self, ctx: dict) -> Optional[PrintInstr]:
        if self.__token_is(TokenVals.PRINT):
            self.__token_must_be(TokenContents.OPEN_PAREN, TokenVals.OPEN_PAREN, same_line=True)
            result = PrintInstr(self.curr_line_index)
            first_arg = self.__tree_expect(
                ctx,
                self.__expr,
                "At least one argument expected in print statement")
            result.add_sub_node(first_arg)
            while self.__token_is(TokenVals.COMMA):
                next_arg = self.__tree_expect(
                    ctx,
                    self.__expr,
                    "Expected argument after comma in print statement")
                result.add_sub_node(next_arg)
            self.__token_must_be(TokenContents.CLOSED_PAREN, TokenVals.CLOSED_PAREN)
            return result
        return None

    def __fun_decl(self, ctx: dict) -> Optional[FunDecl]:
        if self.__token_is(TokenVals.FUNCTION):
            result = FunDecl(self.curr_line_index)
            if self.__token_is(TokenVals.NEW):
                self.__raise_error(SyntaxError(f"Constructors must be procedures, not functions"))
            #
            # Function identifier and opening bracket is expected on the same line as 'function' keyword
            #
            func_name: str = self.__token_expect(TokenVals.ID, same_line=True)
            func_identifier: Identifier = Identifier(self.curr_line_index, func_name)
            result.add_sub_node(func_identifier)
            self.__token_must_be(TokenContents.OPEN_PAREN, TokenVals.OPEN_PAREN, same_line=True)
            result.add_sub_node(self.__param_list(ctx))
            #
            # Closing bracket expected on the same line as the last parameter
            #
            self.__token_must_be(TokenContents.CLOSED_PAREN, TokenVals.CLOSED_PAREN, same_line=True)
            #
            # Instruction block expected to start on a new line
            #
            self.__expect_new_line()
            result.add_sub_node(self.__tree_expect(ctx,
                                                   self.__inner_instr_block,
                                                   "Non-empty function block required"))
            #
            # 'endfunction' should be on a separate line
            #
            self.__expect_new_line()
            self.__token_must_be(TokenContents.ENDFUNCTION, TokenVals.ENDFUNCTION)
            self.__expect_new_line()
            return result
        return None

    def __param_list(self, ctx: dict) -> ParamList:
        result = ParamList(self.curr_line_index)
        if (first_param := self.__param(ctx)) is not None:
            result.add_sub_node(first_param)
            while self.__token_is(TokenVals.COMMA, same_line=True):
                result.add_sub_node(self.__tree_expect(ctx,
                                                       self.__param,
                                                       f"'{TokenContents.COMMA.value}' found but no argument present"))
        return result

    def __param(self, ctx: dict) -> Optional[Param]:
        if (param_name := self.__token_str(TokenVals.ID)) is not None:
            if self.__token_is(TokenVals.COLON, same_line=True):
                if self.__token_is(TokenVals.BYVAL, same_line=True):
                    result = Param(self.curr_line_index, param_name, False)
                elif self.__token_is(TokenVals.BYREF, same_line=True):
                    result = Param(self.curr_line_index, param_name, True)
                else:
                    raise SyntaxError(f"'{TokenContents.BYREF.value}' or '{TokenContents.BYVAL.value}' expected after '{TokenContents.COLON.value}'")
            else:
                result = Param(self.curr_line_index, param_name, False)
            return result
        return None

    def __inner_instr(self, ctx: dict) -> Optional[Instr | GoToInstr]:
        procs = [self.__instr, self.__go_to_instr]
        for proc in procs:
            if (result := proc(ctx)) is not None:
                return result

    def __proc_decl(self, ctx: dict) -> Optional[ProcDecl]:
        if self.__token_is(TokenVals.PROCEDURE):
            result = ProcDecl(self.curr_line_index)
            if (constructor_name := self.__token_str(TokenVals.NEW, same_line=True)) is not None:
                if not ctx.get(ClassDecl.IN_CLASS, False):
                    self.__raise_error(SyntaxError("Cannot declare a constructor outside of a class"))
                if not ctx.get(ClassMember.IS_PUBLIC_FLAG, True):
                    self.__raise_error(SyntaxError("Constructors cannot be private"))
                proc_id: Identifier = Identifier(self.curr_line_index, constructor_name)
            else:
                proc_id: Identifier = Identifier(self.curr_line_index, self.__token_expect(TokenVals.ID, same_line=True))
            result.add_sub_node(proc_id)
            self.__token_must_be(TokenContents.OPEN_PAREN, TokenVals.OPEN_PAREN, same_line=True)
            result.add_sub_node(self.__param_list(ctx))
            self.__token_must_be(TokenContents.CLOSED_PAREN, TokenVals.CLOSED_PAREN, same_line=True)
            self.__expect_new_line()
            result.add_sub_node(self.__tree_expect(
                ctx,
                self.__inner_instr_block,
                "Non-empty procedure block required"
            ))
            self.__expect_new_line()
            self.__token_must_be(TokenContents.ENDPROCEDURE, TokenVals.ENDPROCEDURE)
            self.__expect_new_line()
            return result
        return None

    def __return_instr(self, ctx: dict) -> Optional[ReturnInstr]:
        if self.__token_is(TokenVals.RETURN):
            result = ReturnInstr(self.curr_line_index)
            expr = self.__expr(ctx)
            if expr is not None:
                return result.add_sub_node(expr)
            return result
        return None

    def __class_decl(self, ctx: dict) -> Optional[ClassDecl]:
        if self.__token_is(TokenVals.CLASS):
            class_name: Optional[str] = self.__token_expect(TokenVals.ID, same_line=True)
            parent_name: Optional[str] = None
            if self.__token_is(TokenVals.INHERITS, same_line=True):
                parent_name = self.__token_expect(TokenVals.ID, same_line=True)
            result = ClassDecl(self.curr_line_index, parent_name)
            result.add_sub_node(Identifier(self.curr_line_index, class_name))
            ctx[ClassDecl.IN_CLASS] = True
            self.__expect_new_line()
            result.add_sub_node(self.__tree_expect(ctx,
                                                   self.__class_block,
                                                   "Non-empty class declaration block required"),
                                children=True)
            self.__expect_new_line()
            self.__token_must_be(TokenContents.ENDCLASS, TokenVals.ENDCLASS)
            self.__expect_new_line()
            ctx.pop(ClassDecl.IN_CLASS, False)
            return result

    def __class_block(self, ctx: dict) -> Optional[ClassBlock]:
        result: Optional[ClassBlock] = None
        while (class_member := self.__class_member(ctx)) is not None:
            if result is None:
                result = ClassBlock(class_member.line_index)
            result.add_sub_node(class_member)
            self.__expect_new_line()
        return result

    def __class_member(self, ctx: dict) -> Optional[ClassMember]:
        if self.__token_is(TokenVals.PUBLIC):
            is_public = True
            self.__expect_no_newline()
        elif self.__token_is(TokenVals.PRIVATE):
            is_public = False
            self.__expect_no_newline()
        else:
            is_public = True
        ctx[ClassMember.IS_PUBLIC_FLAG] = is_public
        procs = [self.__attr_decl, self.__proc_decl, self.__fun_decl]
        for proc in procs:
            if (result := proc(ctx)) is not None:
                ctx.pop(ClassMember.IS_PUBLIC_FLAG, False)
                return ClassMember(result.line_index, is_public).add_sub_node(result)

    def __attr_decl(self, ctx: dict) -> Optional[AttrDecl]:
        if (arr_decl := self.__arr_decl(ctx)) is not None:
            return arr_decl
        if (attr_name := self.__token_str(TokenVals.ID)) is not None:
            result = AttrDecl(self.curr_line_index).add_sub_node(Identifier(self.curr_line_index, attr_name))
            if (member_init := self.__member_init(ctx)) is not None:
                result.add_sub_node(member_init)
            return result

    def __member_init(self, ctx: dict) -> Optional[Expr]:
        if self.__token_is(TokenVals.EQUALS, same_line=True):
            self.__expect_no_newline()
            return self.__tree_expect(ctx,
                                      self.__expr,
                                      f"'{TokenContents.EQUALS.value}' detected but no subsequent value found")

    # ------------------------------------------------------------------------------------------------------------------------------------------------------------------- #

    def __token_is(self, val: TokenVals, name: str = None, same_line: bool = False) -> bool:
        """Checks the next token matches the given value. Optionally, it enforces the given token string.

        :param val: token value to check against.
        :param name: optional name of the token to enforce.
        :return: true if the token value matches, false otherwise.
        """
        n = self.__token_str(val, same_line)
        if n and name and n != name:
            self.__raise_error(SyntaxError(f"Expected {name}, received {n}"))
        return n is not None

    def __token_add_op(self) -> Optional[AddOp]:
        """Checks that the next token is an additive operator and if so, it returns its AST node

        :return: an AddOp node if matching, None otherwise
        """
        t: ParsedToken = self.__lexer.next()
        if t:
            if t.val in [TokenVals.PLUS, TokenVals.MINUS]:
                assert t.text in [TokenContents.PLUS.value, TokenContents.MINUS.value]
                self.curr_line_index = t.line_index
                return AddOp(self.curr_line_index, t.val)
            self.__lexer.push_front(t)
        return None

    def __token_mul_op(self) -> Optional[MulOp]:
        """Checks that the next token is a multiplicative operator and if so, it returns its AST node

        :return: a MulOp node if matching, None otherwise
        """
        t: ParsedToken = self.__lexer.next()
        if t:
            if t.val in [TokenVals.MUL, TokenVals.DIV, TokenVals.INT_DIV, TokenVals.MOD]:
                assert t.text in [TokenContents.MUL.value, TokenContents.DIV.value, TokenContents.INT_DIV.value,
                                  TokenContents.MOD.value]
                self.curr_line_index = t.line_index
                return MulOp(self.curr_line_index, t.val)
            self.__lexer.push_front(t)
        return None

    def __token_pow_op(self) -> Optional[PowOp]:
        """Checks that the next token is an index-level operator and if so, it returns its AST node

        :return: a PowOp node if matching, None otherwise
        """
        t: ParsedToken = self.__lexer.next()
        if t:
            if t.val in [TokenVals.POW]:
                assert t.text in [TokenContents.POW.value]
                self.curr_line_index = t.line_index
                return PowOp(self.curr_line_index, t.val)
            self.__lexer.push_front(t)
        return None

    def __token_comp_op(self) -> Optional[CompOp]:
        """Checks that the next token is a comparison operator and if so, it returns its AST node

        :return: a CompOp node if matching, None otherwise
        """
        t: ParsedToken = self.__lexer.next()
        if t:
            if t.val in [TokenVals.EQ, TokenVals.NEQ, TokenVals.GREATER, TokenVals.GREATER_EQ,
                         TokenVals.LOWER, TokenVals.LOWER_EQ]:
                assert t.text in [TokenContents.EQ.value, TokenContents.NEQ.value, TokenContents.GREATER.value,
                                  TokenContents.GREATER_EQ.value, TokenContents.LOWER.value,
                                  TokenContents.LOWER_EQ.value]
                self.curr_line_index = t.line_index
                return CompOp(self.curr_line_index, t.val)
            self.__lexer.push_front(t)
        return None

    def __token_str(self, val: TokenVals, same_line: bool = False) -> Optional[str]:
        """Gets the string of a token if it matches the given token value. It returns None on non-match.

        :param val: token value to match against.
        :param same_line: flag to indicate whether, if a matching token exists, it should throw an error if the matching token
        is not on the same line as the last token.
        :return: string of matching token or None if not matching.
        """
        t: ParsedToken = self.__lexer.next()
        if t:
            if t.val == val:
                if same_line and not self.__token_on_same_line(t):
                    self.__raise_error(SyntaxError(f"'{t}' should be not be on a separate line"))
                self.curr_line_index = t.line_index
                return t.text
            self.__lexer.push_front(t)
        return None

    def __int_token(self, same_line: bool = False) -> Optional[int]:
        """Gets the integer corresponding the upcoming token or None if no integer or no token.

        :return: token value as an integer or None if no integer.
        """
        result: Optional[str] = self.__token_str(TokenVals.INT, same_line)
        if result:
            return int(result)
        return None

    def __str_token(self, same_line: bool = False) -> Optional[str]:
        """Gets the string corresponding the upcoming token or None if no string literal or no token.

        :return: token value as a string or None if no string
        """
        return self.__token_str(TokenVals.STRING, same_line)

    def __num_token(self, same_line: bool = False) -> Optional[float]:
        """Gets the decimal number corresponding the upcoming token or None if no float literal or no token

        :return: token value as a float or None if no float
        """
        result: Optional[str] = self.__token_str(TokenVals.NUM, same_line)
        if result:
            return float(result)
        return None

    def __bool_token(self, same_line: bool = False) -> Optional[bool]:
        if self.__token_is(TokenVals.TRUE, same_line=same_line):
            return True
        if self.__token_is(TokenVals.FALSE, same_line=same_line):
            return False
        return None

    def __token_expect(self, val: TokenVals, same_line: bool = False) -> str:
        """Expects a token with the given value and returns its string. It raises an error if no match.

        :param val: token value to expect.
        :return: token string.
        """
        t: ParsedToken = self.__lexer.next()
        expected_t = ParsedToken().set_val(val)
        if t:
            if t.val == val:
                if same_line and not self.__token_on_same_line(t):
                    self.__raise_error(SyntaxError(f"'{t}' should not be in a separate line"))
                self.curr_line_index = t.line_index
                return t.text
            else:
                self.__raise_error(SyntaxError(f"Expected '{expected_t}', received '{t}'"))
        self.__raise_error(SyntaxError(f"Expected '{expected_t}' before end of file"))

    def __token_must_be(self, name: str | TokenContents, val: TokenVals, same_line: bool = False):
        """Enforces that the upcoming token is of given name and value.

        :param name: token string to expect.
        :param val: token value to expect.
        :return: None
        """
        n = self.__token_expect(val, same_line)
        name_str: str = name if type(name) == str else name.value
        if n != name_str:
            self.__raise_error(SyntaxError(f"Expected '{name}', received '{n}'"))

    def __expect_new_line(self):
        """
        Throws error if the next token is not on a new line
        :return: None
        """
        t: Optional[ParsedToken] = self.__lexer.next()
        if t is not None:
            if self.__token_on_same_line(t):
                self.__raise_error(SyntaxError(f"Newline expected before '{t}'"))
            self.__lexer.push_front(t)

    def __expect_no_newline(self) -> None:
        """
        Throws error if the next token is on a new line
        :return: None
        """
        t: Optional[ParsedToken] = self.__lexer.next()
        if t is not None:
            if not self.__token_on_same_line(t):
                self.__raise_error(SyntaxError(f"'{t}' should not be in a new line"))
            self.__lexer.push_front(t)

    def __token_on_same_line(self, token: ParsedToken) -> bool:
        """
        Utility function to indicate presence of a newline
        :param token: the token which can be on a newline or the same line as the previous token
        :return: True if the next token is on the same line as the previous token, otherwise False
        """
        if self.curr_line_index is not None:
            if token.line_index < self.curr_line_index:
                raise RuntimeError("Line index of the next token should never be lower than that of the previous one")
            if token.line_index > self.curr_line_index:
                return False
        return True

    def __tree_expect(self, ctx: dict, parse_method: Callable, err_msg: str) -> Node:
        """Enforces that a parsing method returns a syntax tree and not None.

        :param ctx: parsing context to pass on to the parsing method.
        :param parse_method: function that returns the parsing tree.
        :param err_msg: error message to raise if parsing fails.
        :return: parsed syntax tree.
        """
        result: Optional[Node] = parse_method(ctx)
        if result:
            return result
        self.__raise_error(SyntaxError(err_msg))

    def __raise_error(self, e: Exception):
        if self.on_error is not None:
            self.on_error(e, [self.curr_line_index])
        raise e
