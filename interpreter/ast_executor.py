import operator
import logging
import os
from typing import Callable, Type, Dict, TypeVar, Optional, List, Tuple
from parsed_ast import Node, Program, VarAssign, Identifier, IntLiteral, ArrayDecl, AddrMember, ExprList, \
    Expr, Term, Factor, UnaryMinus, IfElse, UnaryNot, SwitchCase, ForLoop, GoToInstr, InnerInstrBlock, DoUntil, \
    WhileLoop, StrLiteral, NumLiteral, PrintInstr, FunDecl, AddrIdOrCall, Param, ReturnInstr, CallableSuffix, ProcDecl, CastStr, CastInt, CastFloat, Length, StrSubstring, Input, EndOfFile, \
    ReadLine, WriteLine, FileClose, OpenRead, OpenWrite, ClassDecl, NewExpr, AddrExpr, ClassMember, AttrDecl, BoolLiteral, \
    Comparison, Disjunction, ArithmExpr, Op
from parsed_token import TokenVals, KNOWN_TOKEN_VALS, TokenContents
from parser import Parser
from sym_table import V, SymTable, ArrayVal, SymAddr, NullVal, ObjSymTable
from io import TextIOWrapper

#
# Generic types for evaluation results and symbol table keys, respectively
#
T = TypeVar("T")
K = TypeVar("K")


class ExeCtx(Dict[K, V]):
    """Execution context storing all the data required for AST execution as it progresses.

    The execution context contains the following essential pieces of information:
      - the global symbol table, a hierarchical dictionary holding the recognized names in the program and their values
      - the current symbol table, reference to a temporary sub-table within the global table
      - the evaluation result, an entry storing the result of the last expression evaluation
      - a 'global' flag indicating what symbol table names should be looked up into: the current symbol table or the root one
      - a flag indicating if a continue statement has been detected, which will be reset to False when the execution returns to the outer loop
      - a flag indicating if a break statement has been detected, which will be reset to False when the execution returns to the outer loop
      - a flag indicating if a return statement has been detected, which will be reset to False when the execution returns to a function or procedure
      - a flag indicating if the current instruction is inside a loop
      - a flag indicating if the current instruction is inside a function
      - a flag indicating if the current instruction is inside a procedure
      - a list of the names of parameters passed by reference in subroutine calls (default = [])
      - the outer object in which current execution takes place (e.g. if an object method is called, this value is set to the object storing that method) (default = None)
    """
    GLOBAL_SYM_TABLE: str = "__GLOBAL_SYM_TABLE__"
    CUR_SYM_TABLE: str = "__CUR_SYM_TABLE__"
    EVAL_RESULT: str = "__EVAL_RESULT__"
    IS_GLOBAL: str = "__IS_GLOBAL__"
    CONTINUE_DETECTED: str = "__CONTINUE_DETECTED__"
    BREAK_DETECTED: str = "__BREAK_DETECTED__"
    RETURN_DETECTED: str = "__RETURN_DETECTED__"
    INSIDE_LOOP: str = "__INSIDE_LOOP__"
    INSIDE_FUNC: str = "__INSIDE_FUNC__"
    INSIDE_PROC: str = "__INSIDE_PROC__"
    BY_REF_PARAMS: str = "__BY_REF_PARAMS__"
    OUTER_CLASS: str = "__OUTER_CLASS__"

    def __init__(self):
        super().__init__()
        self[ExeCtx.GLOBAL_SYM_TABLE] = SymTable()
        self[ExeCtx.CUR_SYM_TABLE] = self.global_table

    @property
    def global_table(self) -> SymTable:
        return self[ExeCtx.GLOBAL_SYM_TABLE]

    @property
    def cur_table(self) -> SymTable:
        return self[ExeCtx.CUR_SYM_TABLE]

    @cur_table.setter
    def cur_table(self, tbl: SymTable):
        self[ExeCtx.CUR_SYM_TABLE] = tbl

    @property
    def is_global(self) -> bool:
        return self.get(ExeCtx.IS_GLOBAL, False)

    @is_global.setter
    def is_global(self, val: bool):
        self[ExeCtx.IS_GLOBAL] = val

    @property
    def eval_result(self) -> Optional[V]:
        return self.get(ExeCtx.EVAL_RESULT, None)

    @eval_result.setter
    def eval_result(self, val: Optional[V] = None):
        if val is None:
            self.pop(ExeCtx.EVAL_RESULT, None)
        else:
            self[ExeCtx.EVAL_RESULT] = val

    @property
    def continue_detected(self) -> bool:
        return self.get(ExeCtx.CONTINUE_DETECTED, False)

    @continue_detected.setter
    def continue_detected(self, val: bool):
        self[ExeCtx.CONTINUE_DETECTED] = val

    @property
    def break_detected(self) -> bool:
        return self.get(ExeCtx.BREAK_DETECTED, False)

    @break_detected.setter
    def break_detected(self, val: bool):
        self[ExeCtx.BREAK_DETECTED] = val

    @property
    def return_detected(self) -> bool:
        return self.get(ExeCtx.RETURN_DETECTED, False)

    @return_detected.setter
    def return_detected(self, val: bool):
        self[ExeCtx.RETURN_DETECTED] = val

    @property
    def inside_loop(self) -> bool:
        return self.get(ExeCtx.INSIDE_LOOP, False)

    @inside_loop.setter
    def inside_loop(self, val: bool):
        self[ExeCtx.INSIDE_LOOP] = val

    @property
    def inside_function(self) -> bool:
        return self.get(ExeCtx.INSIDE_FUNC, False)

    @inside_function.setter
    def inside_function(self, val: bool):
        self[ExeCtx.INSIDE_FUNC] = val

    @property
    def inside_procedure(self) -> bool:
        return self.get(ExeCtx.INSIDE_PROC, False)

    @inside_procedure.setter
    def inside_procedure(self, val: bool):
        self[ExeCtx.INSIDE_PROC] = val

    @property
    def by_ref_params(self) -> list[str]:
        return self.get(ExeCtx.BY_REF_PARAMS, [])

    @by_ref_params.setter
    def by_ref_params(self, val: list[str]):
        self[ExeCtx.BY_REF_PARAMS] = val

    @property
    def outer_class(self) -> Optional[ObjSymTable]:
        assert isinstance(self.get(ExeCtx.OUTER_CLASS, None), Optional[ObjSymTable])
        return self.get(ExeCtx.OUTER_CLASS, None)

    @outer_class.setter
    def outer_class(self, val: ObjSymTable):
        self[ExeCtx.OUTER_CLASS] = val


class AstExecutor:
    """
    The executor of the AST, recursively interpreting each node (calling pre-callbacks before execution and calling post-callbacks after execution of each node)
    """

    __FILE_READ_MODE: str = "r+"
    __FILE_WRITE_MODE: str = "w+"
    __FILE_STREAM_TYPE: str = TextIOWrapper

    def __init__(self, parser: Parser, pre_callback: Optional[Callable] = None, post_callback: Optional[Callable] = None, output_stream=None, on_error: Optional[Callable] = None):
        self.__parser = parser
        self.__pre_callbacks = [pre_callback] if pre_callback else []
        self.__post_callbacks = [post_callback] if post_callback else []
        self.push_callback(self.__log_pre)
        self.push_callback(self.__log_post, True)
        #
        # Maps the node type to the appropriate method to execute or evaluate the node
        #
        self.__EXECUTORS: dict[Type, Callable] = {
            Node: self.__execute_node,
            Program: self.__execute_program,
            InnerInstrBlock: self.__execute_inner_instr_block,
            VarAssign: self.__execute_var_assign,
            ArrayDecl: self.__execute_array_decl,
            IfElse: self.__execute_if_else,
            SwitchCase: self.__execute_switch_case,
            ForLoop: self.__execute_for_loop,
            GoToInstr: self.__execute_go_to_instr,
            DoUntil: self.__execute_do_until_loop,
            WhileLoop: self.__execute_while_loop,
            PrintInstr: self.__execute_print_instr,
            FunDecl: self.__execute_fun_decl,
            ProcDecl: self.__execute_proc_decl,
            ClassDecl: self.__execute_class_decl,
            ReturnInstr: self.__execute_return_instr,
            AddrIdOrCall: self.__eval_addr_id_or_call,
            AddrExpr: self.__eval_addr_expr,
            ReadLine: self.__eval_read_line,
            WriteLine: self.__eval_write_line,
            FileClose: self.__eval_file_close,
        }
        self.__EVALUATORS: dict[Type, Callable] = {
            AddrMember: self.__eval_addr_member,
            Expr: self.__eval_expr,
            Disjunction: self.__eval_disjunction,
            Comparison: self.__eval_comparison,
            ArithmExpr: self.__eval_arithm_expr,
            Term: self.__eval_term,
            Factor: self.__eval_factor,
            UnaryMinus: self.__eval_unary_minus,
            UnaryNot: self.__eval_unary_not,
            ExprList: self.__eval_expr_list,
            AddrExpr: self.__eval_addr_expr,
            Identifier: self.__eval_identifier,
            IntLiteral: AstExecutor.__eval_int_literal,
            StrLiteral: AstExecutor.__eval_str_literal,
            NumLiteral: AstExecutor.__eval_num_literal,
            BoolLiteral: AstExecutor.__eval_bool_literal,
            AddrIdOrCall: self.__eval_addr_id_or_call,
            NewExpr: self.__eval_new_expr,
            CastStr: self.__eval_str_cast,
            CastInt: self.__eval_int_cast,
            CastFloat: self.__eval_float_cast,
            Length: self.__eval_length,
            StrSubstring: self.__eval_str_substring,
            Input: self.__eval_input,
            EndOfFile: self.__eval_end_of_file,
            ReadLine: self.__eval_read_line,
            WriteLine: self.__eval_write_line,
            FileClose: self.__eval_file_close,
            OpenRead: self.__eval_open_read,
            OpenWrite: self.__eval_open_write,
        }
        #
        # Maps node type to the right 'addresser' method, which returns a SymAddr instance
        #
        self.__ADDRESSERS: dict[Type, Callable] = {
            AddrMember: self.__address_addr_member,
            Identifier: self.__address_identifier,
            AddrIdOrCall: self.__address_addr_id_or_call,
            AddrExpr: self.__address_addr_expr
        }
        #
        # Maps the TokenVal member representing the operator to the correct operation and the allowed operand types
        #
        self.OP_TO_OPERATION: dict[TokenVals, Callable] = {
            TokenVals.PLUS: operator.add,
            TokenVals.MINUS: operator.sub,
            TokenVals.DIV: operator.truediv,
            TokenVals.POW: operator.pow,
            TokenVals.MUL: operator.mul,
            TokenVals.INT_DIV: operator.floordiv,
            TokenVals.MOD: operator.mod,
            TokenVals.EQ: operator.eq,
            TokenVals.LOWER: operator.lt,
            TokenVals.LOWER_EQ: operator.le,
            TokenVals.GREATER: operator.gt,
            TokenVals.GREATER_EQ: operator.ge,
            TokenVals.NEQ: operator.ne,
            TokenVals.AND: operator.and_,
            TokenVals.OR: operator.or_,
        }
        self.OP_TO_ACCEPTED_TYPES: dict[TokenVals, list[list[type, type]]] = {
            TokenVals.PLUS: [[int, int], [int, float], [float, float], [str, str]],
            TokenVals.MINUS: [[int, int], [float, float], [int, float]],
            TokenVals.DIV: [[int, int], [int, float], [float, float]],
            TokenVals.POW: [[int, int], [int, float], [float, float]],
            TokenVals.MUL: [[int, int], [int, float], [float, float], [int, str]],
            TokenVals.INT_DIV: [[int, int], [int, float], [float, float]],
            TokenVals.MOD: [[int, int], [int, float], [float, float]],
            TokenVals.EQ: [],
            TokenVals.LOWER: [[int, int], [int, float], [float, float]],
            TokenVals.LOWER_EQ: [[int, int], [int, float], [float, float]],
            TokenVals.GREATER: [[int, int], [int, float], [float, float]],
            TokenVals.GREATER_EQ: [[int, int], [int, float], [float, float]],
            TokenVals.NEQ: [],
            TokenVals.AND: [[bool, bool]],
            TokenVals.OR: [[bool, bool]],
        }
        #
        # I/O stream used by print() and input() statements. If None, the default console buffer will be used, but if the output
        # needs to be a different location it can be customised when the ASTExecutor is instantiated.
        #
        self.__output_stream = output_stream
        #
        # Stores instance counts of each class to be able to allocate a unique key for newly instantiated objects
        #
        self.__instance_count = {}
        #
        # Stores method to call when an error is thrown
        #
        self.__on_error = on_error
        #
        # Store for the node which caused the errors
        #
        self.__erroneous_nodes = []

    def push_callback(self, callback: Callable, post: bool = False):
        if post:
            self.__post_callbacks.insert(0, callback)
        else:
            self.__pre_callbacks.append(callback)

    def pop_callback(self, post: bool = False) -> Callable:
        if post:
            result: Callable = self.__post_callbacks.pop(0)
        else:
            result: Callable = self.__pre_callbacks.pop(-1)
        return result

    def execute(self):
        """
        Executes the AST from its root node, if it exists, creating an empty execution context
        (which contains an empty global symbol table).
        :return: None
        """
        parsed: Node = self.__parser.parse()
        if parsed:
            ctx = ExeCtx()
            try:
                self.__execute(parsed, ctx)
            except BaseException as e:
                if self.__on_error is not None:
                    self.__on_error(e, self.__erroneous_nodes)
                raise e
            ctx.global_table.close()

    # ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------- #

    def __execute(self, node: Node | Type[Node], ctx: ExeCtx):
        if (not ctx.break_detected) and (not ctx.continue_detected) and (not ctx.return_detected):
            self.__run_on_node_with_callbacks(node,
                                              ctx,
                                              self.__EXECUTORS[type(node)],
                                              lambda e: f"Error while executing {node.__class__.__name__}: {type(e).__name__} - {e}",
                                              no_run_msg="Nothing to execute")

    def __run_on_node_with_callbacks(self, node: Node, ctx: ExeCtx, proc: Callable, err_msg: Callable,
                                     no_run_msg: str = None):
        """
        Runs the given procedure on a node, first calling pre-callbacks, then calling the subroutine, handling any errors thrown
        and running post-callbacks if no errors are thrown.
        :param node: the node being passed
        :param ctx: the current execution context
        :param proc: the method being executed using the node
        :param err_msg: error message for logging
        :param no_run_msg: message for logging when node given is null
        :return: None
        """
        if node:
            self.__call_back(node, ctx)
            try:
                proc(node, ctx)
            except BaseException as e:
                #
                # If no erroneous nodes are recorded, then the node being executed when the error was raised is set as the erroneous node
                #
                if not self.__erroneous_nodes:
                    self.__erroneous_nodes = [node]
                logging.error(err_msg(e))
                raise e
            else:
                self.__call_back(node, ctx, True)
        else:
            msg: str = no_run_msg if no_run_msg else "Nothing to run"
            logging.warning(msg)

    def __execute_node(self, node: Node, ctx: ExeCtx):
        for sn in node.sub_nodes:
            self.__execute(sn, ctx)

    def __execute_program(self, prg: Program, ctx: ExeCtx):
        self.__execute_node(prg, ctx)

    def __execute_inner_instr_block(self, inner_instr_block: InnerInstrBlock, ctx: ExeCtx):
        for instr in inner_instr_block.sub_nodes:
            self.__execute(instr, ctx)

    def __execute_var_assign(self, var_assign: VarAssign, ctx: ExeCtx):
        """
        Executes variable assignment, obtaining the address to write the new value, evaluating the value to be written
        and writing the value at that address. Note: if the value being overwritten is an open file stream, a SyntaxError will be thrown
        :param var_assign: VarAssign node containing the address expression and new value
        :param ctx: current execution context
        :return: None
        """
        eval_result: T = self.__eval(var_assign.sub_nodes[1], ctx)
        ctx.is_global = var_assign.is_global
        result_addr: SymAddr = self.__address(var_assign.sub_nodes[0], ctx)
        #
        # Checks that the value being overwritten (if it exists) is not an open file stream
        #
        if not result_addr.has_none_value and isinstance(result_addr.value, AstExecutor.__FILE_STREAM_TYPE):
            file_value: AstExecutor.__FILE_STREAM_TYPE = result_addr.value
            if not file_value.closed:
                self.__raise_error([var_assign], SyntaxError(f"There is an open file at '{result_addr.name}'. It must be closed before the variable is overwritten"))
        result_addr.value = eval_result

    def __execute_array_decl(self, array_decl: ArrayDecl, ctx: ExeCtx):
        """
        Executes array declaration, creating an ArrayVal with the given dimensions
        :param array_decl: ArrayDecl node containing array identifier and its dimensions
        :param ctx: current execution context
        :return: None
        """
        sym_table: SymTable = ctx.cur_table
        sym_table = sym_table.root if array_decl.is_global else sym_table
        dims: list[int] = []
        for dim in array_decl.dims:
            evaluated_dim = self.__eval(dim, ctx)
            if type(evaluated_dim) != int:
                self.__raise_error([array_decl], SyntaxError(f"Non-integer dimension given in declaration of the array '{array_decl.name}'"))
            dims.append(evaluated_dim)
        sym_table.update_symbol(array_decl.name, ArrayVal(dims))

    def __execute_if_else(self, if_else: IfElse, ctx: ExeCtx):
        """
        Executes IfElse by evaluating conditions and running the block if true
        :param if_else: IfElse node containing conditions and the associated block
        :param ctx: current execution context
        :return: None
        """
        sub_nodes = if_else.sub_nodes
        #
        # Executing if and elseif statements
        #
        for i in range(0, len(sub_nodes), 2):
            #
            # If there is only one sub-node left, that sub-node represents an else statement
            #
            is_else = i + 1 >= len(sub_nodes)
            if is_else:
                self.__execute(sub_nodes[i], ctx)
                return
            condition = sub_nodes[i]
            instr_block = sub_nodes[i + 1]
            evaluated_condition: T = self.__eval(condition, ctx)
            if type(evaluated_condition) != bool:
                self.__raise_error([condition], TypeError(f"Non-boolean expression '{evaluated_condition}' used as condition in if-else statement"))
            if evaluated_condition:
                self.__execute(instr_block, ctx)
                return

    def __execute_switch_case(self, switch_case: SwitchCase, ctx: ExeCtx):
        """
        Executes SwitchCase by evaluating given argument and testing it against given case expressions,
        executing the right case block
        :param switch_case: the SwitchCase node containing expression to check and case statements
        :param ctx: current execution context
        :return: None
        """
        sub_nodes: list[Node] = switch_case.sub_nodes
        value_to_check_against = self.__eval(switch_case.sub_nodes[0], ctx)
        #
        # Executing case statements
        #
        for i in range(1, len(sub_nodes), 2):
            nodes = sub_nodes[i:i + 2]
            is_default_case: bool = len(nodes) == 1
            if is_default_case:
                self.__execute(nodes[0], ctx)
                return
            case_condition, instr_block = tuple(nodes)
            evaluated_case_condition: T = self.__eval(case_condition, ctx)
            if evaluated_case_condition == value_to_check_against:
                self.__execute(instr_block, ctx)
                return

    def __execute_go_to_instr(self, go_to_instr: GoToInstr, ctx: ExeCtx):
        """
        Used for executing break and continue instructions, enabling the correct context flag
        :param go_to_instr: GoToInstr to execute, storing which type of go-to instruction it is, break or continue
        :param ctx: current execution context
        :return: None
        """
        go_to_instr_val: TokenVals = go_to_instr.val
        if not ctx.inside_loop:
             self.__raise_error([go_to_instr], SyntaxError(f"'{go_to_instr_val}' not allowed outside a loop"))
        assert go_to_instr_val in [TokenVals.CONTINUE, TokenVals.BREAK]
        if go_to_instr_val == TokenVals.CONTINUE:
            ctx.continue_detected = True
        elif go_to_instr_val == TokenVals.BREAK:
            ctx.break_detected = True

    def __execute_for_loop(self, for_loop: ForLoop, ctx: ExeCtx):
        """
        Executes ForLoop by evaluating given identifier, iterating through values in given range and executing block
        :param for_loop: ForLoop containing identifier, range info and the block
        :param ctx: current execution context
        :return: None
        """
        var: Identifier = for_loop.sub_nodes[0]
        #
        # Flag to check if the loop is contained in an outer loop
        #
        in_outer_loop: bool = ctx.inside_loop
        lower_bound = self.__eval(for_loop.sub_nodes[1], ctx)
        upper_bound = self.__eval(for_loop.sub_nodes[2], ctx)
        block = for_loop.sub_nodes[3]
        if type(lower_bound) != int:
            self.__raise_error([for_loop.sub_nodes[1]], TypeError(f"Non-integer value '{lower_bound}' not valid for lower bound of for loop"))
        if type(upper_bound) != int:
            self.__raise_error([for_loop.sub_nodes[2]], TypeError(f"Non-integer value '{upper_bound}' not valid for upper bound of for loop"))
        ctx.inside_loop = True
        for value in range(lower_bound, upper_bound + 1):
            #
            # Assigns value in the range to the variable
            #
            self.__address(var, ctx).value = value
            #
            # Executes block
            #
            self.__execute(block, ctx)
            if ctx.continue_detected:
                ctx.continue_detected = False
            if ctx.break_detected:
                ctx.break_detected = False
                break
            if ctx.return_detected:
                break
        ctx.inside_loop = in_outer_loop

    def __execute_do_until_loop(self, do_until: DoUntil, ctx: ExeCtx):
        """
        Executes DoUntil by running block continuously and breaking the loop if the condition evaluates to true
        :param do_until: DoUntil node where the block and condition is stored
        :param ctx: current execution context
        :return: None
        """
        block = do_until.get_sub_node(0)
        condition = do_until.get_sub_node(1)
        #
        # Flag to check if the loop is in an outer loop
        #
        in_outer_loop: bool = ctx.inside_loop
        ctx.inside_loop = True
        while True:
            #
            # Execute block continuously until condition evaluates to true
            #
            self.__execute(block, ctx)
            #
            # Handling continue and break statements from within block
            #
            if ctx.continue_detected:
                ctx.continue_detected = False
            if ctx.break_detected:
                ctx.break_detected = False
                break
            if ctx.return_detected:
                break
            evaluated_condition: T = self.__eval(condition, ctx)
            if type(evaluated_condition) != bool:
                self.__raise_error([do_until], TypeError(f"Non-boolean expression '{evaluated_condition}' used as condition in do-until condition "))
            if evaluated_condition:
                break
        ctx.inside_loop = in_outer_loop

    def __execute_while_loop(self, while_loop: WhileLoop, ctx: ExeCtx):
        """
        Executes WhileLoop by evaluating condition and executing block if condition is true
        :param while_loop: WhileLoop node containing the condition and block
        :param ctx: current execution context
        :return: None
        """
        condition: Node = while_loop.get_sub_node(0)
        block: Node = while_loop.get_sub_node(1)
        #
        # Flag to check if the loop is in an outer loop
        #
        in_outer_loop: bool = ctx.inside_loop
        ctx.inside_loop = True
        while True:
            #
            # Handling continue and break statements from within block
            #
            if ctx.continue_detected:
                ctx.continue_detected = False
            if ctx.break_detected:
                ctx.break_detected = False
                break
            if ctx.return_detected:
                break
            #
            # Break continuous execution of block if condition evaluates to false
            #
            evaluated_condition = self.__eval(condition, ctx)
            if type(evaluated_condition) != bool:
                self.__raise_error([condition], TypeError(f"Non-boolean expression '{evaluated_condition}' used as condition in while loop condition "))
            if not evaluated_condition:
                break
            self.__execute(block, ctx)

        ctx.inside_loop = in_outer_loop

    def __execute_print_instr(self, print_node: PrintInstr, ctx: ExeCtx):
        """
        Executes PrintInstr by evaluating each of its arguments and printing them, delimited by commas
        :param print_node: PrintInstr containing arguments
        :param ctx: current execution context
        :return: None
        """
        evaluated_args = [self.__eval(print_arg, ctx) for print_arg in print_node.sub_nodes]
        output: str = ", ".join(str(arg) for arg in evaluated_args)
        print(output, file=self.__output_stream)
        logging.debug(f"CONSOLE OUTPUT: {output}")

    def __execute_fun_decl(self, fun_decl: FunDecl, ctx: ExeCtx):
        result_addr: SymAddr = self.__address(fun_decl.sub_nodes[0], ctx)
        result_addr.value = fun_decl

    def __execute_proc_decl(self, proc_decl: ProcDecl, ctx: ExeCtx):
        result_addr: SymAddr = self.__address(proc_decl.sub_nodes[0], ctx)
        result_addr.value = proc_decl

    def __execute_class_decl(self, class_decl: ClassDecl, ctx: ExeCtx):
        result_addr: SymAddr = self.__address(class_decl.sub_nodes[0], ctx)
        result_addr.value = class_decl

    def __execute_return_instr(self, return_instr: ReturnInstr, ctx: ExeCtx):
        """
        Executes ReturnInstr by obtaining and evaluating its expression sub-node, writing the result to ctx.eval_result
        and enabling the return_detected context flag.
        :param return_instr: the ReturnInstr node to execute
        :param ctx: current execution context
        :return: None, value to be returned is written to ctx.eval_result
        """
        return_val: Optional[T] = self.__eval(return_instr.sub_nodes[0], ctx) if return_instr.sub_nodes else None
        if return_val is None and ctx.inside_function:
            self.__raise_error([return_instr], SyntaxError("Functions can only return non-null values"))
        if return_val is not None and ctx.inside_procedure:
            self.__raise_error([return_instr], SyntaxError("Procedures can only return null values"))
        if not ctx.inside_procedure and not ctx.inside_function:
            self.__raise_error([return_instr], SyntaxError("Return statements cannot exist outside of a function or procedure"))
        ctx.return_detected = True
        ctx.eval_result = return_val

    # ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------- #

    def __address(self, node: Node, ctx: ExeCtx) -> SymAddr:
        """
        Calls the relevant addresser method based on the given node's type, along with callbacks for proper error handling and logging.
        :param node: the node to obtain an address from
        :param ctx: current execution context
        :return: SymAddr instance pointing to the entry in symbol table referenced by the given node
        """
        def address_collector(n: Node, c: ExeCtx):
            ctx.eval_result = self.__ADDRESSERS[type(n)](n, c)

        self.__run_on_node_with_callbacks(node, ctx, address_collector,
                                          lambda e: f"Error while addressing {node.__class__.__name__}: {type(e).__name__} - {e}",
                                          "Nothing to execute")
        result: SymAddr = ctx.eval_result
        ctx.eval_result = None
        return result

    def __address_addr_expr(self, addr_expr: AddrExpr, ctx: ExeCtx) -> SymAddr:
        """
        Requires address be returned from evaluating the given AddrExpr
        :param addr_expr: the AddrExpr to address
        :param ctx: current execution context
        :return: SymAddr instance pointing to entry in symbol table referenced by the address expression
        """
        return self.__eval_addr_expr(addr_expr, ctx, require_address=True)

    def __address_addr_member(self, addr_member: AddrMember, ctx: ExeCtx) -> SymAddr:
        """
        Used for addressing identifiers that reference arrays, also handling indexed addressing if any indexes are provided
        in the given AddrMember node
        :param addr_member: Stores identifier data and indexes (if any)
        :param ctx: current execution context
        :return: indexed address to the element at the correct index
        """
        result: SymAddr = self.__address(addr_member.sub_nodes[0], ctx)
        if len(addr_member.sub_nodes) > 1:
            indexes: T = self.__eval(addr_member.sub_nodes[1], ctx)
            if isinstance(indexes, List):
                result = result.addr_of(indexes)
            elif isinstance(indexes, int):
                result = result.addr_of([indexes])
            else:
                self.__raise_error([addr_member], RuntimeError(f"Indexes of invalid type: {indexes}"))
        return result

    def __address_identifier(self, identifier: Identifier, ctx: ExeCtx) -> SymAddr:
        """
        Returns a SymAddr instance pointing to the variable with the given identifier in its symbol table.
        :param identifier: the Identifier node containing the variable name
        :param ctx: current execution context
        :return: a SymAddr instance referencing the variable named in the given Identifier node.
        """
        tbl: SymTable = ctx.cur_table
        tbl = tbl if not ctx.is_global else tbl.root
        name: str = identifier.name
        may_be_ref: bool = name in ctx.by_ref_params
        #
        # If the name is the same as that of a field in the object that contains the current context,
        # an address to that field is returned. If the name is the same as that of a private field of any parent classes,
        # an error is raised to prevent the returned address from being used to access private field of parent classes.
        #
        if ctx.outer_class:
            ret: Optional[Tuple[V, SymTable]] = ctx.outer_class.lookup_symbol_with_table(name)
            if ret is not None:
                _, _tbl = ret
                if _tbl == ctx.outer_class:
                    return _tbl.addr_of(name, may_be_ref=may_be_ref)
                if not _tbl.is_symbol_public(name):
                    self.__raise_error([identifier], SyntaxError(f"Cannot reference private field '{name}'"))
        #
        # Otherwise, return address with sym_table set to the local or global scope.
        #
        return tbl.addr_of(identifier.name, may_be_ref=may_be_ref)

    def __address_addr_id_or_call(self, addr_id_or_call: AddrIdOrCall, ctx: ExeCtx) -> SymAddr:
        """
        Evaluates subroutine call and returns its output if it is a SymAddr instance, otherwise throwing an error
        :param addr_id_or_call: the AddrIdOrCall storing function call data
        :param ctx: current execution context
        :return: SymAddr instance output from subroutine evaluation
        """
        execution_result = self.__eval_addr_id_or_call(addr_id_or_call, ctx)
        if isinstance(execution_result, SymAddr):
            return execution_result
        raise SyntaxError(f"Result '{execution_result}' of subroutine call is not addressable")

    # ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------- #

    def __eval(self, node: Node, ctx: ExeCtx) -> Optional[T]:
        """
        Calls the appropriate evaluator method based on the node's type, with callbacks to ensure proper error handling and logging.
        :param node: the Node to evaluate
        :param ctx: current execution context
        :return: evaluation result
        """
        def eval_result_collector(n: Node, c: ExeCtx):
            ctx.eval_result = self.__EVALUATORS[type(n)](n, c)

        self.__run_on_node_with_callbacks(node, ctx, eval_result_collector,
                                          lambda e: f"Error while evaluating {node.__class__.__name__}: {type(e).__name__} {e}",
                                          "Nothing to evaluate")
        result: Optional[T] = ctx.eval_result
        ctx.eval_result = None
        return result

    def __eval_addr_member(self, addr_member: AddrMember, ctx: ExeCtx) -> T:
        return self.__address_addr_member(addr_member, ctx).value

    def __eval_addr_id_or_call(self, addr_id_or_call: AddrIdOrCall, ctx: ExeCtx) -> T | SymAddr:
        """
        Obtains the subroutine declaration, checking correct number of arguments were given
        and that the function can be accessed, then evaluates it
        :param addr_id_or_call: the AddrIdOrCall node storing name of subroutine to call and its arguments
        :param ctx:
        :return:
        """
        subroutine_id: Identifier = addr_id_or_call.sub_nodes[0]
        subroutine_name: str = subroutine_id.name
        arg_node = addr_id_or_call.sub_nodes[1]
        if type(arg_node) in [CallableSuffix, ExprList]:
            args = arg_node.sub_nodes
        else:
            args = [arg_node]
        subroutine_and_parent_table: Optional[Tuple[V, 'SymTable']] = ctx.cur_table.lookup_symbol_with_table(subroutine_name)
        #
        # Checks that the referenced subroutine exists and can be accessed (i.e. is public or is private but current execution is in that object)
        #
        if subroutine_and_parent_table is None:
            self.__raise_error([addr_id_or_call], SyntaxError(f"Function or procedure '{subroutine_name}' is not defined"))
        subroutine_to_exec, parent_table = subroutine_and_parent_table
        if not (parent_table == ctx.outer_class or parent_table.is_symbol_public(subroutine_name)):
            self.__raise_error([addr_id_or_call], SyntaxError(f"Cannot execute private subroutine '{subroutine_name}()'"))
        #
        # Checks the number of arguments given and parameters specified in function declaration are the same
        #
        params: list[Param] = subroutine_to_exec.sub_nodes[1].sub_nodes
        if len(args) != len(params):
            self.__raise_error([addr_id_or_call], SyntaxError(f"Expected {len(params)} argument(s) but received {len(args)}"))
        if isinstance(subroutine_to_exec, FunDecl):
            execution_result = self.__eval_subroutine(
                args,
                subroutine_to_exec,
                parent_table,
                ctx,
                is_function=True)
        elif isinstance(subroutine_to_exec, ProcDecl):
            execution_result = self.__eval_subroutine(
                args,
                subroutine_to_exec,
                parent_table,
                ctx,
                is_function=False)
        else:
            self.__raise_error([addr_id_or_call], LookupError(f"Subroutine is of unrecognised type {type(subroutine_to_exec)}"))
        return execution_result

    def __eval_new_expr(self, new_expr: NewExpr, ctx: ExeCtx) -> SymAddr:
        """
        Evaluates new expression by obtaining the class declaration, evaluating it, calling the object's constructor and
        writing the result to the current symbol table.
        :param new_expr: the NewExpr node storing the name of the class to instantiate and constructor arguments
        :param ctx: the current execution context
        :return: the address to the newly instantiated object
        """
        class_identifier: Identifier = new_expr.sub_nodes[0]
        constructor_args: list[Node] = new_expr.sub_nodes[1:]
        ret: Optional[Tuple[V, SymTable]] = ctx.cur_table.lookup_symbol_with_table(class_identifier.name)
        if ret is None:
            self.__raise_error([new_expr], SyntaxError(f"'{class_identifier.name}' is not defined"))
        class_decl, tbl = ret
        _object: ObjSymTable = self.__eval_class_decl(class_decl, tbl, ctx)
        ctx.cur_table.update_symbol(_object.storage_key, _object)
        constructor: Optional[ProcDecl] = _object.lookup_symbol(TokenContents.NEW.value)
        if constructor is not None:
            self.__eval_subroutine(constructor_args, constructor, _object, ctx, False)
        return ctx.cur_table.addr_of(_object.storage_key)

    def __eval_class_decl(self, class_decl: ClassDecl, tbl: SymTable, ctx: ExeCtx) -> ObjSymTable:
        """
        Evaluates class declaration, allocating fields and methods in a new ObjSymTable instance (and allocating all parent classes)
        without calling the constructor
        :param class_decl: the ClassDecl node containing information on field and method declarations
        :param tbl: the symbol table in which the declaration of the class to be instantiated is located
        :param ctx: current execution context
        :return: ObjSymTable instance, the resulting object prior to constructor call
        """
        #
        # Initialises a stack of ClassDecl objects representing a class hierarchy, with the root class being at the top of the stack
        # Parent classes must be located in the same or parent table SymTable as the child class declaration.
        #
        stack: list[ClassDecl] = [class_decl]
        while (parent_class_name := stack[-1].parent) is not None:
            ret: Optional[Tuple[V, SymTable]] = tbl.lookup_symbol_with_table(parent_class_name)
            if ret is None:
                self.__raise_error([class_decl], SyntaxError(f"'{parent_class_name}' is not defined"))
            parent_class_decl, tbl = ret
            stack.append(parent_class_decl)
        #
        # Root table is the context in which the uppermost class in the hierarchy is declared,
        # (in this case the global table).
        #
        root_table: SymTable = tbl
        parent: SymTable = root_table

        def get_name_and_val(member: ClassMember) -> Tuple[str, V]:
            inner_node = member.sub_nodes[0]
            if isinstance(inner_node, Identifier):
                return inner_node.name, NullVal()
            if isinstance(inner_node, AttrDecl):
                return inner_node.sub_nodes[0].name, self.__eval(inner_node.sub_nodes[1], ctx)
            if isinstance(inner_node, ProcDecl) or isinstance(inner_node, FunDecl):
                return inner_node.sub_nodes[0].name, inner_node
            raise ValueError(f"Cannot extract data from class member of type {type(member)}")
        #
        # Traverses the stack, initialising and allocating symbols of each class declaration (without calling the constructor)
        # and sets the parent of each new ObjSymTable to the previous ObjSymTable that was higher in the stack.
        # The result is a linked list of ObjSymTable instances.
        #
        prev_cur_table = ctx.cur_table
        prev_outer_class = ctx.outer_class
        obj: Optional[ObjSymTable] = None
        while stack:
            c: ClassDecl = stack.pop()
            members: list[ClassMember] = c.sub_nodes[1:]
            class_name: str = c.sub_nodes[0].name
            obj = ObjSymTable(parent, self.get_instance_key(class_name), class_name)
            #
            # sets the cur_table and outer_class fields of context to the current object as the values of some fields may use the values of other fields when they are first declared
            #
            ctx.cur_table = obj
            ctx.outer_class = obj
            for member in members:
                name, val = get_name_and_val(member)
                obj.add_member(name, val, member.is_public)
            parent = obj
        # Context fields reset
        ctx.cur_table = prev_cur_table
        ctx.outer_class = prev_outer_class
        return obj

    def __eval_expr_list(self, expr_list: ExprList, ctx: ExeCtx) -> list[T]:
        return [self.__eval(n, ctx) for n in expr_list.sub_nodes]

    def __eval_addr_expr(self, addr_expr: AddrExpr, ctx: ExeCtx, require_address: bool = False) -> T:
        """
        Evaluates address expressions.
        Every member that is not the last one should reference an object and should be contained in the previous member.
        :param addr_expr: the AddrExpr to evaluate
        :param ctx: current execution context
        :param require_address: True if the final result of the evaluation should be an address, else False
        :return: the evaluation result
        """
        result: Optional[T] = None
        prev_cur_table = ctx.cur_table
        for i in range(len(addr_expr.sub_nodes)):
            node = addr_expr.sub_nodes[i]
            is_last_node: bool = i == len(addr_expr.sub_nodes) - 1
            if not is_last_node:
                result: SymAddr = self.__address(node, ctx)
                if not (ctx.outer_class == result.sym_table or result.is_public):
                    self.__raise_error([addr_expr], SyntaxError(f"Cannot read from private field '{result.name}'"))
                while isinstance(result.value, SymAddr):
                    result = result.value
                if not isinstance(result.value, ObjSymTable):
                    self.__raise_error([addr_expr], SyntaxError(f"Cannot extract fields and methods from non-object value '{result.value}'"))
                ctx.cur_table = result.value
            else:
                result = self.__address(node, ctx) if require_address else self.__eval(node, ctx)
        ctx.cur_table = prev_cur_table
        return result

    def __eval_identifier(self, identifier: Identifier, ctx: ExeCtx) -> T:
        return self.__address_identifier(identifier, ctx).value

    def __eval_expr(self, expr: Expr, ctx: ExeCtx) -> bool:
        for node in expr.sub_nodes:
            evaluated_node: T = self.__eval(node, ctx)
            if not isinstance(evaluated_node, bool):
                self.__raise_error([node],
                                   SyntaxError(f"Cannot perform logical AND with '{evaluated_node}' of type '{type(evaluated_node)}' as an operand"))
            if not evaluated_node:
                return False
        return True

    def __eval_disjunction(self, disjunction: Disjunction, ctx: ExeCtx) -> bool:
        for node in disjunction.sub_nodes:
            evaluated_node: T = self.__eval(node, ctx)
            if not isinstance(evaluated_node, bool):
                self.__raise_error([node],
                                   SyntaxError(f"Cannot perform logical OR with '{evaluated_node}' of type '{type(evaluated_node)}' as an operand"))
            if evaluated_node:
                return True
        return False

    def __eval_arithm_expr(self, arithm_expr: ArithmExpr, ctx: ExeCtx) -> T:
        return self.__eval_expr_nodes(arithm_expr.sub_nodes, ctx)

    def __eval_comparison(self, comparison: Comparison, ctx: ExeCtx):
        return self.__eval_comp_nodes(comparison.sub_nodes, ctx)

    def __eval_term(self, term: Term, ctx: ExeCtx) -> T:
        return self.__eval_expr_nodes(term.sub_nodes, ctx)

    def __eval_factor(self, factor: Factor, ctx: ExeCtx) -> T:
        return self.__eval_expr_nodes(factor.sub_nodes,
                                      ctx,
                                      left_to_right=False)

    def __eval_expr_nodes(self, nodes: list[Node], ctx: ExeCtx, left_to_right: bool = True) -> T:
        """
        Evaluates list of nodes cumulatively from left-to-right or right-to-left.
        The nodes list is of the form [<operand>,<operator>,<operand>,<operator>...]
        :param nodes - list of nodes to evaluate
        :param ctx - the execution context to pass down to node evaluators when they are called
        :param left_to_right - a boolean value dictating whether evaluation of operands takes place left to right or
                               right to left
        """
        result: T = self.__eval(
            nodes[0] if left_to_right else nodes[-1],
            ctx
        )
        for i in (range(1, len(nodes), 2) if left_to_right else range(len(nodes) - 2, 0, -2)):
            a, op, b = (result, nodes[i], self.__eval(nodes[i + 1], ctx)) if left_to_right else (self.__eval(nodes[i - 1], ctx), nodes[i], result)
            try:
                result = self.__eval_operation(a, op, b)
            except SyntaxError as e:
                self.__raise_error(nodes[i - 1:i + 2], e)
        return result

    def __eval_comp_nodes(self, nodes: list[Node], ctx: ExeCtx) -> bool:
        """
        Evaluates comparison expressions by taking every operand-operator-operand triple and evaluating them individually
        from left to right.
        Returns True only if all individual comparisons evaluate to True
        :param nodes: list of nodes to evlauate
        :param ctx: execution context to pass down to node evaluators
        :return: evaluation result
        """
        for i in range(1, len(nodes) - 1, 2):
            a, op, b = self.__eval(nodes[i - 1], ctx), nodes[i], self.__eval(nodes[i + 1], ctx)
            try:
                if not self.__eval_operation(a, op, b):
                    return False
            except SyntaxError as e:
                self.__raise_error(nodes[i - 1:i + 2], e)
        return True

    def __eval_operation(self, a: T, operator: Op | TokenVals, b: T) -> T:
        #
        # Carries out type checking by checking the types of operands against list of accepted
        # types for the given operator
        #
        operator_val = operator.val if isinstance(operator, Op) else operator
        accepted_types: list[list[type, type]] = self.OP_TO_ACCEPTED_TYPES[operator_val]
        if accepted_types:
            operand_types = [type(a), type(b)]
            types_correct: bool = operand_types in accepted_types or operand_types[::-1] in accepted_types
            operator_str: str = KNOWN_TOKEN_VALS[operator_val].value
            if not types_correct:
                raise SyntaxError("Invalid type for '{}': '{}', '{}'".format(operator_str, *operand_types))
        #
        # Calls relevant operation lambda
        #
        return self.OP_TO_OPERATION[operator_val](a, b)

    """
    Obtains literal value from the .val field of IntLiteral, StrLiteral, NumLiteral and BoolLiteral nodes
    """
    @staticmethod
    def __eval_int_literal(int_literal: IntLiteral, ctx: ExeCtx) -> int:
        return int_literal.val

    @staticmethod
    def __eval_str_literal(str_literal: StrLiteral, ctx: ExeCtx) -> str:
        return str_literal.val

    @staticmethod
    def __eval_num_literal(num_literal: NumLiteral, ctx: ExeCtx) -> float:
        return num_literal.val

    @staticmethod
    def __eval_bool_literal(bool_literal: BoolLiteral, ctx: ExeCtx) -> bool:
        return bool_literal.val

    def __eval_unary_minus(self, unary_minus: UnaryMinus, ctx: ExeCtx) -> T:
        """
        Carries out unary minus operation i.e. -{Expr}
        :param unary_minus: UnaryMinus node in which the expression to negate is stored
        :param ctx: current execution context
        :return: result of unary minus operation
        """
        result = self.__eval(unary_minus.get_sub_node(0), ctx)
        if type(result) not in [float, int]:
            self.__raise_error([unary_minus], SyntaxError(f"Non-number value '{result}' cannot be negated"))
        return -result

    def __eval_unary_not(self, unary_not: UnaryNot, ctx: ExeCtx) -> bool:
        """
        Carrise out boolean NOT
        :param unary_not: UnaryNot node in which value to do logical NOT is stored
        :param ctx: current execution context
        :return: result of boolean NOT operation
        """
        result = self.__eval(unary_not.get_sub_node(0), ctx)
        if type(result) != bool:
            self.__raise_error([unary_not], SyntaxError(f"Boolean NOT operation cannot be performed on non-boolean value '{result}'"))
        return not result

    def __eval_subroutine(self, args: list[Node], subroutine: FunDecl | ProcDecl, parent_table: SymTable, ctx: ExeCtx, is_function: bool) -> T:
        """
        Evaluates the inputted subroutine by setting context flags to correct values (depending on if a function or procedure is being called),
        creates a child symbol table and initialises the subroutine parameters as variables with their values set to the corresponding argument.
        This new local table is set as the current table of the context and the body is executed.
        The returned value is extracted from ctx.eval_result, this method resets the edited context flags and symbol table to their original value
        returns the output of the called subroutine

        :param args, list of arguments in the form of AST nodes
        :param subroutine, the FunDecl or ProcDecl node to call
        :param ctx, current execution context
        :param is_function, True if subroutine is a FunDecl object, False if subroutine is a ProcDecl object
        """
        #
        # Internal check that the is_function flag matches the type of subroutine provided
        #
        assert (is_function and isinstance(subroutine, FunDecl)) or (not is_function and isinstance(subroutine, ProcDecl)), \
            f"is_function is set to {is_function} but the subroutine given is of type {type(subroutine)}"
        params: list[Param] = subroutine.sub_nodes[1].sub_nodes
        body: Node = subroutine.sub_nodes[2]
        in_outer_func: bool = ctx.inside_function
        in_outer_proc: bool = ctx.inside_procedure
        prev_outer_class: Optional[ObjSymTable] = ctx.outer_class
        if isinstance(parent_table, ObjSymTable):
            ctx.outer_class = parent_table
        prev_by_ref_params: list[str] = ctx.by_ref_params
        ctx.inside_function = is_function
        ctx.inside_procedure = not is_function
        #
        # Creates local lookup table for the function, setting the arguments given as variables
        #
        evaluated_args = []
        by_ref_param_names: list[str] = []
        for i in range(len(args)):
            arg = args[i]
            param = params[i]
            if param.is_byref and self.__is_addressable(arg):
                by_ref_param_names.append(param.name)
                arg_addr = self.__address(arg, ctx)
                evaluated_args.append(arg_addr)
            else:
                evaluated_args.append(self.__eval(arg, ctx))
        local_table = SymTable(parent_table, {params[i].name: evaluated_args[i] for i in range(len(evaluated_args))})
        ctx.by_ref_params = by_ref_param_names
        #
        # Sets current table of context to the local table, executes function body,
        # extracts returned value from eval_result and sets current table to outer symbol table
        #
        prev_cur_table: SymTable = ctx.cur_table
        ctx.cur_table = local_table
        self.__execute(body, ctx)
        result: T = ctx.eval_result
        #
        # Context parameters are reset to previous states
        #
        ctx.inside_function = in_outer_func
        ctx.inside_procedure = in_outer_proc
        ctx.by_ref_params = prev_by_ref_params
        ctx.return_detected = False
        ctx.eval_result = None
        ctx.cur_table = prev_cur_table
        ctx.outer_class = prev_outer_class
        #
        # If result is an address to an object that is located in the local SymTable,
        # the object is moved to the scope in which the function was called so that the local
        # table can be closed and garbage-collected
        #
        if isinstance(result, SymAddr):
            assert isinstance(result.value, ObjSymTable)
            if result.sym_table == local_table:
                obj: ObjSymTable = result.value
                ctx.cur_table.update_symbol(obj.storage_key, obj)
                result = ctx.cur_table.addr_of(obj.storage_key)
        local_table.close()
        return result if result is not None else NullVal()

    def __eval_str_cast(self, cast_str: CastStr, ctx: ExeCtx) -> str:
        """
        Carries out casting to string
        :param cast_str: CastStr node in which the argument to cast is stored
        :param ctx: current execution context
        :return: result of str cast
        """
        evaluated_expr: T = self.__eval(cast_str.sub_nodes[0], ctx)
        if AstExecutor.__is_null(evaluated_expr):
            self.__raise_error([cast_str], ValueError(f"Cannot convert null value to string"))
        return str(evaluated_expr)

    def __eval_int_cast(self, cast_int: CastInt, ctx: ExeCtx) -> int:
        """
        Carries out casting to int
        :param cast_int: CastInt node in which the argument to cast is stored
        :param ctx: current execution context
        :return: result of int cast
        """
        evaluated_expr: T = self.__eval(cast_int.sub_nodes[0], ctx)
        if AstExecutor.__is_null(evaluated_expr):
            self.__raise_error([cast_int], ValueError(f"Cannot convert null value to integer"))
        if type(evaluated_expr) == str:
            try:
                return int(evaluated_expr)
            except ValueError:
                self.__raise_error([cast_int.sub_nodes[0]], ValueError(f"Cannot convert string of value '{evaluated_expr}' to integer"))
        elif type(evaluated_expr) in [int, float]:
            return int(evaluated_expr)
        else:
            self.__raise_error([cast_int], ValueError(f"Cannot convert value '{evaluated_expr}' of type '{type(evaluated_expr)}' to integer"))

    def __eval_float_cast(self, cast_float: CastFloat, ctx: ExeCtx) -> float:
        """
        Carries out casting to float
        :param cast_float: CastFloat node in which the argument to cast is stored
        :param ctx: current execution context
        :return: result of float cast
        """
        evaluated_expr: T = self.__eval(cast_float.sub_nodes[0], ctx)
        if AstExecutor.__is_null(evaluated_expr):
            self.__raise_error([cast_float], ValueError(f"Cannot convert null value to float"))
        if type(evaluated_expr) == str:
            try:
                return float(evaluated_expr)
            except ValueError:
                self.__raise_error([cast_float], ValueError(f"Cannot convert string of value '{evaluated_expr}' to float"))
        elif type(evaluated_expr) in [int, float]:
            return float(evaluated_expr)
        else:
            self.__raise_error([cast_float], ValueError(f"Cannot convert value '{evaluated_expr}' of type '{type(evaluated_expr)} to float"))

    def __eval_length(self, length: Length, ctx: ExeCtx) -> int:
        """
        Evaluates .length attribute of strings
        :param length: Length node in which the identifier of the relevant string is stored
        :param ctx: current execution context
        :return: the length of the string referenced
        """
        val: T = self.__eval(length.sub_nodes[0], ctx)
        if isinstance(val, str):
            return len(val)
        if isinstance(val, ArrayVal):
            return val.length
        else:
            self.__raise_error([length], SyntaxError(f"Cannot use 'length' attribute on non-string and non-array value '{val}'"))

    def __eval_str_substring(self, str_substring: StrSubstring, ctx: ExeCtx) -> str:
        """
        Evaluates .subString(start, end)
        :param str_substring: StrSubstring node in which the identifier of the string to open and the arguments are stored
        :param ctx: current execution context
        :return: the string slice
        """
        evaluated_sub_nodes: list[T] = [self.__eval(node, ctx) for node in str_substring.sub_nodes]
        start_index, substring_len, larger_str = tuple(evaluated_sub_nodes)
        #
        # Checking substring arguments are integers
        #
        if not isinstance(start_index, int):
            self.__raise_error([str_substring], SyntaxError(f"First argument '{start_index}' of substring() is not an integer"))
        if not isinstance(substring_len, int):
            self.__raise_error([str_substring], SyntaxError(f"Second argument '{substring_len}' of substring() is not an integer"))
        #
        # Checking the expression on which substring is called evaluates to a string literal, and that the starting index is within the length of that literal
        #
        if not isinstance(larger_str, str):
            self.__raise_error([str_substring], SyntaxError(f"Cannot find substring of non-string value '{larger_str}'"))
        if not 0 <= start_index <= len(larger_str) - 1:
            self.__raise_error([str_substring], SyntaxError(f"Cannot find substring of '{larger_str}' starting from out-of-range index '{start_index}'"))
        return larger_str[start_index:start_index + substring_len]

    def __eval_input(self, input_node: Input, ctx: ExeCtx) -> str:
        """
        Evaluates expression argument to get input message and calls input() with that message
        :param input_node: the Input node in which expression argument is contained
        :param ctx: current execution context
        :return: the user's input, type 'str'
        """
        msg: str = str(self.__eval(input_node.sub_nodes[0], ctx))
        logging.debug(f"AWAITING INPUT: '{msg}'")
        return input(msg)

    def __eval_open_read(self, open_read: OpenRead, ctx: ExeCtx) -> TextIOWrapper:
        """
        Gets file path to open in read mode by evaluating expression argument and opens the file
        :param open_read: the OpenRead node containing the expression argument
        :param ctx: current execution context
        :return: the newly-opened file stream
        """
        file_path: T = self.__eval(open_read.sub_nodes[0], ctx)
        if not isinstance(file_path, str):
            self.__raise_error([open_read], SyntaxError(f"File path argument '{file_path}' is not a string"))
        return open(file_path, AstExecutor.__FILE_READ_MODE)

    def __eval_open_write(self, open_write: OpenWrite, ctx: ExeCtx) -> TextIOWrapper:
        """
        Gets file path to open in write mode by evaluating expression argument and opens the file
        :param open_write: the OpenWrite node containing the expression argument
        :param ctx: current execution context
        :return: the newly-opened file stream
        """
        file_path: T = self.__eval(open_write.sub_nodes[0], ctx)
        if not isinstance(file_path, str):
            self.__raise_error([open_write], SyntaxError(f"File path argument '{file_path}' is not a string"))
        return open(file_path, AstExecutor.__FILE_WRITE_MODE)

    def __eval_read_line(self, read_line: ReadLine, ctx: ExeCtx) -> str:
        """
        Loads referenced file stream and reads a line
        :param read_line: the ReadLine node to interpret, in which the open file identifier is located
        :param ctx: current execution context
        :return: the line that has been read, type 'str'
        """
        file_address: SymAddr = self.__address(read_line.sub_nodes[0], ctx)
        file_value: T = file_address.value
        if not isinstance(file_value, TextIOWrapper):
            self.__raise_error([read_line], SyntaxError(f"Cannot read line from non-file '{file_value}'"))
        if file_value.mode != AstExecutor.__FILE_READ_MODE:
            self.__raise_error([read_line], SyntaxError(f"Cannot read from a file that was opened via openWrite()"))
        if file_value.closed:
            self.__raise_error([read_line], SyntaxError("Cannot read line from closed file stream"))
        result: str = file_value.readline()
        file_address.value = file_value
        return result

    def __eval_write_line(self, write_line: WriteLine, ctx: ExeCtx) -> NullVal:
        """
        Loads referenced file stream and writes a line (a newline character is appended to the end of the line automatically)
        :param write_line: the WriteLine instance to interpret
        :param ctx: current execution context
        :return: NullVal as writeLine() is a procedure
        """
        file_address: SymAddr = self.__address(write_line.sub_nodes[1], ctx)
        file_value: T = file_address.value
        if not isinstance(file_value, TextIOWrapper):
            self.__raise_error([write_line], SyntaxError(f"Cannot write line to non-file '{file_value}'"))
        if file_value.mode != AstExecutor.__FILE_WRITE_MODE:
            self.__raise_error([write_line], SyntaxError("Cannot write to a file that was opened via openRead()"))
        if file_value.closed:
            self.__raise_error([write_line], SyntaxError("Cannot write line to closed file stream"))
        line_to_write: T = self.__eval(write_line.sub_nodes[0], ctx)
        if not isinstance(line_to_write, str):
            self.__raise_error([write_line], SyntaxError(f"Cannot write non-string value '{line_to_write}' to file"))
        file_value.write(line_to_write + "\n")
        file_address.value = file_value
        return NullVal()

    def __eval_end_of_file(self, end_of_file: EndOfFile, ctx: ExeCtx) -> bool:
        """
        Loads referenced file stream and checks if the file pointer is at the end of the file
        :param end_of_file: the EndOfFile instance to interpret
        :param ctx: the current execution context
        :return: True if file pointer is at the end of the file, else False
        """
        file_address: SymAddr = self.__address(end_of_file.sub_nodes[0], ctx)
        file_value: T = file_address.value
        if not isinstance(file_value, TextIOWrapper):
            self.__raise_error([end_of_file], SyntaxError(f"Cannot check for end-of-file of non-file '{file_value}'"))
        if file_value.closed:
            self.__raise_error([end_of_file], SyntaxError(f"Cannot check for end-of-file in closed file stream"))
        #
        # Checks if position of the file pointer is equal to the position of the end of the file
        #
        original_pos = file_value.tell()
        # Moves file pointer to the end of the file
        file_value.seek(0, os.SEEK_END)
        result: bool = file_value.tell() == original_pos
        # Moves file pointer back to the original position
        file_value.seek(original_pos)
        return result

    def __eval_file_close(self, file_close: FileClose, ctx: ExeCtx) -> NullVal:
        """
        Obtains the file stream to close from first sub-node of FileClose, closes it
        and overwrites the old, open file stream with the new, clsoed file stream
        :param file_close: the FileClose node to interpret
        :param ctx: execution context
        :return: NullVal instance, as the fileClose() is a procedure
        """
        file_address: SymAddr = self.__address(file_close.sub_nodes[0], ctx)
        file_value: T = file_address.value
        #
        # Checks that the value at the given location is actually a file stream
        #
        if not isinstance(file_value, TextIOWrapper):
            self.__raise_error([file_close], SyntaxError(f"Cannot perform file close operation on non-file '{file_value}'"))
        file_value.close()
        file_address.value = file_value
        return NullVal()

    # ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------- #

    def __call_back(self, node: Node, ctx, post: bool = False):
        c_list = self.__post_callbacks if post else self.__pre_callbacks
        for c in c_list:
            c(node, ctx)

    def __log_pre(self, node: Node, ctx: ExeCtx):
        self.__log(node, ctx)

    def __log_post(self, node: Node, ctx: ExeCtx):
        self.__log(node, ctx, True)

    def __log(self, node: Node, ctx: ExeCtx, post: bool = False):
        result: str = ctx.eval_result
        prefix: str = "Evaluat" if result else "Execut"
        suffix: str = "ed" if post else "ing"
        ending: str = "." if post else " ..."
        ending = f": {result}{ending}" if result else ending
        logging.debug(f"{prefix}{suffix} node {node.__class__.__name__} at line {node.line_index + 1}: {ending}")

    def __raise_error(self, nodes: list[Node], e: Exception):
        """
        Raises the error and stores the nodes in which the error took place
        :param nodes: nodes in which error is located
        :param e: Exception instance
        :return: None
        """
        self.__erroneous_nodes = nodes
        raise e

    @staticmethod
    def __is_null(val: T) -> bool:
        return isinstance(val, NullVal)

    def __is_addressable(self, node: Node) -> bool:
        """
        A SymAddr instance can be obtained from a node if it has an associated method in __ADDRESSERS
        :param node: the node in question
        :return: True if it has an associated addresser method, else False
        """
        for node_type in self.__ADDRESSERS.keys():
            if isinstance(node, node_type):
                return True
        return False

    def get_instance_key(self, class_name: str) -> str:
        """
        Creates a unique object key in the form "_._{class_name}_{instance # (at time of execution)}_._",
        formatted so it cannot be overwritten through source code
        :param class_name: the name of the class the object is an instance of
        :return: the unique object key
        """
        self.__instance_count[class_name] = self.__instance_count.get(class_name, 0) + 1
        return f"_._{class_name}_{self.__instance_count[class_name]}_._"
