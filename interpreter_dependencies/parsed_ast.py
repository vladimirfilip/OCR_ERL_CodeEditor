from enum import Enum
from functools import cache
from typing import Iterable, Optional

from .parsed_token import TokenVals


class NodeKinds(Enum):
    PROGRAM = 1
    INSTR_BLOCK = 2
    FUN_DECL = 3
    PROC_DECL = 4
    CLASS_DECL = 5


class Node:
    SUB_NODES_FIELD = "sub_nodes"

    def __init__(self, line_index: int):
        self.line_index = line_index
        self.sub_nodes: list['Node'] = []
        assert Node.SUB_NODES_FIELD in self.__dict__

    @property
    @cache
    def end_line_index(self):
        if not self.sub_nodes:
            return self.line_index
        return max(node.end_line_index for node in self.sub_nodes)

    def reduce(self) -> 'Node':
        """Gets the only sub_node as a replacement for the current node, if possible.

        Use this method to eliminate intermediary nodes that do not play a role in the execution of the syntax tree.

        Important: override this method and make it return 'self' for subclasses that have extra attributes besides 'sub_nodes'.

        :return: the only sub-node if any, the current node otherwise.
        """
        if self.sub_nodes and len(self.sub_nodes) < 2:
            return self.sub_nodes[0]
        return self

    def add_sub_node(self, sub_node: 'Node', children: bool = False) -> 'Node':
        if children:
            self.add_sub_nodes([sn.reduce() for sn in sub_node.sub_nodes], False)
        else:
            self.sub_nodes.append(sub_node.reduce())
        return self

    def add_sub_nodes(self, sub_nodes: Iterable['Node'], children: bool = False) -> 'Node':
        for sub_node in sub_nodes:
            self.add_sub_node(sub_node, children)
        return self

    def get_sub_nodes(self) -> Iterable['Node']:
        for n in self.sub_nodes:
            yield n

    def get_sub_node(self, index: int) -> Optional['Node']:
        result: Optional['Node'] = None
        if 0 <= index < len(self.sub_nodes):
            result = self.sub_nodes[index]
        return result

    def count_sub_nodes(self) -> int:
        return len(self.sub_nodes)

    def get_kind(self) -> NodeKinds:
        raise NotImplemented()


class Program(Node):
    def add_block(self, program_block: 'ProgramBlock') -> 'Program':
        match program_block.get_kind():
            case NodeKinds.INSTR_BLOCK:
                self.add_sub_node(program_block, True)
            case NodeKinds.FUN_DECL:
                self.add_sub_node(program_block)
            case NodeKinds.PROC_DECL:
                self.add_sub_node(program_block)
            case NodeKinds.CLASS_DECL:
                self.add_sub_node(program_block)
            case _:
                raise RuntimeError("Unknown node kind: " + program_block.get_kind().name)
        return self

    def get_kind(self) -> NodeKinds:
        return NodeKinds.PROGRAM


class ProgramBlock(Node):
    pass


class InstrBlock(ProgramBlock):
    def get_kind(self) -> NodeKinds:
        return NodeKinds.INSTR_BLOCK


class Instr(Node):
    pass


class GlobDecl(Instr):
    IS_GLOBAL_FIELD: str = "is_global"

    def __init__(self, is_global, line_index):
        super().__init__(line_index)
        self.is_global: bool = is_global
        assert GlobDecl.IS_GLOBAL_FIELD in self.__dict__

    def reduce(self) -> 'Node':
        return self


class BuiltInFunCall(Node):
    def reduce(self):
        return self


class PrintInstr(BuiltInFunCall):
    pass


class Expr(Node):
    pass


class ArrayDecl(GlobDecl):
    NAME_FIELD: str = "name"
    DIMS_FIELD: str = "dims"

    def __init__(self, line_index, is_global: bool = False):
        self.name: str = ""
        self.dims: list[Expr] = []
        super().__init__(is_global, line_index)

    def reduce(self) -> 'Node':
        return self

    def set_name(self, name: str) -> 'ArrayDecl':
        self.name = name
        return self

    def set_dims(self, expr_list: 'ExprList') -> 'ArrayDecl':
        self.dims = expr_list.sub_nodes
        return self


class AddrExpr(Node):
    pass


class AddrIdOrCall(Node):
    IS_INSTR = "is_call"
    pass


class AddrMember(Node):
    pass


class AddrInstr(Node):
    pass


class IndexingSuffix(Node):
    pass


class ExprList(Node):
    pass


class Term(Node):
    pass


class Factor(Node):
    pass


class Op(Node):
    VAL_FIELD: str = "val"

    def __init__(self, line_index, val: TokenVals):
        super().__init__(line_index)
        self.val = val
        assert Op.VAL_FIELD in self.__dict__

    def add_sub_node(self, sub_node: 'Node', children: bool = False) -> 'Node':
        assert False, "Logical error, should never be here"


class AddOp(Op):
    ADD_OP_VALS: list[TokenVals] = [TokenVals.PLUS, TokenVals.MINUS, TokenVals.AND]

    def __init__(self, line_index, val: TokenVals):
        super().__init__(line_index, val)
        assert val in AddOp.ADD_OP_VALS


class MulOp(Op):
    MUL_OP_VALS: list[TokenVals] = [TokenVals.MUL, TokenVals.INT_DIV, TokenVals.MOD, TokenVals.DIV, TokenVals.OR]

    def __init__(self, line_index, val: TokenVals):
        super().__init__(line_index, val)
        assert val in MulOp.MUL_OP_VALS


class PowOp(Op):
    POW_OP_VALS: list[TokenVals] = [TokenVals.POW, TokenVals.EQ, TokenVals.NEQ, TokenVals.GREATER, TokenVals.GREATER_EQ,
                                    TokenVals.LOWER, TokenVals.LOWER_EQ]

    def __init__(self, line_index, val: TokenVals):
        super().__init__(line_index, val)
        assert val in PowOp.POW_OP_VALS


class UnaryMinus(Node):
    def reduce(self):
        return self


class UnaryNot(Node):
    def reduce(self):
        return self


class SimpleExpr(Node):
    pass


# class CallableExpr(Node):
#     pass


class CallableSuffix(Node):
    pass


class Identifier(Node):
    NAME_FIELD = "name"

    def __init__(self, line_index: int, name: str = ""):
        super().__init__(line_index)
        self.name = name
        assert Identifier.NAME_FIELD in self.__dict__

    def reduce(self) -> 'Node':
        return self


class IntLiteral(Node):
    VAL_FIELD = "val"

    val: int

    def __init__(self, line_index: int, val: int = 0):
        super().__init__(line_index)
        self.val = val
        assert IntLiteral.VAL_FIELD in self.__dict__

    def reduce(self) -> 'Node':
        return self


class StrLiteral(Node):
    VAL_FIELD = "val"

    val: str

    def __init__(self, line_index: int, val: str = ""):
        super().__init__(line_index)
        self.val = val
        assert StrLiteral.VAL_FIELD in self.__dict__

    def reduce(self) -> 'Node':
        return self


class NumLiteral(Node):
    VAL_FIELD = "val"

    val: float

    def __init__(self, line_index: int, val: float = 0.0):
        super().__init__(line_index)
        self.val = val
        assert NumLiteral.VAL_FIELD in self.__dict__

    def reduce(self) -> 'Node':
        return self


class BoolLiteral(Node):
    VAL_FIELD = "val"

    val: bool

    def __init__(self, line_index: int, val: bool):
        super().__init__(line_index)
        self.val = val
        assert BoolLiteral.VAL_FIELD in self.__dict__


class AddrAssign(Node):
    IS_ASSIGN = "is_assign"


class VarAssign(GlobDecl):

    def __init__(self, line_index: int, is_global: bool = False):
        super().__init__(is_global, line_index)


class IfElse(Instr):
    def reduce(self):
        return self


class ElseIf(Instr):
    def reduce(self):
        return self


class Else(Instr):
    def reduce(self):
        return self


class InnerInstrBlock(Node):
    pass


class SwitchCase(Instr):
    def reduce(self):
        return self


class SwitchDefault(SwitchCase):
    pass


class GoToInstr(Instr):
    VAL_FIELD = "val"

    def __init__(self, line_index, val: TokenVals):
        super().__init__(line_index)
        self.val = val
        assert GoToInstr.VAL_FIELD in self.__dict__


class ForLoop(Instr):
    pass


class WhileLoop(Instr):
    pass


class DoUntil(Instr):
    pass


class FunDecl(Node):
    def get_kind(self) -> NodeKinds:
        return NodeKinds.FUN_DECL


class FunInstrBlock(Node):
    pass


class ParamList(Node):
    def reduce(self):
        return self


class Param(Node):
    IS_BYREF_FIELD = "is_byref"
    NAME_FIELD = "name"

    def __init__(self, line_index: int, name: str, is_byref: bool):
        super().__init__(line_index)
        self.is_byref = is_byref
        self.name = name
        assert Param.NAME_FIELD in self.__dict__
        assert Param.IS_BYREF_FIELD in self.__dict__


class ReturnInstr(Node):
    def reduce(self):
        return self


class ProcDecl(Node):
    def get_kind(self) -> NodeKinds:
        return NodeKinds.PROC_DECL


class ProcInstrBlock(Node):
    pass


class NewExpr(Node):
    def reduce(self):
        return self


class FunExpr(Node):
    def reduce(self):
        return self


class CastInt(FunExpr):
    pass


class CastFloat(FunExpr):
    pass


class CastStr(FunExpr):
    pass


class Input(FunExpr):
    pass


class Length(Node):
    def reduce(self):
        return self


class StrSubstring(Node):
    pass


class EndOfFile(Node):
    def reduce(self):
        return self


class ReadLine(Node):
    def reduce(self):
        return self


class WriteLine(Node):
    def reduce(self):
        return self


class FileClose(Node):
    def reduce(self):
        return self


class OpenRead(Node):
    def reduce(self):
        return self


class OpenWrite(Node):
    def reduce(self):
        return self


class ClassDecl(Node):
    IN_CLASS: str = "in_class"
    PARENT_FIELD: str = "parent"

    def __init__(self, line_index, parent: str):
        super().__init__(line_index)
        self.parent = parent
        assert ClassDecl.PARENT_FIELD in self.__dict__

    def get_kind(self) -> NodeKinds:
        return NodeKinds.CLASS_DECL


class ClassBlock(Node):
    pass


class ClassMember(Node):
    IS_PUBLIC_FLAG: str = "is_public_flag"
    IS_PUBLIC_FIELD: str = "is_public"

    def __init__(self, line_index, is_public: bool):
        super().__init__(line_index)
        self.is_public = is_public
        assert ClassMember.IS_PUBLIC_FIELD in self.__dict__

    def reduce(self):
        return self


class AttrDecl(Node):
    pass
