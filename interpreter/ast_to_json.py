from typing import Type, Callable
from parsed_ast import Node, GlobDecl, ArrayDecl, Identifier, IntLiteral, StrLiteral, NumLiteral, BoolLiteral, Op, GoToInstr, Param, ClassDecl, ClassMember
from parsed_token import KNOWN_TOKEN_VALS


class ASTToJsonParser:
    def __init__(self):
        self.__NODE_TRANSLATORS: dict[Type, Callable] = {
            Node: self.__node_inst2dict,
            GlobDecl: self.__glob_decl2dict,
            ArrayDecl: self.__array_decl2dict,
            Identifier: self.__id2dict,
            IntLiteral: self.__literal2dict,
            StrLiteral: self.__literal2dict,
            NumLiteral: self.__literal2dict,
            BoolLiteral: self.__literal2dict,
            Op: self.__operator2dict,
            GoToInstr: self.__goto_instr2dict,
            Param: self.__param2dict,
            ClassDecl: self.__class_decl2dict,
            ClassMember: self.__class_member2dict
        }
        self.__NODE_HIERARCHY = {}

    @staticmethod
    def __get_node_class(node: Node) -> str:
        """Gets the name of the class that the current node belongs to.

        :param node: AST node to get class name of.
        :return: name of node's class, with no qualifiers.
        """
        return node.__class__.__name__

    def __get_translated_type(self, typ: Type, exclude_current: bool = False) -> Type:
        """Gets the closest ancestor of the given type that has a translating method associated with it.

        :param typ: node type whose translated ancestor to get.
        :param exclude_current: whether to consider the current type its own ancestor or not. It defaults to false.
        :return: ancestor type with a translator.
        """
        if typ not in self.__NODE_HIERARCHY:
            def translated_parent(parent: Type) -> Type:
                if parent in self.__NODE_TRANSLATORS:
                    return parent
                assert len(parent.__bases__) > 0, f"Type {parent.__name__} has no parent"
                return translated_parent(parent.__bases__[0])

            assert len(typ.__bases__) > 0, f"Type {typ.__name__} has no parent"
            self.__NODE_HIERARCHY[typ] = translated_parent(typ.__bases__[0])
        return typ if not exclude_current and typ in self.__NODE_TRANSLATORS else self.__NODE_HIERARCHY[typ]

    def parse(self, node: Node) -> dict:
        return self.__node2dict(node, self.__get_translated_type(type(node)))

    def __node2dict(self, node: Node, as_of_type: Type = None) -> dict:
        """Converts the given node into a dictionary that can be used in comparisons.

        :param node: AST node to convert
        :param as_of_type: type to interpret the node as. If None then it is assumed the node's real type. If not None, then it must be an ancestor of the node's type that has a
        translating method associated with it.
        :return: dictionary containing the node's blueprint
        """
        as_of_type = type(node) if as_of_type is None else as_of_type
        assert isinstance(node,
                          as_of_type), f"Node type {ASTToJsonParser.__get_node_class(node)} does not extend {as_of_type.__name__}"
        assert as_of_type in self.__NODE_TRANSLATORS, f"No translator for type {str(as_of_type)}"
        return self.__NODE_TRANSLATORS[as_of_type](node)

    def __node_inst2dict(self, node: Node) -> dict:
        sub_nodes: list[dict] = [self.__node2dict(sn, self.__get_translated_type(type(sn))) for sn in node.sub_nodes]
        return {ASTToJsonParser.__get_node_class(node): {Node.SUB_NODES_FIELD: sub_nodes} if sub_nodes else {}}

    def __glob_decl2dict(self, glob_decl: GlobDecl) -> dict:
        result: dict = self.__node2dict(glob_decl, self.__get_translated_type(GlobDecl, True))
        result[ASTToJsonParser.__get_node_class(glob_decl)][GlobDecl.IS_GLOBAL_FIELD] = str(glob_decl.is_global)
        return result

    def __array_decl2dict(self, array_decl: ArrayDecl) -> dict:
        result: dict = self.__node2dict(array_decl, self.__get_translated_type(ArrayDecl, True))
        node_type = ASTToJsonParser.__get_node_class(array_decl)
        result[node_type][ArrayDecl.NAME_FIELD] = array_decl.name
        result[node_type][ArrayDecl.DIMS_FIELD] = [self.__node2dict(node, self.__get_translated_type(type(node))) for node in array_decl.dims]
        return result

    def __operator2dict(self, operator: Op):
        result: dict = self.__node2dict(operator, self.__get_translated_type(Op, True))
        result[ASTToJsonParser.__get_node_class(operator)][operator.__class__.VAL_FIELD] = KNOWN_TOKEN_VALS[operator.val].value
        return result

    def __id2dict(self, identifier: Identifier) -> dict:
        result: dict = self.__node2dict(identifier, self.__get_translated_type(Identifier, True))
        result[ASTToJsonParser.__get_node_class(identifier)][Identifier.NAME_FIELD] = identifier.name
        return result

    def __literal2dict(self, literal: IntLiteral | StrLiteral | NumLiteral | BoolLiteral) -> dict:
        result: dict = self.__node2dict(literal, self.__get_translated_type(literal.__class__, True))
        result[ASTToJsonParser.__get_node_class(literal)][literal.__class__.VAL_FIELD] = literal.val
        return result

    def __goto_instr2dict(self, go_to_instr: GoToInstr) -> dict:
        result: dict = self.__node2dict(go_to_instr, self.__get_translated_type(GoToInstr, True))
        result[ASTToJsonParser.__get_node_class(go_to_instr)][GoToInstr.VAL_FIELD] = KNOWN_TOKEN_VALS[go_to_instr.val].value
        return result

    def __param2dict(self, param: Param) -> dict:
        result: dict = self.__node2dict(param, self.__get_translated_type(Param, True))
        result[ASTToJsonParser.__get_node_class(param)][Param.IS_BYREF_FIELD] = str(param.is_byref)
        result[ASTToJsonParser.__get_node_class(param)][Param.NAME_FIELD] = param.name
        return result

    def __class_decl2dict(self, class_decl: ClassDecl) -> dict:
        result: dict = self.__node2dict(class_decl, self.__get_translated_type(ClassDecl, True))
        result[ASTToJsonParser.__get_node_class(class_decl)][ClassDecl.PARENT_FIELD] = class_decl.parent
        return result

    def __class_member2dict(self, class_member: ClassMember) -> dict:
        result: dict = self.__node2dict(class_member, self.__get_translated_type(ClassMember, True))
        result[ASTToJsonParser.__get_node_class(class_member)][ClassMember.IS_PUBLIC_FIELD] = str(class_member.is_public)
        return result
