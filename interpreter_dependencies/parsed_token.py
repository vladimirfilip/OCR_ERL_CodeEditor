from enum import Enum
from typing import Optional, ClassVar, List


class TokenContents(Enum):
    PLUS = '+'
    MINUS = '-'
    MUL = "*"
    DIV = "/"
    INT_DIV = "DIV"
    MOD = "MOD"
    POW = "^"
    AND = "AND"
    NOT = "NOT"
    OR = "OR"
    EQ = "=="
    NEQ = "!="
    GREATER = ">"
    GREATER_EQ = ">="
    LOWER = "<"
    LOWER_EQ = "<="
    EQUALS = "="
    DOT = "."
    IF = "if"
    THEN = "then"
    ELSEIF = "elseif"
    ELSE = "else"
    ENDIF = "endif"
    CONTINUE = "continue"
    BREAK = "break"
    SWITCH = "switch"
    CASE = "case"
    DEFAULT = "default"
    ENDSWITCH = "endswitch"
    FOR = "for"
    TO = "to"
    NEXT = "next"
    WHILE = "while"
    ENDWHILE = "endwhile"
    DO = "do"
    UNTIL = "until"
    COLON = ":"
    OPEN_PAREN = "("
    CLOSED_PAREN = ")"
    OPEN_BRACKET = '['
    CLOSED_BRACKET = ']'
    GLOBAL = 'global'
    ARRAY = 'array'
    COMMA = ','
    PRINT = "print"
    FUNCTION = "function"
    ENDFUNCTION = "endfunction"
    BYVAL = ":byVal"
    BYREF = ":byRef"
    RETURN = "return"
    PROCEDURE = "procedure"
    ENDPROCEDURE = "endprocedure"
    INT_CAST = "int"
    STR_CAST = "str"
    FLOAT_CAST = "float"
    INPUT = "input"
    LENGTH = "length"
    SUBSTRING = "substring"
    ENDOFFILE = "endOfFile"
    READLINE = "readLine"
    WRITELINE = "writeLine"
    CLOSE = "close"
    OPENREAD = "openRead"
    OPENWRITE = "openWrite"
    CLASS = "class"
    INHERITS = "inherits"
    PUBLIC = "public"
    PRIVATE = "private"
    NEW = "new"
    SUPER = "super"
    ENDCLASS = "endclass"
    TRUE = "true"
    FALSE = "false"


class TokenVals(Enum):
    PLUS = 1
    MINUS = 2
    MUL = 3
    DIV = 4
    INT_DIV = 5
    MOD = 6
    POW = 7
    AND = 8
    NOT = 9
    OR = 10
    EQ = 11
    NEQ = 12
    GREATER = 13
    GREATER_EQ = 14
    LOWER = 15
    LOWER_EQ = 16
    EQUALS = 17
    DOT = 18
    IF = 19
    THEN = 20
    ELSEIF = 21
    ELSE = 22
    ENDIF = 23
    CONTINUE = 24
    BREAK = 25
    SWITCH = 26
    CASE = 27
    DEFAULT = 28
    ENDSWITCH = 29
    FOR = 30
    TO = 31
    NEXT = 32
    WHILE = 33
    ENDWHILE = 34
    DO = 35
    UNTIL = 36
    COLON = 45
    OPEN_PAREN = 46
    CLOSED_PAREN = 47
    OPEN_BRACKET = 48
    CLOSED_BRACKET = 49
    GLOBAL = 50
    ARRAY = 51
    COMMA = 52
    PRINT = 53
    FUNCTION = 54
    ENDFUNCTION = 55
    BYVAL = 56
    BYREF = 57
    RETURN = 58
    PROCEDURE = 59
    ENDPROCEDURE = 60
    INT_CAST = 61
    STR_CAST = 62
    FLOAT_CAST = 63
    INPUT = 64
    LENGTH = 65
    SUBSTRING = 66
    ENDOFFILE = 67
    READLINE = 68
    WRITELINE = 69
    CLOSE = 70
    OPENREAD = 71
    OPENWRITE = 72
    CLASS = 73
    INHERITS = 74
    PUBLIC = 75
    PRIVATE = 76
    NEW = 77
    SUPER = 78
    ENDCLASS = 79
    TRUE = 80
    FALSE = 81
    ID = 100
    STRING = 101
    NUM = 102
    INT = 103
    EOF = 1000


KNOWN_TOKENS = {
    TokenContents.PLUS: TokenVals.PLUS,
    TokenContents.MINUS: TokenVals.MINUS,
    TokenContents.MUL: TokenVals.MUL,
    TokenContents.DIV: TokenVals.DIV,
    TokenContents.INT_DIV: TokenVals.INT_DIV,
    TokenContents.MOD: TokenVals.MOD,
    TokenContents.POW: TokenVals.POW,
    TokenContents.AND: TokenVals.AND,
    TokenContents.NOT: TokenVals.NOT,
    TokenContents.OR: TokenVals.OR,
    TokenContents.EQ: TokenVals.EQ,
    TokenContents.NEQ: TokenVals.NEQ,
    TokenContents.GREATER: TokenVals.GREATER,
    TokenContents.GREATER_EQ: TokenVals.GREATER_EQ,
    TokenContents.LOWER: TokenVals.LOWER,
    TokenContents.LOWER_EQ: TokenVals.LOWER_EQ,
    TokenContents.EQUALS: TokenVals.EQUALS,
    TokenContents.DOT: TokenVals.DOT,
    TokenContents.IF: TokenVals.IF,
    TokenContents.THEN: TokenVals.THEN,
    TokenContents.ELSEIF: TokenVals.ELSEIF,
    TokenContents.ELSE: TokenVals.ELSE,
    TokenContents.ENDIF: TokenVals.ENDIF,
    TokenContents.CONTINUE: TokenVals.CONTINUE,
    TokenContents.BREAK: TokenVals.BREAK,
    TokenContents.SWITCH: TokenVals.SWITCH,
    TokenContents.CASE: TokenVals.CASE,
    TokenContents.DEFAULT: TokenVals.DEFAULT,
    TokenContents.ENDSWITCH: TokenVals.ENDSWITCH,
    TokenContents.FOR: TokenVals.FOR,
    TokenContents.TO: TokenVals.TO,
    TokenContents.NEXT: TokenVals.NEXT,
    TokenContents.WHILE: TokenVals.WHILE,
    TokenContents.ENDWHILE: TokenVals.ENDWHILE,
    TokenContents.DO: TokenVals.DO,
    TokenContents.UNTIL: TokenVals.UNTIL,
    TokenContents.COLON: TokenVals.COLON,
    TokenContents.OPEN_PAREN: TokenVals.OPEN_PAREN,
    TokenContents.CLOSED_PAREN: TokenVals.CLOSED_PAREN,
    TokenContents.OPEN_BRACKET: TokenVals.OPEN_BRACKET,
    TokenContents.CLOSED_BRACKET: TokenVals.CLOSED_BRACKET,
    TokenContents.GLOBAL: TokenVals.GLOBAL,
    TokenContents.ARRAY: TokenVals.ARRAY,
    TokenContents.COMMA: TokenVals.COMMA,
    TokenContents.PRINT: TokenVals.PRINT,
    TokenContents.FUNCTION: TokenVals.FUNCTION,
    TokenContents.ENDFUNCTION: TokenVals.ENDFUNCTION,
    TokenContents.BYVAL: TokenVals.BYVAL,
    TokenContents.BYREF: TokenVals.BYREF,
    TokenContents.RETURN: TokenVals.RETURN,
    TokenContents.PROCEDURE: TokenVals.PROCEDURE,
    TokenContents.ENDPROCEDURE: TokenVals.ENDPROCEDURE,
    TokenContents.INT_CAST: TokenVals.INT_CAST,
    TokenContents.STR_CAST: TokenVals.STR_CAST,
    TokenContents.FLOAT_CAST: TokenVals.FLOAT_CAST,
    TokenContents.INPUT: TokenVals.INPUT,
    TokenContents.LENGTH: TokenVals.LENGTH,
    TokenContents.SUBSTRING: TokenVals.SUBSTRING,
    TokenContents.ENDOFFILE: TokenVals.ENDOFFILE,
    TokenContents.READLINE: TokenVals.READLINE,
    TokenContents.WRITELINE: TokenVals.WRITELINE,
    TokenContents.CLOSE: TokenVals.CLOSE,
    TokenContents.OPENREAD: TokenVals.OPENREAD,
    TokenContents.OPENWRITE: TokenVals.OPENWRITE,
    TokenContents.CLASS: TokenVals.CLASS,
    TokenContents.INHERITS: TokenVals.INHERITS,
    TokenContents.PUBLIC: TokenVals.PUBLIC,
    TokenContents.PRIVATE: TokenVals.PRIVATE,
    TokenContents.NEW: TokenVals.NEW,
    TokenContents.SUPER: TokenVals.SUPER,
    TokenContents.ENDCLASS: TokenVals.ENDCLASS,
    TokenContents.TRUE: TokenVals.TRUE,
    TokenContents.FALSE: TokenVals.FALSE,
}
KNOWN_CONTENTS_DESC = sorted(KNOWN_TOKENS.keys(), key=lambda c: len(c.value), reverse=True)
KNOWN_TOKEN_VALS = {v: k for k, v in KNOWN_TOKENS.items()}

#
# If a known token consists of alphanumeric characters then it could be part of an identifier
#
IS_ALNUM = {k.value: k.value.isalnum() for k in KNOWN_TOKENS.keys()}

#
# KNOWN_TOKENS verification
#
for __t in TokenContents:
    if __t not in KNOWN_TOKENS:
        raise RuntimeError("Token " + __t.name + " not a known variable")
for __t, __v in KNOWN_TOKENS.items():
    if __t.name != __v.name:
        raise RuntimeError("Token name: " + __t.name + " does not match token value: " + __v.name)


class ParsedToken:
    TOKEN_NAMES: ClassVar[list[str]]

    __t_text: str
    __t_content: Optional[TokenContents]
    __t_val: TokenVals

    def __init__(self, **kwargs):
        self.line_index = kwargs.pop("line_index", None)

    def __eq__(self, other: 'ParsedToken') -> bool:
        return self.text == other.text and self.content == other.content and self.val == other.val

    def __hash__(self):
        return hash((self.text, self.content, self.val))

    def __str__(self):
        if self.content:
            result = f"'{self.content.value}'"
        elif self.val:
            suffix: str = "" if self.text is None else f"['{self.text}']"
            result = f"{self.val.name}{suffix}"
        elif self.text:
            result = f"'{self.text}'"
        else:
            result = "NOTHING"
        return result

    @property
    def text(self) -> str:
        return self.__t_text

    @text.setter
    def text(self, t: str):
        self.__t_text = t
        found: list[TokenContents] = [c for c in TokenContents if c.value == t]
        if found:
            self.__t_content = found[0]
            self.__t_val = KNOWN_TOKENS[self.__t_content]
        else:
            self.__t_content = None

    @property
    def content(self) -> Optional[TokenContents]:
        return self.__t_content

    @content.setter
    def content(self, c: Optional[TokenContents]):
        self.__t_content = c
        if self.__t_content:
            self.__t_text = self.__t_content.value
            self.__t_val = KNOWN_TOKENS[self.__t_content]

    @property
    def val(self) -> TokenVals:
        return self.__t_val

    @val.setter
    def val(self, v: TokenVals):
        found: List[TokenContents] = [c for c in KNOWN_TOKENS if KNOWN_TOKENS[c] == v]
        if found:
            self.content = found[0]
        else:
            self.__t_val = v

    def set_text(self, t: str) -> 'ParsedToken':
        self.text = t
        return self

    def set_content(self, c: Optional[TokenContents]) -> 'ParsedToken':
        self.content = c
        return self

    def set_val(self, v: TokenVals) -> 'ParsedToken':
        self.val = v
        return self


ParsedToken.TOKEN_NAMES = [__v.name for __v in TokenVals]
