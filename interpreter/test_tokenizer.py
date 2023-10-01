from typing import Iterator
from unittest import TestCase

from parsed_token import ParsedToken, TokenVals, KNOWN_CONTENTS_DESC, IS_ALNUM, TokenContents
from tokenizer import Tokenizer


class TestTokenizer(TestCase):

    def __init__(self, method_name='runTest'):
        super().__init__(method_name)
        self.tokenizer = Tokenizer()

    #
    # Empty lines
    #
    def test_tokenize_nothing(self):
        self.__test_lines([], [])

    #
    # Comments
    #
    def test_tokenize_only_empty_comment(self):
        self.__test_line("//", [])

    def test_tokenize_only_long_comment(self):
        self.__test_line("// this is a comment", [])

    def test_tokenize_whitespaces_before_comment(self):
        self.__test_line("    // this is a comment", [])

    #
    # One token, single or repeated
    #
    def test_one_known_token(self):
        for c in KNOWN_CONTENTS_DESC:
            self.__test_one_known_token(c.value)

    def __test_one_known_token(self, text: str):
        t: ParsedToken = ParsedToken().set_text(text)
        id_token: ParsedToken = ParsedToken().set_val(TokenVals.ID)
        self.__test_line(text, [t])
        self.__test_line(text + " " * 100, [t])
        self.__test_line(" " * 100 + text, [t])
        expected = [t]
        if text == "/":
            expected = []
        self.__test_line(f"{text}//{text}", expected)
        self.__test_line((text + " ") * 100, [t] * 100)
        expected = [t] * 100
        if IS_ALNUM[text]:
            expected = [id_token.set_text(text * 100)]
        if text == "/":
            expected = []
        if text == "=":
            expected = [ParsedToken().set_val(TokenVals.EQ)] * 50
        self.__test_line(text * 100, expected)

    def test_global_keyword(self):
        ts: list[ParsedToken] = list(self.tokenizer.tokenize(["global"]))
        self.assertEqual(1, len(ts))
        self.assertEqual(TokenVals.GLOBAL, ts[0].val)

    def test_globalee_id(self):
        ts: list[ParsedToken] = list(self.tokenizer.tokenize(["globalee"]))
        self.assertEqual(1, len(ts))
        self.assertEqual(TokenVals.ID, ts[0].val)

    def test_global_space_keyword(self):
        ts: list[ParsedToken] = list(self.tokenizer.tokenize(["global "]))
        self.assertEqual(1, len(ts))
        self.assertEqual(TokenVals.GLOBAL, ts[0].val)

    #
    # Two tokens
    #
    def test_two_known_tokens(self):
        for i in range(len(KNOWN_CONTENTS_DESC) - 1):
            for j in range(i + 1, len(KNOWN_CONTENTS_DESC)):
                c1, c2 = KNOWN_CONTENTS_DESC[i], KNOWN_CONTENTS_DESC[j]
                self.__test_two_known_tokens(c1.value, c2.value)

    def __test_two_known_tokens(self, text1: str, text2: str):
        t1: ParsedToken = ParsedToken().set_text(text1)
        t2: ParsedToken = ParsedToken().set_text(text2)
        #
        # If separated by spaces, tokens must be distinct in any case
        #
        self.__test_line(text1 + " " + text2, [t1, t2])
        self.__test_line(text2 + " " + text1, [t2, t1])
        #
        # Testing that concatenating them (with either text2 first or text1 first) yields correct result
        #
        self.__test_concat_two_known_tokens(text1, text2)
        self.__test_concat_two_known_tokens(text2, text1)
        #
        # comment should stop the second token from being recognised
        #
        self.__test_line(text1 + "//" + text2, [t1] if text1 != "/" else [])
        self.__test_line(text2 + "//" + text1, [t2] if text2 != "/" else [])

    def __test_concat_two_known_tokens(self, text1: str, text2: str):
        #
        # Concatenates the two known token texts together
        # If they are both alphanumeric, then tokenizer should return one ID token
        # If not, they should be identified as separate known tokens
        #
        both_alnum: bool = IS_ALNUM[text1] and IS_ALNUM[text2]
        boolean_operators: list[str] = [TokenContents.EQ.value, TokenContents.NEQ.value, TokenContents.LOWER.value,
                                        TokenContents.LOWER_EQ.value, TokenContents.GREATER.value,
                                        TokenContents.GREATER_EQ.value]

        expected: list[ParsedToken] = [ParsedToken().set_text(text1), ParsedToken().set_text(text2)]

        if both_alnum:
            expected = [self.id_token(text1 + text2)]

        if text1 in boolean_operators and (text2 == "=" or text2 == "=="):
            concat_str = text1 + text2
            expected = [self.id_token(concat_str[:2])]
            if len(concat_str) > 2:
                expected.append(self.id_token(concat_str[2:]))

        if text1 == TokenContents.ELSE.value and text2 == TokenContents.IF.value:
            expected = [self.id_token(TokenContents.ELSEIF.value)]

        if text1 == "=" and text2 == "==":
            expected = [self.id_token("=="), self.id_token("=")]

        self.__test_line(text1 + text2, expected)

    #
    # Testing identifiers and string literals
    #
    def test_str_literal_detection(self):
        #
        # Testing that comments are removed properly
        # and string literals are separated from keywords even, with or without spaces between them
        #
        self.__test_line("__gl_ob//alarray", [self.id_token("__gl_ob")])
        self.__test_line('glob "alarray"', [self.id_token("glob"),
                                            self.str_token("alarray")])
        self.__test_line('glob"alarray"', [self.id_token("glob"),
                                           self.str_token("alarray")])

    def test_str_literals_properly_closed(self):
        #
        # String literals without closing '"' on the same line should raise a SyntaxError
        #
        with self.assertRaises(SyntaxError):
            self.__test_line('"unfinished string', [])
        lines = [
            '"string starts here',
            '    and ends here"'
        ]
        with self.assertRaises(SyntaxError):
            self.__test_lines(lines, [])

    def test_split_identifiers_by_newline(self):
        #
        # identifiers split between two lines must be recognised as two separate tokens
        #
        lines = [
            'glo',
            'bal'
        ]
        self.__test_lines(lines, [self.id_token("glo"), self.id_token("bal")])

    def test_newline_has_no_effect(self):
        #
        # Newlines should not affect sequence of tokens produced,
        # unrecognised tokens should all be either identifiers, nums, ints or string literals
        #
        lines = [
            'global',
            'array',
            'x',
            '[-5]',
            'x',
            '[',
            '2+1',
            ']=5.2'
        ]
        expected_sequence = [
            ParsedToken().set_val(TokenVals.GLOBAL),
            ParsedToken().set_val(TokenVals.ARRAY),
            self.id_token('x'),
            ParsedToken().set_val(TokenVals.OPEN_BRACKET),
            ParsedToken().set_val(TokenVals.MINUS),
            self.int_token('5'),
            ParsedToken().set_val(TokenVals.CLOSED_BRACKET),
            self.id_token('x'),
            ParsedToken().set_val(TokenVals.OPEN_BRACKET),
            self.int_token('2'),
            ParsedToken().set_val(TokenVals.PLUS),
            self.int_token('1'),
            ParsedToken().set_val(TokenVals.CLOSED_BRACKET),
            ParsedToken().set_val(TokenVals.EQUALS),
            self.num_token('5.2'),
        ]
        self.__test_lines(lines, expected_sequence)

    def test_str_literal_contents(self):
        #
        # String literals with keywords or numbers as contents should still be interpreted as string literals
        #
        line = 'global str_literal="123456 -.7890" + "str_literal here" -.33'
        expected_sequence = [
            ParsedToken().set_val(TokenVals.GLOBAL),
            self.id_token("str_literal"),
            ParsedToken().set_val(TokenVals.EQUALS),
            self.str_token("123456 -.7890"),
            ParsedToken().set_val(TokenVals.PLUS),
            self.str_token("str_literal here"),
            ParsedToken().set_val(TokenVals.MINUS),
            self.num_token('.33')
        ]
        self.__test_line(line, expected_sequence)

    #
    # Testing number and integer literals
    #
    def test_valid_nums_accepted(self):
        #
        # Testing all valid numerics are accepted
        #
        line = '.567 89 10.0'
        expected_sequence = [
            self.num_token('.567'),
            self.int_token('89'),
            self.num_token('10.0')
        ]
        self.__test_line(line, expected_sequence)

    def test_comments_interrupt_numerics(self):
        #
        # Interrupting numerics with comments
        #
        lines = ['.//567 89 10.0', '.12 -//34 .567 89 10.0']
        expected_sequence = [
            ParsedToken().set_val(TokenVals.DOT),
            self.num_token('.12'),  # second line
            ParsedToken().set_val(TokenVals.MINUS)
        ]
        self.__test_lines(lines, expected_sequence)

    def test_letters_split_numerics(self):
        #
        # Letters between numerics should split it into a numeric token or symbol tokens on the left
        # and an identifier token on the right
        #
        line = '.a12 3b4 .c567 -8d9 10e.0'
        expected_sequence = [
            ParsedToken().set_val(TokenVals.DOT),
            self.id_token("a12"),
            self.int_token("3"),
            self.id_token("b4"),
            ParsedToken().set_val(TokenVals.DOT),
            self.id_token("c567"),
            ParsedToken().set_val(TokenVals.MINUS),
            self.int_token("8"),
            self.id_token("d9"),
            self.int_token("10"),
            self.id_token("e"),
            self.num_token(".0")
        ]
        self.__test_line(line, expected_sequence)

    #
    # Utility methods
    #

    def __test_line(self, line: str, expected: list[ParsedToken]):
        self.__test_lines([line], expected)

    def __test_lines(self, lines: list[str], expected: list[ParsedToken]):
        parsed: Iterator[ParsedToken] = self.tokenizer.tokenize(lines)
        i: int = 0
        for t in parsed:
            self.assertLess(i, len(expected), f"Expected only {len(expected)} tokens but found {i}")
            e = expected[i]
            self.assertEqual(e, t, f"Position #{i} mismatch: expected={e}, parsed={t}")
            i += 1
        self.assertGreaterEqual(i, len(expected), f"Expected {len(expected)} tokens, received only {i}")

    @staticmethod
    def id_token(txt: str) -> ParsedToken:
        return ParsedToken().set_val(TokenVals.ID).set_text(txt)

    @staticmethod
    def str_token(txt: str) -> ParsedToken:
        return ParsedToken().set_val(TokenVals.STRING).set_text(txt)

    @staticmethod
    def num_token(txt: str) -> ParsedToken:
        return ParsedToken().set_val(TokenVals.NUM).set_text(txt)

    @staticmethod
    def int_token(txt: str) -> ParsedToken:
        return ParsedToken().set_val(TokenVals.INT).set_text(txt)
