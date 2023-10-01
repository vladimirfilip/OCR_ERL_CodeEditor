from typing import Optional, List
from unittest import TestCase
from lexer import Lexer
from parsed_token import ParsedToken, TokenVals
from tokenizer import Tokenizer


class TestLexer(TestCase):
    def test_next(self):
        #
        # If tokenization input is empty, lexer should have an empty buffer
        #
        lines: List[str] = []
        lexer: Lexer = self.init_lexer(lines)
        self.assertIsNone(lexer.next())

    def test_buffer_empty_after_full_retrieval(self):
        #
        # After popping all tokens, lexer should have an empty buffer and all tokens should be returned
        #
        lines: list[str] = ['x=5']
        lexer: Lexer = TestLexer.init_lexer(lines)
        tokens: List[Optional[ParsedToken]] = [lexer.next(), lexer.next(), lexer.next()]
        self.assertIsNone(lexer.next())
        for token in tokens:
            self.assertIsNotNone(token)

    def test_buffer_priority(self):
        #
        # Testing that lexer returns tokens from self.__buf before returning tokens from self.__tokens
        #
        lines = ['x=5']
        lexer = TestLexer.init_lexer(lines)
        # populating self.__buf
        lexer.push_front(ParsedToken().set_val(TokenVals.CLOSED_PAREN))
        lexer.push_front(ParsedToken().set_val(TokenVals.OPEN_PAREN))
        #
        # Total number of tokens stored in the lexer should now be 5,
        # therefore calling lexer.next() for the 6th time should return None
        # Tokens appended to self.__buf (i.e. the open and closed paren tokens) are expected first
        #
        stored_tokens = [lexer.next() for _ in range(6)]
        expected_tokens = [
            ParsedToken().set_val(TokenVals.OPEN_PAREN),
            ParsedToken().set_val(TokenVals.CLOSED_PAREN),
            ParsedToken().set_val(TokenVals.ID).set_text("x"),
            ParsedToken().set_val(TokenVals.EQUALS),
            ParsedToken().set_val(TokenVals.INT).set_text("5"),
            None
        ]
        self.assertEqual(stored_tokens, expected_tokens)

    def test_push_front(self):
        #
        # Verifying pushing token pushes to the front of a buffer
        #
        lines: List[str] = ['=5']
        lexer: Lexer = TestLexer.init_lexer(lines)
        # pushing 'x' identifier token
        token_to_add = ParsedToken().set_val(TokenVals.ID).set_text("x")
        lexer.push_front(token_to_add)
        tokens: List[Optional[ParsedToken]] = [lexer.next() for _ in range(4)]
        # 'x' identifier token should come first
        expected_tokens = [token_to_add,
                           ParsedToken().set_val(TokenVals.EQUALS),
                           ParsedToken().set_val(TokenVals.INT).set_text("5"),
                           None]
        self.assertEqual(tokens, expected_tokens)

    @staticmethod
    def init_lexer(lines):
        return Lexer(Tokenizer(), lines)
