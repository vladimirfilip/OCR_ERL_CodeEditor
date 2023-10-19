from typing import Optional, Iterator, Iterable
from parsed_token import ParsedToken
from tokenizer import TokenVals, Tokenizer


class Lexer:
    """Converter of a sequence of lines into a sequence of tokens with the possibility to feed back tokens in front,
    when needed. """
    __tokenizer: Tokenizer
    __buf: list[ParsedToken] = []
    __tokens: Iterator[ParsedToken]

    def __init__(self, tokenizer: Tokenizer, lines: Iterable[str]):
        """Initializes the lexer with a tokenizer and the sequence of lines to extract tokens from.

        :param tokenizer: converter of lines into tokens.
        :param lines: sequence of lines to tokenize.
        """
        self.__tokenizer = tokenizer
        self.__tokens = self.__tokenizer.tokenize(lines)

    def next(self) -> Optional[ParsedToken]:
        """Gets the next token, associated with its token value.

        :return: next token or None if no more tokens exist.
        """
        if self.__buf:
            return self.__buf.pop(0)
        result: (str, TokenVals) = next(self.__tokens, ParsedToken().set_val(TokenVals.EOF))
        if result.val == TokenVals.EOF:
            result = None
        return result

    def push_front(self, token: ParsedToken) -> None:
        """Feeds the given token back into the lexer so that it will be retrieved again.

        :param token: token to push back to the front token stream.
        :return: None.
        """
        if self.__buf is None:
            raise RuntimeError("Tokenization not started")
        self.__buf.insert(0, token)
