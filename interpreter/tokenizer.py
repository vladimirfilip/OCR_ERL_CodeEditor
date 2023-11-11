from typing import Iterator, ClassVar, Iterable, Tuple, Optional, Callable

from parsed_token import TokenVals, ParsedToken, KNOWN_CONTENTS_DESC


class Tokenizer:
    """Converter of line sequences into a sequence of tokens, associated with their token value"""
    __STR_SEP: ClassVar[str] = '"'
    __STR_ESCAPE: ClassVar[str] = '\\'
    __COMMENT: ClassVar[str] = '//'
    __UNDERSCORE: ClassVar[str] = '_'
    CURRENT_LINE: int

    def __init__(self, on_new_line_input: Optional[Callable] = None):
        """
        Takes an optional Callable as input which is called for every line in the source code input that is iterated over.
        """
        self.on_new_line_input = on_new_line_input

    def tokenize(self, lines: Iterable[str]) -> Iterator[ParsedToken]:
        """Converts an iterable of lines into an iterator of tokens.

        :param lines: succession of lines.
        :return: succession of tokens, up to comment delimiter.
        """
        Tokenizer.CURRENT_LINE = 0
        for line in lines:
            if self.on_new_line_input is not None:
                self.on_new_line_input(line)
            line = self.__rem_comment(line)
            for token in self.__tokenize(line):
                yield token
            Tokenizer.CURRENT_LINE += 1

    def __rem_comment(self, line: str) -> str:
        """Removes the commented part from a line.

        :param line: line of code to remove comments from.
        :return: line without comments.
        """
        result: str = ""
        for chunk, with_comment in self.__str_chunks(line):
            result += chunk
            if with_comment:
                break
        return result

    def __str_chunks(self, line: str) -> Iterator[Tuple[str, bool]]:
        """Converts a line into an iterator of chunks, each chunk being either a string or a non-string. It returns
        with each chunk a flag indicating whether the chunk ended in a comment.

        :param line: line to split into string or non-string chunks
        :return: iterator of chunks
        """
        str_pos: int = line.find(Tokenizer.__STR_SEP)
        #
        # If there is no string separator present, the line is checked for the presence of a comment symbol ('//')
        # If a comment is found, the line up to the comment is returned and True to indicate that the chunk ended in a comment.
        # If a comment is not found, the whole line is outputted as a chunk and False to indicate that there is comment
        #
        if str_pos < 0:
            comment_pos: int = line.find(Tokenizer.__COMMENT)
            yield (line, False) if comment_pos < 0 else (line[:comment_pos], True)
        #
        # str_pos > 0 indicates that the line consists of a chunk that is not a string and a chunk that is a string, and so
        # self.__chunks_with_non_str is called
        #
        elif str_pos > 0:
            for chunk, with_comment in self.__chunks_with_non_str(line, str_pos):
                yield chunk, with_comment
        #
        # str_pos == 0 indicates that the line starts with '"', and so self.__chunks_with_str is called
        #
        elif str_pos == 0:
            for chunk, with_comment in self.__chunks_with_str(line):
                yield chunk, with_comment

    def __chunks_with_non_str(self, line: str, str_pos: int) -> Iterator[Tuple[str, bool]]:
        """Converts a line that does not start with a string into a succession of chunks: the first one a non-string
        and the others either a string or a non-string, up to the comment delimiter (if present)

        :param line: line to split into a non-string chunk and other chunks
        :param str_pos: position of the first string within the line
        :return: iterator of chunks
        """
        comment_pos: int = line.find(Tokenizer.__COMMENT, 0, str_pos)
        yield (line[:comment_pos], True) if comment_pos >= 0 else (line[:str_pos], False)
        if comment_pos < 0:
            for chunk, with_comment in self.__str_chunks(line[str_pos:]):
                yield chunk, with_comment

    def __chunks_with_str(self, line: str) -> Iterator[Tuple[str, bool]]:
        """Converts a line that starts with a string into a succession of chunks: the first one a string and the
        others either a string or a non-string, up to the comment delimiter (if present)

        :param line: line to split into a string chunk and other chunks
        :return: iterator of chunks
        """
        first: str = Tokenizer.__split_first_str(line)
        yield first, False
        for chunk, with_comment in self.__str_chunks(line[len(first):]):
            yield chunk, with_comment

    @staticmethod
    def __split_first_str(line: str) -> str:
        """Splits the string from the beginning of a line. It is expected that the line does start with a string.
        This method recognizes escaped characters, and it raises an error when encountering un-closed strings (such
        as "this is not closed).

        :param line: line whose front to split the string from.
        :return: string from the front of the line
        """
        #
        # The resulted string must start with the string separator character
        #
        result: str = Tokenizer.__STR_SEP
        escaped: bool = False
        pos: Optional[int] = None
        for i in range(1, len(line)):
            pos = i
            c: str = line[i]
            result += c
            if c == Tokenizer.__STR_ESCAPE:
                #
                # The escape character needs to be added, in order not to alter the original string, but now we are
                # in escaped mode
                #
                escaped = True
            elif c == Tokenizer.__STR_SEP:
                #
                # End-of-string encountered. If escaped, continue. If not escaped, stop iterating
                #
                if escaped:
                    escaped = False
                else:
                    break
            elif escaped:
                #
                # Ordinary character that has just been escaped. It's been added to the resulted string, escaping
                # needs to be turned off.
                #
                escaped = False
        #
        # If we exhausted the whole line without finding the end of the string, then the string was not properly
        # closed. Raise an error.
        #
        if pos == len(line) - 1 and Tokenizer.__STR_SEP != line[-1]:
            raise SyntaxError("Invalid string literal: " + result)
        #
        # The string was parsed and it is properly delimited. Just return it
        #
        return result

    def __tokenize(self, line: str) -> Iterator[ParsedToken]:
        """Converts a line into a sequence of recognized tokens of the language.
        :param line: line to tokenize.
        :return: succession of tokens.
        """
        for chunk, _ in self.__str_chunks(line):
            if chunk and chunk[0] == Tokenizer.__STR_SEP:
                #
                # If a string, returns string literal token
                # with text set to the chunk minus the '"' on both ends.
                #
                yield (ParsedToken(line_index=Tokenizer.CURRENT_LINE)
                       .set_val(TokenVals.STRING).set_text(chunk[1:-1]))
            else:
                #
                # Eliminate white spaces
                #
                for meta_token in chunk.split():
                    #
                    # Split each sequence of non-whitespaces into recognized tokens of the language
                    #
                    for token in Tokenizer.__tokens(meta_token):
                        yield token

    @staticmethod
    def __tokens(meta_token: str) -> Iterator[ParsedToken]:
        """Splits a sequence of non-whitespaces into a succession of recognized tokens of the language.

        :param meta_token: sequence of non-whitespaces to split.
        :return: succession of recognized tokens.
        """

        i: int = 0
        while i < len(meta_token):
            #
            # Determine the nature of the token based on the current character
            # (and in the case of number detection, 1-2 characters ahead)
            #
            first_char: str = meta_token[i]
            second_char: str = meta_token[i + 1:i + 2]  # this yields '' if out of bounds
            if Tokenizer.__id_cond(first_char):
                t, i = Tokenizer.__id_token_or_known(first_char, meta_token, i)
                yield t
            elif Tokenizer.__num_cond(first_char, second_char):
                t, i = Tokenizer.__num_token(first_char, second_char, meta_token, i)
                yield t
            else:
                #
                # Used to detect known tokens that do not start with an id beginning character, like '+', '-', etc
                #
                skip: bool = False
                for known_content in KNOWN_CONTENTS_DESC:
                    if meta_token.startswith(known_content.value, i):
                        yield ParsedToken(line_index=Tokenizer.CURRENT_LINE).set_content(known_content)
                        i += len(known_content.value)
                        skip = True
                        break
                if skip:
                    #
                    # Found a known token, resume from the position past of it
                    #
                    continue
                #
                # Any other possibility gets rejected
                #
                raise SyntaxError("Unrecognized input: " + meta_token[i:])

    @staticmethod
    def __id_cond(first_char: str) -> bool:
        return first_char == Tokenizer.__UNDERSCORE or first_char.isalpha()

    @staticmethod
    def __id_token_or_known(first_char: str, meta_token: str, i: int) -> Tuple[ParsedToken, int]:
        #
        # Valid identifiers start with alphabetical characters or underscores and continue with those as well as
        # digits. Will therefore extract the largest possible substring such that these criteria are met
        #
        text: str = first_char
        i += 1
        while i < len(meta_token) and (meta_token[i] == Tokenizer.__UNDERSCORE or meta_token[i].isalnum()):
            text += meta_token[i]
            i += 1
        #
        # set_val() set the token value to ID, but set_text() overrides that if the text matches that
        # of a keyword or symbol in KNOWN_TOKENS
        #
        return ParsedToken(line_index=Tokenizer.CURRENT_LINE).set_val(TokenVals.ID).set_text(text), i

    @staticmethod
    def __num_cond(first_char: str, second_char: str) -> bool:
        return first_char.isdigit() or first_char == "." and second_char.isdigit()

    @staticmethod
    def __num_token(first_char: str, second_char: str, meta_token: str, i: int) -> Tuple[ParsedToken, int]:

        def is_number(s: str) -> bool:
            """Utility function to check if a string represents a number.
            :param s: string to check for being a number.
            :return: true if the string is a number, false otherwise
            """
            if not s:
                return False
            try:
                float(s)
            except ValueError:
                return False
            return True

        #
        # Extracts the longest possible substring out of meta_token such that the substring represents a valid number
        #
        if first_char.isdigit():
            token_len = 1
        else:
            assert first_char == '.' and second_char.isdigit()
            token_len = 2
        while i + token_len < len(meta_token):
            if is_number(meta_token[i:i + token_len + 1]):
                token_len += 1
            else:
                break
        text: str = meta_token[i:i + token_len]
        #
        # If the substring represents an int literal, the value of the new token should be TokenVals.INT
        # If not, the value should be TokenVals.NUM
        #
        val = TokenVals.INT if text.isdigit() else TokenVals.NUM
        return ParsedToken(line_index=Tokenizer.CURRENT_LINE).set_val(val).set_text(text), i + token_len
