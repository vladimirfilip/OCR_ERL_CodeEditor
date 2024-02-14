import json
from typing import Iterable, Iterator, Optional
from sys import argv, stdout
from time import time_ns
from ast_executor import AstExecutor
from parsed_ast import Node
from lexer import Lexer
from parser import Parser
from tokenizer import Tokenizer
from ast_to_json import ASTToJsonParser
import logging


class Interpreter:
    def __init__(self, lines: Iterable[str]):
        """
        Sets up logging and initialises lexer, parser and executor with input source code lines
        :param lines: iterable of strings
        """
        logging.basicConfig(filename="interpreterlog.log", format="[%(asctime)s:%(created).9f %(levelname)s] %(message)s", level=logging.DEBUG)
        self.source_code: list[str] = []
        self.parse_begin_time = None
        self.__tokenizer = Tokenizer(on_new_line_input=lambda s: self.source_code.append(s))
        self.__lexer = Lexer(self.__tokenizer, lines)
        self.__parser = Parser(self.__lexer, on_parse_begin=self.on_parse_begin, on_parse_finish=self.on_parse_finish, on_error=lambda *args: self.on_error(*args, post_parse=False))
        self.__executor = AstExecutor(self.__parser, on_error=self.on_executor_error)

    def interpret(self):
        logging.debug("\n\n" + "#" * 50 + "\n" + "BEGINNING EXECUTION" + "\n" + "#" * 50)
        self.__executor.execute()

    def log_full_source_code(self):
        logging.debug(f"COMPLETE SOURCE CODE")
        formatted_source_code = self.get_formatted_source_code_lines(list(range(len(self.source_code))))
        for line in formatted_source_code:
            logging.debug(line)

    def on_error(self, e: Exception, source_code_indices: list[int], post_parse: bool):
        """
        Formats the error message by including the lines in the original source code in which the error took place
        :param e: the exception
        :param source_code_indices: the indices of the erroneous source code lines
        :param post_parse: True if error was thrown during execution, else False
        :return: None
        """
        heading = f"Error in parsing:" if not post_parse else f"Error during execution:"
        code_snippet = self.get_formatted_source_code_lines(source_code_indices)
        if not post_parse:
            self.log_full_source_code()
            logging.debug("....")
        ex_msg = f"{e.__class__.__name__}: {str(e)}"
        whole_msg = [heading] + code_snippet + [ex_msg]
        max_src_code_line_len = max(len(line) for line in code_snippet)
        whole_msg.insert(1, "-" * max_src_code_line_len)
        whole_msg.insert(-1, "-" * max_src_code_line_len)
        for line in whole_msg:
            logging.error(line)
        stdout.write("\n".join(whole_msg) + "\n")
        stdout.flush()
        exit(-1)

    def on_executor_error(self, e: Exception, nodes: list[Node]):
        """
        Obtains the erroneous lines of source code baed on the erroneous nodes
        :param e: the exception
        :param nodes: erroneous nodes
        :return: None
        """
        source_code_indices = []
        for node in nodes:
            source_code_indices += list(range(node.line_index, node.end_line_index + 1))
        source_code_indices = sorted(list(set(source_code_indices)))
        self.on_error(e, source_code_indices, post_parse=True)

    def on_parse_finish(self, ast_node: Optional[Node]):
        """
        Logs how long parsing took and the AST produced in JSON form
        :param ast_node: the root of produced AST
        :return: None
        """
        logging.debug(f" ### FINISHED PARSING IN {time_ns() - self.parse_begin_time} nanoseconds ###")
        self.log_full_source_code()
        if ast_node is not None:
            ast_json = ASTToJsonParser().parse(ast_node)
            logging.debug(json.dumps(ast_json, indent=4))

    def on_parse_begin(self):
        self.parse_begin_time = time_ns()

    def get_formatted_source_code_lines(self, indices: list[int]) -> list[str]:
        """
        Formats source code lines, adding line number
        :param indices: source code line indices
        :return: list of formatted strings
        """
        result = []
        for i, n in enumerate(indices):
            if i > 0 and indices[i - 1] < n - 1:
                result.append("...")
            result.append(f"Line {str(n + 1).zfill(len(str(len(self.source_code))))}: {self.source_code[n]}")
        return result


def get_lines(file_name: str) -> Iterator[str]:
    with open(file_name, "r") as file:
        while True:
            line = file.readline()
            if not line:
                break
            yield line.rstrip("\n")


if __name__ == "__main__":
    assert len(argv) > 1, "name of input text file required"
    Interpreter(get_lines(argv[1])).interpret()
