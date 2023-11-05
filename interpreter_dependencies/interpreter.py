import json
from typing import Iterable, Iterator
from sys import argv, stderr
from time import time
from ast_executor import AstExecutor
from parsed_ast import Node
from lexer import Lexer
from parser import Parser
from tokenizer import Tokenizer
from ast_to_json import ASTToJsonParser
import logging


class Interpreter:
    def __init__(self, lines: Iterable[str]):
        logging.basicConfig(filename="interpreterlog.log", format="[%(asctime)s %(levelname)s] %(message)s", level=logging.DEBUG)
        self.source_code: list[str] = []
        self.parse_begin_time = None
        self.__tokenizer = Tokenizer(on_new_line_input=lambda s: self.source_code.append(s))
        self.__lexer = Lexer(self.__tokenizer, lines)
        self.__parser = Parser(self.__lexer, on_parse_begin=self.on_parse_begin, on_parse_finish=self.on_parse_finish, on_error=lambda *args: self.on_error(*args, post_parse=False))
        self.__executor = AstExecutor(self.__parser, on_error=self.on_executor_error)

    def interpret(self):
        logging.debug("\n\n" + "#" * 50 + "\n" + "BEGINNING EXECUTION" + "\n" + "#" * 50)
        self.__executor.execute()

    def on_error(self, e: Exception, source_code_indices: list[int], post_parse: bool):
        heading = f"Error in parsing:" if not post_parse else f"Error during execution:"
        code_snippet = self.get_formatted_source_code_lines(source_code_indices)
        ex_msg = f"{e.__class__.__name__}: {str(e)}"
        whole_msg = [heading] + code_snippet + [ex_msg]
        max_line_len = max(len(line) for line in whole_msg)
        whole_msg.insert(1, "-" * max_line_len)
        whole_msg.insert(-1, "-" * max_line_len)
        for line in whole_msg:
            logging.error(line)
        stderr.write("\n".join(whole_msg) + "\n")
        stderr.flush()
        exit(-1)

    def on_executor_error(self, e: Exception, nodes: list[Node]):
        source_code_indices = []
        for node in nodes:
            source_code_indices += list(range(node.line_index, node.end_line_index + 1))
        source_code_indices = sorted(list(set(source_code_indices)))
        self.on_error(e, source_code_indices, post_parse=True)

    def on_parse_finish(self, ast_node: Node):
        logging.debug(f" ### FINISHED PARSING IN {time() - self.parse_begin_time} seconds ###")
        logging.debug(f"COMPLETE SOURCE CODE")
        formatted_source_code = self.get_formatted_source_code_lines(list(range(len(self.source_code))))
        for line in formatted_source_code:
            logging.debug(line)
        ast_json = ASTToJsonParser().parse(ast_node)
        logging.debug(json.dumps(ast_json, indent=4))

    def on_parse_begin(self):
        self.parse_begin_time = time()

    def get_formatted_source_code_lines(self, indices: list[int]) -> list[str]:
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
