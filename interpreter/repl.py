import logging
from io import StringIO
from sys import stdout
from typing import Iterable
import re

from interpreter import Interpreter
from ast_executor import AstExecutor
from parsed_ast import Node
from lexer import Lexer
from parser import Parser
from tokenizer import Tokenizer

output_buffer = StringIO()


class ReplInterpreter(Interpreter):
    def __init__(self, lines: Iterable[str]):
        super().__init__(lines)
        self.__tokenizer = Tokenizer(on_new_line_input=lambda s: self.source_code.append(s))
        self.__lexer = Lexer(self.__tokenizer, lines)
        self.__parser = Parser(self.__lexer, on_parse_begin=self.on_parse_begin, on_parse_finish=self.on_parse_finish,
                               on_error=lambda *args: self.on_error(*args, post_parse=False))
        self.__executor = AstExecutor(self.__parser, on_error=self.on_executor_error, output_stream=output_buffer)

    def interpret(self):
        self.__executor.execute()

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

    def on_executor_error(self, e: Exception, nodes: list[Node]):
        """
        Obtains the erroneous lines of source code based on the erroneous nodes
        :param e: the exception
        :param nodes: erroneous nodes
        :return: None
        """
        source_code_indices = []
        for node in nodes:
            source_code_indices += list(range(node.line_index, node.end_line_index + 1))
        source_code_indices = sorted(list(set(source_code_indices)))
        self.on_error(e, source_code_indices, post_parse=True)


def get_iterable(arr: list[str]):
    for i in arr:
        yield i


# Claude wrote these two functions ;)
def detect_input_usage(code_string):
    pattern = r'input\s*\([\'"](.+?)[\'"]\)'
    match = re.search(pattern, code_string)

    if match:
        prompt = match.group(1)  # The text inside input()
        return True, prompt
    else:
        return False, None


def replace_input_with_string(code_string, new_value):
    # Example:
    # Input: x = input("Hello"), "hi"
    # Output: x =  "hi"

    pattern = r'input\s*\([^\)]*\)'
    replacement = f'"{new_value}"'
    return re.sub(pattern, replacement, code_string)


lines = []
indented_block = []
num_of_outputs = 0
depth = 0
symbols = [
    ">", ".", "-", "=", "→", "⇒", "►", "→", "≥"]

# For example: if you write "function name(parameters)", it makes no sense to evaluate this code straight away
# since it will always yield an error, due there being no way to finish the function
# In this case, the REPL should wait for endfunction to be written before evaluating all of the code block
increase_depth_words = ["for", "while", "do", "if", "switch", "function", "procedure", "class"]
decrease_depth_words = ["next", "endwhile", "until", "endif", "endswitch", "endfunction", "endprocedure", "endclass"]
# Code is evaluated when depth = 0

while "quit()" not in lines:
    error = False

    # Clear the output stream
    output_buffer.seek(0)
    output_buffer.truncate(0)

    next_line = input((symbols[depth % len(symbols)] * 3) + " ")

    # Handle inputs manually and rewrite them to make code deterministic
    is_input, text = detect_input_usage(next_line)
    if is_input:
        value_inputted = input(text)
        next_line = replace_input_with_string(next_line, value_inputted)

    # Use split to avoid issues such as "if" being in "endif" and being flagged as both incrementer and decrementer of depth
    for i in increase_depth_words:
        # All increase/decrease depth words are the first words in their line
        # The only exception is in classes where public/private may be specified
        if i == next_line.split()[0] or ((next_line.split()[0] == "public" or next_line.split()[0] == "private") and i == next_line.split()[1]):
            depth += 1
            break

    for i in decrease_depth_words:
        if i == next_line.split()[0] or ((next_line.split()[0] == "public" or next_line.split()[0] == "private") and i == next_line.split()[1]):
            depth -= 1
            break

    if depth <= 0:
        run = lines + indented_block

        run.append(next_line)
        try:
            ReplInterpreter(get_iterable(run)).interpret()
        except:
            error = True
            depth = 0
            indented_block = []

        if not error:
            lines = run
            indented_block = []
            output = output_buffer.getvalue().split('\n')

            # Remove empty [""]
            output.pop()

            # Relies on ERL programs being deterministic, and yielding the same output on every run
            if len(output) > num_of_outputs:
                for i in range(num_of_outputs, len(output)):
                    print(output[i])
            num_of_outputs = len(output)
    else:
        # If depth != 0, don't run code
        indented_block.append(next_line)
