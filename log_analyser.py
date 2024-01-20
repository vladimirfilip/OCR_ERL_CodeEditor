from re import match, findall
from sys import argv
from typing import Iterable

from _decimal import Decimal

assert len(argv) > 1
log_path = argv[1]

node_name_stack: list[str] = []
node_time_stack: list[Decimal] = []
log = open(log_path, "r")


def get_lines():
    while (s := log.readline()) != "":
        yield s.rstrip("\n")
    yield None


line_iterator = get_lines()

num_times_executed: int = 0
parse_times: list[Decimal] = []
cumulative_time_to_execute: Decimal = Decimal(0)
node_to_eval_times: dict[str, list[Decimal]] = {}

DEBUG_PREFIX: str = r"\[\d+-\d+-\d+ \d+:\d+:\d+,\d+:\d+\.\d+ DEBUG] "
EXECUTION_BEGIN_PREFIX: str = DEBUG_PREFIX + "Executing node"
EXECUTION_END_PREFIX: str = DEBUG_PREFIX + "Executed node"
EVALUATION_BEGIN_PREFIX: str = DEBUG_PREFIX + "Evaluating node"
EVALUATION_END_PREFIX: str = DEBUG_PREFIX + "Evaluated node"
PARSE_FINISH: str = DEBUG_PREFIX + r" ### FINISHED PARSING IN \d+ nanoseconds ###"


def get_node_name_and_log_time(line: str) -> tuple[str, Decimal]:
    """
    Extracts node name, which is the first word after 'node', and Unix time, existing as the last decimal number in the log line,
    :param line: the line from which name and Unix time is extracted
    :return: tuple of name and log time
    """
    node_name_begin: int = line.index("node ") + 5
    node_name_end: int = line.find(" ", node_name_begin)
    name: str = line[node_name_begin:node_name_end]
    return name, Decimal(findall(r"\d+\.\d+", line)[-1])


while True:
    line = next(line_iterator)
    if line is None:
        break
    #
    # Start of execution log leads to increment of counter
    #
    if line == "##################################################":
        line = next(line_iterator)
        assert line == "BEGINNING EXECUTION"
        line = next(line_iterator)
        assert line == "##################################################"
        num_times_executed += 1
    #
    # Parsing time is extracted
    #
    if match(PARSE_FINISH, line):
        parse_time = Decimal(findall(r"\d+", line)[-1])
        parse_times.append(parse_time)
    #
    # Start time and name of the node whose execution has just started is extracted and to their respective stacks.
    #
    if match(EVALUATION_BEGIN_PREFIX, line) or match(EXECUTION_BEGIN_PREFIX, line):
        node_name, node_time = get_node_name_and_log_time(line)
        node_name_stack.append(node_name)
        node_time_stack.append(node_time)
        cumulative_time_to_execute = Decimal(0)
    #
    # End time and name of the node whose execution has just ended is extracted and subtracted by the start time from the top of the stack.
    # The time taken to execute child nodes is taken into account and also subtracted, store in cumulative_time_to_execute
    # cumulative_time_to_execute is incremented with this time, as the node that has just ended is a child node of a node whose execution will finish later on.
    # This way, only the evaluation time for each individual node is counted, regardless of its child nodes.
    #
    if match(EVALUATION_END_PREFIX, line) or match(EXECUTION_END_PREFIX, line):
        node_name, node_time = get_node_name_and_log_time(line)
        assert node_name == node_name_stack[-1], f"Node {node_name} that has finished processing does not match {node_name_stack[-1]} that is at the top of the stack"
        node_name_stack.pop()
        node_end_time: Decimal = node_time
        node_start_time: Decimal = node_time_stack.pop()
        assert node_end_time >= node_start_time, f"Node {node_name} started at {node_start_time} but ended at {node_end_time}"
        node_eval_time = node_end_time - node_start_time - cumulative_time_to_execute
        cumulative_time_to_execute += node_eval_time
        if node_name not in node_to_eval_times:
            node_to_eval_times[node_name] = []
        node_to_eval_times[node_name].append(node_eval_time)


def get_min_mean_max(data_points: list[list[Decimal]]):
    for row in data_points:
        _min, mean, _max = min(row), sum(row) / Decimal(len(row)), max(row)
        yield _min, mean, _max


def draw_table(column_headers: list[str], row_names: list[str], data_points: Iterable[Iterable[Decimal]]):
    """
    Outputs to console a table of data points with properly formatted rows and columns

    :param column_headers: names of each column header from left to right
    :param row_names: names of each row from top to bottom
    :param data_points: 2D array of elements
    :return: None, outputs to console
    """
    #
    # Creates the table that has n_cols + 1 columns (to fit row names) and n_rows + 1 rows (to fit column names)
    #
    n_cols, n_rows = len(column_headers), len(row_names)
    m = [["" for _ in range(n_cols + 1)] for __ in range(n_rows + 1)]
    #
    # Populates edges of table with row and column names
    #
    for i in range(n_cols):
        m[0][i + 1] = column_headers[i]
    for i in range(n_rows):
        m[i + 1][0] = row_names[i]
    #
    # Writes data points to table
    #
    for y, row in enumerate(data_points):
        for x, elem in enumerate(row):
            m[y + 1][x + 1] = str(round(float(elem), 9))
    assert m[-1][1] != "", f"Not enough data point rows provided"
    #
    # Pads each element with trailing spaces so that it is equal in width to the longest element in its column
    #
    for x in range(n_cols + 1):
        max_elem_len = max(len(m[y][x]) for y in range(len(m)))
        for y in range(n_rows + 1):
            m[y][x] = m[y][x].ljust(max_elem_len)
    #
    # Prints table
    #
    print("\n".join(" ".join(row) for row in m))


print("Number of program execution logs:", num_times_executed)
draw_table(
    column_headers=["Min", "Mean", "Max"],
    row_names=["Mean parse time / ns"] + [k + " / s" for k in node_to_eval_times.keys()],
    data_points=get_min_mean_max([parse_times] + list(node_to_eval_times.values()))
)
log.close()
