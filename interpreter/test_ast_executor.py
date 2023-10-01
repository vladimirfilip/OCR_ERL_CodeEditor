import os
from io import StringIO
from typing import Iterable, Callable, Optional
from unittest import TestCase
from unittest.mock import patch

from ast_executor import AstExecutor, ExeCtx
from lexer import Lexer
from parsed_ast import Node, Program, VarAssign, ArrayDecl, ForLoop, GoToInstr, WhileLoop, DoUntil
from parser import Parser
from sym_table import ArrayVal, SymAddr, NullVal
from tokenizer import Tokenizer


class TestAstExecutor(TestCase):
    def tearDown(self) -> None:
        self.__executor = None
        self.__parser = None
        self.__lexer = None
        self.__tokenizer = None

    def test_add_callback(self):
        count = 0

        def callback(node: Node, ctx: dict):
            nonlocal count
            count += 1

        def another_callback(node: Node, ctx: dict):
            nonlocal count
            count += 1

        self.__init_executor([], callback)
        self.__executor.push_callback(another_callback)
        self.__executor.execute()
        self.assertEqual(0, count)

    def test_program(self):
        count = 0

        def callback(node: Node, ctx: ExeCtx):
            nonlocal count
            if isinstance(node, Program):
                count += 1

        self.__init_executor(["x = 1"], callback)
        self.__executor.execute()
        self.assertEqual(1, count)

    def test_x_equal_3(self):
        count = 0

        def callback(node: Node, ctx: ExeCtx):
            nonlocal count
            if isinstance(node, VarAssign):
                self.assertEqual(3, ctx.global_table.lookup_symbol('x'))
                count += 1

        self.__init_executor(["x = 3"], callback)
        self.__executor.execute()
        self.assertEqual(1, count)

    def test_x_equal_minus_3_times_6_power_2(self):
        count = 0

        def callback(node: Node, ctx: ExeCtx):
            nonlocal count
            if isinstance(node, VarAssign):
                self.assertEqual(-108, ctx.global_table.lookup_symbol('x'))
                count += 1

        self.__init_executor(["x = -3*6^2"], callback)
        self.__executor.execute()
        self.assertEqual(1, count)

    def test_x_equal_3_power_2_power_2(self):
        count = 0

        def callback(node: Node, ctx: ExeCtx):
            nonlocal count
            if isinstance(node, VarAssign):
                self.assertEqual(19683, ctx.global_table.lookup_symbol('x'))
                count += 1

        #
        # Power evaluation must take place right to left, so expression should be evaluated as 3^(3^2), not (3^3)^2
        #
        self.__init_executor(["x = 3^3^2"], callback)
        self.__executor.execute()
        self.assertEqual(1, count)

    def test_x_equal_3_eq_3(self):
        count = 0

        def callback(node: Node, ctx: ExeCtx):
            nonlocal count
            if isinstance(node, VarAssign):
                self.assertEqual(True, ctx.global_table.lookup_symbol('x'))
                count += 1

        #
        # Power evaluation must take place right to left, so expression should be evaluated as 3^(3^2), not (3^3)^2
        #
        self.__init_executor(["x = 3 == 3"], callback)
        self.__executor.execute()
        self.assertEqual(1, count)

    def test_array_assignment_with_boolean(self):
        count = 0

        def callback(node: Node, ctx: ExeCtx):
            nonlocal count
            if isinstance(node, VarAssign):
                if count == 2:
                    sym_addr: SymAddr = ctx.global_table.addr_of('y', [1])
                    self.assertEqual(2, sym_addr.value)
                elif count == 1:
                    self.assertEqual(0, ctx.global_table.lookup_symbol("x"))
                count += 1

        #
        # Power evaluation must take place right to left, so expression should be evaluated as 3^(3^2), not (3^3)^2
        #
        self.__init_executor(["array y[3]", "x = 0", "y[x == 0] = 2"], callback)
        self.__executor.execute()
        self.assertEqual(2, count)

    def test_x_equal_long_math_expression(self):
        count = 0

        def callback(node: Node, ctx: ExeCtx):
            nonlocal count
            if isinstance(node, VarAssign):
                self.assertEqual(9, ctx.global_table.lookup_symbol('x'))
                count += 1

        self.__init_executor(["x = -6 * (3 + 2 ^ 2) MOD 3 + 9 MOD 5 * 7 DIV 3"], callback)
        self.__executor.execute()
        self.assertEqual(1, count)

    def test_y_equal_x_equal_3(self):
        count: int = 0

        def callback(node: Node, ctx: ExeCtx):
            nonlocal count
            if isinstance(node, VarAssign):
                self.assertEqual(3, ctx.global_table.lookup_symbol('x'))
                if count > 0:
                    self.assertEqual(3, ctx.global_table.lookup_symbol('y'))
                count += 1

        self.__init_executor(["x = 3", "y = x"], callback)
        self.__executor.execute()
        self.assertEqual(2, count)

    def test_x_equal_minus_y(self):
        count = 0

        def callback(node: Node, ctx: ExeCtx):
            nonlocal count
            if isinstance(node, VarAssign):
                count += 1
                if count == 2:
                    self.assertEqual((1 / -9), ctx.global_table.lookup_symbol('x'))
                else:
                    self.assertEqual(9, ctx.global_table.lookup_symbol('y'))

        self.__init_executor(["y = 9", "x = -y ^ -1"], callback)
        self.__executor.execute()
        self.assertEqual(2, count)

    def test_x_equal_str_concat(self):
        count = 0

        def callback(node: Node, ctx: ExeCtx):
            nonlocal count
            if isinstance(node, VarAssign):
                count += 1
                if count >= 1:
                    self.assertEqual("this is a ", ctx.global_table.lookup_symbol("s1"))
                if count >= 2:
                    self.assertEqual("complete sentence", ctx.global_table.lookup_symbol('s2'))
                if count >= 3:
                    self.assertEqual("this is a complete sentence", ctx.global_table.lookup_symbol("x"))

        self.__init_executor(
            [
                "s1 = \"this is a \"",
                "s2=\"complete sentence\"",
                "x=s1+s2",
            ],
            callback)
        self.__executor.execute()
        self.assertEqual(3, count)

    def test_x_equal_str_mul(self):
        count = 0

        def callback(node: Node, ctx: ExeCtx):
            nonlocal count
            if isinstance(node, VarAssign):
                count += 1
                if count >= 1:
                    self.assertEqual("this is a this is a this is a ", ctx.global_table.lookup_symbol("s1"))
                if count >= 2:
                    self.assertEqual("complete sentence complete sentence ", ctx.global_table.lookup_symbol('s2'))
                if count >= 3:
                    self.assertEqual("this is a this is a this is a complete sentence complete sentence ", ctx.global_table.lookup_symbol("x"))

        self.__init_executor(
            [
                "s1 = \"this is a \" * 3",
                "s2=(2 * 3000 ^ 0)*\"complete sentence \" ",
                "x=s1+s2",
            ],
            callback)
        self.__executor.execute()
        self.assertEqual(3, count)

    def test_1_dim_array(self):
        count: int = 0

        def callback(node: Node, ctx: ExeCtx):
            nonlocal count
            if isinstance(node, ArrayDecl):
                self.__test_declared_1_dim_array(ctx)
                count += 1

        self.__init_executor(["array v[3]"], callback)
        self.__executor.execute()
        self.assertEqual(1, count)

    def __test_declared_1_dim_array(self, ctx: ExeCtx):
        v: ArrayVal = ctx.global_table.lookup_symbol('v')
        self.assertIsNotNone(v)
        self.assertListEqual([3], v.dims)
        null = NullVal()
        for i in range(0, 3):
            self.assertEqual(null, v.get_at([i]))

    def test_1_dim_array_assign(self):
        assign_count: int = 0
        arr_count: int = 0

        def callback(node: Node, ctx: ExeCtx):
            nonlocal assign_count, arr_count
            if isinstance(node, ArrayDecl):
                self.__test_declared_1_dim_array(ctx)
                arr_count += 1
            elif isinstance(node, VarAssign):
                addr: SymAddr = ctx.global_table.addr_of('v', [assign_count])
                self.assertEqual(assign_count + 1, addr.value)
                assign_count += 1

        self.__init_executor(["array v[3]", "v[0] = 1", "v[1] = 2"], callback)
        self.__executor.execute()
        self.assertEqual(2, assign_count)
        self.assertEqual(1, arr_count)

    def test_1_dim_from_var_array_assign(self):
        assign_count: int = 0
        arr_count: int = 0

        def callback(node: Node, ctx: ExeCtx):
            nonlocal assign_count, arr_count
            if isinstance(node, ArrayDecl):
                self.__test_declared_1_dim_array(ctx)
                arr_count += 1
            elif isinstance(node, VarAssign):
                if assign_count > 1:
                    addr: SymAddr = ctx.global_table.addr_of('v', [assign_count - 1])
                    self.assertEqual(assign_count, addr.value)
                else:
                    addr: SymAddr = ctx.global_table.addr_of('x')
                    self.assertEqual(3, addr.value)
                assign_count += 1

        self.__init_executor(["x = 3", "array v[x]", "v[0] = 1", "v[1] = 2"], callback)
        self.__executor.execute()
        self.assertEqual(3, assign_count)
        self.assertEqual(1, arr_count)

    def test_1_dim_array_assign_reuse(self):
        assign_count: int = 0

        def callback(node: Node, ctx: ExeCtx):
            nonlocal assign_count
            if isinstance(node, VarAssign):
                addr: SymAddr = ctx.global_table.addr_of('v', [assign_count])
                self.assertEqual(1, addr.value)
                assign_count += 1

        self.__init_executor(["array v[3]", "v[0] = 1", "v[1] = v[0]"], callback)
        self.__executor.execute()
        self.assertEqual(2, assign_count)

    def test_1_dim_array_indirect_assign(self):
        assign_count: int = 0

        def callback(node: Node, ctx: ExeCtx):
            nonlocal assign_count
            if isinstance(node, VarAssign):
                addr: SymAddr = ctx.global_table.addr_of('v', [assign_count])
                self.assertEqual(assign_count + 1, addr.value)
                assign_count += 1

        self.__init_executor(["array v[3]", "v[0] = 1", "v[v[0]] = 2"], callback)
        self.__executor.execute()
        self.assertEqual(2, assign_count)

    def test_declared_multi_dim_array(self):
        count: int = 0

        def callback(node: Node, ctx: ExeCtx):
            nonlocal count
            if isinstance(node, ArrayDecl):
                v: ArrayVal = ctx.global_table.lookup_symbol('v')
                self.assertIsNotNone(v)
                self.assertListEqual([2, 3, 4], v.dims)
                null = NullVal()
                for i in range(0, 2):
                    for j in range(0, 3):
                        for k in range(0, 4):
                            self.assertEqual(null, v.get_at([i, j, k]))
                count += 1

        self.__init_executor(["array v[2, 3, 4]"], callback)
        self.__executor.execute()
        self.assertEqual(1, count)

    def test_multi_dim_array_assign(self):
        assign_count: int = 0

        def callback(node: Node, ctx: ExeCtx):
            nonlocal assign_count
            if isinstance(node, VarAssign):
                addr: SymAddr = ctx.global_table.addr_of('v', [assign_count, 2, 3])
                self.assertEqual(assign_count + 1, addr.value)
                assign_count += 1

        self.__init_executor(["array v[2, 3, 4]", "v[0, 2, 3] = 1", "v[1, 2, 3] = 2"], callback)
        self.__executor.execute()
        self.assertEqual(2, assign_count)

    def test_multi_dim_array_assign_reuse(self):
        assign_count: int = 0

        def callback(node: Node, ctx: ExeCtx):
            nonlocal assign_count
            if isinstance(node, VarAssign):
                addr: SymAddr = ctx.global_table.addr_of('v', [assign_count, assign_count + 1, assign_count + 2])
                self.assertEqual(1, addr.value)
                assign_count += 1

        self.__init_executor(["array v[2, 3, 4]", "v[0, 1, 2] = 1", "v[1, 2, 3] = v[0, 1, 2]"], callback)
        self.__executor.execute()
        self.assertEqual(2, assign_count)

    def test_multi_dim_array_indirect_assign(self):
        assign_count: int = 0

        def callback(node: Node, ctx: ExeCtx):
            nonlocal assign_count
            if isinstance(node, VarAssign):
                addr: SymAddr = ctx.global_table.addr_of('v', [min(1, assign_count), assign_count, assign_count + 1])
                self.assertEqual(assign_count + 1, addr.value)
                assign_count += 1

        self.__init_executor(
            ["array v[2, 3, 4]", "v[0, 0, 1] = 1", "v[v[0, 0, 1], 1, 2] = 2", "v[1, v[1, 1, 2], 3] = 3"], callback)
        self.__executor.execute()
        self.assertEqual(3, assign_count)

    def test_if_statement(self):
        assign_count: int = 0

        def callback(node: Node, ctx: ExeCtx):
            nonlocal assign_count
            if isinstance(node, VarAssign):
                if assign_count == 0:
                    val: int = ctx.global_table.lookup_symbol("x")
                    self.assertEqual(0, val)
                if assign_count == 1:
                    val: str = ctx.global_table.lookup_symbol("x")
                    self.assertEqual("1", val)
                assign_count += 1

        lines = [
            "x = 0",
            "if x == 0 then",
            "x = \"1\"",
            "endif"
        ]
        self.__init_executor(lines, callback)
        self.__executor.execute()
        self.assertEqual(2, assign_count)

    def test_if_elseif_statement(self):
        assign_count: int = 0

        def callback(node: Node, ctx: ExeCtx):
            nonlocal assign_count
            if isinstance(node, VarAssign):
                if assign_count == 0:
                    val: int = ctx.global_table.lookup_symbol("x")
                    self.assertEqual(17, val)
                if assign_count == 1:
                    val: str = ctx.global_table.lookup_symbol("x")
                    self.assertEqual("xxxxx", val)
                assign_count += 1

        lines = [
            "x = 2 + 5 * 3",
            "if x == 21 then",
            "x = 3",
            "elseif x == (17 * 1) then",
            "x = 5 * \"x\"",
            "endif"
        ]
        self.__init_executor(lines, callback)
        self.__executor.execute()
        self.assertEqual(2, assign_count)

    def test_if_elseif_statement_none_accepted(self):
        assign_count: int = 0

        def callback(node: Node, ctx: ExeCtx):
            nonlocal assign_count
            if isinstance(node, VarAssign):
                if assign_count == 0:
                    val: int = ctx.global_table.lookup_symbol("x")
                    self.assertEqual(17, val)
                assign_count += 1

        lines = [
            "x = 2 + 5 * 3",
            "if x == 21 then",
            "x = 500",
            "elseif x == 25 then",
            "x = 17000",
            "endif"
        ]
        self.__init_executor(lines, callback)
        self.__executor.execute()
        self.assertEqual(1, assign_count)

    def test_switch_case(self):
        assign_count: int = 0

        def callback(node: Node, ctx: ExeCtx):
            nonlocal assign_count
            if isinstance(node, VarAssign):
                if assign_count == 0:
                    val: int = ctx.global_table.lookup_symbol("x")
                    self.assertEqual(2, val)
                if assign_count == 1:
                    val: int = ctx.global_table.lookup_symbol("x")
                    self.assertEqual(3, val)
                assign_count += 1

        lines = [
            "x = 2",
            "switch x:",
            "    case 2:",
            "        x = 3",
            "    case 3:",  # this case should not be evaluated, even though x has been assigned a value of 3 in the previous case block
            "        x = 4",
            "endswitch"
        ]
        self.__init_executor(lines, callback)
        self.__executor.execute()
        self.assertEqual(2, assign_count)

    def test_switch_case_with_default(self):
        assign_count: int = 0

        def callback(node: Node, ctx: ExeCtx):
            nonlocal assign_count
            if isinstance(node, VarAssign):
                if assign_count == 0:
                    val = ctx.global_table.lookup_symbol("x")
                    self.assertEqual(2, val)
                    self.assertEqual(float, type(val))
                if assign_count == 1:
                    val: int = ctx.global_table.lookup_symbol("x")
                    self.assertEqual(9, val)
                assign_count += 1

        lines = [
            "x = 2.0",
            "switch x:",
            "    case 4:",
            "        x = 3",
            "    case 3:",
            "        x = 4",
            "    default: ",
            "        x = 3 ^ x",
            "endswitch"
        ]
        self.__init_executor(lines, callback)
        self.__executor.execute()
        self.assertEqual(2, assign_count)

    def test_switch_case_just_default(self):
        assign_count: int = 0

        def callback(node: Node, ctx: ExeCtx):
            nonlocal assign_count
            if isinstance(node, VarAssign):
                if assign_count == 0:
                    val: int = ctx.global_table.lookup_symbol("x")
                    self.assertEqual(2, val)
                if assign_count == 1:
                    val: int = ctx.global_table.lookup_symbol("x")
                    self.assertEqual(256, val)
                assign_count += 1

        lines = [
            "x = 2",
            "switch x:",
            "    default: ",
            "        x = x ^ 2 ^ 3",
            "endswitch"
        ]
        self.__init_executor(lines, callback)
        self.__executor.execute()
        self.assertEqual(2, assign_count)

    def test_for_loop(self):
        assign_count: int = 0

        def callback(node: Node, ctx: ExeCtx):
            nonlocal assign_count
            if isinstance(node, VarAssign):
                assign_count += 1
                if assign_count == 1:
                    self.assertEqual(0, ctx.global_table.lookup_symbol("x"))
                else:
                    self.assertEqual(True, ctx.inside_loop)
                    self.assertEqual(False, ctx.break_detected)
                    self.assertEqual(False, ctx.continue_detected)
                    self.assertEqual(assign_count - 2, ctx.global_table.lookup_symbol("x"))
            elif isinstance(node, GoToInstr):
                self.assertEqual(True, ctx.inside_loop)
                self.assertEqual(True, ctx.break_detected)
                self.assertEqual(False, ctx.continue_detected)

        lines = [
            "x = 0",
            "for i = 0 to 9",
            "x = i",
            "if x == 7 then",
            "break",
            "endif",
            "next i",
        ]
        self.__init_executor(lines, callback)
        self.__executor.execute()
        self.assertEqual(9, assign_count)

    def test_nested_for_loops(self):
        assign_count: int = 0
        for_loop_count: int = 0

        def callback(node: Node, ctx: ExeCtx):
            nonlocal assign_count, for_loop_count
            if isinstance(node, VarAssign):
                assign_count += 1
                self.assertEqual(assign_count - 1, ctx.global_table.lookup_symbol("x"))
                if assign_count >= 2:
                    self.assertEqual(True, ctx.inside_loop)
                    self.assertEqual(False, ctx.continue_detected)
                    self.assertEqual(False, ctx.break_detected)
            if isinstance(node, ForLoop):
                for_loop_count += 1
                #
                # Inner for loop has been executed
                #
                if for_loop_count <= 10:
                    self.assertEqual(10 * for_loop_count, ctx.global_table.lookup_symbol("x"))
                #
                # Outer for loop has been executed
                #
                else:
                    self.assertEqual(100, ctx.global_table.lookup_symbol("x"))

        lines = [
            "x = 0",
            "for i = 0 to 9",
            "    x = x + 1",
            "    for j = 1 to 9",
            "        x = x + 1",
            "    next j",
            "next i",
        ]
        self.__init_executor(lines, callback)
        self.__executor.execute()
        self.assertEqual(101, assign_count)

    def test_while_loop(self):
        assign_count: int = 0
        while_loop_count: int = 0

        def callback(node: Node, ctx: ExeCtx):
            nonlocal assign_count, while_loop_count
            if isinstance(node, VarAssign):
                assign_count += 1
                self.assertEqual(10, ctx.global_table.lookup_symbol("x"))
                if assign_count > 1:
                    self.assertEqual(assign_count - 2, ctx.global_table.lookup_symbol("i"))
            if isinstance(node, WhileLoop):
                while_loop_count += 1
                self.assertEqual(10, ctx.global_table.lookup_symbol("x"))
                self.assertEqual(10, ctx.global_table.lookup_symbol("i"))

        lines = [
            "x = 10",
            "i = 0",
            "while i < x",
            "i = i + 1",
            "endwhile",
        ]
        self.__init_executor(lines, callback)
        self.__executor.execute()
        self.assertEqual(12, assign_count)
        self.assertEqual(1, while_loop_count)

    def test_while_loop_with_break(self):
        assign_count: int = 0
        while_loop_count: int = 0

        def callback(node: Node, ctx: ExeCtx):
            nonlocal assign_count, while_loop_count
            if isinstance(node, VarAssign):
                assign_count += 1
                self.assertEqual(10, ctx.global_table.lookup_symbol("x"))
                if assign_count > 1:
                    self.assertEqual(assign_count - 2, ctx.global_table.lookup_symbol("i"))
            if isinstance(node, WhileLoop):
                while_loop_count += 1
                self.assertEqual(10, ctx.global_table.lookup_symbol("x"))
                self.assertEqual(5, ctx.global_table.lookup_symbol("i"))

        lines = [
            "x = 10",
            "i = 0",
            "while i < x",
            "i = i + 1",
            "if (i MOD 5) == 0 then",
            "break",
            "endif",
            "endwhile",
        ]
        self.__init_executor(lines, callback)
        self.__executor.execute()
        self.assertEqual(7, assign_count)
        self.assertEqual(1, while_loop_count)

    def test_while_condition_checked_before_block_exec(self):
        assign_count: int = 0
        while_loop_count: int = 0

        def callback(node: Node, ctx: ExeCtx):
            nonlocal assign_count, while_loop_count
            if isinstance(node, VarAssign):
                assign_count += 1
                if assign_count > 1:
                    self.assertEqual(1, ctx.global_table.lookup_symbol("x"))
                else:
                    self.assertEqual(0, ctx.global_table.lookup_symbol("x"))
            if isinstance(node, WhileLoop):
                while_loop_count += 1
                self.assertEqual(0, ctx.global_table.lookup_symbol("x"))

        lines = [
            "x = 0",
            "while x > 2",
            "   x = x * 2",
            "endwhile",
            "x = 2 ^ x",
        ]
        self.__init_executor(lines, callback)
        self.__executor.execute()
        self.assertEqual(2, assign_count)
        self.assertEqual(1, while_loop_count)

    def test_do_until(self):
        assign_count: int = 0
        do_until_count: int = 0

        def callback(node: Node, ctx: ExeCtx):
            nonlocal assign_count, do_until_count
            if isinstance(node, VarAssign):
                assign_count += 1
                self.assertEqual(10, ctx.global_table.lookup_symbol("x"))
                if assign_count > 1:
                    self.assertEqual(assign_count - 2, ctx.global_table.lookup_symbol("i"))
            if isinstance(node, DoUntil):
                do_until_count += 1
                self.assertEqual(10, ctx.global_table.lookup_symbol("x"))
                self.assertEqual(10, ctx.global_table.lookup_symbol("i"))

        lines = [
            "x = 10",
            "i = 0",
            "do",
            "i = i + 1",
            "until i == x"
        ]
        self.__init_executor(lines, callback)
        self.__executor.execute()
        self.assertEqual(12, assign_count)
        self.assertEqual(1, do_until_count)

    def test_do_until_executes_block_at_least_once(self):
        assign_count: int = 0
        do_until_count: int = 0

        def callback(node: Node, ctx: ExeCtx):
            nonlocal assign_count, do_until_count
            if isinstance(node, VarAssign):
                assign_count += 1
                self.assertEqual(0, ctx.global_table.lookup_symbol("x"))
                if assign_count > 1:
                    self.assertEqual(0, ctx.global_table.lookup_symbol("y"))
            if isinstance(node, DoUntil):
                do_until_count += 1
                self.assertEqual(0, ctx.global_table.lookup_symbol("x"))
                self.assertEqual(0, ctx.global_table.lookup_symbol("y"))

        lines = [
            "x = 0",
            "do",
            "y = x",
            "until x == 0"
        ]
        self.__init_executor(lines, callback)
        self.__executor.execute()
        self.assertEqual(2, assign_count)
        self.assertEqual(1, do_until_count)

    def test_print_statement_one_arg(self):
        lines = [
            "print(2 + 2)"
        ]
        expected_output_lines = [
            '4',
            ''
        ]
        self.__test_print_output(lines, expected_output_lines)

    def test_print_statement_many_args(self):
        lines = [
            "x = 3",
            "array arr[5]",
            "arr[1] = 6.2",
            'y = "x is " + " 3"',
            'array bigarr[2,5]',
            "for i = 0 to 4",
            "    bigarr[1, i] = i + 1",
            "next i",
            "print(x, arr[x ^ 0], y, x MOD 2, x == 10, arr)",
            "print(bigarr)",
        ]
        expected_output_lines = [
            '3, 6.2, x is  3, 1, False, [null, 6.2, null, null, null]',
            '[[null, null, null, null, null], [1, 2, 3, 4, 5]]',
            ''
        ]
        self.__test_print_output(lines, expected_output_lines)

    def test_simple_func(self):
        lines = [
            "function f()",
            "    return 3",
            "endfunction",
            "print(f())"
        ]
        expected_output_lines = [
            '3',
            ''
        ]
        self.__test_print_output(lines, expected_output_lines)

    def test_func_simple_param_passing(self):
        lines = [
            "function mul(a, b)",
            "    return a * b",
            "endfunction",
            "print(mul(3, 2))"
        ]
        expected_output_lines = [
            '6',
            ''
        ]
        self.__test_print_output(lines, expected_output_lines)

    def test_func_param_by_ref(self):
        lines = [
            "a = 3",
            "function square(x:byRef)",
            "    x = x ^ 2",
            "    return x",
            "endfunction",
            "print(square(a))",
            "print(a)",
        ]
        expected_output_lines = [
            '9',
            '9',
            ''
        ]
        self.__test_print_output(lines, expected_output_lines)

    def test_func_recursion(self):
        lines = [
            "function findNumEven(n:byVal)",
            "// finds every even number from 0 to n inclusive",
            "    if n == 0 then",
            "        return 1",
            "    endif",
            "    return 1 - n MOD 2 + findNumEven(n - 1)",
            "endfunction",
            "print(findNumEven(4))",
        ]
        expected_output_lines = [
            '3',
            ''
        ]
        self.__test_print_output(lines, expected_output_lines)

    def test_func_outer_scope_read(self):
        lines = [
            "x = 3",
            "function f()",
            "    return x + 2",
            "endfunction",
            "print(f())",
            "print(x)",
        ]
        expected_output_lines = [
            '5',
            '3',
            ''
        ]
        self.__test_print_output(lines, expected_output_lines)

    def test_func_local_non_overwrite(self):
        lines = [
            "x = 1",
            "function f()",
            "    x = 2",
            "    return x",
            "endfunction",
            "print(f())",
            "print(x)",
            "x = f()",
            "print(x)",
        ]
        expected_output_lines = [
            '2',
            '1',
            '2',
            ''
        ]
        self.__test_print_output(lines, expected_output_lines)

    def test_func_global_overwrite(self):
        lines = [
            "x = 1",
            "function f()",
            "    global x = 2",
            "    return x",
            "endfunction",
            "print(f())",
            "print(x)",
        ]
        expected_output_lines = [
            '2',
            '2',
            ''
        ]
        self.__test_print_output(lines, expected_output_lines)

    def test_func_call_other_func(self):
        lines = [
            "x = 3",
            "function g()",
            "   return x",
            "endfunction",
            "function f()",
            "   x = 2",
            "   return g()",
            "endfunction",
            "print(f())",
        ]
        expected_output_lines = [
            '3',
            ''
        ]
        self.__test_print_output(lines, expected_output_lines)

    def test_func_array_return(self):
        lines = [
            "function generateMatrix(w, h)",
            "    array m[h,w]",
            "    for y = 0 to h - 1",
            "        for x = 0 to w - 1",
            "            m[y,x] = x + y",
            "        next x",
            "    next y",
            "    return m",
            "endfunction",
            "print(generateMatrix(3, 4))"
        ]
        expected_output_lines = [
            str([[x + y for x in range(3)] for y in range(4)]),
            ''
        ]
        self.__test_print_output(lines, expected_output_lines)

    def test_func_array_return_index(self):
        lines = [
            "function generate3DMatrix(w:byRef, h:byRef, d:byRef)",
            "    array m[d, h,w]",
            "    for z = 0 to d - 1",
            "        for y = 0 to h - 1",
            "            for x = 0 to w - 1",
            "                m[z,y,x] = x - y + z DIV 2",
            "            next x",
            "        next y",
            "    next z",
            "    return m",
            "endfunction",
            "x = generate3DMatrix(3, 4, 2)",
            "print(x[1,2])",
        ]
        expected_output_lines = [
            str([[[x - y + z // 2 for x in range(3)] for y in range(4)] for z in range(2)][1][2]),
            ''
        ]
        self.__test_print_output(lines, expected_output_lines)

    def test_func_call_instr(self):
        lines = [
            "function f()",
            "    print(2)",
            "    return 2",
            "endfunction",
            "f()",
        ]
        expected_output_lines = [
            '2',
            ''
        ]
        self.__test_print_output(lines, expected_output_lines)

    def test_proc_call(self):
        lines = [
            "result = 0",
            "function factorial(n)",
            "    result = 1",
            "    for i = 2 to n",
            "        result = result * i",
            "    next i",
            "    return result",
            "endfunction",
            "procedure nCr(n, k, output:byRef)",
            "    output = factorial(n) DIV (factorial(k) * factorial(n - k))",
            "    print(output)",
            "endprocedure",
            "nCr(5, 2, result)",
            "print(result)"
        ]
        expected_output_lines = [
            '10',
            '10',
            ''
        ]
        self.__test_print_output(lines, expected_output_lines)

    def test_proc_call_no_byref(self):
        #
        # As no parameter was passed by reference, the procedure should not edit any addressable arguments in-place
        #
        lines = [
            "x = 1",
            "procedure increment(amount, n)",
            "    n = n + amount",
            "    print(n)",
            "endprocedure",
            "increment(9, x)",
            "print(x)",
        ]
        expected_output_lines = [
            '10',
            '1',
            ''
        ]
        self.__test_print_output(lines, expected_output_lines)

    def test_type_casting(self):
        lines = [
            'print(float("9.0") DIV 3)',
            'print(str(1) + "1")',
            'print(9.9 MOD 3)'
        ]
        expected_output_lines = [
            '3.0',
            '11',
            '0.9000000000000004',
            ''
        ]
        self.__test_print_output(lines, expected_output_lines)

    def test_str_functions(self):
        lines = [
            's = "This is a bigger string"',
            'print(s.length)',
            'sub = s.substring(4 + 2 + 1 + 6 + 4 * 1, 6)',
            'print(sub)',
            'print(sub.length)',
            'print(sub.substring(3, 3))',
            'array arr[3]',
            'arr[0] = 1',
            'arr[1] = 7',
            'arr[2] = "big string"',
            'print(arr[2].substring(arr[0], arr[1]))',
        ]
        expected_output_lines = [
            '23',
            'string',
            '6',
            'ing',
            'ig stri',
            ''
        ]
        self.__test_print_output(lines, expected_output_lines)

    def test_input(self):
        lines = [
            's = input("Enter a number. ")',
        ]

        def callback(node: Node, ctx: ExeCtx):
            if isinstance(node, VarAssign):
                addr: SymAddr = ctx.global_table.addr_of('s')
                self.assertEqual("102dvuibiubvr", addr.value)

        self.__init_executor(lines, callback)
        with patch("builtins.input", return_value="102dvuibiubvr"):
            self.__executor.execute()

    def test_input_as_inner_expr(self):
        lines = [
            'array msgs[2]',
            'msgs[0] = "You chose 0"',
            'msgs[1] = "You chose 1"',
            's = msgs[int(input("Enter 0 or 1. "))]'
        ]
        assign_count: int = 0

        def callback(node: Node, ctx: ExeCtx):
            nonlocal assign_count
            if isinstance(node, VarAssign):
                assign_count += 1
                if assign_count == 1:
                    addr: SymAddr = ctx.global_table.addr_of('msgs', [0])
                    self.assertEqual("You chose 0", addr.value)
                if assign_count == 2:
                    addr: SymAddr = ctx.global_table.addr_of('msgs', [1])
                    self.assertEqual("You chose 1", addr.value)
                if assign_count == 3:
                    addr: SymAddr = ctx.global_table.addr_of('s')
                    self.assertEqual("You chose 1", addr.value)

        self.__init_executor(lines, callback)
        with patch("builtins.input", return_value="1"):
            self.__executor.execute()

    def test_file_functions(self):
        file_path: str = "file.txt"
        lines = [
            f'f = openWrite("{file_path}")',
            'array lines[2]',
            'lines[0] = "first line"',
            'lines[1] = "second line"',
            'for i = 0 to lines.length - 1',
            '    f.writeLine(lines[i])',
            'next i',
            'print(f.endOfFile())',
            'f.close()',
            f'file = openRead("{file_path}")',
            'while NOT file.endOfFile()',
            '    print(file.readLine())',
            'endwhile',
            'print(file.readLine())',
            'print(file.endOfFile())',
            'file.close()',
        ]
        expected_output_lines = [
            'True',
            'first line',
            '',
            'second line',
            '',
            '',
            'True',
            ''
        ]
        self.__test_print_output(lines, expected_output_lines)
        os.remove(file_path)

    def test_class_simple(self):
        lines = [
            "class A",
            "    val = 0",
            "    public procedure new()",
            "        return",
            "    endprocedure",
            "endclass",
            "a = new A()",
            "print(a.val)",
        ]
        expected_output_lines = [
            '0',
            ''
        ]
        self.__test_print_output(lines, expected_output_lines)

    def test_class_inherit(self):
        #
        # Also tests that object fields (regardless of being public or private) can be modified from within methods
        #
        lines = [
            "class A",
            "    val_a = 0",
            "    private val_d = 4",
            "    public procedure new()",
            "        return",
            "    endprocedure",
            "    private procedure a()",
            "        val_a = 1",
            "    endprocedure",
            "endclass",
            "class B inherits A",
            "    private val_b = 2",
            "    public val_c = val_b + 1",
            "    public procedure new()",
            "        super.new()",
            "    endprocedure",
            "    function get_val_b()",
            "        val_b = 3",
            "        return val_b",
            "    endfunction",
            "endclass",
            "a = new B()",
            "print(a.val_a)",
            "print(a.val_c)",
            "print(a.get_val_b())",
        ]
        expected_output_lines = [
            '0',
            '3',
            '3',
            ''
        ]
        self.__test_print_output(lines, expected_output_lines)

    def test_bool_literal(self):
        lines = [
            "x = true",
            "while x",
            "    x = false",
            "    print(x)",
            "endwhile"
        ]
        expected_output_lines = ["False", ""]
        self.__test_print_output(lines, expected_output_lines)

    def test_instantiate_in_return(self):
        lines = [
            "class A",
            "    val = 0",
            "    procedure new(x)",
            "        val = x",
            "    endprocedure",
            "    function get_val()",
            "        return val",
            "    endfunction",
            "endclass",
            "function create_obj(v)",
            "    return new A(v)",
            "endfunction",
            "n = 50",
            "array arr[n]",
            "for i = 0 to n - 1",
            "    arr[i] = create_obj(i)",
            "next i",
            "for i = 0 to n - 1",
            "    print(arr[i].get_val())",
            "next i",
        ]
        expected_output_lines = [str(x) for x in range(50)] + ['']
        self.__test_print_output(lines, expected_output_lines)

    def test_deep_fields(self):
        lines = [
            "class A",
            "    val = \"a\"",
            "endclass",
            "class B",
            "    a = new A()",
            "endclass",
            "class C",
            "    b = new B()",
            "endclass",
            "c = new C()",
            "print(c.b.a.val)",
            "c.b.a.val = c.b.a.val + \"b\"",
            "print(c.b.a.val)",
        ]
        expected_output_lines = [
            "a",
            "ab",
            ""
        ]
        self.__test_print_output(lines, expected_output_lines)

    def test_correct_obj_ref(self):
        lines = [
            "class A",
            "    val = \"a\"",
            "endclass",
            "a = new A()",
            "array l[1]",
            "l[0] = a",
            "print(l[0].val)",
            "print(a.val)",
            "l[0] = 2",
            "print(l[0])",
            "print(a.val)",
            "b = a",
            "b.val = \"b\"",
            "print(b.val)",
            "print(a.val)",
        ]
        expected_output_lines = [
            "a",
            "a",
            "2",
            "a",
            "b",
            "b",
            ""
        ]
        self.__test_print_output(lines, expected_output_lines)

    # ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------- #

    def __test_print_output(self, input_lines: list[str], expected_output_lines: list[str]):
        buffer = StringIO()
        self.__init_executor(input_lines, None, buffer)
        self.__executor.execute()

        expected_output_string = "\n".join(expected_output_lines)
        actual_output_string = buffer.getvalue()
        self.assertEqual(actual_output_string, expected_output_string)

    def __init_executor(self, lines: Iterable[str], callback: Optional[Callable], output_stream=None):
        self.__tokenizer = Tokenizer()
        self.__lexer = Lexer(self.__tokenizer, lines)
        self.__parser = Parser(self.__lexer)
        self.__executor = AstExecutor(self.__parser, None, callback, output_stream)
