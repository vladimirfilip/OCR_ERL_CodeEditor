import json
from typing import Iterable, Optional
from unittest import TestCase
from ast_to_json import ASTToJsonParser
from lexer import Lexer
from parsed_ast import Node
from parser import Parser
from tokenizer import Tokenizer


class TestParser(TestCase):
    EXPECTED: dict

    @classmethod
    def setUpClass(cls) -> None:
        with open("parser_test_data.json", "r") as file:
            cls.EXPECTED = json.load(file)

    def setUp(self):
        self.ast_to_dict_parser = ASTToJsonParser()
        self.maxDiff = None

    def tearDown(self) -> None:
        self.__parser = None
        self.__lexer = None
        self.__tokenizer = None

    def test_parse_no_lines(self):
        self.__init_parser([])
        self.assertIsNone(self.__parser.parse())

    def test_parse_empty_lines(self):
        self.__init_parser([""] * 100)
        self.assertIsNone(self.__parser.parse())

    def test_parse_whitespace_lines(self):
        self.__init_parser([" " * 100] * 100)
        self.assertIsNone(self.__parser.parse())

    def test_arr_decl(self):
        self.__init_parser(["array x[2,y]"])
        self.__test_node(self.__parser.parse(), "test_arr_decl")

    def test_global_arr_decl(self):
        self.__init_parser(["global array y[3, 5]"])
        self.__test_node(self.__parser.parse(), "test_global_arr_decl")

    def test_arr_decl_no_dims(self):
        with self.assertRaises(SyntaxError) as ex:
            self.__init_parser(["global array y[]"])
            self.__parser.parse()
        self.assertEqual(str(ex.exception), "Syntax error: list of expressions expected")
        self.tearDown()

    def test_arr_decl_no_id(self):
        with self.assertRaises(SyntaxError) as ex:
            self.__init_parser(["global array []"])
            self.__parser.parse()
        self.assertEqual(str(ex.exception), "Expected 'ID', received '['")
        self.tearDown()

    def test_arr_decl_enforce_no_newline(self):
        #
        # array declaration should be on the same line
        #
        with self.assertRaises(SyntaxError) as ex:
            self.__init_parser([
                "global",
                "array arr[2]",
            ])
            self.__parser.parse()
        self.assertEqual(str(ex.exception), "'array' should not be in a new line")

    def test_arr_decl_no_array(self):
        #
        # With no array keyword, the parser should think the statement is a variable assignment
        #
        with self.assertRaises(SyntaxError) as ex:
            self.__init_parser(["global x[3,3]"])
            self.__parser.parse()
        self.assertEqual(str(ex.exception), "Expected variable assignment")

    def test_just_expr(self):
        #
        # Just "x + 2" is not a valid program
        #
        with self.assertRaises(SyntaxError) as ex:
            self.__init_parser(["x + 2"])
            self.__parser.parse()
        self.assertEqual(str(ex.exception), "Expected variable assignment")

    def test_var_assign(self):
        self.__init_parser(["x = 3"])
        self.__test_node(self.__parser.parse(), "test_var_assign")

    def test_var_assign_unary_minus(self):
        self.__init_parser(["x = -3 * 5"])
        self.__test_node(self.__parser.parse(), "test_var_assign_unary_minus")

    def test_var_assign_with_expr(self):
        self.__init_parser(["x = 3 + 4 * 5"])
        self.__test_node(self.__parser.parse(), "test_var_assign_with_expr")

    def test_var_assign_with_bool_expr(self):
        self.__init_parser(["x = y == \"3\""])
        self.__test_node(self.__parser.parse(), "test_var_assign_with_bool_expr")

    def test_var_assign_with_longer_bool_expr(self):
        self.__init_parser(["x = NOT (y == 3.09 OR 4 != 6) AND y > 7"])
        self.__test_node(self.__parser.parse(), "test_var_assign_with_longer_bool_expr")

    def test_var_assign_with_longer_expr(self):
        self.__init_parser(["x = -3.2 + 4 * 5 / (5 - 2 ^ 2) MOD (3 DIV 2)"])
        self.__test_node(self.__parser.parse(), "test_var_assign_with_longer_expr")

    def test_field_assign(self):
        self.__init_parser(["x.y = 3"])
        self.__test_node(self.__parser.parse(), "test_field_assign")

    def test_indexed_field_assign(self):
        self.__init_parser(["x.y[0] = 3"])
        self.__test_node(self.__parser.parse(), "test_indexed_field_assign")

    def test_indexed_field_assign_no_index(self):
        with self.assertRaises(SyntaxError):
            self.__init_parser(["x.y[] = 3"])
            self.__test_node(self.__parser.parse(), "")

    def test_matrix_element_assign(self):
        self.__init_parser(["x.y[0,1] = -3"])
        self.__test_node(self.__parser.parse(), "test_matrix_element_assign")

    def test_field_of_variable_index_assign(self):
        self.__init_parser(["x.y[f(.3)] = 3"])
        self.__test_node(self.__parser.parse(), "test_field_of_variable_index_assign")

    def test_mixed_assign(self):
        self.__init_parser(["x.y[f(g(u.v[1, 2]))] = h(i, k.l[m.o.p, q])"])
        self.__test_node(self.__parser.parse(), "test_mixed_assign")

    def test_callable_member_assign(self):
        self.__init_parser(["f(a).b = c"])
        self.__test_node(self.__parser.parse(), "test_callable_member_assign")

    def test_sub_callable_member_assign(self):
        self.__init_parser(["a.f(b).c = d"])
        self.__test_node(self.__parser.parse(), "test_sub_callable_member_assign")

    def test_long_sub_callable_member_assign(self):
        self.__init_parser(["a.f(b)[c].d = e"])
        self.__test_node(self.__parser.parse(), "test_long_sub_callable_member_assign")

    def test_if_statement(self):
        self.__init_parser([
            "if x == 3 then",
            "x = \"4\"",
            "elseif x == \"4\" then",
            "y = 6.25 + x",
            "else",
            "y = x ^ 2",
            "endif",
        ])
        self.__test_node(self.__parser.parse(), "test_if_statement")

    def test_if_statement_no_then(self):
        with self.assertRaises(SyntaxError) as ex:
            self.__init_parser(["if x == 3 ", "x = 4", "endif"])
            self.__test_node(self.__parser.parse(), "")
        self.assertEqual(str(ex.exception), "Expected 'then', received 'ID['x']'")

    def test_if_statement_no_endif(self):
        with self.assertRaises(SyntaxError) as ex:
            self.__init_parser(["if x == 3 then", "x = 4"])
            self.__test_node(self.__parser.parse(), "")
        self.assertEqual(str(ex.exception), "Expected 'endif' before end of file")

    def test_if_else_if_statement(self):
        self.__init_parser([
            "if x == 3 then",
            "x = 4",
            "elseif x == 4 then",
            "y = -6.0232 + x",
            "endif"
        ])
        self.__test_node(self.__parser.parse(), "test_if_else_if_statement")

    def test_if_statement_with_inner_if(self):
        self.__init_parser([
            "if x == 6 then",
            "    y = 7",
            "    if x == 100 then",
            "        y.z = 3 * 6 ^ x",
            "    endif",
            "elseif x == 7 then",
            "    y.z = 6 * x + x - 3",
            "else",
            "    y.z = (6 + x) == (y DIV 2)",
            "endif"
        ])
        self.__test_node(self.__parser.parse(), "test_if_statement_with_inner_if")

    def test_for_loop(self):
        self.__init_parser([
            "for i = 0 to x",
            "y = i * 2",
            "a = i * 3",
            "next i",
        ])
        self.__test_node(self.__parser.parse(), "test_for_loop")

    def test_for_loop_wrong_id(self):
        with self.assertRaises(SyntaxError) as ex:
            self.__init_parser([
                "for i = 0 to x",
                "y = i * 2",
                "a = i * 3",
                "next j",
            ])
            self.__test_node(self.__parser.parse(), "")
        self.assertEqual(str(ex.exception), "Expected 'i', received 'j'")

    def test_for_loop_nested(self):
        self.__init_parser([
            "for i = 0 to n",
            "z = i",
            "for j = 0 to n",
            "x = j",
            "y = i",
            "next j",
            "next i"
        ])
        self.__test_node(self.__parser.parse(), "test_for_loop_nested")

    def test_while_loop(self):
        self.__init_parser([
            "while x == y.z OR 9 == 0",
            "x = \"no \" + x",
            "z = z ^ 2",
            "endwhile"
        ])
        self.__test_node(self.__parser.parse(), "test_while_loop")

    def test_while_loop_no_endwhile(self):
        with self.assertRaises(SyntaxError) as ex:
            self.__init_parser([
                "while x == y.z OR 9 == 0",
                "x = \"no \" + x",
                "z = z ^ 2",
            ])
            self.__test_node(self.__parser.parse(), "")
        self.assertEqual(str(ex.exception), "Expected 'endwhile' before end of file")

    def test_nested_while_loops(self):
        self.__init_parser([
            "while x == y.z AND j == k",
            "x = \"not equal to y.z\"",
            "while a != b",
            "x = y + 1",
            "endwhile",
            "endwhile",
        ])

    def test_do_until_loop(self):
        self.__init_parser([
            "do",
            "x = 3",
            "y = 4 * 6",
            "continue",
            "until y == 24"
        ])
        self.__test_node(self.__parser.parse(), "test_do_until_loop")

    def test_do_until_loop_no_cond(self):
        with self.assertRaises(SyntaxError) as ex:
            self.__init_parser([
                "do",
                "x = 3",
                "y = 4 * 6",
                "continue",
                "until"
            ])
            self.__test_node(self.__parser.parse(), "")
        self.assertEqual(str(ex.exception), "Expression missing from do-until loop declaration")

    def test_switch_case(self):
        self.__init_parser([
            "switch x:",
            "    case 2:",
            "        x = x ^ 2",
            "    case 3:",
            "        x = 45454 + x",
            "    default:",
            "        x = \"valid case not found\"",
            "endswitch",
        ])
        self.__test_node(self.__parser.parse(), "test_switch_case")

    def test_switch_case_missing_block(self):
        with self.assertRaises(SyntaxError) as ex:
            self.__init_parser([
                "switch x:",
                "    case 2:",
                "        x = x ^ 2",
                "    case 3:",
                "    default:",
                "        x = \"valid case not found\"",
                "endswitch",
            ])
            self.__test_node(self.__parser.parse(), "")
        self.assertEqual(str(ex.exception), "Block missing from case statement")

    def test_switch_case_just_default(self):
        self.__init_parser([
            "switch x:",
            "    default:",
            "        x = x ^ 2",
            "endswitch"
        ])
        self.__test_node(self.__parser.parse(), "test_switch_case_just_default")

    def test_switch_case_no_default(self):
        self.__init_parser([
            "switch x:",
            "    case 3444:",
            "        x = x ^ (1 / 2)",
            "endswitch",
        ])
        self.__test_node(self.__parser.parse(), "test_switch_case_no_default")

    def test_mixed_loops_and_ifs(self):
        self.__init_parser([
            "x = 0",
            "if x == 0 then",
            "    for i = 0 to 1000 ^ y",
            "        x = (x + i) ^ 2",
            "    next i",
            "elseif x == k.z[3] then",
            "    do",
            "        j = 0",
            "        while j <= (y MOD 3)",
            "            k.z[j] = y DIV j",
            "            j = j + 1",
            "        endwhile",
            "    until x != k.z[3]",
            "endif"
        ])
        self.__test_node(self.__parser.parse(), "test_mixed_loops_and_ifs")

    def test_print(self):
        self.__init_parser([
            "print(x, y, x + 2, x ^ 2 - 2, f(z.a[3]), b.l.y)"
        ])
        self.__test_node(self.__parser.parse(), "test_print")

    def test_print_no_closing_paren(self):
        with self.assertRaises(SyntaxError) as ex:
            self.__init_parser([
                "print(x, y, x + 2, x ^ 2 - 2, f(z.a[3]), b.l.y"
            ])
            self.__test_node(self.__parser.parse(), "")
        self.assertEqual(str(ex.exception), "Expected ')' before end of file")

    def test_print_no_args(self):
        with self.assertRaises(SyntaxError) as ex:
            self.__init_parser([
                "print()"
            ])
            self.__test_node(self.__parser.parse(), "")
        self.assertEqual(str(ex.exception), "At least one argument expected in print statement")

    def test_fun_decl(self):
        self.__init_parser([
            "function sum(a, b, c)",
            "    return a + b + c",
            "endfunction",
        ])
        self.__test_node(self.__parser.parse(), "test_fun_decl")

    def test_fun_decl_no_id(self):
        with self.assertRaises(SyntaxError) as ex:
            self.__init_parser([
                "function (a, b, c)",
                "    return a + b + c",
                "endfunction",
            ])
            self.__test_node(self.__parser.parse(), "")
        self.assertEqual(str(ex.exception), "Expected 'ID', received '('")

    def test_fun_decl_no_endfunction(self):
        with self.assertRaises(SyntaxError) as ex:
            self.__init_parser([
                "function f(a, b, c)",
                "    return a + b + c"
            ])
            self.__test_node(self.__parser.parse(), "")
        self.assertEqual(str(ex.exception), "Expected 'endfunction' before end of file")

    def test_fun_decl_long_block(self):
        self.__init_parser([
            "function complex_func(a, b, c, d)",
            "    for i = 0 to a",
            "        a = a DIV b MOD c ^ d",
            "        b = b + 1",
            "        if a < b then",
            "            a = a + 1",
            "            c = c DIV 2",
            "        elseif a == b then",
            "            do",
            "                c = c MOD d",
            "            until c == a",
            "        else",
            "            break",
            "        endif",
            "    next i",
            "    return a + b + c + d",
            "endfunction"
        ])
        self.__test_node(self.__parser.parse(), "test_fun_decl_long_block")

    def test_fun_decl_ref_types(self):
        self.__init_parser([
            "function f(a:byRef, b:byVal, c)",
            "    return a * b * c",
            "endfunction",
        ])
        self.__test_node(self.__parser.parse(), "test_fun_decl_ref_types")

    def test_fun_call_no_assign(self):
        self.__init_parser([
            "f()",
            "a.b.c.d()",
            "a.b.c.d(e,f[g],h)",
            "a.b(c,d).e.f(g,h[i])"
        ])
        self.__test_node(self.__parser.parse(), "test_fun_call_no_assign")

    def test_fun_call_in_assign(self):
        self.__init_parser([
            "x = f()",
            "global y = obj1.obj2.f1(a,b).obj3.f2(c,d)"
        ])
        self.__test_node(self.__parser.parse(), "test_fun_call_in_assign")

    def test_proc_decl(self):
        self.__init_parser([
            "procedure sum(a:byRef, b:byVal, c)",
            "    global result = a + b + c",
            "    return",
            "endprocedure"
        ])
        self.__test_node(self.__parser.parse(), "test_proc_decl")

    # will not be testing calling of procedures as there is no distinction made at parsing level
    # between procedure calls and function calls when carried out as singular instructions

    def test_fun_expr(self):
        self.__init_parser([
            "a = int(x)",
            "b = float(y)",
            "c = str(x.y.z)",
            'd = input("Enter a value here: ")',
        ])
        self.__test_node(self.__parser.parse(), "test_fun_expr")

    def test_nested_fun_expr(self):
        self.__init_parser([
            "x.y[0] = int(a.b + str(float(int(str(float(input(\"Enter a number: \")))))))"
        ])
        self.__test_node(self.__parser.parse(), "test_nested_fun_expr")

    def test_str_functions(self):
        self.__init_parser([
            "print(x.y.z.s.length)",
            "print(x.y.z[0].s.substring(2,0))"
        ])
        self.__test_node(self.__parser.parse(), "test_str_functions")

    def test_file_functions(self):
        self.__init_parser([
            "x = openRead(\"filename.txt\")",
            "line = x.readLine()",
            "y = x.writeLine(\"another\" + \" line\")",
            "z = x.endOfFile()",
            "a.b = openWrite(\"filename2.txt\")",
            "x.close()",
            "a.b.close()",
        ])
        self.__test_node(self.__parser.parse(), "test_file_functions")

    def test_class_decl_simple(self):
        self.__init_parser([
            "class A",
            "    __val",
            "    procedure new(val)",
            "        __val = val",
            "    endprocedure",
            "endclass",
        ])
        self.__test_node(self.__parser.parse(), "test_class_decl_simple")

    def test_class_decl_no_id(self):
        with self.assertRaises(SyntaxError) as ex:
            self.__init_parser([
                "class ",
                "    public __val"
            ])
            self.__test_node(self.__parser.parse(), "")
        self.assertEqual(str(ex.exception), "Expected 'ID', received 'public'")

    def test_class_decl_no_endclass(self):
        with self.assertRaises(SyntaxError) as ex:
            self.__init_parser([
                "class A",
                "    public __val"
            ])
            self.__test_node(self.__parser.parse(), "")
        self.assertEqual(str(ex.exception), "Expected 'endclass' before end of file")

    def test_class_decl_inherit(self):
        self.__init_parser([
            "class B inherits A",
            "    private __val",
            "    public val",
            "    procedure new(v)",
            "        super.new()",
            "        val = v",
            "    endprocedure",
            "endclass",
        ])
        self.__test_node(self.__parser.parse(), "test_class_decl_inherit")

    def test_class_instantiate(self):
        self.__init_parser([
            'a = new A("a" + "b", 3 ^ 2 DIV 6, 2 / 5)'
        ])
        self.__test_node(self.__parser.parse(), "test_class_instantiate")

    # ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------- #

    def __test_node(self, node: Optional[Node], expected_json_key: str):
        expected_dict: dict = TestParser.EXPECTED[expected_json_key]
        node_dict: dict = self.ast_to_dict_parser.parse(node)
        self.assertDictEqual(expected_dict, node_dict)

    # ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------- #

    def __init_parser(self, lines: Iterable[str]):
        self.__tokenizer = Tokenizer()
        self.__lexer = Lexer(self.__tokenizer, lines)
        self.__parser = Parser(self.__lexer)
