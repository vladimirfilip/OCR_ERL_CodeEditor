from unittest import TestCase

from sym_table import SymTable, ArrayVal, SymAddr


class TestArrayVal(TestCase):

    def setUp(self) -> None:
        self.arr = ArrayVal([2, 3, 4, 5, 6])

    def tearDown(self) -> None:
        self.arr = None

    def test_get_slice(self):
        #
        # Try some arbitrary sub-indexing
        #
        start, size = self.arr.get_slice([0, 0, 0])
        self.assertEqual(0, start)
        self.assertEqual(size, 5 * 6)
        #
        # Try only one value
        #
        start, size = self.arr.get_slice([0] * len(self.arr.dims))
        self.assertEqual(0, start)
        self.assertEqual(1, size)
        #
        # Try all dimensions but the first
        #
        start, size = self.arr.get_slice([0])
        self.assertEqual(0, start)
        self.assertEqual(self.arr.size // self.arr.dims[0], size)

    def test_get_value_at(self):
        self.__init_contiguous_arr()
        #
        # Try single values
        #
        v = 0
        for i in range(0, self.arr.dims[0]):
            for j in range(0, self.arr.dims[1]):
                for k in range(0, self.arr.dims[2]):
                    for l in range(0, self.arr.dims[3]):
                        for m in range(0, self.arr.dims[4]):
                            self.assertEqual(v, self.arr.get_at([i, j, k, l, m]))
                            v += 1
        self.assertIsNotNone(self.arr)

    def test_get_sub_arr_at(self):
        self.__init_contiguous_arr()
        sub_arr: SymTable.ArrayVal = self.arr.get_at([1, 2, 3])
        self.assertEqual(len(self.arr.dims) - 3, len(sub_arr.dims))
        vals: list[int] = []
        v = 0
        for i in range(0, self.arr.dims[0]):
            for j in range(0, self.arr.dims[1]):
                for k in range(0, self.arr.dims[2]):
                    for l in range(0, self.arr.dims[3]):
                        for m in range(0, self.arr.dims[4]):
                            if i == 1 and j == 2 and k == 3:
                                vals.append(v)
                            v += 1
        self.assertListEqual([5, 6], sub_arr.dims)
        self.assertListEqual(vals, sub_arr.vals)

    def test_set_at(self):
        self.__init_contiguous_arr()
        self.assertListEqual([i for i in range(0, self.arr.size)], self.arr.vals)

    def __init_contiguous_arr(self):
        v = 0
        for i in range(0, self.arr.dims[0]):
            for j in range(0, self.arr.dims[1]):
                for k in range(0, self.arr.dims[2]):
                    for l in range(0, self.arr.dims[3]):
                        for m in range(0, self.arr.dims[4]):
                            self.arr.set_at([i, j, k, l, m], v)
                            v += 1


class TestSymAddr(TestCase):

    def setUp(self) -> None:
        self.sym_table: SymTable = SymTable()
        self.scalar_addr: SymAddr = SymAddr(self.sym_table, 'scalar')
        self.vect_addr: SymAddr = SymAddr(self.sym_table, 'vector')
        self.vect_addr.value = ArrayVal([2, 3, 4])
        self.cell_addr: SymAddr = SymAddr(self.sym_table, self.vect_addr.name, [1, 2, 3])
        self.slice_addr: SymAddr = SymAddr(self.sym_table, self.vect_addr.name, [1, 2])

    def tearDown(self) -> None:
        self.slice_addr = None
        self.cell_addr = None
        self.vect_addr = None
        self.scalar_addr = None
        self.sym_table = None

    def test_set_value(self):
        #
        # Set scalar
        #
        self.scalar_addr.value = 5
        self.assertEqual(5, self.scalar_addr.value)
        #
        # Set vector
        #
        self.cell_addr.value = 7
        self.assertEqual(7, self.cell_addr.value)
        self.assertEqual(7, self.slice_addr.value.get_at([3]))

    def test_get_value(self):
        v: int = 0
        vals: list[int] = []
        for i in range(0, 2):
            for j in range(0, 3):
                for k in range(0, 4):
                    self.vect_addr.value.set_at([i, j, k], v)
                    if i == 1 and j == 2:
                        vals.append(v)
                    v += 1
        arr: ArrayVal = self.slice_addr.value
        self.assertListEqual([4], arr.dims)
        self.assertEqual(4, len(vals))
        self.assertListEqual(vals, arr.vals)

    def test_sym_table(self):
        for addr in [self.scalar_addr, self.vect_addr, self.cell_addr, self.slice_addr]:
            self.assertIs(self.sym_table, addr.sym_table)

    def test_name(self):
        self.assertEqual('scalar', self.scalar_addr.name)
        for addr in [self.vect_addr, self.cell_addr, self.slice_addr]:
            self.assertEqual('vector', addr.name)

    def test_dims(self):
        self.assertEqual(0, self.scalar_addr.dims)
        self.assertEqual(0, self.vect_addr.dims)
        self.assertEqual(3, self.cell_addr.dims)
        self.assertEqual(2, self.slice_addr.dims)

    def test_index_at(self):
        for i in range(0, 3):
            self.assertEqual(i + 1, self.cell_addr.index_at(i))
            if i < 2:
                self.assertEqual(i + 1, self.slice_addr.index_at(i))


class TestSymTable(TestCase):

    def setUp(self) -> None:
        self.sym_table = SymTable()

    def tearDown(self) -> None:
        self.sym_table.close(True)
        self.sym_table = None

    def test_close(self):
        tbl: SymTable = SymTable(self.sym_table)
        self.assertIs(self.sym_table, tbl.parent)
        self.assertIs(self.sym_table, tbl.root)
        tbl.close()
        self.assertIsNone(tbl.parent)
        self.assertIs(tbl, tbl.root)

    def test_root(self):
        self.assertIs(self.sym_table, self.sym_table.root)

    def test_update_symbol(self):
        self.sym_table.update_symbol('x', 3)
        self.assertEqual(3, self.sym_table.lookup_symbol('x'))
        self.sym_table.update_symbol('x', None)
        self.assertIsNone(self.sym_table.lookup_symbol('x'))

    def test_update_symbols(self):
        #
        # Set in bulk
        #
        self.sym_table.update_symbols({str(i): i + 1 for i in range(0, 10)})
        for i in range(0, 10):
            self.assertEqual(i + 1, self.sym_table.lookup_symbol(str(i)))
        #
        # Cancel in bulk
        #
        self.sym_table.update_symbols({str(i): None for i in range(0, 10) if 1 == i % 2})
        for i in range(0, 10):
            if 1 == i % 2:
                self.assertIsNone(self.sym_table.lookup_symbol(str(i)))
            else:
                self.assertEqual(i + 1, self.sym_table.lookup_symbol(str(i)))

    def test_clear_symbol(self):
        self.assertEqual(3, self.sym_table.lookup_symbol('x', 3))
        self.sym_table.clear_symbol('x')
        self.assertIsNone(self.sym_table.lookup_symbol('x'))

    def test_clear_symbols(self):
        self.sym_table.clear_symbols({})
        self.assertEqual(0, len(self.sym_table))

    def test_lookup_symbol(self):
        self.assertIsNone(self.sym_table.lookup_symbol('x'))
        self.assertEqual(3, self.sym_table.lookup_symbol('x', 3))
        self.assertEqual(3, self.sym_table.lookup_symbol('x'))
