import io
from typing import Dict, TypeVar, Optional, Iterable, Tuple, Iterator

from parsed_token import TokenContents

V = TypeVar("V")


class NullVal:
    #
    # Used to provide distinction between a null value in the language
    # and None in Python used to indicate the lack of the existence of
    # a value in a symbol table
    #
    def __str__(self) -> str:
        return 'null'

    def __eq__(self, other) -> str:
        # All NullVal instances are the same
        return type(other) == NullVal

    def __hash__(self) -> int:
        return 9691


class ArrayVal:
    """Representation of an array value in the symbol table."""

    dims: list[int]
    vals: list[V]
    size: int

    def __init__(self, dims: list[int]):
        """Initializes the array value with a list of dimensions.

        :param dims: list of sizes for each dimension.
        """
        assert dims, "Illegal empty list of array dimensions"
        for d in dims:
            assert d > 0, f"Illegal array dimension: {d}"
        self.dims = dims
        self.size = 1
        for d in self.dims:
            self.size *= d
        self.vals = [NullVal()] * self.size

    def get_slice(self, indexes: list[int]) -> Tuple[int, int]:
        """Gets the range within the 'vals' attribute corresponding to the given indexes.

        :param indexes: list of indexes identifying the range.
        :return: pair of (start, size) values where 'start' is the index within the 'vals' array and 'size' is the length of the slice.
        """
        if 0 < len(indexes) <= len(self.dims):
            start: int = 0
            size: int = self.size
            for i in range(0, len(indexes)):
                size //= self.dims[i]
                start += size * indexes[i]
            return start, size
        raise IndexError(f"Illegal array indexes: {indexes}")

    def get_at(self, indexes: list[int]) -> 'ArrayVal' | V:
        """Gets the values at the given indexes.

        If the dimensionality of the result is 1, then this method returns a single value. Otherwise, it returns a ValArray of reduced dimensionality and set with the slice
        of values corresponding to the list of indexes.

        :param indexes: list of indexes identifying the slice or value to retrieve.
        :return: sub-array value or scalar value, depending on the dimensionality of the result.
        """
        start, size = self.get_slice(indexes)
        assert start >= 0 and size >= 1, "Logical error, should never be here"
        if size == 1:
            return self.vals[start]
        end = start + size
        assert len(indexes) < len(self.dims), "Logical error, should never be here"
        result: 'ArrayVal' = ArrayVal(self.dims[len(indexes):])
        result.vals = self.vals[start:end]
        result.size = size
        return result

    def set_at(self, indexes: list[int], val: V) -> V:
        """Sets a single value within the array, at the position given by the indexes.

        :param val: value to set.
        :param indexes: list of indexes to set the value at. The length of this list must match the dimensionality of the array.
        :return: the value that has been set.
        """
        start, size = self.get_slice(indexes)
        assert start >= 0 and size >= 1, "Logical error, should never be here"
        if size > 1:
            raise IndexError(f"Illegal indexing for value setting: {indexes}")
        self.vals[start] = val
        return val

    @property
    def length(self) -> int:
        return self.dims[0]

    def __eq__(self, other: 'ArrayVal') -> bool:
        return self.vals == other.vals

    def __str__(self) -> str:
        result = ArrayVal.split_vals(self.vals.copy(), self.dims.copy())
        return ArrayVal.stringify_list(result)

    @staticmethod
    def split_vals(vals: list[V], dims: list[int]) -> list:
        #
        # As the values in ArrayVal are stored as a contiguous array, regardless of dimensionality,
        # this function is used to create an array with sub-arrays of correct length according to the dimensions
        #
        if len(dims) == 1:
            return vals
        first_dim = dims.pop(0)
        slice_length = len(vals) // first_dim
        result = [vals[i:i + slice_length] for i in range(0, len(vals), slice_length)]
        for i in range(len(result)):
            result[i] = ArrayVal.split_vals(result[i], dims)
        return result

    @staticmethod
    def stringify_list(vals: list[V]) -> str:
        #
        # Casts any object values of a type present in types_to_cast to string
        #
        casted_values = []
        for val in vals:
            if type(val) == str:
                casted_values.append(f"'{val}'")
            elif type(val) == list:
                casted_values.append(ArrayVal.stringify_list(val))
            else:
                casted_values.append(str(val))
        return '[' + ", ".join(casted_values) + ']'


class SymAddr:
    """Address of a value in a symbol table."""

    def __init__(self, tbl: 'SymTable', name: str, indexes: list[int] = None):
        """Initializes the address with a symbol table, a symbol name and an optional list of indexes.

        :param tbl: symbol table storing the symbol and its value.
        :param name: symbol in the table to get the value from or set the value to.
        :param indexes: list of indexes to set array values. Usable for arrays only. It must be None for scalars.
        """
        self.__tbl = tbl
        self.__name = name
        self.__indexes = None
        if indexes is not None:
            assert indexes, "List of addressing indexes may not be empty"
            for index in indexes:
                assert index >= 0, f"Invalid array dimension value: {index}"
            self.__indexes = indexes

    def addr_of(self, indexes: list[int]) -> 'SymAddr':
        """Gets the indexed address from the current address, for the same symbol table and under the same name.

        This method raises an error on array addresses. It must be called on scalars only.

        :param indexes: indexes to apply upon the current address to get the new address.
        :return: indexed address for the same symbol table and under the same name.
        """
        if self.__indexes is None:
            return self.__tbl.addr_of(self.__name, indexes)
        raise RuntimeError(f"Address {self.__name} already has indexes: {self.__indexes}")

    @property
    def value(self) -> V | ArrayVal:
        """Gets the value associated with the symbol (and indexes, if any)."""
        result: V = self.sym_table.lookup_symbol(self.name)
        if result is not None:
            if self.dims:
                arr: ArrayVal = result
                return arr.get_at(self.__indexes)
            return result
        raise ValueError(f"Unknown symbol to get value of: {self.name}")

    @value.setter
    def value(self, val: V):
        """Sets the value associated with the symbol (and indexes, if any).

        In presence of address indexes, they must match in number the number of dimensions of the underlying array value - even though getting values allowes for retrieval of
        sub-array slices.

        :param val: value to set at the given symbol (and indexes, if the symbol is bound to an array value)
        """
        if self.__indexes:
            arr: ArrayVal = self.sym_table.lookup_symbol(self.name)
            if arr:
                if len(arr.dims) != len(self.__indexes):
                    raise RuntimeError(f"Invalid number of indexes to set: expected={len(arr.dims)}, provided={self.__indexes}")
                arr.set_at(self.__indexes, val)
        else:
            self.sym_table.update_symbol(self.name, val)

    @property
    def has_none_value(self) -> bool:
        return self.sym_table.lookup_symbol(self.name) is None

    @property
    def sym_table(self) -> 'SymTable':
        """Gets the symbol table to the address refers to."""
        return self.__tbl

    @property
    def name(self) -> str:
        """Gets the symbol that the address refers to."""
        return self.__name

    @property
    def dims(self) -> int:
        """Gets the number of dimensions of the array associated with the symbol or 0 if the value is a scalar."""
        return len(self.__indexes) if self.__indexes else 0

    @property
    def is_public(self) -> bool:
        """Returns whether the symbol the address is referring to is public"""
        return self.sym_table.is_symbol_public(self.name)

    def index_at(self, dim: int) -> int:
        """Gets the index for the given dimension, if the underlying value is an array.

        :param dim: ordinal of the dimension to get the index for. The first dimension is 0.
        :return: index corresponding to the dimension.
        """
        if self.dims:
            if 0 <= dim < self.dims:
                return self.__indexes[dim]
            raise IndexError(f"Index out of bounds: {dim} (negative or not less than {self.dims})")
        raise RuntimeError("Illegal to get index from non-dimensional address")

    def __str__(self) -> str:
        try:
            value = self.value
        except ValueError:
            value = None
        return f"Address to {self.name}" + f" of value {value}" if value is not None else " of null value"


class SymRef(SymAddr):
    def __init__(self, tbl: 'SymTable', name: str, addr: SymAddr = None):
        super().__init__(tbl, name)
        self.__addr = None
        if addr is None:
            addr = super().value
        if isinstance(addr, SymAddr):
            self.__addr = addr
        else:
            raise SyntaxError("Reference given is not an address")

    def addr_of(self, indexes: list[int]) -> 'SymAddr':
        """Gets the indexed address from the current address, for the same symbol table and under the same name.

        This method raises an error on array addresses. It must be called on scalars only.

        :param indexes: indexes to apply upon the current address to get the new address.
        :return: indexed address for the same symbol table and under the same name.
        """
        return self.__addr.addr_of(indexes)

    @property
    def value(self) -> V | ArrayVal:
        """Gets the value associated with the symbol (and indexes, if any)."""
        return self.__addr.value

    @value.setter
    def value(self, val: V):
        """Sets the value associated with the symbol (and indexes, if any).

        In presence of address indexes, they must match in number the number of dimensions of the underlying array value - even though getting values allowes for retrieval of
        sub-array slices.

        :param val: value to set at the given symbol (and indexes, if the symbol is bound to an array value)
        """
        self.__addr.value = val

    @property
    def dims(self) -> int:
        """Gets the number of dimensions of the array associated with the symbol or 0 if the value is a scalar.
        If address of field of the object is not yet initialised then the parent dims property is used
        """
        return self.__addr.dims if self.__addr is not None else super().dims

    def index_at(self, dim: int) -> int:
        """Gets the index for the given dimension, if the underlying value is an array.

        :param dim: ordinal of the dimension to get the index for. The first dimension is 0.
        :return: index corresponding to the dimension.
        """
        return self.__addr.index_at(dim)


class SymTable(Dict[str, V]):
    """Symbol table.

    It is a hierarchical dictionary of (name, value) correspondence, each 'name' representing a variable, and each 'value' the value associated with it.

    Each symbol table has a reference to its parent table (if any), the root symbol table (defined as the table with no parent) and a collection of sub-tables. Each sub-table
    is identified by a key, usually the name of the element (class, function, variable) that defines the sub-context represented by the sub-table.

    Important: creating sub-tables should go via SymTable.sub_table_from() via a 'with' statement. This class has the auto-closing mechanism in place which removes circular
               references before removing the object from
    """

    parent: 'SymTable'
    sub_tables: Dict[str, 'SymTable']

    def __init__(self, parent: 'SymTable' = None, init_symbols: Dict[str, V] = {}):
        """Initializes the symbol table.

        If the table has a parent, then the reference to the parent gets stored inside. The symbol table may be pre-populated with symbols.

        :param parent: symbol table that the current table is a child of.
        :param init_symbols: dictionary of (name, value) pairs indicating the symbols that the table needs to be pre-populated with.
        :param temp: flag indicating whether the table is temporary. If temporary, then its close
        """
        super().__init__()
        self.parent = parent
        self.__root = None
        self.sub_tables = {}
        self.update_symbols(init_symbols)

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.close()

    def close(self, recursive: bool = False):
        """Closes the object by removing circular references to parent and root symbol tables."""
        self.__root = None
        self.parent = None
        if recursive:
            for st in self.sub_tables.values():
                sub_table: SymTable = st
                sub_table.close(recursive)
        #
        # Closes any I/O streams present
        #
        for v in self.values():
            if isinstance(v, io.IOBase):
                v.close()

    @property
    def root(self) -> 'SymTable':
        """Gets the root symbol table, corresponding to the global context.

        :return: the symbol table with no parent.
        """
        if self.parent is None:
            return self
        if self.__root is None:
            self.__root = self.parent.root
        return self.__root

    def addr_of(self, name: str, indexes: list[int] = None, may_be_ref: bool = False) -> SymAddr:
        """Gets the symbol table address corresponding to the given symbol (and, optionally, list of indexes).

        :param name: symbol to address via the returned instance.
        :param indexes: list of indexes to address locations within the array associated with the symbol. It must be None for scalars and non-empty for arrays.
        :param may_be_ref: True if the address may be a SymAddr instance, False otherwise
        :return: address within the current symbol table.
        """
        if may_be_ref and isinstance(self.lookup_symbol(name), SymAddr):
            result = SymRef(self, name)
            if indexes is not None:
                result = result.addr_of(indexes)
            return result
        return SymAddr(self, name, indexes)

    def update_symbol(self, name: str, val: V = None) -> 'SymTable':
        """Updates a symbol by associating it with the given value.

        :param name: symbol to associate with the value.
        :param val: value to associate the symbol with. If None then the symbol gets removed from the table.
        :return: this symbol table.
        """
        if val is None:
            self.pop(name, None)
        else:
            self[name] = val
        return self

    def update_symbols(self, symbols: Dict[str, V]) -> 'SymTable':
        """Updates a set of symbols with the given values.

        :param symbols: dictionary of (name, value) pairs, indicating the value to associated with each symbol in the symbol table. A None value indicates the corresponding symbol
                        will get removed from the table.
        :return: this symbol table.
        """
        for n, v in symbols.items():
            self.update_symbol(n, v)
        return self

    def clear_symbol(self, name: str) -> 'SymTable':
        """Removes the symbol from table, if present.

        :param name: symbol to remove from the table.
        :return: this symbol table.
        """
        return self.clear_symbols([name])

    def clear_symbols(self, names: Iterable[str]) -> 'SymTable':
        """Removes the given symbols from the table.

        :param names: sequence of symbols to remove.
        :return: this symbol table.
        """
        self.update_symbols({n: None for n in names})
        return self

    def sub_table_from(self, name: str, new_if_absent: bool = False) -> Optional['SymTable']:
        """Gets the sub-table associated with the given key.

        :param name: sub-table identifier.
        :param new_if_absent: whether to create the sub-table if it doesn't exist. It defaults to False.
        :return: the sub-table associated with the key, if any.
        """
        result: Optional['SymTable'] = self.sub_tables.get(name, None)
        if result is None and new_if_absent:
            result = SymTable(self)
            self.sub_tables[name] = result
        return result

    def sub_table_of(self, names: Iterable[str] = []) -> Optional['SymTable']:
        """Gets the sub-table corresponding to the given sequence of names.

        :param names: sequence of names to lookup by. If empty then the function returns the current symbol table.
        :return: symbol sub-table corresponding to the given sequence of names. None if the lookup breaks before reaching the end of the sequence.
        """
        result: 'SymTable' = self
        for n in names:
            result = result.sub_table_from(n)
            if result is None:
                break
        return result

    def clear_sub_table(self, name: str = None) -> Optional['SymTable'] | list['SymTable']:
        """Removes the sub-table corresponding to the given identifying key.

        This method also closes the cleared sub-table(s).

        :param name: identifier for the sub-table to remove. If None, then all sub-tables get removed.
        :return: the cleared sub-table(s)
        """
        if name:
            result: SymTable = self.sub_tables.pop(name, None)
            if result:
                result.close()
        else:
            names: list[str] = list(self.sub_tables.keys())
            result: list[SymTable] = [self.clear_sub_table(n) for n in names]
            result = [t for t in result if t is not None]
        return result

    def lookup_symbol(self, name: str, value_if_absent: V = None) -> Optional[V]:
        """Gets the value associated with a symbol or a sequence of symbols.

        :param name: symbol to get the value for. It may not be None.
        :param value_if_absent: value to assign to the symbol if the name (or the last symbol in the sequence) cannot be found in its corresponding table. This parameter must
                                not be None for the assignment to take effect.
        :return: value associated with the symbol, if any.
        """
        ret: Optional[Tuple[V, 'SymTable']] = self.lookup_symbol_with_table(name)
        if ret is not None:
            result, _ = ret
        elif value_if_absent is not None:
            result = value_if_absent
            self[name] = result
        else:
            result = None
        return result

    def lookup_symbol_with_table(self, name: str) -> Optional[Tuple[V, 'SymTable']]:
        sym_table = self
        while name not in sym_table and sym_table.parent is not None:
            sym_table = sym_table.parent
        return (sym_table.get(name), sym_table) if name in sym_table else None

    def lookup_deep_symbol(self, names: Iterable[str], value_if_absent: V = None) -> Optional[V]:
        """Gets the value associated with a complex symbol represented a sequence of symbols.

        :param names: sequence of names to look the value up by. It may not be empty. Each symbol in the sequence but the last one determine the successive sub-tables to look
                      up by.
        :param value_if_absent: value to assign to the symbol if the name (or the last symbol in the sequence) cannot be found in its corresponding table. This parameter must
                                not be None for the assignment to take effect.
        :return: value associated with the symbol or sequence of symbols, if any.
        """
        name, tbl = self.__lookup_table(names=names)
        if tbl is not None:
            if name not in tbl and value_if_absent is not None:
                tbl[name] = value_if_absent
            return tbl.get(name, None)
        return None

    def update_deep_symbol(self, names: Iterable[str], val: V = None, strict: bool = False) -> bool:
        """Updates the value associated with a complex symbol represented as a sequence of symbols.

        :param names: sequence of names indicating the symbol (and its symbol table) to set the value for. It may not be empty. Each symbol in the sequence but the last one
                      determine the successive sub-tables to lookup by.
        :param val: value to assign to the symbol. If None then the symbol gets removed from its own table. If not None and the symbol does not exist, it gets created.
        :param strict: True if the symbol's table must exist before assignment. This flag matters only if the lookup can't find the symbol table owning the symbol.
        :return: True if the table for the symbol is found and the symbol was updated successfully. False if the there is no symbol table that may contain the symbol corresponding
                 to the given name or sequence of names.
        """
        name, tbl = self.__lookup_table(names=names)
        if tbl is not None:
            tbl.update_symbol(name, val)
            return True
        if strict:
            raise RuntimeError(f"Failed symbol(s) lookup: {TokenContents.DOT.value.join(names)}")
        return False

    def __lookup_table(self, name: str = None, names: Iterable[str] = []) -> Tuple[str, Optional['SymTable']]:
        if name is not None:
            return self.__lookup_table(names=[name])
        name_list: list[str] = list(names)
        if name_list:
            return name_list[-1], self.sub_table_of(names=name_list[:-1])
        else:
            raise RuntimeError("No lookup symbol(s)")

    def is_symbol_public(self, name: str) -> bool:
        #
        # By default, all entries in a symbol table are public
        #
        return True


class ObjSymTable(SymTable):
    def __init__(self, parent: SymTable, storage_key: str, class_name: str):
        super().__init__(parent)
        self.class_name = class_name
        self.STORAGE_KEY = storage_key
        self.public_symbols = []
        if isinstance(parent, ObjSymTable):
            self.add_parent_class(parent)

    @property
    def storage_key(self) -> str:
        return self.STORAGE_KEY

    def add_member(self, name: str, val: V, is_public: bool):
        ret: Optional[Tuple[V, SymTable]] = self.lookup_symbol_with_table(name)
        if ret is not None:
            _, tbl = ret
            if tbl == self:
                raise SyntaxError(f"Declaration of the attribute '{name}' cannot appear more than once")
        self.update_symbol(name, val)
        if is_public:
            self.public_symbols.append(name)

    def add_parent_class(self, parent: SymTable):
        assert parent == self.parent, ("Parent class of the object cannot be different "
                                       "from parent of the corresponding symbol table")
        self.add_member(TokenContents.SUPER.value, parent, is_public=True)

    def is_symbol_public(self, name: str) -> bool:
        return name in self.public_symbols

    def get_fields(self) -> Iterator[tuple[str, V]]:
        for k, v in self.items():
            yield k, v
        if isinstance(self.parent, ObjSymTable):
            for k, v in self.parent.get_fields():
                yield k, v

    def __str__(self) -> str:
        return "<Instance of '{}': {}>".format(self.class_name, '{' + ", ".join(f"{str(k)}: {str(v)}" for k, v in self.get_fields()) + '}')
