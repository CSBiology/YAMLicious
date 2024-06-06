from __future__ import annotations
from typing import (Any, TypeVar)
from .fable_modules.fable_library.reflection import (TypeInfo, class_type)
from .fable_modules.fable_library.string_ import (to_console, printf)
from .fable_modules.fable_library.util import (get_enumerator, IEnumerator)

__A = TypeVar("__A")

def _expr1() -> TypeInfo:
    return class_type("YAMLicious.Main", None, Main)


class Main:
    @staticmethod
    def hello(name: str, from_: str | None=None) -> None:
        if from_ is None:
            to_console(printf("Hello, %s!"))(name)

        else: 
            f: str = from_
            to_console(printf("Hello, %s! (from %s)"))(name)(f)


    @staticmethod
    def print_tuples(tuples: Any | None=None) -> None:
        def _arrow0(__unit: None=None) -> IEnumerator[tuple[str, str]]:
            copy_of_struct: __A = tuples
            return get_enumerator(copy_of_struct)

        with _arrow0() as enumerator:
            while enumerator.System_Collections_IEnumerator_MoveNext():
                for_loop_var: tuple[str, str] = enumerator.System_Collections_Generic_IEnumerator_1_get_Current()
                to_console(printf("%s: %s"))(for_loop_var[0])(for_loop_var[1])


Main_reflection = _expr1

__all__ = ["Main_reflection"]

