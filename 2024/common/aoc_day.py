import inspect
import time
from abc import ABC
from collections import defaultdict
from copy import deepcopy
from pathlib import Path
from typing import Any, List, Tuple, Union


class AoCDay(ABC):
    def __init__(self, test: bool = False):
        self.test = test
        self.working_dir = Path(inspect.getfile(self.__class__)).parent
        self.raw_input = self._load_input()

    def __call__(self):
        start = time.time()
        puzzle_input = self.process_input(self.raw_input)
        input_processing_time = time.time() - start

        start = time.time()
        part1_solution = self.part1(deepcopy(puzzle_input))
        part1_time = time.time() - start

        start = time.time()
        part2_solutions = self.part2(deepcopy(puzzle_input))
        part2_time = time.time() - start

        table = [
            ["Input Processing", input_processing_time, "N/A"],
            ["Part 1", part1_time, part1_solution],
            ["Part 2", part2_time, part2_solutions],
            ["Total", input_processing_time + part1_time + part2_time, "N/A"],
        ]
        self._print_table(table)

    def _print_table(self, table: List[Tuple[str, float, Union[int, str]]]) -> None:
        table_name = self.__class__.__name__
        col_names = ["Step", "Time", "Result"]

        for row in table:
            for limit, unit in [(60, "m "), (1, "s "), (0.001, "ms"), (0.000001, "µs")]:
                if row[1] > limit:
                    break
            row[1] = f"{row[1]/limit:.2f} {unit}"

        longest_in_col = defaultdict(int)
        for row in table:
            for i, cell in enumerate(row):
                longest_in_col[i] = max(longest_in_col[i], len(str(cell)))

        table_width = sum(longest_in_col.values()) + 3 * len(longest_in_col) + 1
        print("-" * table_width)
        print(table_name.center(table_width))
        print("-" * table_width)
        print(
            " | ".join(
                col_name.center(longest_in_col[i])
                for i, col_name in enumerate(col_names)
            )
        )
        for row in table:
            print(row[0].ljust(longest_in_col[0]), end=" | ")
            print(row[1].rjust(longest_in_col[1]), end=" | ")
            print(str(row[2]).rjust(longest_in_col[2]))

    def _load_input(self) -> List[str]:
        input_file = None
        if not self.test:
            input_file = self.working_dir / "input.txt"
            if not input_file.exists():
                input_file = None
        if input_file is None:
            input_file = self.working_dir / "test.txt"

        with open(input_file) as f:
            lines = f.readlines()

        return [x for x in lines if x.strip()]

    def process_input(self, raw_input: List[str]) -> Any:
        raise NotImplementedError

    def part1(self, puzzle_input: Any) -> int:
        raise NotImplementedError

    def part2(self, puzzle_input: Any) -> int:
        raise NotImplementedError
