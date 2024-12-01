import inspect
from abc import ABC
from pathlib import Path
from typing import Any, List


class AoCDay(ABC):
    def __init__(self, test: bool = False):
        self.test = test
        self.working_dir = Path(inspect.getfile(self.__class__)).parent
        self.input = self._load_input()
        self.processed_input = self.process_input(self.input)

    def __call__(self):
        print("Part 1:")
        self.part1(self.processed_input)
        print("Part 2:")
        self.part2(self.processed_input)

    def _load_input(self) -> List[str]:
        input_file = None
        if not self.test:
            input_file = self.working_dir / "input.txt"
            if not input_file.exists():
                input_file = None
        if input_file is None:
            input_file = self.working_dir / "test.txt"

        with open(input_file) as f:
            return f.readlines()

    def process_input(self, raw_input: List[str]) -> Any:
        raise NotImplementedError

    def part1(self, input) -> None:
        raise NotImplementedError

    def part2(self, input) -> None:
        raise NotImplementedError
