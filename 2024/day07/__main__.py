from itertools import product
from operator import add, mul
from typing import Callable, List

from common.aoc_day import AoCDay


class Day07(AoCDay):
    def process_input(self, raw_input: List[str]) -> List[int]:
        return [
            map(int, line.replace(":", "").strip().split(" ")) for line in raw_input
        ]

    @staticmethod
    def equation_satisfied(
        test_value: int, calibration_values: List[int], operators: List[Callable]
    ):
        for ops in product(operators, repeat=len(calibration_values) - 1):
            accum = calibration_values[0]
            for i, op in enumerate(ops):
                accum = op(accum, calibration_values[i + 1])
            if accum == test_value:
                return True
        return False

    def part1(self, input: List[int]) -> None:
        operators = [add, mul]
        print(
            sum(
                test_value
                * self.equation_satisfied(test_value, calibration_values, operators)
                for test_value, *calibration_values in input
            )
        )

    def part2(self, input: List[int]) -> None:
        operators = [add, mul, lambda x, y: int(str(x) + str(y))]
        print(
            sum(
                test_value
                * self.equation_satisfied(test_value, calibration_values, operators)
                for test_value, *calibration_values in input
            )
        )


if __name__ == "__main__":
    Day07(0)()
