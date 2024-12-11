from operator import add, mul
from typing import Callable, List

from common.aoc_day import AoCDay
from tqdm import tqdm


class Day07(AoCDay):
    def process_input(self, raw_input: List[str]) -> List[int]:
        return [
            map(int, line.replace(":", "").strip().split(" ")) for line in raw_input
        ]

    @staticmethod
    def equation_satisfied(
        test_value: int, calibration_values: List[int], operators: List[Callable]
    ):
        calculated_values = {calibration_values[0]}
        for y in calibration_values[1:]:
            calculated_values = {
                op(x, y) for x in calculated_values for op in operators
            }
            calculated_values = {x for x in calculated_values if x <= test_value}
            if test_value in calculated_values:
                return True
        return False

    def part1(self, eq_inputs: List[int]) -> int:
        operators = [add, mul]
        sum_value = 0
        for test_value, *calibration_values in tqdm(eq_inputs):
            sum_value += test_value * self.equation_satisfied(
                test_value, calibration_values, operators
            )
        return sum_value

    def part2(self, eq_inputs: List[int]) -> int:
        operators = [add, mul, lambda x, y: x * (10 ** len(str(y))) + y]
        sum_value = 0
        for test_value, *calibration_values in tqdm(eq_inputs):
            sum_value += test_value * self.equation_satisfied(
                test_value, calibration_values, operators
            )
        return sum_value


if __name__ == "__main__":
    Day07()()
