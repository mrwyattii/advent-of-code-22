import re
from typing import List

from common.aoc_day import AoCDay


class Day03(AoCDay):
    def process_input(self, raw_input: List[str]) -> str:
        return "_".join(raw_input).replace("\n", "_")

    def part1(self, input: str) -> None:
        mul_re = r"mul\(([0-9]{1,3}),([0-9]{1,3})\)"
        print(sum([int(x) * int(y) for x, y in re.findall(mul_re, input)]))

    def part2(self, input: str) -> None:
        input = "do()" + input + "don't()"
        do_dont_re = r"do\(\)(.*?)don\'t\(\)"
        self.part1("".join(re.findall(do_dont_re, input)))


if __name__ == "__main__":
    Day03()()
