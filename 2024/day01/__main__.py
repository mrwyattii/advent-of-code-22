from collections import Counter
from typing import List, Tuple

from common.aoc_day import AoCDay


class Day01(AoCDay):
    def process_input(self, raw_input: List[str]) -> List[Tuple[int, int]]:
        return list(map(lambda x: tuple(map(int, x.split())), raw_input))

    def part1(self, location_ids: List[Tuple[int, int]]) -> int:
        left, right = zip(*location_ids)
        return sum(abs(x - y) for x, y in zip(sorted(left), sorted(right)))

    def part2(self, location_ids: List[Tuple[int, int]]) -> int:
        left, right = zip(*location_ids)
        counter = Counter(right)
        return sum(x * counter[x] for x in left)


if __name__ == "__main__":
    Day01()()
