import sys
from itertools import product
from typing import Dict, Generator, List

from common.aoc_day import AoCDay
from tqdm import tqdm


class Day20(AoCDay):
    def process_input(self, raw_input: List[str]) -> Dict[complex, int]:
        race_map = set()
        start_stop = []
        for y, line in enumerate(raw_input):
            for x, char in enumerate(line):
                if char in ".SE":
                    race_map.add(complex(x, y))
                if char in "SE":
                    start_stop.append(complex(x, y))

        race_path_length = {}
        loc, steps = start_stop[0], 0
        while loc != start_stop[1]:
            for d in [1, 1j, -1, -1j]:
                neighbor = loc + d
                if (neighbor not in race_path_length) and (neighbor in race_map):
                    break
            race_path_length[loc] = steps
            loc, steps = neighbor, steps + 1
        race_path_length[loc] = steps

        return race_path_length

    def solve(
        self, race_path_length: Dict[complex, int], max_cheat_time: int
    ) -> Generator:
        for loc in tqdm(race_path_length.keys()):
            for x, y in product(range(-max_cheat_time, max_cheat_time + 1), repeat=2):
                if (x == 0 and y == 0) or (abs(x) + abs(y) > max_cheat_time):
                    continue
                step = complex(x, y)
                yield race_path_length[loc] - race_path_length.get(
                    loc + step, sys.maxsize
                ) - abs(x) - abs(y)

    def part1(self, race_path_length: Dict[complex, int]) -> int:
        min_cheat_improvement = 100
        return sum(
            cheat_improvement >= min_cheat_improvement
            for cheat_improvement in self.solve(race_path_length, 2)
        )

    def part2(self, race_path_length: Dict[complex, int]) -> int:
        min_cheat_improvement = 100
        return sum(
            cheat_improvement >= min_cheat_improvement
            for cheat_improvement in self.solve(race_path_length, 20)
        )


if __name__ == "__main__":
    Day20()()
