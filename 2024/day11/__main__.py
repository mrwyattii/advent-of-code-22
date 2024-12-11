from collections import defaultdict
from typing import DefaultDict, List

from common.aoc_day import AoCDay
from tqdm import trange


class Day11(AoCDay):
    def process_input(self, raw_input: List[str]) -> DefaultDict[int, int]:
        stones = defaultdict(int)
        for s in raw_input[0].strip().split(" "):
            stones[int(s)] += 1
        return stones

    @staticmethod
    def blink(stones: DefaultDict[int, int]) -> DefaultDict[int, int]:
        new_stones = defaultdict(int)
        for stone_val, num_stones in stones.items():
            if stone_val == 0:
                new_stones[stone_val + 1] += num_stones
            elif len(str(stone_val)) % 2 == 0:
                str_stone_val = str(stone_val)
                half_len = len(str_stone_val) // 2
                new_stones[int(str_stone_val[:half_len])] += num_stones
                new_stones[int(str_stone_val[half_len:])] += num_stones
            else:
                new_stones[stone_val * 2024] += num_stones
        return new_stones

    def part1(self, stones: DefaultDict[int, int]) -> int:
        for _ in trange(25):
            stones = self.blink(stones)
        return sum(stones.values())

    def part2(self, stones: DefaultDict[int, int]) -> int:
        for _ in trange(75):
            stones = self.blink(stones)
        return sum(stones.values())


if __name__ == "__main__":
    Day11()()
