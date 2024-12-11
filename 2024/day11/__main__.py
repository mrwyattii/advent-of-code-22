from collections import defaultdict
from typing import DefaultDict, List

from common.aoc_day import AoCDay
from tqdm import trange


class Day11(AoCDay):
    def process_input(self, raw_input: List[str]) -> DefaultDict[int, int]:
        stone_dict = defaultdict(int)
        for s in raw_input[0].strip().split(" "):
            stone_dict[int(s)] += 1
        return stone_dict

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

    def part1(self, input: DefaultDict[int, int]) -> None:
        for _ in trange(25):
            input = self.blink(input)
        print(sum(input.values()))

    def part2(self, input: DefaultDict[int, int]) -> None:
        for _ in trange(75):
            input = self.blink(input)
        print(sum(input.values()))


if __name__ == "__main__":
    Day11(0)()
