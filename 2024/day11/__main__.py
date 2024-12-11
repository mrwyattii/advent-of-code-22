from collections import defaultdict
from typing import DefaultDict, List, Tuple

import seaborn as sns
from common.aoc_day import AoCDay
from matplotlib import pyplot as plt
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

    def visualize(self, stone_history: List[Tuple[int, int]]) -> None:
        _, ax = plt.subplots(figsize=(10, 6))
        unique_stones, stone_count = zip(*stone_history)
        sns.lineplot(
            x=range(len(stone_history)),
            y=unique_stones,
            ax=ax,
            label="Unique Stones",
            color="g",
        )
        sns.lineplot(
            x=range(len(stone_history)),
            y=stone_count,
            ax=ax,
            label="Total Stones",
            color="b",
        )
        ax.set_yscale("log")
        ax.set_ylim(1, None)
        plt.savefig("day11/day11.png")

    def part1(self, input: DefaultDict[int, int]) -> None:
        for _ in trange(25):
            input = self.blink(input)
        print(sum(input.values()))

    def part2(self, input: DefaultDict[int, int]) -> None:
        stone_history = [(len(input), sum(input.values()))]
        for _ in trange(75):
            input = self.blink(input)
            stone_history.append((len(input), sum(input.values())))
        self.visualize(stone_history)
        print(sum(input.values()))


if __name__ == "__main__":
    Day11(0)()
