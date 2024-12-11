from collections import defaultdict
from itertools import combinations
from typing import Dict, List, Tuple

from common.aoc_day import AoCDay


class Day05(AoCDay):
    def process_input(
        self, raw_input: List[str]
    ) -> Tuple[Dict[int, List[int]], List[int]]:
        rules = defaultdict(list)
        updates = []
        for line in raw_input:
            if "|" in line:
                k, v = line.strip().split("|")
                rules[int(k)].append(int(v))
            elif "," in line:
                updates.append(list(map(int, line.strip().split(","))))

        return rules, updates

    @staticmethod
    def is_valid(rules: Dict[int, List[int]], update: List[int]) -> bool:
        for v1, v2 in combinations(update, 2):
            if v1 in rules[v2]:
                return False
        return True

    @staticmethod
    def get_middle_value(update: List[int]) -> int:
        return update[(len(update) - 1) // 2]

    def part1(self, puzzle_input: Tuple[Dict[int, List[int]], List[int]]) -> int:
        rules, updates = puzzle_input
        valid_updates = filter(lambda u: self.is_valid(rules, u), updates)
        return sum(map(self.get_middle_value, valid_updates))

    @staticmethod
    def fix_update(rules: Dict[int, List[int]], update: List[int]) -> List[int]:
        rule_count = {v: sum(map(lambda u: v in rules[u], update)) for v in update}
        return sorted(rule_count, key=rule_count.get)

    def part2(self, puzzle_input: Tuple[Dict[int, List[int]], List[int]]) -> int:
        rules, updates = puzzle_input
        invalid_updates = filter(lambda u: not self.is_valid(rules, u), updates)
        fixed_updates = map(lambda u: self.fix_update(rules, u), invalid_updates)
        return sum(map(self.get_middle_value, fixed_updates))


if __name__ == "__main__":
    Day05()()
