from typing import List

from common.aoc_day import AoCDay


class Day02(AoCDay):
    def process_input(self, raw_input: List[str]) -> List[List[int]]:
        return list(map(lambda x: list(map(int, x.split())), raw_input))

    @staticmethod
    def is_safe(report: List[int]) -> bool:
        def get_sign(val: int) -> str:
            return "+" if val >= 0 else "-"

        sign = get_sign(report[1] - report[0])

        for i in range(0, len(report) - 1):
            diff = report[i + 1] - report[i]
            if not ((get_sign(diff) == sign) and (1 <= abs(diff) <= 3)):
                return False

        return True

    def part1(self, reports: List[List[int]]) -> int:
        return sum(map(self.is_safe, reports))

    def part2(self, reports: List[List[int]]) -> int:
        safe_count = 0
        for report in reports:
            if any(
                self.is_safe(report[:i] + report[i + 1 :]) for i in range(len(report))
            ):
                safe_count += 1
        return safe_count


if __name__ == "__main__":
    Day02()()
