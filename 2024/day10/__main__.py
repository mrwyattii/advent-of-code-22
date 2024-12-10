from typing import Dict, List

from common.aoc_day import AoCDay


class Day10(AoCDay):
    def process_input(self, raw_input: List[str]) -> Dict[complex, int]:
        return {
            complex(x, y): int(cell)
            for y, row in enumerate(raw_input)
            for x, cell in enumerate(row.strip())
        }

    def climb(self, topo_map: Dict[complex, int], loc: complex, h=0) -> List[complex]:
        elev = topo_map.get(loc, 0)
        if elev != h:
            return []
        if elev == 9:
            return [loc]
        return sum(
            [self.climb(topo_map, loc + d, elev + 1) for d in [1j, 1, -1j, -1]], []
        )

    def part1(self, input: Dict[complex, int]) -> None:
        print(sum(len(set(self.climb(input, loc))) for loc in input))

    def part2(self, input: Dict[complex, int]) -> None:
        print(sum(len(self.climb(input, loc)) for loc in input))


if __name__ == "__main__":
    Day10(0)()
