from typing import Dict, List

from common.aoc_day import AoCDay


class Day10(AoCDay):
    def process_input(self, raw_input: List[str]) -> Dict[complex, int]:
        return {
            complex(x, y): int(cell)
            for y, row in enumerate(raw_input)
            for x, cell in enumerate(row.strip())
        }

    def climb(self, topo_map: Dict[complex, int], loc: complex) -> List[complex]:
        elevation = topo_map[loc]

        if elevation == 9:
            return [loc]

        peaks = []
        for d in [0 + 1j, 1 + 0j, 0 - 1j, -1 + 0j]:
            next_loc = loc + d
            if topo_map.get(next_loc, 0) - elevation == 1:
                peaks += self.climb(topo_map, next_loc)
        return peaks

    def part1(self, input: Dict[complex, int]) -> None:
        trail_score = 0
        for trail_head in [loc for loc, elev in input.items() if elev == 0]:
            trail_score += len(set(self.climb(input, trail_head)))
        print(trail_score)

    def part2(self, input: Dict[complex, int]) -> None:
        trail_score = 0
        for trail_head in [loc for loc, elev in input.items() if elev == 0]:
            trail_score += len(self.climb(input, trail_head))
        print(trail_score)


if __name__ == "__main__":
    Day10(0)()
