from typing import Dict, List, Optional, Set

from common.aoc_day import AoCDay


class Region:
    def __init__(self, locs: Set[complex]):
        self.locs = locs

    @property
    def area(self):
        return len(self.locs)

    @property
    def perimeter(self):
        p = 0
        for loc in self.locs:
            p += sum([loc + d not in self.locs for d in [1, -1, 1j, -1j]])
        return p

    def count_corners(self, loc):
        count = 0
        corners = [1 + 1j, 1 - 1j, -1 + 1j, -1 - 1j]
        for c in corners:
            vert, diag, horz = loc + complex(0, c.imag), loc + c, loc + c.real
            if (vert in self.locs) and (diag not in self.locs) and (horz in self.locs):
                # inside corner
                count += 1
            if (vert not in self.locs) and (horz not in self.locs):
                # outside corner
                count += 1
        return count

    @property
    def num_sides(self):
        return sum([self.count_corners(loc) for loc in self.locs])

    @staticmethod
    def from_garden(garden: Dict[complex, str], loc: Optional[complex] = None):
        if loc is None:
            loc = list(garden.keys())[0]
        if loc not in garden:
            raise ValueError("Invalid garden location!")

        def dfs(loc):
            if loc in region or garden.get(loc, None) != val:
                return
            region.add(loc)
            for d in [1, -1, 1j, -1j]:
                dfs(loc + d)

        region = set()
        val = garden[loc]
        dfs(loc)
        return Region(region)


class Day12(AoCDay):
    def process_input(self, raw_input: List[str]) -> Dict[complex, str]:
        garden = {}
        for y, row in enumerate(raw_input):
            for x, cell in enumerate(row.strip()):
                garden[complex(x, y)] = cell
        return garden

    def part1(self, garden: Dict[complex, str]) -> int:
        fence_price = 0
        while garden:
            region = Region.from_garden(garden)
            fence_price += region.area * region.perimeter
            for loc in region.locs:
                _ = garden.pop(loc)

        return fence_price

    def part2(self, garden: Dict[complex, str]) -> int:
        fence_price = 0
        while garden:
            region = Region.from_garden(garden)
            area, sides = region.area, region.num_sides
            fence_price += area * sides
            for loc in region.locs:
                _ = garden.pop(loc)

        return fence_price


if __name__ == "__main__":
    Day12()()
