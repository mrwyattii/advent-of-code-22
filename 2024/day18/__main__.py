from itertools import product
from typing import List

import networkx as nx
from common.aoc_day import AoCDay


class Day18(AoCDay):
    def process_input(self, raw_input: List[str]) -> List[complex]:
        byte_locs = []
        for line in raw_input:
            x, y = line.strip().split(",")
            byte_locs.append(complex(int(x), int(y)))
        return byte_locs

    def part1(self, byte_locs: List[complex]) -> int:
        grid_size, truncate_size = 7, 12
        if grid_size < max(byte_locs, key=abs).real:
            grid_size, truncate_size = 71, 1024

        G = nx.Graph()
        for x, y in product(range(grid_size), repeat=2):
            loc = complex(x, y)
            for d in [1, -1, 1j, -1j]:
                G.add_edge(loc, loc + d)

        for loc in byte_locs[:truncate_size]:
            G.remove_node(loc)

        return nx.shortest_path_length(
            G, complex(0, 0), complex(grid_size - 1, grid_size - 1)
        )

    def part2(self, byte_locs: List[complex]) -> str:
        grid_size, truncate_size = 7, 12
        if grid_size < max(byte_locs, key=abs).real:
            grid_size, truncate_size = 71, 1024
        G = nx.Graph()

        for x, y in product(range(grid_size), repeat=2):
            loc = complex(x, y)
            for d in [1, -1, 1j, -1j]:
                G.add_edge(loc, loc + d)

        for i, loc in enumerate(byte_locs):
            G.remove_node(loc)
            if i > truncate_size and not nx.has_path(
                G, complex(0, 0), complex(grid_size - 1, grid_size - 1)
            ):
                break

        return f"{int(loc.real)},{int(loc.imag)}"


if __name__ == "__main__":
    Day18()()
