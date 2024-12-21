import sys
from typing import List, Set, Tuple

import networkx as nx
from common.aoc_day import AoCDay
from tqdm import tqdm


class Day16(AoCDay):
    def process_input(
        self, raw_input: List[str]
    ) -> Tuple[Set[complex], complex, complex]:
        locs = set()
        for y, line in enumerate(raw_input):
            for x, char in enumerate(line.strip()):
                if char != "#":
                    locs.add(x + y * 1j)
                    if char == "S":
                        start = x + y * 1j
                    if char == "E":
                        end = x + y * 1j

        G = nx.Graph()
        for loc in locs:
            for direction in [1, -1, 1j, -1j]:
                G.add_node((loc, direction))

        for loc, direction in G.nodes:
            for next_direction in [1, -1, 1j, -1j]:
                new_loc = loc + next_direction
                if new_loc in locs:
                    G.add_edge(
                        (loc, direction),
                        (new_loc, next_direction),
                        weight=1 if direction == next_direction else 1001,
                    )

        return G, start, end

    def part1(self, puzzle_input: Tuple[Set[complex], complex, complex]) -> int:
        G, start, end = puzzle_input
        shortest_path = sys.maxsize
        for direction in tqdm([1, -1, 1j, -1j]):
            shortest_path = min(
                shortest_path,
                nx.shortest_path_length(
                    G, source=(start, 1), target=(end, direction), weight="weight"
                ),
            )
        return shortest_path

    def part2(self, puzzle_input) -> int:
        G, start, end = puzzle_input
        paths = []
        for direction in tqdm([1, -1, 1j, -1j]):
            paths.append(
                list(
                    nx.all_shortest_paths(
                        G, source=(start, 1), target=(end, direction), weight="weight"
                    )
                )
            )
        path_weights = [
            (nx.path_weight(G, p, weight="weight"), p)
            for p_list in paths
            for p in p_list
        ]
        min_weight = min(path_weights, key=lambda x: x[0])[0]
        min_path_locs = set()
        for p_weight, path in path_weights:
            if p_weight == min_weight:
                for loc, _ in path:
                    min_path_locs.add(loc)
        return len(min_path_locs)


if __name__ == "__main__":
    Day16()()
