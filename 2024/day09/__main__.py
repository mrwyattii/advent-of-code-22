import sys
from heapq import heappop, heappush
from itertools import groupby
from typing import List, Tuple

from common.aoc_day import AoCDay
from tqdm import tqdm


class NoSpaceError(Exception):
    pass


class Disk:
    def __init__(self, blocks: List[int]) -> None:
        self.blocks = blocks

        block_groups = [
            (file_id, [idx for idx, _ in blocks])
            for file_id, blocks in groupby(enumerate(self.blocks), key=lambda x: x[1])
        ]
        self.free_space = {i: [] for i in range(10)}
        self.file_list = []
        for file_id, blocks in block_groups:
            if file_id == -1:
                heappush(self.free_space[len(blocks)], blocks[0])
            else:
                self.file_list.append(blocks)

    def get_leftest_free_block(self, min_size: int) -> Tuple[int, int]:
        free_sizes = {
            i: heappop(self.free_space[i])
            for i in range(min_size, 10)
            if self.free_space[i]
        }
        if not free_sizes:
            raise NoSpaceError("No free space available")
        min_key = min(free_sizes, key=free_sizes.get)
        for k, v in free_sizes.items():
            heappush(self.free_space[k], v)
        return min_key, heappop(self.free_space[min_key])

    def get_free_blocks(
        self, min_size: int, total_size: int, max_idx: int
    ) -> List[int]:
        try:
            free_space_size, free_space_idx = self.get_leftest_free_block(min_size)
        except NoSpaceError:
            return []

        if free_space_idx > max_idx:
            heappush(self.free_space[free_space_size], free_space_idx)
            return []

        take_size = min(free_space_size, total_size)
        heappush(
            self.free_space[free_space_size - take_size], free_space_idx + take_size
        )

        free_space_idx_list = [
            x for x in range(free_space_idx, free_space_idx + take_size)
        ]
        if take_size == total_size:
            return free_space_idx_list
        else:
            return free_space_idx_list + self.get_free_blocks(
                min_size, total_size - take_size, max_idx
            )

    def move_file(self, move_from: List[int], move_to: List[int]) -> None:
        for f, t in zip(reversed(move_from), move_to):
            self.blocks[t] = self.blocks[f]
            self.blocks[f] = -1

    def defrag(self, chunk_size: int = sys.maxsize) -> None:
        file_list = self.file_list
        t = tqdm(total=len(file_list), desc="Moving files")
        for file_block in reversed(file_list):
            start_idx = file_block[0]
            if self.blocks.index(-1) > start_idx:
                t.update(t.total - t.n)
                break
            move_to = self.get_free_blocks(
                min_size=min(len(file_block), chunk_size),
                total_size=len(file_block),
                max_idx=start_idx,
            )
            self.move_file(file_block, move_to)
            t.update(1)
        t.close()


class Day09(AoCDay):
    def process_input(self, raw_input: List[str]) -> Disk:
        blocks = []
        for i, size in enumerate(raw_input[0].strip()):
            blocks.extend([i // 2 if i % 2 == 0 else -1] * int(size))
        return Disk(blocks)

    def calculate_checksum(self, disk: Disk) -> int:
        return sum([i * block for i, block in enumerate(disk.blocks) if block != -1])

    def part1(self, disk: Disk) -> int:
        disk.defrag(chunk_size=1)
        return self.calculate_checksum(disk)

    def part2(self, disk: Disk) -> int:
        disk.defrag()
        return self.calculate_checksum(disk)


if __name__ == "__main__":
    Day09()()
