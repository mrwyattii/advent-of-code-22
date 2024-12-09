import sys
from itertools import groupby
from typing import List

from common.aoc_day import AoCDay
from tqdm import tqdm


class Disk:
    def __init__(self, blocks: List[int]) -> None:
        self.blocks = blocks

    def get_free_blocks(self, size: int, max_idx: int) -> List[int]:
        idx = self.blocks.index(-1)
        free_blocks = []
        if max_idx < idx:
            return free_blocks

        for i in range(idx, max_idx):
            if self.blocks[i] == -1:
                free_blocks.append(i)
                if len(free_blocks) == size:
                    break
            else:
                free_blocks = []
        return free_blocks

    def move_file(self, move_from: List[int], move_to: List[int]) -> None:
        if len(move_from) != len(move_to):
            return
        for f, t in zip(move_from, move_to):
            self.blocks[t] = self.blocks[f]
            self.blocks[f] = -1

    def defrag(self, chunk_size: int = sys.maxsize) -> None:
        def chunk_file_block(block: List[int]) -> List[List[int]]:
            return [block[i : i + chunk_size] for i in range(0, len(block), chunk_size)]

        file_list = [
            [idx for idx, _ in blocks]
            for file_id, blocks in groupby(enumerate(self.blocks), key=lambda x: x[1])
            if file_id != -1
        ]
        file_list = [
            block for file_block in file_list for block in chunk_file_block(file_block)
        ]
        t = tqdm(total=len(file_list), desc="Moving files")
        for file_block in reversed(file_list):
            start_idx = file_block[0]
            if self.blocks.index(-1) > start_idx:
                t.update(t.total - t.n)
                break
            move_to = self.get_free_blocks(len(file_block), max_idx=start_idx)
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

    def part1(self, input: Disk) -> None:
        input.defrag(chunk_size=1)
        print(self.calculate_checksum(input))

    def part2(self, input: List[int]) -> None:
        input.defrag()
        print(self.calculate_checksum(input))


if __name__ == "__main__":
    Day09()()
