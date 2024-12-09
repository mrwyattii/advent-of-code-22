from dataclasses import dataclass
from typing import List

from common.aoc_day import AoCDay
from tqdm import tqdm


@dataclass
class Block:
    start: int
    end: int
    file_id: int = -1

    @property
    def size(self) -> int:
        return self.end - self.start

    @property
    def is_free(self) -> int:
        return self.file_id == -1


class Disk:
    def __init__(self, blocks: List[Block]) -> None:
        self.files = [b for b in blocks if not b.is_free]
        self.empty = [b for b in blocks if b.is_free]

    def get_first_free(self, size: int, max_start: int) -> Block:
        for block in self.empty:
            if block.start > max_start:
                return None
            if block.size >= size:
                return block
        return None

    def move_file(self, move_from: Block, move_to: Block) -> None:
        assert move_to.is_free
        assert move_to.size >= move_from.size

        self.files.remove(move_from)
        self.empty.remove(move_to)

        self.files += [
            Block(move_to.start, move_to.start + move_from.size, move_from.file_id)
        ]
        self.empty += [
            Block(move_from.start, move_from.end, -1),
            Block(move_to.start + move_from.size, move_to.end, -1),
        ]

        self.files = [b for b in self.files if b.size > 0]
        self.empty = [b for b in self.empty if b.size > 0]

        self.files.sort(key=lambda x: x.start)
        self.empty.sort(key=lambda x: x.start)


class Day09(AoCDay):
    def process_input(self, raw_input: List[str]) -> List[int]:
        return map(int, raw_input[0].strip())

    def calculate_checksum(self, disk: Disk) -> int:
        checksum = 0
        for block in disk.files:
            for i in range(block.start, block.end):
                checksum += i * block.file_id
        return checksum

    def defrag_disk(self, disk: Disk) -> None:
        t = tqdm(total=len(disk.files))
        for i in range(-1, -len(disk.files), -1):
            while True:
                t.update(1)
                move_from = disk.files[i]
                move_to = disk.get_first_free(move_from.size, max_start=move_from.start)
                if move_to is None:
                    break
                disk.move_file(move_from, move_to)
        t.close()

    def part1(self, input: List[int]) -> None:
        block_count = 0
        blocks = []
        for i, size in enumerate(input):
            for j in range(block_count, block_count + size):
                file_id = i // 2 if (i % 2 == 0) else -1
                blocks.append(Block(start=j, end=j + 1, file_id=file_id))
            block_count += size
        disk = Disk(blocks)
        self.defrag_disk(disk)
        print(self.calculate_checksum(disk))

    def part2(self, input: List[int]) -> None:
        block_count = 0
        blocks = []
        for i, size in enumerate(input):
            file_id = i // 2 if (i % 2 == 0) else -1
            blocks.append(
                Block(start=block_count, end=block_count + size, file_id=file_id)
            )
            block_count += size
        disk = Disk(blocks)
        self.defrag_disk(disk)
        print(self.calculate_checksum(disk))


if __name__ == "__main__":
    Day09()()
