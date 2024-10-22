This is a solver for the [Cilada](https://www.estrela.com.br/jogo-cilada-estrela/p) puzzle game which consist in placing
on a board a predefined set of pieces of two shape (`I` shaped `2x1` pieces and `L` shaped `2X2` pieces) matching the
shape of the holes of piece with that of the corresponding square on the board.

The available pieces are labeled by a letter as follows:

| Name | Pattern        |
|------|----------------|
| `A`  | `◯ ✚`          |
| `B`  | `▢ ✚`          |
| `C`  | `▢ ◯`          |
| `D`  | `✚ ✚`          |
| `E`  | `▢ ▢`          |
| `F`  | `◯ ◯`          |
| `G`  | `▢ ◯` <br> `▢` |
| `H`  | `✚ ◯` <br> `▢` |
| `I`  | `◯ ▢` <br> `✚` |
| `J`  | `✚ ✚` <br> `▢` |
| `K`  | `◯ ◯` <br> `▢` |
| `L`  | `◯ ▢` <br> `◯` |
| `M`  | `✚ ▢` <br> `◯` |
| `N`  | `▢ ✚` <br> `◯` |

Each variant consists in a subset of these pieces which can be assembled to cover the board. For example, the variant 1
is represented in the code as
`AABCDDEFGIKN`
and means:

- 2 `A` pieces
- 1 `B` piece
- 1 `C` piece
- 2 `D` pieces
- 1 `E` piece
- 1 `F` piece
- 1 `G` piece
- 1 `I` piece
- 1 `K` piece
- 1 `N` piece

# Quick start

Just run:

```bash
cargo run
```

To output the number of solution for each of the 50 game setup, run:

```shell
cargo run -- --mode count
```

To output each solution, run:

```shell
cargo run -- --mode explicit
```

To restrict the output to a given game setup, run:

```shell
cargo run -- --variant <variant number from 1 to 50>
```

# Algorithm

The code operates as follows:

- For each square on the board, determine which pieces are fit to cover that square
- Recursively:
    - Pick a square with the smallest number of covering pieces.
    - Add it to a set of placed pieces
    - Eliminate any piece covering a neighboring square that would overlap with the placed piece
    - If the pieces of this type are exhausted, eliminate pieces of this type from the list of pieces covering other
      squares
    - Repeat the procedure.

The code thus constructs all the combinations of placed pieces on the board that constitute each solution.
