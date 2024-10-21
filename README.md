This is a solver for the [Cilada](https://www.estrela.com.br/jogo-cilada-estrela/) puzzle game which consist in placing
on a board a predefined set of pieces of two shape (`I` shaped `2x1` pieces and `L` shaped `2X2` pieces) matching the
shape of the holes of piece with that of the corresponding square on the board.

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
