https://adventofcode.com/2022/day/6

  $ export AOC_DAY=6

Part 1: TBD

  $ aoc2022 1 < test
  7

Original set based deduplication
❯ hyperfine --warmup 10 "_build/install/default/bin/aoc2022 1 < test/day6.t/input"
Benchmark 1: _build/install/default/bin/aoc2022 1 < test/day6.t/input
Time (mean ± σ):      12.0 ms ±   0.5 ms    [User: 10.3 ms, System: 1.2 ms]
Range (min … max):    11.4 ms …  14.1 ms    209 runs

  $ aoc2022 1 < input
  1140

Part 2: TBD

  $ aoc2022 2 < test
  19

Original set based deduplication
❯ hyperfine --warmup 10 "_build/install/default/bin/aoc2022 2 < test/day6.t/input"
Benchmark 1: _build/install/default/bin/aoc2022 2 < test/day6.t/input
Time (mean ± σ):      14.9 ms ±   0.5 ms    [User: 13.1 ms, System: 1.3 ms]
Range (min … max):    14.2 ms …  16.6 ms    167 runs

  $ aoc2022 2 < input
  3495
