https://adventofcode.com/2022/day/12

  $ export AOC_DAY=12

Part 1: Find the shortest path from start to end on the given topographic map

  $ aoc2022 1 < test0
  2

  $ aoc2022 1 < test1
  4

  $ aoc2022 1 < test
  31

  $ aoc2022 1 < input
  370

Performance notes: A* with linear best fscore selection
❯ hyperfine "_build/install/default/bin/aoc2022 1 < test/day12.t/input"
Benchmark 1: _build/install/default/bin/aoc2022 1 < test/day12.t/input
Time (mean ± σ):      15.3 ms ±   0.4 ms    [User: 13.5 ms, System: 1.2 ms]
Range (min … max):    14.6 ms …  16.8 ms    173 runs

Performance notes: A* with Pairing Heap best fscore selection
❯ hyperfine "_build/install/default/bin/aoc2022 1 < test/day12.t/input"
Benchmark 1: _build/install/default/bin/aoc2022 1 < test/day12.t/input
Time (mean ± σ):       7.5 ms ±   0.4 ms    [User: 5.7 ms, System: 1.2 ms]
Range (min … max):     6.9 ms …   8.7 ms    289 runs

Attempt at DFS with hill climbing heuristics:

  $ aoc2022 10 < test0
  summary: backtracks=1 prunes=0 goals=2 mingoallen=3
  2

  $ aoc2022 10 < test1
  summary: backtracks=3 prunes=0 goals=1 mingoallen=5
  4

  $ aoc2022 10 < test
  summary: backtracks=1280 prunes=23 goals=7 mingoallen=32
  31


Part 2: Find the shortest path from *any* height=0 start to end on the given topographic map

  $ aoc2022 2 < test
  29

  $ aoc2022 2 < input
  363

Performance notes: A* with linear best fscore selection
❯ hyperfine "_build/install/default/bin/aoc2022 2 < test/day12.t/input"
Benchmark 1: _build/install/default/bin/aoc2022 2 < test/day12.t/input
Time (mean ± σ):      4.552 s ±  0.020 s    [User: 4.536 s, System: 0.014 s]
Range (min … max):    4.528 s …  4.588 s    10 runs

Performance notes: A* with Pairing Heap best fscore selection
❯ hyperfine "_build/install/default/bin/aoc2022 2 < test/day12.t/input"
Benchmark 1: _build/install/default/bin/aoc2022 2 < test/day12.t/input
Time (mean ± σ):      1.533 s ±  0.008 s    [User: 1.525 s, System: 0.006 s]
Range (min … max):    1.519 s …  1.547 s    10 runs
