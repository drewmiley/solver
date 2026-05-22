# solver

A small routine that solves Countdown number rounds

# To run

1. In a terminal, run `sbt`
2. Then run `run` for random round generation
3. Solutions will then be outputted in terminal

# Options

* To run with specific target number, run `run target TARGET_NUMBER` where TARGET_NUMBER is as it suggests
* To run with specific picked numbers run `run picked A,B,C,D,E` where A,B,C,D,E are picked numbers separated by a ',' only
* `smallRandom` and `largeRandom` are ignored when `picked` is defined
* To run with specific number of `largeRandom`, run `run largeRandom LARGE_RANDOM` where LARGE_RANDOM is number of large random to pick.
* `largeRandom` defaults to 1 when it is not set
* To run with specific number of `smallRandom`, run `run smallRandom SMALL_RANDOM` where SMALL_RANDOM is number of small random to pick.
* `smallRandom` defaults to 6 - largeRandom when it is not set
* To leave in duplicate solutions, run `run filterDuplicate false`.
* `filterDuplicate` defaults to true when not specified
* To specify multiple options, just define them all after `run` in an additive way