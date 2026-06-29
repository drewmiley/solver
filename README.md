# solver

A small routine that solves Countdown number rounds

# To run

1. In a terminal, run `sbt`
2. Then run `run` for random round generation
3. Solutions will then be outputted in terminal

# Options

* To specify multiple options, just define them all after `run` in an additive way (applicable for both modes)

* To run with specific picked numbers run `run picked A,B,C,D,E` where A,B,C,D,E are picked numbers separated by a ',' only
* `smallRandom` and `largeRandom` are ignored when `picked` is defined
* To run with specific number of `largeRandom`, run `run largeRandom LARGE_RANDOM` where LARGE_RANDOM is number of large random to pick.
* `largeRandom` defaults to 1 when it is not set
* To run with specific number of `smallRandom`, run `run smallRandom SMALL_RANDOM` where SMALL_RANDOM is number of small random to pick.
* `smallRandom` defaults to 6 - largeRandom when it is not set

## SOLUTIONS MODE (Default) - to discover how many solutions there are for a specific number

* To run with specific target number, run `run target TARGET_NUMBER` where TARGET_NUMBER is as it suggests
* To leave in duplicate solutions, run `run filterDuplicate false`.
* `filterDuplicate` defaults to true when not specified
* To not display solutions, run `run displaySolution false`.
* `displaySolution` defaults to true when not specified

## NO SOLUTIONS MODE - to discover what numbers can't be solved for a set of input numbers

* To run in no solutions mode, run `run noSolutions true`
* `noSolutions` defaults to false when not specified (and will run in solutions mode by default)
* To run with specific target min, run `run targetMin TARGET_MIN` where TARGET_MIN is as it suggests
* To run with specific target max, run `run targetMin TARGET_MAX` where TARGET_MAX is as it suggests

## DEMO MODE - for demoing

* Run via IDE, as only really makes sense with breakpoints. For IDE, add as env variable in `sbt task`.
* If you do insist, run `export DEMO=true` in terminal window
* Run `sbt`
* Run `run`

## EXPERIMENT MODE - for experimenting

* Can run via IDE or in the terminal window. For IDE, add as env variable in `sbt task`.
* Run `export EXPERIMENT=true` in terminal window
* Run `sbt`
* Run `run`
