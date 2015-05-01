# npuzzles

A cool program for solving sliding puzzles.

## Installation

Clone the GitHub repo from https://github.com/ezig/npuzzles or
download the source as a zip.

## Usage

A compiled java executable version of the project is in the src/uberjar folder
To run the file from that directory, run 

    $ java -jar npuzzles-1.0-standalone.jar [args]

If you want to mess around with the code, you can also download and install Clojure
through Leiningen (from http://leiningen.org/) and launch an interactive REPL.

## Options and Example

Calling the program with -h or --help will bring up a help dialog with a brief
description of flags
There are two basic modes: puzzle solving mode and data collection mode. If no
-d or --data flag is provided, then the program will default to puzzle solving mode.
If the -d flag is passed, the program will run in data collection mode

In both modes, the -a or --alg flag is REQUIRED and must be either 
-a genetic or -a astar (spelled exactly) to specify the solving algorithm

In puzzle solving mode:

Regardless of which algorithm you are running, the additional arguments needed
are two integers, row and column (integers), and a string of integers representing the puzzle
from left to right top to bottom order. So, for example, to solve the following puzzle using a genetic algorithm

123
456
708

We would run

	$ java -jar npuzzles-1.0-standalone.jar -a genetic 3 3 123456708 

And we could solve this puzzle by astar

312
504

	$ java -jar npuzzles-1.0-standalone.jar -a astar 2 3 312504

In data collection mode (triggered by -d flag):

For the A* algorithm:

The additional arguments needed are the number of trials to run, the row and 
column dimensions (all integers) and the heuristic function to use.

For example, if we wanted to run 100 trials of 3 by 3 puzzles using manhattan-distance,
we would run

	$ java -jar npuzzles-1.0-standalone.jar -d -a astar 100 3 3 manhattan-distance

For the Genetic algorithm:

The additional arguments needed are the number of trials to run, the row and 
column dimensions, the initial population size, the number of phases to run,
and the number of generatoins per phase (all integers) and the heuristic function to use.

For example, if we wanted to run 100 trials of 3 by 3 puzzles using an initial
population of 200, 5 phases, 500 generations per phase, and a linear-conflict heuristic,
we could run

	$ java -jar npuzzles-1.0-standalone.jar -d -a genetic 100 3 3 200 5 500 linear-conflict

## Currently Available Heuristic Functions
manhattan-distance
tiles-out-of
linear-conflict
misplaced-tiles

## License

Copyright © 2015 Ezra Zigmond, Eric Chan, Liam Mulshine, Luca Schroeder

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
