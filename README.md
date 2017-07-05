## scalacheck-example

The problem is to simulate mowers moving (and presumably mowing also) on a rectangular surface, say a garden/park. The surface is divided in smaller squares. Each square can hold only one mower. Each square has coordinates (x, y). A mower has an initial position, thus it occupies one square and has an initial bearing/position (it can point to north, south, east or west). A mower can make only three types of actions: rotate 90° to the left, rotate 90° to the right or advance one square. A mower can advance in the direction of its bearing, but it can not move past obstacles (other mowers), nor can it exit the boundaries of the surface. If a mower is blocked than an advance action is ignored, the mower stays in the same square. The rules are relatively simple.

The program is fed an input file. The input file starts with a line, giving the dimensions of the surface. Then, for each mower there are two lines, one giving the initial position and orientation of the mower, and the second giving a series of actions. The codes for actions are G (gauche - left in french) for rotating left, D (droit - right in french) for rotating right, A for advancing one square. A sample input file is given in the project folder.

Executing the program, runs the simulation and it prints (on a separate line) the position of each mower.

This somewhat artificial and contrived problem, has just enough rules to showcase how can they be tested with ScalaCheck.
