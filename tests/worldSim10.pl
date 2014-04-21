:- dynamic goVia/1, visited/1, myPos/1, oldActions/1, worldSize/1, gameStarted/0, wumpusIntel/1, worldSize/1, wumpusDead/1.
:- dynamic stench/0, breeze/0, glitter/0, bump/0, scream/0.
myPos(','(','(2,1),east)).
safeSpots([','(3,2),','(1,3),','(2,2),','(1,2),','(2,1),','(1,1)]).
visited([','(3,2),','(3,1),','(2,1),','(2,2),','(1,3),','(1,2),','(1,1)]).
worldSize(','(7,7)).
wumpusIntel([stenchOn(','(2,2)),stenchOn(','(1,3))]).
gameStarted.


