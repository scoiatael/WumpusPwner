:- dynamic goVia/1 .
:- dynamic stench/0, breeze/0, glitter/0, bump/0, scream/0.
myPos(','(','(3,1),east)).
visited([','(2,1),','(1,1)]).
oldActions([moveForward,moveForward]).
safeSpots([','(3,1),','(2,2),','(1,1),','(2,1),','(1,2)]).
worldSize(','(7,7)).
gameStarted.
breeze.
