:- dynamic goVia/1, visited/1, myPos/1, oldActions/1, worldSize/1, gameStarted/0.
:- dynamic stench/0, breeze/0, glitter/0, bump/0, scream/0.
myPos(','(','(2,1),east)).
safeSpots([','(2,1),','(1,2),','(2,1),','(1,2)]).
visited([','(1,1)]).
worldSize(','(7,7)).
oldActions([]).
gameStarted.
