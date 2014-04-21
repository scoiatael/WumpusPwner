:- dynamic goVia/1, visited/1, myPos/1, oldActions/1, worldSize/1, gameStarted/0, wumpusIntel/1, worldSize/1, wumpusDead/1.
:- dynamic breeze/0, scream/0, stench/0, glitter/0.
myPos(','(','(3,1),east)).
safeSpots([','(3,1),','(3,3),','(1,5),','(2,4),','(1,4),','(2,3),','(1,3),','(2,2),','(1,2),','(2,1),','(1,1)]).
visited([','(2,1),','(2,2),','(3,3),','(2,3),','(2,4),','(1,5),','(1,4),','(1,3),','(1,2),','(1,1)]).
worldSize(','(7,7)).
wumpusIntel([stenchOn(','(2,2)),stenchOn(','(3,3)),breezeOn(','(2,4)),breezeOn(','(1,5))]).
gameStarted.
stench.
