:- dynamic goVia/1, visited/1, myPos/1, oldActions/1, worldSize/1, gameStarted/0.
:- dynamic stench/0, breeze/0, glitter/0, bump/0, scream/0.
myPos(','(','(4,1),west)).
safeSpots([','(7,2),','(7,1),','(6,1),','(5,1),','(5,3),','(6,2),','(4,3),','(5,2),','(3,3),','(4,2),','(3,2),','(4,1),','(3,1),','(1,3),','(2,2),','(1,2),','(2,1),','(1,1)]).
visited([','(5,3),','(6,2),','(7,2),','(7,1),','(6,1),','(5,1),','(5,2),','(4,3),','(4,2),','(3,3),','(3,2),','(3,1),','(2,1),','(2,2),','(1,3),','(1,2),','(1,1)]).
worldSize(','(7,7)).
wumpusIntel([breezeOn(','(5,3)),breezeOn(','(7,2)),breezeOn(','(4,3)),breezeOn(','(3,3)),stenchOn(','(2,2)),stenchOn(','(1,3))]).
gameStarted.



