:- dynamic goVia/1, visited/1, myPos/1, oldActions/1, worldSize/1, gameStarted/0.
:- dynamic stench/0, breeze/0, glitter/0, bump/0, scream/0.
myPos(','(','(1,1),east)).
wumpusIntel([stenchOn(','(2,1)),stenchOn(','(2,1)),stenchOn(','(2,1)),stenchOn(','(2,1)),breezeOn(','(6,6)),breezeOn(','(5,7)),breezeOn(','(7,3)),breezeOn(','(6,4)),breezeOn(','(5,5)),breezeOn(','(6,2)),breezeOn(','(7,1)),stenchOn(','(4,1)),stenchOn(','(3,2))]).
visited([','(2,1),','(2,2),','(2,3),','(2,4),','(2,5),','(2,6),','(4,7),','(6,6),','(5,7),','(5,6),','(4,6),','(4,5),','(4,4),','(7,3),','(6,3),','(6,4),','(5,5),','(5,4),','(5,3),','(6,2),','(7,1),','(6,1),','(5,1),','(5,2),','(4,1),','(4,2),','(4,3),','(3,2),','(3,3),','(3,4),','(3,5),','(3,6),','(3,7),','(2,7),','(1,7),','(1,6),','(1,5),','(1,4),','(1,3),','(1,2),','(1,1)]).
worldSize(','(7,7)).
safeSpots([','(1,1),','(1,1),','(5,7),','(6,6),','(5,6),','(7,3),','(5,5),','(6,4),','(5,4),','(6,3),','(7,1),','(6,1),','(5,1),','(6,2),','(4,1),','(5,2),','(4,2),','(5,3),','(3,2),','(4,3),','(3,3),','(4,4),','(3,4),','(4,5),','(3,5),','(4,6),','(3,6),','(4,7),','(3,7),','(2,7),','(1,7),','(2,6),','(1,6),','(2,5),','(1,5),','(2,4),','(1,4),','(2,3),','(1,3),','(2,2),','(1,2),','(2,1),','(1,1)]).
gameStarted.


