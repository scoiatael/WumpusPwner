:- dynamic goVia/1, visited/1, myPos/1, oldActions/1, worldSize/1, gameStarted/0.
:- dynamic stench/0, breeze/0, glitter/0, bump/0, scream/0.
safeSpots([','(3,5),','(4,4),','(7,1),','(7,5),','(7,4),','(6,4),','(7,3),','(6,3),','(7,2),','(5,3),','(6,2),','(5,2),','(6,1),','(5,1),','(4,1),','(3,1),','(4,2),','(3,2),','(3,4),','(4,3),','(3,3),','(1,5),','(2,4),','(1,4),','(2,3),','(1,3),','(2,2),','(1,2),','(2,1),','(1,1)]).
worldSize(','(7,7)).
myPos(','(','(1,1),west)).
wumpusIntel([breezeOn(','(4,4)),breezeOn(','(3,5)),stenchOn(','(7,5)),breezeOn(','(6,4)),breezeOn(','(5,3)),breezeOn(','(2,4)),breezeOn(','(1,5))]).
visited([]).
gameStarted.


