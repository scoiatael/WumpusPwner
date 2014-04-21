:- dynamic goVia/1, visited/1, myPos/1, oldActions/1, worldSize/1, gameStarted/0.
:- dynamic stench/0, breeze/0, glitter/0, bump/0, scream/0.
myPos(','(','(1,1),south)).
safeSpots([(1,1),(1,2),(1,4),
            (2,1), (2,2), (2,3), (2,4),
              (3,1), (3,2), (3,3), (3,4),
                (4,1), (4,2), (4,3), (4,4)
]).
wumpusIntel([stenchOn((1,2)), stenchOn((1,4))]).
worldSize(','(7,7)).
visited([(1,1)]).

