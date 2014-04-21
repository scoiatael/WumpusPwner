% vim: syntax=prolog:
act(A, K) :- 
 \+ gameStarted,
 assert(gameStarted),
 assert(myPos(((1,1), east))),
 assert(wumpusIntel([])),
 (worldSize(X,Y) ; worldSize((X,Y))),
 assert(worldSize((X,Y))),
 assert(visited([])),
 assert(safeSpots([(1,1)])),
 act(A, K).

act(Action, K) :-
  gameStarted, 
  saveKnowledge(OldKnowledge),
  updateVisited(OldKnowledge, Kk),
  processPercepts(Kk, Ke),
  updateIfSafe(Ke, Knowledge),
  doAction(Action, Knowledge, Kp),
  updateMyPos(Action, Kp, K).

updateVisited(Ok, K) :-
  exists(myPos((V,_)), Ok),
  exists(visited(Vs), Ok),
  addIfNotMember(V, Vs, Vsp),
  my_retract(visited(_), Ok, Kp),
  my_assert(visited(Vsp), Kp, K).

addIfNotMember(A, B, C) :-
  member(A,B), !,
  C = B.

addIfNotMember(A,B,C) :-
  C = [A | B].

updateMyPos(turnLeft, Ok, K):-
  exists(myPos((V, D)), Ok),
  my_retract(myPos(_), Ok, Kp),
  turnDirLeft(D, Dp),
  my_assert(myPos((V,Dp)), Kp, K).

updateMyPos(turnRight, Ok, K):-
  exists(myPos((V, D)), Ok),
  my_retract(myPos(_), Ok, Kp),
  turnDirRight(D, Dp),
  my_assert(myPos((V,Dp)), Kp, K).

updateMyPos(moveForward, Ok, K):-
  exists(myPos((V, D)), Ok),
  my_retract(myPos(_), Ok, Kp),
  forwardVecPos(Ok, Vp),
  addVec(V, Vp, Vpp),
  my_assert(myPos((Vpp,D)), Kp, K).

updateMyPos(_, K, K).

forwardVecPos(Ok, V) :-
  exists(myPos((_, D)), Ok),
  forwardVec(D, V).

forwardVec(east, (1,0)).
forwardVec(south, (0,-1)).
forwardVec(north, (0, 1)).
forwardVec(west, (-1, 0)).

turnDirLeft(east, north).
turnDirLeft(south, east).
turnDirLeft(north, west).
turnDirLeft(west, south).

turnDirRight(east, south).
turnDirRight(north, east).
turnDirRight(south, west).
turnDirRight(west, north).

addVec((Ax, Ay), (Bx, By), (Cx, Cy)) :-
  Cx is Ax + Bx,
  Cy is Ay + By.

subVec((Ax, Ay), (Bx, By), (Cx, Cy)) :-
  Cx is Ax - Bx,
  Cy is Ay - By.

valuableKnowledge([wumpusIntel, goVia, myPos, safeSpots, worldSize, visited, wumpusDead]).

saveKnowledge(K) :-
  saveOldActions(X, [gameStarted]),
  saveGoBack(E, X),
  saveSafeSpots(W,E),
  saveMyPos(R, W),
  saveVisited(T, R),
  saveWumpusDead(Y, T),
  saveWorldSize(K, Y).

saveVisited(K, A) :-
  visited(X), !,
  K = [visited(X) | A].

saveWorldSize(K, A) :-
  worldSize(X), !,
  K = [worldSize(X) | A].

saveOldActions(K, A) :-
  wumpusIntel(X), !,
  K = [wumpusIntel(X) | A].

saveOldActions(K,K).

saveWumpusDead(K, A) :-
  wumpusDead(X), !,
  K = [wumpusDead(X) | A].

saveWumpusDead(K,K).

saveGoBack(K, A) :-
  goVia(X), !,
  K = [goVia(X) | A ].

saveGoBack(K, K).

saveMyPos(K, A) :-
  myPos(X), !,
  K = [myPos(X) | A ].

saveSafeSpots(K, A) :-
  safeSpots(X), !,
  K = [safeSpots(X) | A ].

saveSafeSpots(K, K).

exists(A, B) :-
  member(A,B),
  !.

my_retract(A, B, C) :-
  my_delete(A, B, C).

my_delete(_, [], []).
my_delete(A, [H|T], Tp) :-
  \+ \+ A = H, !,
  my_delete(A, T, Tp).
my_delete(A, [H|T], [H|Tp]) :-
  my_delete(A, T, Tp).

my_assert(A, B, C) :-
  C = [A | B].

runAway(Ok, K) :-
  findPathTo((1,1), Ok, W),
  join(W, [exit], Wp),
  my_retract(goVia(_), Ok, Kp),
  my_assert(goVia(Wp), Kp, K).

doAction(A, Ok, K) :-
  glitter, !,
  A = grab,
  runAway(Ok, K).

doAction(A, Ok, K) :-
  exists(goVia([A|T]), Ok),
  my_retract(goVia(_), Ok, Kp),
  ((T = [], !, K = Kp)
  ; my_assert(goVia(T), Kp, K)).

doAction(A, Ok, Kn) :-
  findNewDirection(Ok, K),
  doAction(A, K, Kn).

processPercepts(Ok, Kn) :- 
  ((breeze, !, feltBreeze(Ok, Kp))
  ; Ok = Kp ),
  ((stench, !, smelledStench(Kp, K))
  ; K = Kp),
  ((scream, !, my_assert(wumpusDead([]), K, Kn))
  ; Kn = K).

smelledStench(Ok, Kp) :-
  exists(myPos((V,_)), Ok),
  addWumpusIntel(stenchOn(V), Ok, Kp).

feltBreeze(Ok, Kp) :-
  exists(myPos((V,_)), Ok),
  addWumpusIntel(breezeOn(V), Ok, Kp).

addWumpusIntel(I, Ok, K) :- 
  exists(wumpusIntel(X), Ok), !,
  my_retract(wumpusIntel(_), Ok, Kp),
  addIfNotMember(I, X, Xp),
  my_assert(wumpusIntel(Xp), Kp, K).

addWumpusIntel(_, K, K). 

findNewDirection(Kp, K) :-
  ( goToUnexploredNode(Kp, K), ! )
  ; 
  ( killWumpus(Kp, K), ! )
  ;
  ( runAway(Kp, K), !).

goToUnexploredNode(Kp, K) :-
  exists(safeSpots(SS), Kp),
  exists(visited(V), Kp),
  member(X, SS),
  \+ member(X, V),
  findPathTo(X, Kp, P), !,
  my_retract(goVia(_), Kp, Kpp),
  my_assert(goVia(P), Kpp, K).

updateIfSafe(Ok, K) :-
  \+ breeze,
  (\+ stench ; exists(wumpusDead(_), Ok)),
  !,
  updateSafeSpots(Ok,K).

updateIfSafe(K,K).

goForward(moveForward).

updateBumpedPos(Ok, K) :-
  forwardVecPos(Ok, V),
  exists(myPos((Vp, Dir)), Ok),
  subVec(Vp, V, Vpp),
  my_retract(myPos(_), Ok, Kp),
  my_assert(myPos((Vpp, Dir)), Kp, K).

updateSafeSpots(Ok, K) :-
  ((exists(safeSpots(L), Ok), my_retract(safeSpots(_), Ok, Kp)) 
  ; (L = [], Kp = Ok)),
  exists(myPos((P, _)), Kp),
  neighbours(P, Kp, N),
  joinSafe(N,L, Lp),
  my_assert(safeSpots(Lp), Kp, K).

join([], K, K).
join([H|T], L, [H|K]) :- join(T, L, K).

joinSafe([], K, K).
joinSafe([H|T], L, K) :- addIfNotMember(H, L, Kp), joinSafe(T, Kp, K).

neighbours((X,Y), Ok, L) :-
  NY is Y + 1,
  SY is Y - 1,
  EX is X + 1,
  WX is X - 1,
  exists(worldSize((WoX, WoY)), Ok),
  addIfH(WX, 0, (WX,Y), [], LW),
  addIfH(SY, 0, (X,SY), LW, LS),
  addIfLE(NY, WoY, (X, NY), LS, LN),
  addIfLE(EX, WoX, (EX, Y), LN, L).

addIfLE(A,B,C,D,E):-
  A =< B, !,
  E = [ C | D].
addIfLE(_,_,_,D,D).

addIfH(A,B,C,D,E):-
  A > B, !,
  E = [ C | D].
addIfH(_,_,_,D,D).

reverseAction(moveForward, moveForward).
reverseAction(turnLeft, turnRight).
reverseAction(turnRight, turnLeft).
reverseAction(exit, exit).

findPathTo(PosB, Ok, Way) :-
  exists(safeSpots(SS), Ok),
  exists(myPos((V,Dir)), Ok),
  findWayTo(V, PosB, SS, W, Ok),
  wayToPath(W, Dir, Way).

wayToPath([_], _, []).
wayToPath([H1,H2|T], Dir, W) :- 
  subVec(H2, H1, Diff),
  diffToAct(Diff, Dir, Acts, Dirp),
  join(Acts, Wp, W),
  wayToPath([H2|T], Dirp, Wp).

diffToAct((-1, 0), east,  [ turnLeft, turnLeft, moveForward ], west).
diffToAct(( 0,-1), east,  [ turnRight, moveForward ], south).
diffToAct(( 0, 1), east,  [ turnLeft, moveForward ], north).
diffToAct(( 1, 0), east,  [ moveForward ], east).

diffToAct((-1, 0), west,  [ moveForward ], west).
diffToAct(( 0,-1), west,  [ turnLeft, moveForward ], south).
diffToAct(( 0, 1), west,  [ turnRight, moveForward ], north).
diffToAct(( 1, 0), west,  [ turnLeft, turnLeft, moveForward ], east).

diffToAct((-1, 0), north, [ turnLeft, moveForward ], west).
diffToAct(( 0,-1), north, [ turnLeft, turnLeft, moveForward ], south).
diffToAct(( 0, 1), north, [ moveForward ], north).
diffToAct(( 1, 0), north, [ turnRight, moveForward ], east).

diffToAct((-1, 0), south, [ turnRight, moveForward ], west).
diffToAct(( 0,-1), south, [ moveForward], south).
diffToAct(( 0, 1), south, [ turnLeft, turnLeft, moveForward], north).
diffToAct(( 1, 0), south, [ turnLeft, moveForward], east).

findWayTo(PosA, PosB, SafeSpots, Way, Ok) :-
  bfs([[PosA]], PosB, SafeSpots, [], Way, Ok).

bfs([[H|T]|_], H, _, _, Way, _) :- 
  reverse([H|T], Way).
bfs([[H|_]| AltWays], PosB, SS, Visited, Way, Ok) :-
  member(H, Visited), !,
  bfs(AltWays, PosB, SS, Visited, Way, Ok).
bfs([[H|T]| AltWays], PosB, SS, Visited, Way, Ok) :-
  neighbours(H, Ok, N),
  filterSafeN(N, SS, NSS),
  makeNewWays(NSS, [H|T], NW),
  Vp = [H | Visited],
  join(AltWays, NW, AltWaysEx),
  bfs(AltWaysEx, PosB, SS, Vp, Way, Ok).

makeNewWays([], _, []).
makeNewWays([H|T], W, [[H|W]| Wp]) :- 
  makeNewWays(T, W, Wp).

filterSafeN([], _, []).
filterSafeN([H|T], SS, [H|Tp]) :-
  member(H, SS), !,
  filterSafeN(T, SS, Tp).
filterSafeN([_|T], SS, Tp) :-
  filterSafeN(T, SS, Tp).

killWumpus(Ok, Kn):-
  exists(wumpusIntel(X), Ok),
  refactorWumpusInfo(X, (S, B)),
  locateWumpus(S, Ok, Pos),
  noWind(Pos, B, Ok),
  calculateReward(Pos, Ok, Kp),
  findPathTo(Pos, Kp, Path), % seeks in new state, as it steps only on safe tiles
  prepareShot(Path, Operation),
  execute(Operation, Kp, K),
  my_retract(wumpusIntel(X), K, Kn).

refactorWumpusInfo([], ([],[])):- !.
refactorWumpusInfo([stenchOn(X) | T], ([X|S], B)) :-
  refactorWumpusInfo(T, (S, B)).
refactorWumpusInfo([breezeOn(X) | T], (S, [X|B])) :-
  refactorWumpusInfo(T, (S, B)).

locateWumpus(S, Kn, Pos) :-
  locateByCrosshair(S, S, Kn, Pos)
; locateByElimination(S, S, Kn, Pos).

locateByCrosshair([P1, P2, P3 | _], _, _, Pos) :-
  subVec(P1, P2, Diff1),
  subVec(P1, P3, Diff2),
  subVec(P2, P3, Diff3),
  ( diff2ToWumpusPos(Diff1, P2, Pos, [], []) 
  ; diff2ToWumpusPos(Diff2, P3, Pos, [], [])
  ; diff2ToWumpusPos(Diff3, P3, Pos, [], [])).

locateByCrosshair([P1, P2], S, K, Pos) :-
  subVec(P1, P2, Diff),
  diff2ToWumpusPos(Diff, P2, Pos, S, K).

diff2ToWumpusPos((-2, 0), (X,Y), (X1,Y1), _, _):- X1 is X-1, Y1 = Y.
diff2ToWumpusPos(( 2, 0), (X,Y), (X1,Y1), _, _):- X1 is X+1, Y1 = Y.
diff2ToWumpusPos(( 0, 2), (X,Y), (X1,Y1), _, _):- X1 = X,    Y1 is Y+1.
diff2ToWumpusPos(( 0,-2), (X,Y), (X1,Y1), _, _):- X1 = X,    Y1 is Y-1.
  
diff2ToWumpusPos((-1, 1), (X,Y), P, S, K):- S = [_|_], X1 is X-1, Y1 = Y,
  X2 = X, Y2 is Y+1, checkWhichOne((X1,Y1), (X2, Y2), S, K, P).
diff2ToWumpusPos(( 1, 1), (X,Y), P, S, K):- S = [_|_], X1 is X+1, Y1 = Y,
  X2 = X, Y2 is Y+1, checkWhichOne((X1,Y1), (X2, Y2), S, K, P).
diff2ToWumpusPos((-1,-1), (X,Y), P, S, K):- S = [_|_], X1 = X,    Y1 is Y-1,
  X2 is X-1, Y2 = Y, checkWhichOne((X1,Y1), (X2, Y2), S, K, P).
diff2ToWumpusPos(( 1,-1), (X,Y), P, S, K):- S = [_|_], X1 = X,    Y1 is Y-1,
  X2 is X+1, Y2 = Y, checkWhichOne((X1,Y1), (X2, Y2), S, K, P).

checkWhichOne(P1, P2, S, K, P) :-
  ( checkThisOne(P1, K, S),
    \+ checkThisOne(P2, K, S),
    P = P1 )
  ;
  ( checkThisOne(P2, K, S),
    \+ checkThisOne(P1, K, S),
    P = P2 ).

locateByElimination([S|_], St, Kn, Pos) :-
  neighbours(S, Kn, N),
  checkEachNeighbour(N, Kn, St, [Pos]).
locateByElimination([_ | T], St, Kn, Pos) :-
  locateByElimination(T, St, Kn, Pos).

checkEachNeighbour([], _, _, []).
checkEachNeighbour([N|T], K, S, [N|Tp]) :-
  checkThisOne(N, K, S), 
  checkEachNeighbour(T, K, S, Tp).
checkEachNeighbour([_|T], K, S, Tp) :-
  checkEachNeighbour(T, K, S, Tp).

checkThisOne(N, K, S) :-
  \+ isSafe(N,K), 
  stenchAllAround(N, K, S).

stenchAllAround(N, K, S) :-
  neighbours(N, K, Neigh),
  exists(visited(X), K),
  filterSafeN(Neigh, X, NX), % those neighbours he visited
  filterSafeN(NX, S, NS), % those that smell
  NS = NX.

isSafe(N,K) :-
  exists(safeSpots(SS), K),
  member(N, SS).

noWind(P, B, K) :-
  neighbours(P, K, N),
  exists(visited(X), K),
  filterSafeN(N, X, NN), % those i have info about
  filterSafeN(NN, B, NB),
  \+ NB = NN. % at least one is not breezy --- there is no hole for sure

prepareShot([moveForward], [shoot]).
prepareShot([H|T], [H|Tp]) :- prepareShot(T, Tp).

calculateReward(Pos, Ok, K) :-
  exists(safeSpots(SS), Ok),
  my_retract(safeSpots(_), Ok, Kp),
  my_assert(safeSpots([Pos|SS]), Kp, K).

execute(Op, Ok, K) :-
  my_assert(goVia(Op), Ok, K).
