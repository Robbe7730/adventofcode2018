# I gave up on Haskell for this one after I spent hours optimizing...

from blist import *

numPlayers = 458
numBalls = 7130700

scores = blist([0]*numPlayers)
balls = blist([0])
lastPos = 0

for i in range(1, numBalls):
    if (i%1000 == 0):
        print(i, numBalls)
    player = i % numPlayers
#    print(i, lastPos, player, scores, balls)
    if (i % 23) == 0:
        lastPos = (lastPos-7) % len(balls)
        scores[player] += i
        scores[player] += balls[lastPos]
        del balls[lastPos]
    else:
        lastPos = 1+(lastPos+1) % (len(balls))
        balls.insert(lastPos, i)
#print(i, lastPos, player, scores, balls)

print(max(scores))
