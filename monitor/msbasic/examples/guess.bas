10 mx=100:n=0
20 print "You have to guess a number between 1 and ";mx
30 x=int(rnd(1)*mx+1)
40 print "Your guess: ";:input g:n=n+1
50 if g<x then print "Too small!":goto 40
60 if g>x then print "Too big!":goto 40
70 print "You found the number after ";n;" guesses."
