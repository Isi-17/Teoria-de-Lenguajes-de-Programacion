program FactorialRepeat(x, fac);

// A simple While program to compute the factorial of 'x'
// using a 'repeat until' loop

x := 5;
fac := 1;

repeat
   fac := fac * x;
   x := x - 1
until x = 0
