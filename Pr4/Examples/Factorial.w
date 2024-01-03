program Factorial(x,y);

// a simple program to compute the factorial of x

begin
   var x:= 5;
   var y:= 1;
   while (1 <= x) do begin
      y := y * x;
      x := x - 1
   end
end

// expected output:
// Program Factorial finalized.
// Memory dump: [(0,3),(1,0),(2,120)]
