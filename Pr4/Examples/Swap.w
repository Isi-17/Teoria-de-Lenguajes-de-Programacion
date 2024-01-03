program Swap(x,y,z);

// the swap program in WHILE

begin
   var x:= 5;
   var y:= 7;
   var z:= 0;

   z:= x;
   x:= y;
   y:= z
end

// expected output:
// Program Swap finalized.
// Memory dump: [(0,4),(1,7),(2,5),(3,5)]
