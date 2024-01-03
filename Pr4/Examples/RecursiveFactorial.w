program RecursiveFactorial(x,y);

begin
   var y:= 1;
   var x:= 5;

   proc factorial is
   begin
      var z:= x;

      if x = 1 then
         skip
      else begin
         x:= x - 1;
         call factorial;
         y:= y * z
      end
    end;

    call factorial
end

// expected output:
// Program RecursiveFactorial finalized.
// Memory dump: [(0,8),(1,120),(2,1),(3,5),(4,4),(5,3),(6,2),(7,1)]
