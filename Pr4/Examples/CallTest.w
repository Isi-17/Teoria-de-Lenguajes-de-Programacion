program CallTest(x,y);

begin
   var y:= 0;
   var x:= 0;

   proc p is x:= x * 2;

   proc q is call p;

   begin
      var x:= 5;

      proc p is x:= x + 1;

      call q;
      y:= x
   end
end

// expected output:
// Program CallTest finalized.
// Memory dump: [(0,4),(1,5),(2,0),(3,5)]
