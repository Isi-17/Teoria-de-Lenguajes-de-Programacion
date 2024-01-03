program TestCase(x,y,z);

x:= 1;
y:= 2;
z:= 3;

case z*2 of
   1, 5, 2 : x:= 100
   0 : begin
          x:= 4;
          z:= 5
       end
   6, 8, 112, 91 : if x = 0 then
                      x:= 10
                   else
                      y:= 20
   default : y:= 666
end
