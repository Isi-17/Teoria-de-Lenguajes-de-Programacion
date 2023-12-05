program Abort3(x,y);

x:= 1;
while x <= 5 do begin
   if x = 3 then
      abort
   else
      x:= x+ 1
end
