program TestParser(x,y,z);

// A simple, meaningless While program to test the parser

// The concrete syntax of While is inspired by that of Pascal.
// Note that the semicolon is a separator; i.e. the last sentence
// in a sequence of sentences is not terminated by a semicolon.

x := 1;

skip;

if x = 1 then       // simple sentence requires no begin .. end
   y := 1
else
   y := 2;

while x <= 10 do   // simple sentence needs no begin .. end
   x := x + 1;

if !(x = 3) then   // compound sentence requires begin .. end
  begin
    x := x + 1;
    z := 1
  end
else
  begin
    x := x - 1;
    z := 2
  end;

if x = 0 then       // nested ifs
   if y = 1 then
     begin
       a := 1;
       z := 1
     end
   else
     begin
       a := 2;
       z := 2
   end
else
   z := 3;

x := 1;
while x <= 5 do begin  // nested whiles
   x := x + 1;
   y := 1;
   while y <= 3 do
      y := y + 2
end;

repeat
     x := x * 5
until x = 10;

repeat            // no need to use begin .. end
     x := x * 5;
     y := y - 3
until x = 10;

repeat           // pretty ugly
   begin
     x := x * 5;
     y := y - 3
   end
until x = 10;

y := 1;
for x := 1 to 10 do  // simple sentence requires no begin .. end
   y := y * 2;

for x := 0 to 2*y do begin  // compound sentence requires begin .. end
   z := z + 1;
   z := z * 2
end;

for i := 1 to 10 do        // nested fors
   for j := i to 10 do
      z := z + i*j;

for i := 1 to 100 do begin   // nested fors
   for j := i*2 to 50 do begin
      z := z + i*j;
      z := z - 3
   end;
   z := z - 1
end;

begin                  // nested compound sentences
  begin
     begin
      a := 1
     end
  end
end
