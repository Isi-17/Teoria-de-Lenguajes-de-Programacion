program Power(x, y, z);

x := 3;
y := 4;
z := 1; // pow(a, 0) = 1

while 1 <= y do begin
    z := z * x;
    y := y - 1
end