program Power(x, y, z);

x := 3;
y := 4;
z := 1; // pow(a, 0) = 1

repeat
    z := z * x;
    y := y - 1
until 1 = y