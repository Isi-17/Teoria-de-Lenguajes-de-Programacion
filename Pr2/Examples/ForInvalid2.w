program ForInvalid2(x, i, j);

// A simple While program to test the for statement

// This program should be rejected by the compiler

x := 6;

for i := 1 to 10 do begin
    x := x + i;
    for j := 1 to 10 do begin
        if i = 100 * j then
           i := i      // assignment to control variable
        else
           x := 0;
        x := x * 2
    end
end
