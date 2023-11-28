program ForInvalid1(x, i);

// A simple While program to test the for statement

// This program should be rejected by the compiler

x := 6;

for i := 1 to 10 do begin
    x := x + i;
    i := i + 1    // assignment to control variable
end
