program TestFor(x, ac, i, start, stop);

// A simple While program to test the for statement

x := 6;

start := 1;
stop := x;
ac := 1;
for i:= start to stop do begin
   ac := ac * i;
   start := start + 2;  // the loop should be unaffected by this assignment
   stop := stop - 2     // the loop should be unaffected by this assignment
end

// Expected output:

// Program TestFor finalized.
// Final State: ["x->6","ac->720","i->6","start->13","stop->-6"]
