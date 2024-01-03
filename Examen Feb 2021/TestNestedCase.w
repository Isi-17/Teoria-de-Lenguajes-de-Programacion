program TestNestedCase(x,y);

x:= 2;
y:= 3;

case x of
   0, 2, 4 : case y of
                0, 1 : x:= 100
                2, 3 : x:= 200
             end
   1, 5, 7 : case y of
                4, 5 : x:= 300
                6, 7 : x:= 400
                default : x:= 500
             end
   10, 20, 30 : case y of
                   8, 9 : x:= 600
                   10, 11 : x:= 700
                end
   default : x:= 6666
end
