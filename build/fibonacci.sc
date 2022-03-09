begin {
    n := 8;
    a := 0;
    b := 1;
    s := 0;
    while (n-1) do {
        begin {
            s := a+b;
            a := b;
            b := s;
            n := n-1; }
        end; };
    end; }
end;