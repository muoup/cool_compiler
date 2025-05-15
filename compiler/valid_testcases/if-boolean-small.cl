class Main inherits IO {
  main(): Object {
    let y : Int <- in_int(),
        m : Int <- in_int(),
        a : Int <- in_int(),
        n : Int <- in_int() in
    {
        out_int(y);
        out_int(m);
        out_int(a);
        out_int(n);

        if (((not ((not ((y = 1))))) < ((m < 5) = ((not ((a <= 2))) = (n = 4))))) then out_int(3) else out_int(1) fi
        ;
    }
  };
};