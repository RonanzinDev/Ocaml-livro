let quadrado x = x * 2


let rec fatorial n = 
    if n = 0 then 1 else n * fatorial (n - 1)