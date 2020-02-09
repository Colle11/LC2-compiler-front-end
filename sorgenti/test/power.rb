=begin
Il seguente programma è costituito da una funzione
power(m,n) che eleva un intero m alla potenza intera n,
e dalla funzione main che utilizza power.
=end

def int main()
    int i = 0

    for i in 0..9
        int x = power(2, i)
        int y = power(-3, i)

        writeInt(x)
        writeString(", ")
        writeInt(y)
        writeString("\n")
    end

    return 0
end

def int power(int base, int n)
    int p = 1
    int i = 0

    for i in 1..n
        p = p * base
    end

    return p  # restituisce il valore di p al chiamante
end