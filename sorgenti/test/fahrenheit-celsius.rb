=begin
il programma stampa la tabella Fahrenheit-Celsius
per l'intervallo di valori Fahrenheit da 0 a 300
=end

def int main()
    # dichiarazione di 2 variabili di tipo float
    float fahr = 0.0
    float celsius = 0.0
    # dichiarazione di 3 variabili di tipo int
    int lower = 0; int upper = 0; int step = 0

    lower=0; upper=300; step=20; # inizializzazione variabili
    writeString("Tabella Fahrenheit-Celsius\n")
    fahr=lower

    while(fahr <= upper) do # ciclo while
        celsius = (5/9.0)*(fahr-32.0)
        writeFloat(fahr)
        writeString(" - ")
        writeFloat(celsius)
        writeString("\n")
    end

    return 0
end