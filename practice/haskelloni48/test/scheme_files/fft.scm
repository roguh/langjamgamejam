{-

Everything in the universe sings!
If you can measure something, the Fourier Transform finds the rhythm.
If you have a computer, the Fast Fourier Transform algorithm is a fast way to compute the frequency or period of almost any dataset.

-}

{- implement DFT

fun dft(x) {
    var N = #x;
    var Y = [];
    var k; // TODO for-loop scoping
    // TODO increment ++
    for (k = 0; k < N; k = k + 1) {
        var sum = 0 * I;
        var c = -2 * M_PI * k / N;
        var n;
        for (n = 0; n < N; n = n + 1) {
            var a = c * n;
            sum = sum + x[n] * (ccos(a) + I * csin(a));
        }
        Y = Y + [sum];
    }
    return Y;
}


-}

-- TODO run the FFT on browser mic input
-- TODO visualize FFT from live mic data
-- TODO run FFT on 2D image or video
-- TODO how slow is this code exactly lol
(let
    (pi Math.PI
     -- For now, complex numbers are represented as a list of two numbers
     -- ccos is the complex cosine C->C
     ccos (lambda (num) num)
     csin (lambda (num) num)
     -- Add two complex numbers
     cadd (lambda (num) num)
     -- Multiply two complex numbers
     cmul (lambda (num) num)
     dft (lambda (x) x)
     testX '('(1 0) '(2 0) '(3 1) '(4 1) '(5 6) '(7 8) '(9 10) '(11 12))
    )
(console.log (dft testX))
)
