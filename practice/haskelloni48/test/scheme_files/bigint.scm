(letrec (
    expected (BigInt "57896044618658097711785492504343953926634992332820282019728792003956564819949")
    computed (- (^ (BigInt 2) (BigInt 255)) (BigInt 19) )

    -- Custom bigint stdlib
    -- Imagine JS didn't have built-in bigints!
    zeros (lambda (n) (make-vector n 0))
    iszero (lambda (a) (
        fold
        (lambda (acc elem) (&& (== 0 elem) acc))
        #t a))
    -- this function is recursive so we use letrec instead of let
    fromnum (lambda (i)
        (if (< i 1)
            '()
            -- cons would make more sense in lisp
            (snoc (fromnum (bitwise-or 0 (/ i 10))) (% i 10))
        )
    )
)
(console.log
    computed (== computed expected)
    (zeros 5)
    (iszero (zeros 5))
    (fromnum 123456)
    #(1 2 3 4 5 6)
    (=== (fromnum 123456) #(1 2 3 4 5 6))
))
