-- Correct value:
-- python3 -c 'import cmath;print(cmath.cos(3+1j))'
-- (-1.5276382501165433-0.1658444019189788j)
(console.log (
    let
        (ccos (lambda (r m)
            -- ccos(r+mI) = cos r * cosh m - I * sin r * sinh m
            '((*
                (Math.cos r) (Math.cosh m))
                (- (*
                    (Math.sin r) (Math.sinh m)))
             )))
    (ccos 3 1)
))
