(let (
    expected (BigInt "57896044618658097711785492504343953926634992332820282019728792003956564819949")
    computed (- (^ (BigInt 2) (BigInt 255)) (BigInt 19) )
) (console.log computed (== computed expected))
)
