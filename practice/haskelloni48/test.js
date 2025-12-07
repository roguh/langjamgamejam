const __scope__ = {};
((__scope__) => (
__scope__["a"] = (Object.fromEntries([["X", 1], ["Y", 2]])),
__scope__["b"] = (true),
__scope__["c"] = ([3, 1, 4, 1, 5]),
(
 __scope__["b"]
) ? (
 console.log(__scope__["a"], __scope__["c"])
) : (
 console.log(__scope__["c"], __scope__["a"])
)
)
)({...__scope__})
