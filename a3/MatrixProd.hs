module MatrixProd where

{--
You *MAY* use packages from base
https://hackage.haskell.org/package/base
but no others.

You may remove the comments if you like.

Given two Matrices AA and BB their MATRIX PRODUCT AA x BB is defined by
    (AA x BB)[i,j] == row(i, AA) DOT col(j, BB)
where DOT is the "dot product" or "scalar product" defined by
    [x1, x2, ... xk] (DOT) [y1, y2, ... yk] = x1*y1 + x2*y2 + ... + xk*yk

=======
EXAMPLE
=======

[[1,2],[3,4],[5,6]] x [[7,8,9],[10,11,12]]
[[27,30,33],[61,68,75],[95,106,117]]

Implement matrix multiplication on the Matix type, which is just a type
synononym for [[Int]].  Your matrixMult **must be total** by utilizing the
provided Error datattype:

data Error = DimMismatch (Either Error Matrix) (Either Error Matrix) | NotAMatrix Matrix deriving Show

==============
Complication 1
==============
A matrix is a list of rows of EQUAL length.  The type [[Int]] cannot distinguish
 between a well-formed Matrix like:
    > aa = [ [1,2], [3,4], [5,6] ]
and an ill-formed one like:
    > bb = [ [1,2], [3,4,5], [6,7,8,9] ]

When encountering ill-formed matrices return the "NotAMatrix" error.

NOTE:  [[]] is NOT a matrix.  The smallest matrix is [[x]].

=======
EXAMPLE
=======

> matProd [[1,2],[3,4],[5,6]] [[1,2],[3,4,5],[6,7,8,9]]
[NotAMatrix [[1,2],[3,4,5],[6,7,8,9]]]
Left $ DimMismatch (Right [[1,2],[3,4],[5,6]]) (Left $ NotAMatrix [[1,2],[3,4,5],[6,7,8,9]])

> matProd [[1],[2,3]] [[]]
Left $ DimMismatch (Left $ NotAMatrix [[1],[2,3]]) (Left $ NotAMatrix [[]])

==============
Complication 2
==============
Matrix multiplication only works when the matrices have compatable dimension.

That is, (AA)(BB) is a well-defined operation when
    AA has J rows and K columns
    BB has K rows and L columns
then
    (AA)(BB) has J rows and L columns.

Your matProd function should return the following when encountering mismatched
dimensions on well-formed matrices
    > matProd (Right [[1,2],[3,4],[5,6]]) (Right [[7,8],[9,10],[11,12]])
    Left $ DimMismatch (Right [[1,2],[3,4],[5,6]]) (Right [[7,8],[9,10],[11,12]])
and the following when multiplying ill-formed matrices.
    > matProd (Right [[1,2],[3,4],[5,6]]) (Right [[7,8],[9],[11,12]])
    Left $ DimMismatch (Right [[1,2],[3,4],[5,6]]) (Left $ NotAMatrix [[7,8],[9],[11,12]])

==============
COMPLICATION 3
==============
When doing a sequence of matrix multiplications, you must return a recursive
Error like the following:
    > matProd (Right [[3,4,5], [1]]) (matProd (Right [[1],[1,2]]) (Right []))
    Left $ DimMismatch (Left $ NotAMatrix [[3,4,5], [1]]) (Left $ DimMismatch (Left $ NotAMatrix [[1],[1,2]]) (Left $ NotAMatrix []))

==============
COMPLICATION 4
===============
Matrix multiplication is ASSOCIATIVE.  Thereby, it will be possible to minimize
the number of errors lifted by your code.

For example:
    > matProd (Right [[1,1],[2,2]]) (matProd (Right [[1,1,1],[2,2,2]]) (Right [[1],[1,2]]))
    Left $ DimMismatch (Right [[3,3,3],[6,6,6]]) (Left $ NotAMatrix [[1],[1,2]])
and NOT
    Left $ DimMismatch (Right [[1,1],[2,2]]) (Left $ DimMismatch (Right [[1,1,1],[2,2,2]]) (Left $ NotAMatrix [[1],[1,2]]))

--}

-- not a matrix is if it's not rectangular, lengths should be the same.

data Error = DimMismatch (Either Error Matrix) (Either Error Matrix) | NotAMatrix Matrix deriving Show

type Matrix = [[Int]]

matrixMult :: Either Error Matrix -> Either Error Matrix -> Either Error Matrix
matrixMult = undefined

