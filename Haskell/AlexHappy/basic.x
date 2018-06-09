{
module Main (main) where
}

%wrapper "basic"

ab :-

  a+ {\s -> A (length s)}
  b+ {\s -> B (length s)}

{

data Token = A Int
           | B Int
    deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
