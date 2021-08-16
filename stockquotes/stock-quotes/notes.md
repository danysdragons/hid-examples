Warning: Installation path /home/hamelm/.local/bin not found on the PATH environment variable.

dot -Tpng -o stock1.png test.gv
dot -Tsvg -o stocknew.svg stocknew.gv
-Kdot not needed since dot is the default layout engine
dot -Tpng -Kdot -o stock4.png test.gv
dot -Tpng -Kcirco -o stock3.png test.gv
dot -Tpng -Kneato -o stock4.png test.gv
dot -Tpng -Ksfdp -o stock4.png test.gv



stack exec stock-quotes-exe data/quotes.csv --chart --html MyFile.html


stack exec stock-quotes-exe data/quotes.csv --chart --html MyFile.html



stack exec "stock-quotes-exe data/quotes.csv --chart --html MyFile.html"

stack exec stock-quotes-exe "data/quotes.csv --chart --html MyFile.html"


### These Work! ###
stack exec -- stock-quotes-exe data/quotes.csv
stack exec -- stock-quotes-exe data/quotes.csv --name "Mike" --chart
stack exec -- stock-quotes-exe data/quotes.csv --chart --html NiceFile3.

ln -s /home/hamelm/HaskellProjects/hid-examples/stockquotes/stock-quotes ~/stock-quotes

/home/hamelm/HaskellProjects/hid-examples/stockquotes/stock-quotes



import Fmt ( Buildable (..), Builder, fixedF, pretty, (+|), (+||), (|+), (||+))


import Fmt
  ( Buildable (..),
    Builder,
    fixedF,
    pretty,
    (+|),
    (+||),
    (|+),
    (||+)
  )

|   |  Haskell | OCaml  |   |   |
|---|---|---|---|---|
| Term  |   |   |   |   |
| Type  |   |   |   |   |
|   |   |   |   |   |