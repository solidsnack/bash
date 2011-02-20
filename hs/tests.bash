#!/bin/bash
set -o nounset -o errexit -o pipefail
#> :load ./Language/Bash/PrettyPrinter.hs
# This script is literate Bashkell.

#> let ls = SimpleCommand "ls" . (:[])
#> let ifStmt = IfThenElse (ls ".") (ls ".") (ls "/")
#> let whileStmt = While ifStmt (SimpleCommand "echo" ["ok"])
#> let redirectStmt = Redirect whileStmt Append 1 (Left "fo&o")
#> Data.ByteString.Char8.putStr (bytes redirectStmt)
while if ls .
      then
        ls .
      else
        ls /
      fi
do
  echo ok
done 1>>$'fo&o'


