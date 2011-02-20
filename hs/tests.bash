#!/bin/bash
# This script is literate Bashkell.
set -o nounset -o errexit -o pipefail
function body          { sed -r '1,/^exit 0$/ d' ;}
function ghci_commands { sed -rn '/^#> (.+)$/ { s//\1/ ; p } ; /^$/ { p }' ;}
case "${1:-}" in
  ghci)         cat "$0" | body | ghci_commands ;;
  ''|tests)     cat "$0" | body ;;
  *)            echo "Arugment error." 1>&2 ;;
esac
exit 0

#> :load ./Language/Bash/PrettyPrinter.hs

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


