#!/bin/bash
# This script is literate Bashkell.
set -o nounset -o errexit -o pipefail

function _sed {
    case "$(uname)" in
        Darwin)  sed -E "$@" ;;
        Linux)   sed -r "$@" ;;
        *)       echo "Unknown UNIX." 1>&2 ; exit 1 ;;
    esac
}

function body          { _sed '1,/^exit 0$/ d' ;}
function ghci_commands { _sed -n '/^#> (.+)$/ { s//\1/ ; p; } ; /^$/ { p; }' ;}

case "${1:-}" in
  ghci)         cat settings.ghci; cat "$0" | body | ghci_commands ;;
  ''|tests)     cat "$0" | body ;;
  *)            echo "Arugment error." 1>&2 ;;
esac
exit 0

#> :set prompt "#> "
#> :set -XOverloadedStrings
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

#> let echo ss = SimpleCommand "echo" ss
#> let groupedStmt = Sequence (echo ["-n","hello "]) (echo ["dudes."])
#> let redirectStmt = Redirect groupedStmt Out 1 (Left "msg")
#> Data.ByteString.Char8.putStr (bytes redirectStmt)
{ echo -n $'hello '
  echo dudes. ;} 1>msg
