#!/bin/bash
# This script is literate Bashkell.
set -o nounset -o errexit -o pipefail

declare -a esed=()
if sed --version | fgrep -q GNU &>/dev/null
then
  esed=(sed -r)
else
  esed=(sed -E)
fi

function body          {
  ${esed[@]} '1,/^exit 0$/ d'
}

function ghci_commands {
  ${esed[@]} -n '/^#> (.+)$/ { s//\1/ ; p ;} ; /^$/ { p ;}'
}

function results_filter {
  ${esed[@]} -n '/^########!/,/^########-/ {
                   /^########/ { s/^.+$// ;}
                   p
                 }' "$@"
}

case "${1:-}" in
  commands)     cat "$0" | body | ghci_commands ;;
  ''|tests)     cat "$0" | body ;;
  filter)       results_filter "$@" ;;
  ghci)         cat "$0" | body | ghci_commands | ghci ;;
  test)         "$0" ghci 2>/dev/null | results_filter ;;
  *)            echo "Arugment error." 1>&2 ;;
esac

exit 0

#> :set prompt "#>\n"
#> :set -XOverloadedStrings
#> :set -XNoMonomorphismRestriction
#> :load ./Language/Bash/PrettyPrinter.hs
#> let start = "\n########!\n"
#> let end = "\n########-\n"
#> let render x = Data.ByteString.Char8.putStr (concat [start, bytes x, end])

#> let ls = SimpleCommand "ls" . (:[])
#> let ifStmt = IfThenElse (ls ".") (ls ".") (ls "/")
#> let whileStmt = While ifStmt (SimpleCommand "echo" ["ok"])
#> let redirectStmt = Redirect whileStmt Append 1 (Left "fo&o")
#> render redirectStmt
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
#> render redirectStmt
{ echo -n $'hello '
  echo dudes. ;} 1>msg

#> let (varX :: Identifier) = "x"
#> let readX = ReadVarSafe (Right varX)
#> let lsX = SimpleCommand "ls" [readX]
#> let lsMsg = SimpleCommand "echo" ["Failed to `ls':", readX]
#> let lsErr = Redirect lsMsg Out 1 (Right 2)
#> let apacheLogs = "/var/log/apache2/" `Concat` Concat Asterisk ".log"
#> let forStmt = For varX [apacheLogs, "/var/log/httpd"] (lsX `OrOr` lsErr)
#> render forStmt
for x in /var/log/apache2/*.log /var/log/httpd
do
  ls "${x:-}" ||
  { echo $'Failed to `ls\':' "${x:-}" 1>&2 ;}
done

#> let cdApache = SimpleCommand "cd" ["/var/www"]
#> let lsWWWStmt = Redirect (Sequence cdApache whileStmt) Append 2 (Left "/err")
#> let wfStmt = lsWWWStmt `AndAnd` forStmt
#> render wfStmt
{ cd /var/www
  while if ls .
        then
          ls .
        else
          ls /
        fi
  do
    echo ok
  done ;} 2>>/err &&
for x in /var/log/apache2/*.log /var/log/httpd
do
  ls "${x:-}" ||
  echo $'Failed to `ls\':' "${x:-}" 1>&2
done

#> let redirected = Redirect whileStmt Out 2 (Left "/err") `OrOr` forStmt
#> render redirected
while if ls .
      then
        ls .
      else
        ls /
      fi
do
  echo ok
done 2>/err ||
for x in /var/log/apache2/*.log /var/log/httpd
do
  ls "${x:-}" ||
  echo $'Failed to `ls\':' "${x:-}" 1>&2
done

