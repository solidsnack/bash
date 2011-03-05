#!/bin/bash
# This script is literate Bashkell. Lines beginning with #> are passed to
# GHCi during the test.
## Sketching quine-style test script.
## let set = SimpleCommand "set"
## let set' = set ["-o", "nounset", "-o", "errexit", "-o", "pipefail"]
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
                   /^########[!-]/ { s/^.+$// ;}
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
#> :set -XScopedTypeVariables
#> :load ./Language/Bash.hs
#> let start = "\n########!\n"
#> let end = "\n########-\n"
#> let concatB = Data.ByteString.Char8.concat
#> let bookend b = Data.ByteString.Char8.putStr (concatB [start, b, end])
#> let render = bookend . bytes
#> let unlazy = concatB . Data.ByteString.Lazy.toChunks
#> let unbuilder = unlazy . Data.Binary.Builder.toLazyByteString
#> let buildrender = bookend . unbuilder

#> let ls = Annotated () . SimpleCommand "ls" . (:[])
#> let echo = Annotated () . SimpleCommand "echo"
#> let commented a b = Annotated (Lines a b)

#> let ifStmt = Annotated () (IfThenElse (ls ".") (ls ".") (ls "/"))
#> let whileStmt = Annotated () (While ifStmt (echo ["ok"]))
#> let redirectStmt = Annotated () (Redirect whileStmt Append 1 (Left "fo&o"))
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

#> let sqnc = Annotated () (Sequence (echo ["-n","hello "]) (echo ["dudes."]))
#> let redirectStmt = Annotated () (Redirect sqnc Out 1 (Left "msg"))
#> render redirectStmt
{ echo -n $'hello '
  echo dudes. ;} 1>msg

#> let (varX :: Identifier) = "x"
#> let readX = ReadVarSafe (Right varX)
#> let lsX = Annotated () (SimpleCommand "ls" [readX])
#> let lsMsg = Annotated () (SimpleCommand "echo" ["Failed to `ls':", readX])
#> let lsErr = Annotated () (Redirect lsMsg Out 1 (Right 2))
#> let lsOr = Annotated () (lsX `OrOr` lsErr)
#> let apacheLogs = "/var/log/apache2/" `Concat` Concat Asterisk ".log"
#> let for = For varX [apacheLogs, "/var/log/httpd"] lsOr
#> let forStmt = Annotated () for
#> render forStmt
for x in /var/log/apache2/*.log /var/log/httpd
do
  ls "${x:-}" ||
  { echo $'Failed to `ls\':' "${x:-}" 1>&2 ;}
done

#> let cdApache = Annotated () $ SimpleCommand "cd" ["/var/www"]
#> let cdWhile = Annotated () (Sequence cdApache whileStmt)
#> let lsWWW = Redirect cdWhile Append 2 (Left "/err")
#> let wfStmt = Annotated () lsWWW `AndAnd` forStmt
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

#> let redirected = Annotated () $ Redirect whileStmt Out 2 (Left "/err")
#> render $ OrOr redirected forStmt
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

#> let rem = Annotated (Lines ["# Remark."] [])
#> let noRem = Annotated (Lines [] [])
#> let echoRem = Annotated (Lines ["#>echo"] ["#<echo"]) . SimpleCommand "echo"
#> let echoCase = echoRem ["-n", "case: "]
#> let clause ex = (ex, rem (Sequence echoCase (echoRem [ex])))
#> let clause0 = clause "first"
#> let clause1 = clause "second"
#> let clause_ = (Asterisk, echoRem ["?"])
#> let clauses = [clause0, clause1, clause_]
#> let caseStmt = noRem $ Case (ReadVarSafe (Left Dollar1)) clauses
#> render caseStmt
case "${1:-}" in
  first)  # Remark.
          #>echo
          echo -n $'case: '
          #<echo
          #>echo
          echo first
          #<echo
          ;;
  second)  # Remark.
           #>echo
           echo -n $'case: '
           #<echo
           #>echo
           echo second
           #<echo
           ;;
  *)  #>echo
      echo $'?'
      #<echo
      ;;
esac

#> let a = SimpleCommand "echo" ["a"] :: Statement ()
#> let b = SimpleCommand "echo" ["b"] :: Statement ()
#> let pre = script_sha1 a b
#> buildrender pre
#!/bin/bash
set -o errexit -o nounset -o pipefail

######## Setup.

echo a

######## Main.

if fgrep -q f4b85e070cfe2c7990b5fa6b0603182921430d56 "$0"
then
  echo b
fi

#> let remWhile = fmap (const (Lines ["# Comment."] [])) whileStmt
#> let echoWhile = echoRem [Eval remWhile, "between", Eval remWhile, "after"]
#> render echoWhile

