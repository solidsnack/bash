#!/bin/bash
# This script is literate Bashkell. Lines beginning with #> are passed to
# GHCi during the test.
## Sketching quine-style test script.
## let set = SimpleCommand "set"
## let set' = set ["-o", "nounset", "-o", "errexit", "-o", "pipefail"]
set -o nounset -o errexit -o pipefail

declare -a esed=()
if { sed --version | fgrep -q GNU ;} &>/dev/null
then
  esed=(sed -r)
else
  esed=(sed -E)
fi

function body          {
  "${esed[@]}" '1,/^exit 0$/ d'
}

function ghci_commands {
  "${esed[@]}" -n '/^#> (.+)$/ { s//\1/ ; p ;} ; /^$/ { p ;}'
}

function results_filter {
  "${esed[@]}" -n '/^########!/,/^########-/ {
                     /^########[!-]/ { s/^.+$// ;}
                     p
                   }' "$@"
}

function interpreter {
  cabal repl
}

case "${1:-}" in
  commands)     cat "$0" | body | ghci_commands ;;
  ''|tests)     cat "$0" | body ;;
  filter)       results_filter "$@" ;;
  interpret)    cat "$0" | body | ghci_commands | interpreter ;;
  test)         "$0" interpret 2>/dev/null | results_filter ;;
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
#> let vRANDOM = VarIdent "RANDOM"
#> let randomLT_ n = SimpleCommand "[" [ReadVar vRANDOM, "-lt", n, "]"]
#> let randomLT n = Annotated () (randomLT_ n)
#> let commented a b = Annotated (Lines a b)

#> let ifAnn a b c = Annotated () (IfThenElse a b c)
#> let ifStmt = ifAnn (randomLT "40000") (randomLT "10000") (randomLT "45000")
#> let whileStmt = Annotated () (While ifStmt (echo ["ok"]))
#> let redirectStmt = Annotated () (Redirect whileStmt Append 1 (Left "fo&o"))
#> render redirectStmt
while if $'[' "$RANDOM" -lt 40000 $']'
      then
        $'[' "$RANDOM" -lt 10000 $']'
      else
        $'[' "$RANDOM" -lt 45000 $']'
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
#> let readX = ReadVarSafe (VarIdent varX)
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
  while if $'[' "$RANDOM" -lt 40000 $']'
        then
          $'[' "$RANDOM" -lt 10000 $']'
        else
          $'[' "$RANDOM" -lt 45000 $']'
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
while if $'[' "$RANDOM" -lt 40000 $']'
      then
        $'[' "$RANDOM" -lt 10000 $']'
      else
        $'[' "$RANDOM" -lt 45000 $']'
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
#> let echoRem = Annotated (Lines ["#+echo"] ["#-echo"]) . SimpleCommand "echo"
#> let echoCase = echoRem ["-n", "case: "]
#> let clause ex = (ex, rem (Sequence echoCase (echoRem [ex])))
#> let clause0 = clause "first"
#> let clause1 = clause "second"
#> let clause_ = (Asterisk, echoRem ["?"])
#> let clauses = [clause0, clause1, clause_]
#> let caseStmt = noRem $ Case (ReadVarSafe (VarSpecial Dollar1)) clauses
#> render caseStmt
case "${1:-}" in
  first)  # Remark.
          #+echo
          echo -n $'case: '
          #-echo
          #+echo
          echo first
          #-echo
          ;;
  second)  # Remark.
           #+echo
           echo -n $'case: '
           #-echo
           #+echo
           echo second
           #-echo
           ;;
  *)  #+echo
      echo $'?'
      #-echo
      ;;
esac

#> let a = SimpleCommand "echo" ["a"] :: Statement ()
#> let b = SimpleCommand "echo" ["b"] :: Statement ()
#> let pre = script_sha1 "# Silly script." a b
#> buildrender pre
#!/bin/bash

# Silly script.

######## Setup.

echo a

######## Main.

if fgrep -q f4b85e070cfe2c7990b5fa6b0603182921430d56 "$0"
then
  set -o errexit -o nounset -o pipefail
  echo b
fi

#> let remWhile = fmap (const (Lines ["# Comment."] ["# Comment."])) whileStmt
#> let processWhile = ProcessIn remWhile
#> let evalWhile = Eval remWhile
#> let evalEcho = Eval (echoRem ["before"])
#> let echoWhile = echoRem [evalEcho, evalWhile, "mid", processWhile, "after"]
#> render echoWhile
#+echo
echo "$( #+echo
         echo before
         #-echo
         )" "$( # Comment.
                while # Comment.
                      if # Comment.
                         $'[' "$RANDOM" -lt 40000 $']'
                         # Comment.
                      then
                        # Comment.
                        $'[' "$RANDOM" -lt 10000 $']'
                        # Comment.
                      else
                        # Comment.
                        $'[' "$RANDOM" -lt 45000 $']'
                        # Comment.
                      fi
                      # Comment.
                do
                  # Comment.
                  echo ok
                  # Comment.
                done
                # Comment.
                )" mid <( # Comment.
                          while # Comment.
                                if # Comment.
                                   $'[' "$RANDOM" -lt 40000 $']'
                                   # Comment.
                                then
                                  # Comment.
                                  $'[' "$RANDOM" -lt 10000 $']'
                                  # Comment.
                                else
                                  # Comment.
                                  $'[' "$RANDOM" -lt 45000 $']'
                                  # Comment.
                                fi
                                # Comment.
                          do
                            # Comment.
                            echo ok
                            # Comment.
                          done
                          # Comment.
                          ) after
#-echo

#> let (varX :: Identifier) = "x"
#> let readX = ReadVar (VarIdent varX)
#> let lengthX = Length (VarIdent varX)
#> let echoX = Annotated () (SimpleCommand "echo" [readX, "----", lengthX])
#> let seq4 = Annotated () (SimpleCommand "seq" ["1","4"])
#> let forSeq = For varX [Eval seq4, EvalUnquoted seq4] echoX
#> render (Annotated () forSeq)
for x in "$( seq 1 4 )" $( seq 1 4 )
do
  echo "$x" ---- "${#x}"
done

#> let lsRoot = ls (Concat Tilde "root")
#> render lsRoot
ls ~root

#> let (patterns :: Identifier) = "patterns"
#> let (string :: Identifier) = "string"
#> let identSTRING = VarIdent string
#> let readSTRING = ReadVar identSTRING
#> let readPATTERN0 = ReadArray patterns (literal "leading")
#> let readPATTERN1 = ReadArray patterns (literal "trailing")
#> let setSTRING = Annotated () (Assign $ Var string "ab/cd/ef")
#> let setPATTERNS = Annotated () (Assign $ Dict patterns [("leading", "ab")])
#> let setPATTERN1 = Annotated () (DictUpdate patterns "trailing" "ef")
#> let trimLFixed = Trim ShortestLeading identSTRING readPATTERN0
#> let trimTFixed = Trim ShortestTrailing identSTRING readPATTERN1
#> let glob0 :: Expression () = Concat Asterisk "/"
#> let glob1 :: Expression () = Concat "/" Asterisk
#> let trimSL = Trim ShortestLeading identSTRING glob0
#> let trimLL = Trim LongestLeading identSTRING glob0
#> let trimST = Trim ShortestTrailing identSTRING glob1
#> let trimLT = Trim LongestTrailing identSTRING glob1
#> let trims = [trimLFixed, trimTFixed, trimSL, trimLL, trimST, trimLT]
#> let sequence a b = Annotated () (Sequence a b)
#> let set_ = setSTRING `sequence` setPATTERNS `sequence` setPATTERN1
#> let echoSubs = Annotated () (SimpleCommand "echo" trims)
#> render (set_ `sequence` echoSubs)
string=ab/cd/ef
declare -A patterns=( [leading]=ab )
patterns[trailing]=ef
echo "${string#"${patterns[leading]}"}" "${string%"${patterns[trailing]}"}" \
     "${string#*/}" "${string##*/}" "${string%/*}" "${string%%/*}"

#> let redirectI f stmt = Redirect (Annotated () stmt) In  0 (Left f)
#> let redirectO f stmt = Redirect (Annotated () stmt) Out 1 (Left f)
#> let redirectE f stmt = Redirect (Annotated () stmt) Out 2 (Left f)
#> let clauseSimple ex stmt = (ex, Annotated () stmt)
#> let echo_ = SimpleCommand "echo"
#> let echo__ t = Sequence (Annotated () (echo_ [t])) (Annotated () (echo_ [t]))
#> let echoIO = (redirectO "o" . redirectI "i") (echo_ ["->y"])
#> let setFOO = Assign $ Var "foo" (Eval seq4)
#> let c__ = clauseSimple "w" (echo_ ["->w"])
#> let c2  = clauseSimple "x" ((Bang . Annotated()) (echo__ "->x"))
#> let cIO = clauseSimple "y" echoIO
#> let cI_ = clauseSimple "z" (redirectI "i" (echo_ ["->z"]))
#> let long = Sequence (Annotated() setFOO) (Annotated() echoIO)
#> let cL = clauseSimple "a" long
#> let read_1 = ReadVarSafe (VarSpecial Dollar1)
#> let caseClauseRedirect = Annotated () (Case read_1 [c__, c2, cIO, cI_, cL])
#> render caseClauseRedirect
case "${1:-}" in
  w)  echo $'->w' ;;
  x)  ! { echo $'->x'
          echo $'->x' ;} ;;
  y)  { echo $'->y' 0<i ;} 1>o ;;
  z)  echo $'->z' 0<i ;;
  a)  foo="$( seq 1 4 )"
      { echo $'->y' 0<i ;} 1>o ;;
esac

#> let echo2 = Annotated () ((Bang . Annotated()) (echo__ "->x"))
#> let echoFor = Annotated () $ echo2 `AndAnd` forStmt
#> render echoFor
{ ! { echo $'->x'
      echo $'->x' ;} ;} &&
for x in /var/log/apache2/*.log /var/log/httpd
do
  ls "${x:-}" ||
  echo $'Failed to `ls\':' "${x:-}" 1>&2
done

#> let (var :: Identifier) = "var"
#> let setEVAL = Annotated () (Assign $ Var var (Eval whileStmt))
#> render setEVAL
var="$( while if $'[' "$RANDOM" -lt 40000 $']' 
              then
                $'[' "$RANDOM" -lt 10000 $']' 
              else
                $'[' "$RANDOM" -lt 45000 $']' 
              fi 
        do
          echo ok 
        done )"

#> let (var :: Identifier) = "var"
#> let evalUnquotedEcho = (EvalUnquoted . Annotated ()) (echo__ "text")
#> let setEVALUNQUOTED = Annotated () (Assign $ Var var evalUnquotedEcho)
#> render setEVALUNQUOTED
var=$( echo text
       echo text )

