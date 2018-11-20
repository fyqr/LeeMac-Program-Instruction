;;--------------------=={ LISP Styler }==---------------------;;
;;                                                            ;;
;;  A generic styling engine allowing the user to add         ;;
;;  specified styling tags to various code elements of        ;;
;;  a LISP file.                                              ;;
;;                                                            ;;
;;  Output file is saved to the same directory as supplied    ;;
;;  input file.                                               ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  file   - filename of lsp file to convert                  ;;
;;  styles - list of styling tags (format described below)    ;;
;;  extn   - extension of the output file                     ;;
;;  <>     - Boolean flag determining whether < & > are       ;;
;;           converted to &lt; and &gt; in the output file    ;;
;;------------------------------------------------------------;;

(defun LM:LISPStyler

  (

    ;; -- Arguments --
   
    file    ;; - Filename of LISP file to Convert

    styles  ;; - List of Styles tags for each code item

    extn    ;; - Extension for resultant file (e.g. ".html")

    <>      ;; - T if '<' and '>' are to be replaced with &lt; &gt;

    /

    ;; -- Subfunctions --

    *error*
    _read
    _fix<>
    _PadBetween

    ;; -- Local Variables --

    _bracket
    _bracketl
    _container
    _containerl
    _integer
    _integerl
    _mcomment
    _mcommentl
    _protected
    _protectedl
    _quote
    _quotel
    _real
    _reall
    _scomment
    _scommentl
    _string
    _stringl
    a
    exceptions
    fl
    i
    ichrs
    l
    lines
    ochrs
    of
    outfile
    s
    str
    w
    wf
    x
   
  )
  
  ;;---------------------------------------------------------------;;
  ;; Code based on an algorithm concocted by the genius            ;;
  ;; of ElpanovEvgeniy. Rewritten & modified by Lee Mac 2011 to    ;;
  ;; include line & character reports, and to enable the user to   ;;
  ;; use custom styles.                                            ;;
  ;;                                                               ;;
  ;;---------------------------------------------------------------;;
  ;; Notes on Function Arguments:                                  ;;
  ;;---------------------------------------------------------------;;
  ;;                                                               ;;
  ;;---------------------------------------------------------------;;
  ;; file                                                          ;;
  ;;---------------------------------------------------------------;;
  ;; Full filename of LISP file to convert, forward slashes or     ;;
  ;; double-backslashes may be used in filename.                   ;;
  ;;                                                               ;;
  ;;---------------------------------------------------------------;;
  ;; styles                                                        ;;
  ;;---------------------------------------------------------------;;
  ;; List of tags to enclose various code items, list must be in   ;;
  ;; the following form:                                           ;;
  ;;                                                               ;;
  ;; (                                                             ;;
  ;;   (<container open> <container close>)   e.g. (<pre> </pre>)  ;;
  ;;   (<quote     open> <quote     close>)   quotes/dots          ;;
  ;;   (<bracket   open> <bracket   close>)   parentheses          ;;
  ;;   (<mcomment  open> <mcomment  close>)   multiline comments   ;;
  ;;   (<scomment  open> <scomment  close>)   single comments      ;;
  ;;   (<string    open> <string    close>)   strings              ;;
  ;;   (<protected open> <protected close>)   protected symbols    ;;
  ;;   (<integer   open> <integer   close>)   integers             ;;
  ;;   (<real      open> <real      close>)   reals                ;;
  ;; )                                                             ;;
  ;;                                                               ;;
  ;; If no tag is required for an item, the entry must still be    ;;
  ;; present, as:                                                  ;;
  ;;                                                               ;;
  ;;   ("" "")                                                     ;;
  ;;                                                               ;;
  ;;---------------------------------------------------------------;;
  ;; extn                                                          ;;
  ;;---------------------------------------------------------------;;
  ;; Extension for resultant file - dot must be included.          ;;
  ;;                                                               ;;
  ;; Example:   ".html"                                            ;;
  ;;                                                               ;;
  ;;---------------------------------------------------------------;;
  ;; <>                                                            ;;
  ;;---------------------------------------------------------------;;
  ;; Boolean flag to determine whether the characters '<' and '>'  ;;
  ;; are to be replaced with &lt; and &gt;                         ;;
  ;;                                                               ;;
  ;; If T, '<' and '>' are replaced.                               ;;
  ;;                                                               ;;
  ;;---------------------------------------------------------------;;

  ;;---------------------------------------------------------------;;
  ;; Note that by the nature of the algorithm to determine the     ;;
  ;; type of code item, any user-defined symbols in the current    ;;
  ;; namespace will be interpreted as the datatype to which they   ;;
  ;; point.                                                        ;;
  ;;---------------------------------------------------------------;;
  ;; For this reason, a list of exceptions is required to avoid    ;;
  ;; the incorrect styling of symbols appearing as functions &     ;;
  ;; variables in this program or indeed any other defined         ;;
  ;; functions or global variables.                                ;;
  ;;---------------------------------------------------------------;;
  ;; This list may be extended to suit the user's environment.     ;;
  ;;---------------------------------------------------------------;;

  (setq exceptions

     ;; Subfunctions, Arguments & Variables in this Program

    '("*error*"    "_read"
      "_fix<>"     "_padbetween"

      "file"       "styles"
      "extn"       "<>"

      "_bracket"   "_bracketl"
      "_container" "_containerl"
      "_integer"   "_integerl"
      "_mcomment"  "_mcommentl"
      "_protected" "_protectedl"
      "_quote"     "_quotel"
      "_real"      "_reall"
      "_scomment"  "_scommentl"
      "_string"    "_stringl"
      "a"          "exceptions"
      "fl"         "i"
      "ichrs"      "l"
      "lines"      "ochrs"
      "of"         "outfile"
      "s"          "str"
      "w"          "wf"
      "x"

      "lm:lispstyler"
     )
  )

  ;;---------------------------------------------------------------;;

  ;; Error Handler

  ;;---------------------------------------------------------------;;

  (defun *error* ( msg )
    (mapcar
      (function
        (lambda ( sym ) (and sym (close sym)))
      )
      (list of wf)
    )
    
    (if
      (and msg
        (not
          (member (strcase msg)
           '(
              "CONSOLE BREAK"
              "FUNCTION CANCELLED"
              "QUIT / EXIT ABORT"
            )
          )
        )
      )
      (princ (strcat "\n** Error: " msg " **"))
    )
    (princ)
  )

  ;;---------------------------------------------------------------;;

  ;; Attempts to read file with supplied filename, returns a list
  ;; of strings in the file.

  ;;---------------------------------------------------------------;;

  (defun _read ( file / of l )
    (cond
      (
        (setq of (open file "r"))
        (while (car (setq l (cons (read-line of) l))))

        (setq of (close of)) (reverse (cdr l))
      )
    )
  )

  ;;---------------------------------------------------------------;;

  ;; Substitutes all instances of '<' and '>' with &lt; and &gt; in
  ;; supplied string.

  ;;---------------------------------------------------------------;;
  
  (defun _fix<> ( string / _stringsubst )

    (defun _stringsubst ( new old string / i nl )
      (setq i 0 nl (strlen new))

      (while (and (< i (strlen string)) (setq i (vl-string-search old string i)))
        (setq string (vl-string-subst new old string i) i (+ i nl))
      )
      string
    )
    
    (_stringsubst "&lt;" "<" (_stringsubst "&gt;" ">" string))
  )

  ;;---------------------------------------------------------------;;

  ;; String padding function - provides some eye-candy for the
  ;; output report.

  ;;---------------------------------------------------------------;;

  (defun _PadBetween ( s1 s2 ch ln )
    (if (< (+ (strlen s1) (strlen s2)) ln)
      (_PadBetween (strcat s1 ch) s2 ch ln)
      (strcat s1 s2)
    )
  )

  ;;---------------------------------------------------------------;;

  ;;---------------------------------------------------------------;;
  ;;                        Main Function                          ;;
  ;;---------------------------------------------------------------;;

  ;;---------------------------------------------------------------;;

  (mapcar 'set
   '(
      _container
      _quote
      _bracket
      _mcomment
      _scomment
      _string
      _protected
      _integer
      _real
    )
    styles
  )

  (mapcar 'set
   '(
      _containerL
      _quoteL
      _bracketL
      _mcommentL
      _scommentL
      _stringL
      _protectedL
      _integerL
      _realL
    )
    (mapcar '(lambda ( x ) (mapcar 'strlen x)) styles)
  )

  ;;---------------------------------------------------------------;;

  (cond
    (
      (not
        (and (setq l (_read file))
          (setq wf
            (open
              (setq outfile
                (strcat
                  (vl-string-translate "/" "\\" (vl-filename-directory file))
                  "\\" (vl-filename-base file) extn
                )
              )
              "w"
            )
          )
        )
      )
      (princ "\n** File Error **")
    )
    (t

      (setq lines (length l)
            ochrs 0
            ichrs (apply '+ (mapcar 'strlen l))
      )

      (setq s (car l) l (cdr l) str "" i 0)

      (write-line (car _container) wf) (setq ochrs (+ ochrs (car _containerL)))

      (while s
        (cond
          (
            (= (setq a (ascii s)) 0) ; Empty String
               
            (write-line str wf)

            (setq s (car l) l (cdr l) str "")
          )
          (
            (or (= a 32) (= a 9)) ; Space & Tab

            (setq str (strcat str (chr a)) s (substr s 2) ochrs (1+ ochrs) i 0)
          )
          (
            (or (= a 39) (= a 46)) ; ' or .

            (setq str (strcat str (car _quote) (chr a)) s (substr s 2) ochrs (+ ochrs (car _quoteL) 1))

            (while (= (ascii s) 39)
              (setq str (strcat str (substr s 1 1)) s (substr s 2) ochrs (1+ ochrs))
            )

            (setq str (strcat str (cadr _quote)) ochrs (+ ochrs (cadr _quoteL)))
          )
          (
            (< 38 a 42) ; Brackets

            (setq str (strcat str (car _bracket) (chr a)) ochrs (+ ochrs (car _bracketL) 1) s (substr s 2))

            (while (< 39 (ascii s) 42)
              (setq str (strcat str (substr s 1 1)) s (substr s 2) ochrs (1+ ochrs))
            )

            (setq str (strcat str (cadr _bracket)) ochrs (+ ochrs (cadr _bracketL)))
          )
          (
            (= a 59) ; Single or Multiline Comments

            (if (eq (substr s 2 1) "|")
              (progn
                (setq str (strcat str (car _mcomment)) ochrs (+ ochrs (car _mcommentL)))

                (while (and s (not (setq i (vl-string-search "|;" s))))
                  (write-line (strcat str (if <> (_fix<> s) s)) wf)
                  (setq ochrs (+ ochrs (strlen (strcat str s))) s (car l) l (cdr l) str "")
                )

                (setq str (strcat str (substr s 1 (+ 2 i)) (cadr _mcomment)) ochrs (+ ochrs (+ 2 i) (cadr _mcommentL)))
                    
                (setq s (substr s (+ 3 i)))
              )
              (progn
                (write-line (strcat str (car _scomment) (if <> (_fix<> s) s) (cadr _scomment)) wf)
                    
                (setq ochrs (+ ochrs (car _scommentL) (strlen s) (cadr _scommentL)) s (car l) l (cdr l) str "")
              )
            )
          )
          (
            (= a 34) ; Strings

            (setq str (strcat str (car _string)) ochrs (+ ochrs (car _stringL)) i 0 fl t)

            (while (and s fl)

              (while
                (and s (setq i (vl-string-search "\"" s (setq i (1+ i))))
                  (not
                    (or
                      (zerop i)
                      (not (eq (substr s i 1) "\\"))
                      (eq (substr s (1- i) 2) "\\\\")
                    )
                  )
                )
              )

              (if (null i)
                (setq str   (strcat str (if <> (_fix<> s) s) "\n") ochrs (+ ochrs (strlen s)) s (car l) l (cdr l) i -1)
                (setq str   (strcat str (if <> (_fix<> (substr s 1 (1+ i))) (substr s 1 (1+ i))) (cadr _string))
                      ochrs (+ ochrs (1+ i) (cadr _stringL))
                          s (substr s (+ 2 i)) fl nil i 0
                )
              )
            )
          )
          (
            (progn
              (setq i
                (apply (function min)
                  (vl-remove-if (function null)
                    (cons (strlen s)
                      (mapcar
                        (function
                          (lambda ( pat )
                            (vl-string-search pat s)
                          )
                        )
                       '("(" ")" " " "\t" "'" ";" "\"")
                      )
                    )
                  )
                )
                w (substr s 1 i)
                s (substr s (1+ i))
              )
              (or
                (eq (setq a (type (eval (read w)))) 'SUBR)
                (vl-position (strcase w) '("NIL" "T" "PI"))
                (and a (eq (type (read w)) 'SYM)
                  (not
                    (vl-position (strcase w t) exceptions)
                  )
                )
              )
            )

            (setq str   (strcat str (car _protected) (if <> (_fix<> w) w) (cadr _protected))
                  ochrs (+ ochrs (car _protectedL) i (cadr _protectedL))
            )
          )
          (
            (or (eq a 'USUBR) (vl-position (strcase w t) exceptions))

            (setq str (strcat str (if <> (_fix<> w) w)) ochrs (+ ochrs i))
          )
          (
            (eq a 'INT)

            (setq str (strcat str (car _integer) w (cadr _integer)) ochrs (+ ochrs (car _integerL) i (cadr _integerL)))
          )
          (
            (eq a 'REAL)

            (setq str (strcat str (car _real) w (cadr _real)) ochrs (+ ochrs (car _realL) i (cadr _realL)))
          )
          (t
            (setq str (strcat str (if <> (_fix<> w) w)) ochrs (+ ochrs i))
          )
        )

        (if (and s (= (ascii s) 0))
          (progn
            (write-line str wf)

            (setq s (car l) l (cdr l) str "" i 0)
          )
        )
      )

      (write-line (cadr _container) wf) (setq wf (close wf) ochrs (+ ochrs (cadr _containerL)))

      (princ
        (strcat
          (_PadBetween "\n" "" "-" 40)
          "\nConversion Report: Successful"
          (_PadBetween "\n" "" "-" 40)
          "\nInput File Statistics"
          (_PadBetween "\n" "" "-" 40)
              
          (_PadBetween "\nLines"      (itoa lines) "." 35) 
          (_PadBetween "\nCharacters" (itoa ichrs) "." 35)
              
          (_PadBetween "\n" "" "-" 40)

          "\nOutput File Statistics"
          (_PadBetween "\n" "" "-" 40)

          (_PadBetween "\nLines"      (itoa (+ 2 lines)) "." 35)
          (_PadBetween "\nCharacters" (itoa      ochrs)  "." 35)

          (_PadBetween "\n" "" "-" 40)
          "\n"
        )
      )

      (startapp "notepad" outfile)
    )
  )

  (princ)
)


;;------------------------------------------------------------;;
;;  Example calling Functions                                 ;;
;;------------------------------------------------------------;;

;; LISP 2 BBCode (for use in forums)

(defun c:lsp2bbc ( / file )
  ;; © Lee Mac 2011

  (cond
    (
      (not
        (setq file
          (getfiled "Select LISP File to Convert"
            (cond ( (getenv "LMAC_LISPStyler") ) ( "" )) "lsp" 16
          )
        )
      )
      (princ "\n*Cancel*")
    )
    (t
      (setenv "LMAC_LISPStyler" (vl-filename-directory file))
     
      (LM:LISPStyler file

       '(
          ("[code]"            "[/code]" )  ;; Container
          ("[color=DARKRED]"   "[/color]")  ;; Quotes/Dots
          ("[color=RED]"       "[/color]")  ;; Brackets
          ("[color=#990099]"   "[/color]")  ;; Multiline Comments
          ("[color=#990099]"   "[/color]")  ;; Single Comments
          ("[color=#a52a2a]"   "[/color]")  ;; Strings
          ("[color=BLUE]"      "[/color]")  ;; Protected Symbols
          ("[color=#009900]"   "[/color]")  ;; Integers
          ("[color=#009999]"   "[/color]")  ;; Reals
        )

        ".bbc"
        nil
      )
    )      
  )

  (princ)
)

;; LISP 2 HTML - for use on websites.

(defun c:lsp2html ( / file )
  ;; © Lee Mac 2011

  (cond
    (
      (not
        (setq file
          (getfiled "Select LISP File to Convert"
            (cond ( (getenv "LMAC_LISPStyler") ) ( "" )) "lsp" 16
          )
        )
      )
      (princ "\n*Cancel*")
    )
    (t
      (setenv "LMAC_LISPStyler" (vl-filename-directory file))
     
      (LM:LISPStyler file

       '(
          ("<pre>"                 "</pre>" )  ;; Container
          ("<span class=\"quot\">" "</span>")  ;; Quotes/Dots
          ("<span class=\"brkt\">" "</span>")  ;; Brackets
          ("<span class=\"cmt\">"  "</span>")  ;; Multiline Comments
          ("<span class=\"cmt\">"  "</span>")  ;; Single Comments
          ("<span class=\"str\">"  "</span>")  ;; Strings
          ("<span class=\"func\">" "</span>")  ;; Protected Symbols
          ("<span class=\"int\">"  "</span>")  ;; Integers
          ("<span class=\"rea\">"  "</span>")  ;; Reals
        )

        ".html"
        T
      )
    )      
  )

  (princ)
)

;;------------------------------------------------------------;;
;;                       End of File                          ;;
;;------------------------------------------------------------;;