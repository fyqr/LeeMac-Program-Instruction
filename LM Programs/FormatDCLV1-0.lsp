;;---------------------=={ Format DCL }==---------------------;;
;;                                                            ;;
;;  Reads a selected DCL file and creates a formatted version ;;
;;  of the dcl code in the same directory.                    ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Version 1.0    -    28-11-2011                            ;;
;;                                                            ;;
;;  First Release.                                            ;;
;;------------------------------------------------------------;;

(defun c:FormatDCL ( / _Read _PrinDCL _Main )
    
    (defun _Read ( file / _break line cpos alst )

        (defun _break ( line chrs / _breakit )

            (defun _breakit ( strn delm / cpos )
                (if (setq cpos (vl-string-search delm strn))
                    (vl-remove ""
                        (cons (substr strn 1 cpos)
                            (cons delm (_breakit (substr strn (+ 2 cpos)) delm))
                        )
                    )
                    (list strn)
                )
            )

            (if (cdr chrs)
                (apply 'append
                    (mapcar '(lambda ( x ) (_break x (cdr chrs))) (_breakit line (car chrs)))
                )
                (_breakit line (car chrs))
            )
        )
        
        (if (setq file (open file "r"))
            (progn
                (while (setq line (read-line file))
                    (cond
                        (   (setq cpos (vl-string-search "//" line))
                            (setq line (substr line 1 cpos))
                        )
                        (   (setq cpos (vl-string-search "/*" line))
                            (setq _str (substr line 1 cpos))
                            (while
                                (and line
                                    (not (setq cpos (vl-string-search "*/" line)))
                                )
                                (setq line (read-line file))
                            )
                            (if line
                                (setq str_ (substr line (+ 3 cpos)))
                                (setq str_ "")
                            )
                            (setq line (strcat _str str_))
                        )
                    )
                    (setq alst
                        (append alst
                            (vl-remove-if 'null
                                (mapcar
                                    (function
                                        (lambda ( x )
                                            (setq x (vl-string-trim " " x))
                                            (cond
                                                (   (member x '("{" "}"))
                                                    x
                                                )
                                                (   (< 0 (strlen x))
                                                    (vl-prin1-to-string (vl-string-trim " " x))
                                                )
                                            )
                                        )
                                    )
                                    (_break line '("{" "}"))
                                )
                            )
                        )
                    )
                )
                (setq file (close file))
            )
        )
        alst
    )

    ;; _PrinDcl by MP - (modified to print to file)
    ;; http://www.theswamp.org/index.php?topic=10398.msg132473#msg132473

    (defun _PrinDcl ( x f / _PrintItem _PrintList _Main file )

        (defun _PrintItem ( _PrintMethod item indents )
            (cond
                (   item
                    (princ "\n" file)
                    (repeat indents (princ "    " file))
                    (_PrintMethod item file)
                )
                (   (princ " { }" file)  )
            )    
            (princ)
        )
       
        (defun _PrintList ( _PrintMethod lst indents )
            (if (< -1 indents) (_PrintItem _PrintMethod "{" indents))
            ((lambda ( i ) (foreach x lst (_Main x i))) (1+ indents))
            (if (< -1 indents) (_PrintItem _PrintMethod "}" indents) (princ))
        )
       
        (defun _Main ( x indents )   
            (if (vl-consp x)
                (if ((lambda ( x ) (and x (atom x))) (cdr x))
                    (_PrintItem princ x indents)
                    (_PrintList princ x indents)
                )   
                (_PrintItem princ x indents)
            )
        )

        (cond
            (   (setq file (open f "w"))
                (_Main x -1)
                (close file)
                (startapp "notepad" f)
            )
        )
    )

    (defun _Main ( / file )
        (if (setq file (getfiled "Select DCL File" "" "dcl" 16))
            (_PrinDCL
                (read
                    (strcat "(" (vl-string-translate "{}" "()" (apply 'strcat (_Read file))) ")")
                )
                (vl-filename-mktemp
                    (vl-filename-base      file)
                    (vl-filename-directory file)
                    (vl-filename-extension file)
                )
            )
            (princ "\n*Cancel*")
        )
        (princ)
    )

    (_Main)
)
(princ)