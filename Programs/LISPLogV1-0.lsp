;;--------------------=={ Log LISP Usage }==------------------;;
;;                                                            ;;
;;  Automatically records daily LISP command usage to a CSV   ;;
;;  log file, stored in the folder indicated at the top of    ;;
;;  the code.                                                 ;;
;;                                                            ;;
;;  To enable automatic logging, load this LISP file on       ;;
;;  startup (recommend using the ACADDOC.lsp).                ;;
;;                                                            ;;
;;  To enable or disable LISP command logging at any time,    ;;
;;  type at the command-line 'LispLogON' or 'LispLogOFF'      ;;
;;  respectively.                                             ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Version 1.0    -    07-11-2011                            ;;
;;------------------------------------------------------------;;

;;------------------------------------------------------------;;
;;  Log File Save Folder                                      ;;
;;------------------------------------------------------------;;
;;  LISP Log files will be saved to this folder.              ;;
;;  The folder will be created if it doesn't exist.           ;;
;;------------------------------------------------------------;;

(setq *lisp-log-folder* "C:\\LISP-Logs")

;;------------------------------------------------------------;;

(defun c:LispLogON nil
    (if (null *log-lisp-reactor*)
        (setq *log-lisp-reactor* (vlr-lisp-reactor "LISP-Log" '((:vlr-lispwillstart . lisplog:lispstarted))))
    )
    (if (null *log-save-reactor*)
        (setq *log-save-reactor* (vlr-editor-reactor "LISP-Log" '((:vlr-beginsave . lisplog:savelisplogs))))
    )
    (princ "\nLISP Logging Enabled.")
    (princ)
)

;;------------------------------------------------------------;;

(defun c:LispLogOFF nil
    (if *log-lisp-reactor*
        (progn (vlr-remove *log-lisp-reactor*) (setq *log-lisp-reactor* nil))
    )
    (if *log-save-reactor*
        (progn (vlr-remove *log-save-reactor*) (setq *log-save-reactor* nil))
    )
    (setq *lisp-log-list* nil)
    (vl-propagate '*lisp-log-list*)
    (princ "\nLISP Logging Disabled.")
    (princ)
)

;;------------------------------------------------------------;;

(defun lisplog:lispstarted ( reactor params )
    (if
        (and
            (wcmatch (setq params (strcase (car params))) "(C:*")
            (not (member params '("(C:LISPLOGON)" "(C:LISPLOGOFF)")))
        )
        (progn
            (setq *lisp-log-list*
                (LM:nAssoc++
                    (list
                        (strcat (getvar 'DWGPREFIX) (getvar 'DWGNAME))
                        (substr (vl-string-trim "()" params) 3)
                    )
                    *lisp-log-list*
                )
            )
            (vl-propagate '*lisp-log-list*)
        )
    )
    (princ)
)

;;------------------------------------------------------------;;

(defun lisplog:savelisplogs ( reactor params / *error* directory existing filename file )

    (defun *error* ( msg )
        (if (and file (eq 'FILE (type file))) (setq file (close file)))
        (if (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    
    (if *lisp-log-list*
        (progn
            (if *lisp-log-folder*
                (setq *lisp-log-folder* (vl-string-right-trim "\\" (vl-string-translate "/" "\\" *lisp-log-folder*)))
                (setq *lisp-log-folder* (vl-string-right-trim "\\" (getvar 'DWGPREFIX)))
            )
            (setq directory (strcat *lisp-log-folder* "\\" (LM:Date "MONTH YYYY"))
                  filename  (strcat directory "\\Log_" (LM:Date "YYYYMODD") ".csv")
            )
            (if (null (vl-file-directory-p directory))
                (LM:CreateDirectory directory)
            )
            (if (findfile filename)
                (setq existing (LM:ReadLog filename))
            )
            (if (setq file (open filename "w"))
                (progn
                    (if existing
                        (setq *lisp-log-list* (LM:MergeLists *lisp-log-list* existing))
                    )
                    (foreach dwg (vl-sort *lisp-log-list* '(lambda ( a b ) (< (car a) (car b))))
                        (write-line (car dwg) file)
                        (foreach cmd (vl-sort (cdr dwg) '(lambda ( a b ) (> (cadr a) (cadr b))))
                            (write-line (strcat (car cmd) "," (itoa (cadr cmd))) file)
                        )
                        (write-line "" file)
                    )
                    (setq file (close file))
                    (setq *lisp-log-list* nil)
                    (vl-propagate '*lisp-log-list*)
                )
                (princ "\nUnable to write LISP Log - Check that the Log file is not in use.")
            )
        )
    )
    (princ)
)

;;------------------------------------------------------------;;

(defun LM:nAssoc++ ( key lst / pair )
    (if key
        (if (setq pair (assoc (car key) lst))
            (subst (cons (car key) (LM:nAssoc++ (cdr key) (cdr pair))) pair lst)
            (cons  (cons (car key) (LM:nAssoc++ (cdr key) nil)) lst)
        )
        (if lst (list (1+ (car lst))) '(1))
    )
)

;;------------------------------------------------------------;;

(defun LM:Date ( format )
    (menucmd (strcat "m=$(edtime,$(getvar,DATE)," format ")"))
)

;;------------------------------------------------------------;;

(defun LM:CreateDirectory ( dir / CreateDirectory folders )

    (defun CreateDirectory ( root folders )
        (if folders
            (   (lambda ( dir ) (vl-mkdir dir) (CreateDirectory dir (cdr folders)))
                (strcat root "\\" (car folders))
            )
        )
    )
  
    (if (setq folders (LM:str->lst (vl-string-translate "/" "\\" dir) "\\"))
        (CreateDirectory (car folders) (cdr folders))
    )
    (vl-file-directory-p dir)
)

;;------------------------------------------------------------;;

(defun LM:str->lst ( str del / pos )
    (if (setq pos (vl-string-search del str))
        (vl-remove "" (cons (substr str 1 pos) (LM:str->lst (substr str (+ pos 1 (strlen del))) del)))
        (list str)
    )
)

;;------------------------------------------------------------;;

(defun LM:ReadLog ( filename / file line lst sub1 sub2 )
    (if (setq file (open filename "r"))
        (progn
            (while (setq line (read-line file))
                (cond
                    (   (eq "" line)
                    )
                    (   (= 1 (length (setq line (LM:str->lst line ","))))
                        (if (and sub1 sub2)
                            (setq lst (cons (cons sub1 sub2) lst)
                                  sub1 nil
                                  sub2 nil
                            )
                        )
                        (setq sub1 (car line))
                    )
                    (   (= 2 (length line))
                        (setq sub2 (cons (list (car line) (atoi (cadr line))) sub2))
                    )
                )
            )
            (if (and sub1 sub2)
                (setq lst (cons (cons sub1 sub2) lst))
            )
            (setq file (close file))
            lst
        )
    )
)

;;------------------------------------------------------------;;

(defun LM:MergeLists ( l1 l2 / items item )
    (foreach group l2
        (if (setq items (cdr (assoc (car group) l1)))
            (progn
                (foreach pair (cdr group)
                    (if (setq item  (assoc (car pair) items))
                        (setq items (subst (list (car pair) (+ (cadr pair) (cadr item))) item items))
                        (setq items (cons pair items))
                    )
                )
                (setq l1 (subst (cons (car group) items) (assoc (car group) l1) l1))
            )
            (setq l1 (cons group l1))
        )
    )
    l1
)

;;------------------------------------------------------------;;

(vl-load-com) (c:LispLogON) (princ)

;;------------------------------------------------------------;;
;;                         End of File                        ;;
;;------------------------------------------------------------;;