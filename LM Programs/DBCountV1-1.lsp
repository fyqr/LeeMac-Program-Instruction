;;--------------------=={ Dynamic Block Counter }==---------------------;;
;;                                                                      ;;
;;  This program enables the user to count all or a selection of        ;;
;;  standard blocks, dynamic blocks & xrefs in the current layout of a  ;;
;;  drawing.                                                            ;;
;;                                                                      ;;
;;  For every dynamic block with a Visibility Parameter, the program    ;;
;;  will also output the number of blocks using each Visibility State.  ;;
;;                                                                      ;;
;;  The results are printed to the command-line and may also be         ;;
;;  written to a Text or CSV file automatically created in the working  ;;
;;  directory with the same filename as the active drawing.             ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2013  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2013-11-27                                      ;;
;;----------------------------------------------------------------------;;

(defun c:dbcount ( / *error* all bln del des idx lst obj ofn out sel vis vsl )

    (defun *error* ( msg )
        (if (= 'file (type des)) (close des))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    
    (cond
        (   (null
                (setq all
                    (ssget "_X"
                        (list
                           '(0 . "INSERT")
                            (if (= 1 (getvar 'cvport))
                                (cons 410 (getvar 'ctab))
                               '(410 . "Model")
                            )
                        )
                    )
                )
            )
            (princ "\nNo blocks found in the current layout.")
        )
        (   (progn
                (setvar 'nomutt 1)
                (princ "\nSelect blocks to count <all>: ")
                (setq sel
                    (cond
                        (   (null (setq sel (vl-catch-all-apply 'ssget '(((0 . "INSERT"))))))
                            all
                        )
                        (   (null (vl-catch-all-error-p sel))
                            sel
                        )
                    )
                )
                (setvar 'nomutt 0)
                sel
            )
            (repeat (setq idx (sslength sel))
                (setq lst
                    (LM:nassoc++
                        (cons (setq bln (LM:blockname (setq obj (vlax-ename->vla-object (ssname sel (setq idx (1- idx)))))))
                            (if
                                (and
                                    (setq vis
                                        (cdr
                                            (cond
                                                (   (assoc bln vsl))
                                                (   (car (setq vsl (cons (cons bln (LM:getvisibilityparametername obj)) vsl))))
                                            )
                                        )
                                    )
                                    (setq vis
                                        (vl-some
                                           '(lambda ( x )
                                                (if (= vis (vla-get-propertyname x))
                                                    (vlax-get x 'value)
                                                )
                                            )
                                            (vlax-invoke obj 'getdynamicblockproperties)
                                        )
                                    )
                                )
                                (list vis)
                            )
                        )
                        lst
                    )
                )
            )
            (princ (LM:padbetween "\n" "" "=" 46))
            (princ (LM:padbetween "\n Block" "Count" "." 46))
            (princ (LM:padbetween "\n" "" "=" 46))
            (foreach blk (setq lst (vl-sort lst '(lambda ( a b ) (< (car a) (car b)))))
                (cond
                    (   (listp (cadr blk))
                        (princ (LM:padbetween (strcat "\n " (car blk)) (itoa (apply '+ (mapcar 'cadr (cdr blk)))) "." 46))
                        (foreach vis (cdr blk)
                            (princ (LM:padbetween (strcat "\n    " (car vis)) (itoa (cadr vis)) "." 46))
                        )
                    )
                    (   (princ (LM:padbetween (strcat "\n " (car blk)) (itoa (cadr blk)) "." 46))   )
                )
                (princ (LM:padbetween "\n" "" "-" 46))
            )
            (princ (LM:padbetween "\r" "" "=" 46))
            (textpage)

            (initget "TXT CSV")
            (if
                (and
                    (setq out (getkword "\nOutput results to [TXT/CSV] <exit>: "))
                    (setq ofn (LM:uniquefilename (strcat (getvar 'dwgprefix) (vl-filename-base (getvar 'dwgname))) (strcat "." (strcase out t))))
                )
                (if (setq des (open ofn "w"))
                    (progn
                        (setq del (if (= "TXT" out) "\t" ","))
                        (write-line (strcat "Block" del del "Count") des)
                        (foreach blk lst
                            (cond
                                (   (listp (cadr blk))
                                    (write-line (strcat (car blk) del del (itoa (apply '+ (mapcar 'cadr (cdr blk))))) des)
                                    (foreach vis (cdr blk)
                                        (write-line (strcat del (car vis) del (itoa (cadr vis))) des)
                                    )
                                )
                                (   (write-line (strcat (car blk) del del (itoa (cadr blk))) des))
                            )
                        )
                        (setq des (close des))
                        (startapp "explorer" ofn)
                    )
                    (princ (strcat "\nUnable to open \"" ofn "\" for writing."))
                )
            )
            (graphscr)
        )
    )
    (princ)
)

;; Unique Filename  -  Lee Mac
;; Returns a filename suffixed for uniqueness

(defun LM:uniquefilename ( pth ext / fnm tmp )
    (if (findfile (setq fnm (strcat pth ext)))
        (progn
            (setq tmp 1)
            (while (findfile (setq fnm (strcat pth "(" (itoa (setq tmp (1+ tmp))) ")" ext))))
        )
    )
    fnm
)

;; Block Name  -  Lee Mac
;; Returns the true (effective) name of a supplied block reference
                        
(defun LM:blockname ( obj )
    (if (vlax-property-available-p obj 'effectivename)
        (defun LM:blockname ( obj ) (vla-get-effectivename obj))
        (defun LM:blockname ( obj ) (vla-get-name obj))
    )
    (LM:blockname obj)
)

;; Nested Assoc++  -  Lee Mac
;; Increments the value of a key in an association list with possible
;; nested structure, or adds the set of keys to the list if not present.
;; key - [lst] List of keys
;; lst - [lst] Association list or nil
;; Returns: [lst] Association list with key incremented or added

(defun LM:nassoc++ ( key lst / itm )
    (if key
        (if (setq itm (assoc (car key) lst))
            (subst (cons (car key) (LM:nassoc++ (cdr key) (cdr itm))) itm lst)
            (cons  (cons (car key) (LM:nassoc++ (cdr key) nil)) lst)
        )
        (if lst (list (1+ (car lst))) '(1))
    )
)

;; Pad Between Strings  -  Lee Mac
;; Returns a string of a minimum specified length which is the concatenation
;; of two supplied strings padded to a desired length using a supplied character.
;; s1,s2 - [str] Strings to be concatenated
;; ch    - [str] Single character for padding
;; ln    - [int] Minimum length of returned string
;; Returns: [str] Concatenation of s1,s2 padded to a minimum length

(defun LM:padbetween ( s1 s2 ch ln )
    (
        (lambda ( a b c )
            (repeat (- ln (length b) (length c)) (setq c (cons a c)))
            (vl-list->string (append b c))
        )
        (ascii ch)
        (vl-string->list s1)
        (vl-string->list s2)
    )
)

;; Get Visibility Parameter Name  -  Lee Mac
;; Returns the name of the Visibility Parameter of a Dynamic Block (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; Returns: [str] Name of Visibility Parameter, else nil

(defun LM:getvisibilityparametername ( blk / vis )  
    (if
        (and
            (vlax-property-available-p blk 'effectivename)
            (setq blk
                (vla-item
                    (vla-get-blocks (vla-get-document blk))
                    (vla-get-effectivename blk)
                )
            )
           ;(= :vlax-true (vla-get-isdynamicblock blk)) to account for NUS dynamic blocks
            (= :vlax-true (vla-get-hasextensiondictionary blk))
            (setq vis
                (vl-some
                   '(lambda ( pair )
                        (if
                            (and
                                (= 360 (car pair))
                                (= "BLOCKVISIBILITYPARAMETER" (cdr (assoc 0 (entget (cdr pair)))))
                            )
                            (cdr pair)
                        )
                    )
                    (dictsearch
                        (vlax-vla-object->ename (vla-getextensiondictionary blk))
                        "ACAD_ENHANCEDBLOCK"
                    )
                )
            )
        )
        (cdr (assoc 301 (entget vis)))
    )
)

;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: DBCount.lsp | Version 1.1 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"DBCount\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;