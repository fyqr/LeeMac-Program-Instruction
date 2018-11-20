;;------------------------=={ Field Arithmetic }==----------------------;;
;;                                                                      ;;
;;  -----------------------------------                                 ;;
;;  Program Overview                                                    ;;
;;  -----------------------------------                                 ;;
;;                                                                      ;;
;;  This program enables the user to perform arithmetic operations      ;;
;;  (add, subtract, multiply, divide) on numerical text or fields,      ;;
;;  with the result of the calculation represented using a field        ;;
;;  expression.                                                         ;;
;;                                                                      ;;
;;  Upon issuing the command syntax 'fieldmath' at the command-line,    ;;
;;  the program prompts the user to select a text object with           ;;
;;  numerical content. At this prompt, the user may select a            ;;
;;  single-line text object (DText), multiline text (MText), block      ;;
;;  attribute, multileader (MLeader), or dimension containing numerical ;;
;;  content, or choose the 'Constant' option to enter an arbitrary      ;;
;;  fixed number for use in the calculation.                            ;;
;;                                                                      ;;
;;  The program then prompts the user to choose an operator (addition,  ;;
;;  subtraction, multiplication, division) to follow the selected or    ;;
;;  entered numerical value in the calculation. At this prompt, the     ;;
;;  user is also provided with the option to 'Undo' the last numerical  ;;
;;  value added to the calculation, or to complete the calculation by   ;;
;;  choosing the 'Result' option.                                       ;;
;;                                                                      ;;
;;  Upon selecting the 'Result' option, the program prompts the user    ;;
;;  for an insertion point and then generates a multiline text (MText)  ;;
;;  object containing a field expression representing the result of     ;;
;;  the calculation.                                                    ;;
;;                                                                      ;;
;;  -----------------------------------                                 ;;
;;  Nested Field Expressions                                            ;;
;;  -----------------------------------                                 ;;
;;                                                                      ;;
;;  The program also supports nested field expressions: that is,        ;;
;;  annotation objects which themselves contain field expressions       ;;
;;  referencing numerical values.                                       ;;
;;                                                                      ;;
;;  When encountering such objects, by default the program references   ;;
;;  the nested field expression directly in the calculation (ignoring   ;;
;;  any field formatting), as opposed to referencing the text content   ;;
;;  of the object containing the field expression.                      ;;
;;                                                                      ;;
;;  This default behaviour may be toggled by changing the 'nst'         ;;
;;  parameter within the 'Program Parameters' section of the program    ;;
;;  source code below.                                                  ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2017  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2017-05-14                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2017-06-13                                      ;;
;;                                                                      ;;
;;  - Updated LM:fieldcode function to account for field expressions    ;;
;;    greater than 250 characters in length.                            ;;
;;----------------------------------------------------------------------;;

(defun c:fieldmath ( / *error* fmt ini ins itm lst msg nst opr tmp )

    (setq

;;----------------------------------------------------------------------;;
;;                          Program Parameters                          ;;
;;----------------------------------------------------------------------;;

        ;; Field formatting for result ("" for no formatting)
        fmt "%lu6%qf1"

        ;; Reference nested fields directly (t=Yes/nil=No)
        nst t

;;----------------------------------------------------------------------;;

    ) 

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (LM:startundo (LM:acdoc))
    (if (car (setq lst (list (fieldmath:selectnumericalcontent "Exit" nst))))
        (progn
            (while
                (progn
                    (princ (apply 'strcat (cons "\n" (reverse (mapcar 'cadr lst)))))
                    (if (cddr lst)
                        (setq ini "Add Subtract Multiply Divide Undo Result"
                              msg "\nSpecify operator [Add/Subtract/Multiply/Divide] or [Undo/Result] <"
                        )
                        (setq ini "Add Subtract Multiply Divide Result"
                              msg "\nSpecify operator [Add/Subtract/Multiply/Divide] or [Result] <"
                    	)
                    )
                    (initget ini)
                    (setq opr (cond ((getkword (strcat msg (cond (opr) ("Exit")) ">: "))) (opr)))
                    (cond
                        (   (or (null opr) (= "Result" opr)) nil)
                        (   (= "Undo" opr)
                            (setq lst (cddr lst))
                        )
                        (   (setq itm (fieldmath:selectnumericalcontent "Back" nst))
                            (setq tmp (cdr (assoc opr '(("Add" . " + ") ("Subtract" . " - ") ("Multiply" . " * ") ("Divide" . " / "))))
                                  lst (vl-list* itm (list tmp tmp) lst)
                            )
                        )
                    )
                )
            )
            (if (setq ins (getpoint "\nSpecify insertion point for mtext <exit>: "))
                (vla-addmtext
                    (vlax-get-property (LM:acdoc) (if (= 1 (getvar 'cvport)) 'paperspace 'modelspace))
                    (vlax-3D-point (trans ins 1 0))
                    0.0
                    (if (cddr lst)
                        (strcat "%<\\AcExpr (" (apply 'strcat (reverse (mapcar 'car lst))) ")" (if (= "" fmt) "" (strcat " \\f \"" fmt "\"")) ">%")
                        (if (= "" fmt)
                            (caar lst)
                            (strcat (substr (caar lst) 1 (- (strlen (caar lst)) 2)) " \\f \"" fmt "\">%")
                        )
                    )
                )
            )
        )
    )
    (LM:endundo (LM:acdoc))
    (princ)
)

;;----------------------------------------------------------------------;;

(defun fieldmath:selectnumericalcontent ( bck nst / ent fld num rtn sel tmp typ )
    (while
        (not
            (progn
                (setvar 'errno 0)
                (initget (strcat  "Constant " bck))
                (setq sel (nentsel (strcat "\nSelect an object with numerical content [Constant/" bck "] <" bck ">: ")))
                (cond
                    (   (= 7 (getvar 'errno))
                        (prompt "\nMissed, try again.")
                    )
                    (   (or (null sel) (= bck sel)))
                    (   (= "Constant" sel)
                        (initget (strcat "Object " bck))
                        (cond
                            (   (null (setq tmp (getreal (strcat "\nEnter a number [Object/" bck "] <" bck ">: ")))))
                            (   (= bck tmp))
                            (   (= "Object" tmp) nil)
                            (   (setq tmp (LM:num->str tmp)
                                      rtn (list tmp tmp)
                                )
                            )
                        )
                    )
                    (   (progn
                            (if (= 4 (length sel))
                                (setq ent (last (last sel)))
                                (setq ent (car sel))
                            )
                            (not (wcmatch (setq typ (cdr (assoc 0 (entget ent)))) "TEXT,MTEXT,ATTRIB,MULTILEADER,*DIMENSION"))
                        )
                        (prompt "\nPlease select a Text, MText, Attribute, Multileader or Dimension.")
                    )
                    (   (not
                            (setq num
                                (LM:numericalfield
                                    (setq fld
                                        (cond
                                            (   (and nst (setq tmp (LM:fieldcode ent)))
                                                (LM:removefieldformatting tmp)
                                            )
                                            (   (strcat
                                                    "%<\\AcObjProp Object(%<\\_ObjId "
                                                    (LM:objectid (vlax-ename->vla-object ent))
                                                    ">%)." (if (wcmatch typ "*DIMENSION") "Measurement" "TextString") ">%"
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                        (prompt "\nThe selected object does not contain numerical content.")
                    )
                    (   (setq rtn (list fld (LM:num->str num))))
                )
            )
        )
    )
    rtn
)

;; Number to String  -  Lee Mac
;; Converts a supplied numerical argument to a string

(defun LM:num->str ( num / dim rtn )
    (if (equal num (atoi (rtos num 2 0)) 1e-8)
        (rtos num 2 0)
        (progn
            (setq dim (getvar 'dimzin))
            (setvar 'dimzin 8)
            (setq rtn (rtos num 2 8))
            (setvar 'dimzin dim)
            rtn
        )
    )
)

;; Numerical Field  -  Lee Mac
;; Returns the numerical content described by a supplied field expression

(defun LM:numericalfield ( fld / obj rtn )
    (vl-catch-all-apply
       '(lambda nil
            (setq obj (vla-addmtext (vla-get-modelspace (LM:acdoc)) (vlax-3D-point 0 0) 0.0 fld)
                  rtn (distof (vla-get-textstring obj) 2)
            )
        )
    )
    (if (= 'vla-object (type obj)) (vla-delete obj))
    rtn
)

;; Remove Field Formatting  -  Lee Mac
;; Removes all formatting codes from a field expression

(defun LM:removefieldformatting ( fld / ps1 ps2 )
    (if
        (and
            (setq ps1 (vl-string-search " \\f \"" fld))
            (setq ps2 (vl-string-search "\">%" (substr fld (+ 6 ps1))))
        )
        (strcat (substr fld 1 ps1) ">%" (LM:removefieldformatting (substr fld (+ 9 ps1 ps2))))
        fld
    )
)

;; Field Code  -  Lee Mac
;; Returns the field expression associated with an entity

(defun LM:fieldcode ( ent / replacefield replaceobject fieldstring enx )

    (defun replacefield ( str enx / ent fld pos )
        (if (setq pos (vl-string-search "\\_FldIdx" (setq str (replaceobject str enx))))
            (progn
                (setq ent (assoc 360 enx)
                      fld (entget (cdr ent))
                )
                (strcat
                    (substr str 1 pos)
                    (replacefield (fieldstring fld) fld)
                    (replacefield (substr str (1+ (vl-string-search ">%" str pos))) (cdr (member ent enx)))
                )
            )
            str
        )
    )

    (defun replaceobject ( str enx / ent pos )
        (if (setq pos (vl-string-search "ObjIdx" str))
            (strcat
                (substr str 1 (+ pos 5)) " "
                (LM:ObjectID (vlax-ename->vla-object (cdr (setq ent (assoc 331 enx)))))
                (replaceobject (substr str (1+ (vl-string-search ">%" str pos))) (cdr (member ent enx)))
            )
            str
        )
    )

    (defun fieldstring ( enx / itm )
        (if (setq itm (assoc 3 enx))
            (strcat (cdr itm) (fieldstring (cdr (member itm enx))))
            (cond ((cdr (assoc 2 enx))) (""))
        )
    )
    
    (if (and (wcmatch  (cdr (assoc 0 (setq enx (entget ent)))) "TEXT,MTEXT,ATTRIB,MULTILEADER,*DIMENSION")
             (setq enx (cdr (assoc 360 enx)))
             (setq enx (dictsearch enx "ACAD_FIELD"))
             (setq enx (dictsearch (cdr (assoc -1 enx)) "TEXT"))
        )
        (replacefield (fieldstring enx) enx)
    )
)

;; ObjectID  -  Lee Mac
;; Returns a string containing the ObjectID of a supplied VLA-Object
;; Compatible with 32-bit & 64-bit systems

(defun LM:objectid ( obj )
    (eval
        (list 'defun 'LM:objectid '( obj )
            (if (wcmatch (getenv "PROCESSOR_ARCHITECTURE") "*64*")
                (if (vlax-method-applicable-p (vla-get-utility (LM:acdoc)) 'getobjectidstring)
                    (list 'vla-getobjectidstring (vla-get-utility (LM:acdoc)) 'obj ':vlax-false)
                   '(LM:ename->objectid (vlax-vla-object->ename obj))
                )
               '(itoa (vla-get-objectid obj))
            )
        )
    )
    (LM:objectid obj)
)

;; Entity Name to ObjectID  -  Lee Mac
;; Returns the 32-bit or 64-bit ObjectID for a supplied entity name

(defun LM:ename->objectid ( ent )
    (LM:hex->decstr
        (setq ent (vl-string-right-trim ">" (vl-prin1-to-string ent))
              ent (substr ent (+ (vl-string-position 58 ent) 3))
        )
    )
)

;; Hex to Decimal String  -  Lee Mac
;; Returns the decimal representation of a supplied hexadecimal string

(defun LM:hex->decstr ( hex / foo bar )
    (defun foo ( lst rtn )
        (if lst
            (foo (cdr lst) (bar (- (car lst) (if (< 57 (car lst)) 55 48)) rtn))
            (apply 'strcat (mapcar 'itoa (reverse rtn)))
        )
    )
    (defun bar ( int lst )
        (if lst
            (if (or (< 0 (setq int (+ (* 16 (car lst)) int))) (cdr lst))
                (cons (rem int 10) (bar (/ int 10) (cdr lst)))
            )
            (bar int '(0))
        )
    )
    (foo (vl-string->list (strcase hex)) nil)
)

;; Start Undo  -  Lee Mac
;; Opens an Undo Group.

(defun LM:startundo ( doc )
    (LM:endundo doc)
    (vla-startundomark doc)
)

;; End Undo  -  Lee Mac
;; Closes an Undo Group.

(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;; Active Document  -  Lee Mac
;; Returns the VLA Active Document Object

(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)

;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: FieldArithmetic.lsp | Version 1.1 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"fieldmath\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;