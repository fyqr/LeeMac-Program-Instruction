;;-------------------=={ Length Field to Attribute }==------------------;;
;;                                                                      ;;
;;  This program allows a user to populate a selected attribute with    ;;
;;  a Field Expression referencing the length or perimeter (or sum of   ;;
;;  lengths or perimeters), of one or more objects.                     ;;
;;                                                                      ;;
;;  Upon issuing the command syntax 'L2A' the user is prompted to       ;;
;;  make a selection of objects for which to retrieve the length; if    ;;
;;  more than one object is selected, the cumulative length of all      ;;
;;  objects will be displayed by the resultant Field Expression.        ;;
;;                                                                      ;;
;;  Following object selection, the user is then prompted to select     ;;
;;  either an attributed block or an attribute in which to house the    ;;
;;  Field Expression. The Field will display the sum of the lengths of  ;;
;;  the selected objects, formatted using the Field formatting code     ;;
;;  specified at the top of the program.                                ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2017  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2017-03-18                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;

(defun c:l2a ( / *error* ats att enx fmt idx inc lst obj prp sel tag tmp )

    (setq fmt "%lu6" ;; Field Formatting
          tag nil    ;; Optional predefined attribute tag
    )
    
    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (setq prp
       '(
            ("AcDbArc"        . "ArcLength")
            ("AcDbCircle"     . "Circumference")
            ("AcDbLine"       . "Length")
            ("AcDbPolyline"   . "Length")
            ("AcDb2dPolyline" . "Length")
            ("AcDb3dPolyline" . "Length")
        )
    )

    (if
        (and
            (setq sel
                (ssget
                   '(   (000 . "ARC,CIRCLE,LINE,*POLYLINE")
                        (-04 . "<NOT")
                            (-04 . "<AND")
                                (000 . "POLYLINE") (-04 . "&") (070 . 80)
                            (-04 . "AND>")
                        (-04 . "NOT>")
                    )
                )
            )
            (progn
                (while
                    (progn (setvar 'errno 0) (setq ats (nentsel "\nSelect attribute or attributed block: "))
                        (cond
                            (   (= 7 (getvar 'errno))
                                (princ "\nMissed, try again.")
                            )
                            (   (null ats)
                                nil
                            )
                            (   (and (= "ATTRIB" (cdr (assoc 0 (setq enx (entget (car ats))))))
                                     (/= 'str (type tag))
                                )
                                (setq att (vlax-ename->vla-object (car ats)))
                                nil
                            )
                            (   (and
                                    (or
                                        (and (= "ATTRIB" (cdr (assoc 0 enx)))
                                             (setq tmp (cdr (assoc 330 enx)))
                                        )
                                        (and (setq tmp  (last (cadddr ats)))
                                             (= "INSERT" (cdr (assoc 0 (entget tmp))))
                                        )
                                    )
                                    (setq tmp (vlax-invoke (vlax-ename->vla-object tmp) 'getattributes))
                                )
                                (not
                                    (or
                                        (and (= 'str (type tag))
                                             (setq idx (vl-position (strcase tag) (mapcar 'vla-get-tagstring tmp)))
                                             (setq att (nth idx tmp))
                                        )
                                        (and (not (cdr tmp))
                                             (setq att (car tmp))
                                        )
                                        (and (setq idx (LM:listbox "Choose Attribute" (mapcar 'vla-get-tagstring tmp) 2))
                                             (setq att (nth (car idx) tmp))
                                        )
                                    )
                                )
                            )
                            (   (princ "\nThe selected object is not an attribute or attributed block."))
                        )
                    )
                )
                (= 'vla-object (type att))
            )
        )
        (progn
            (LM:startundo (LM:acdoc))
            (if (= 1 (sslength sel))
                (progn
                    (setq obj (vlax-ename->vla-object (ssname sel 0)))
                    (vla-put-textstring att
                        (strcat
                            "%<\\AcObjProp Object(%<\\_ObjId "
                            (LM:objectid obj)
                            ">%)." (cdr (assoc (vla-get-objectname obj) prp)) " \\f \"" fmt "\">%"
                        )
                    )
                )
                (progn
                    (repeat (setq inc (sslength sel))
                        (setq obj (vlax-ename->vla-object (ssname sel (setq inc (1- inc))))
                              lst
                            (vl-list*
                                "%<\\AcObjProp Object(%<\\_ObjId "
                                (LM:objectid obj)
                                ">%)." (cdr (assoc (vla-get-objectname obj) prp)) ">%" " + "
                                lst
                            )
                        )
                    )
                    (vla-put-textstring att
                        (strcat
                            "%<\\AcExpr "
                            (apply 'strcat (reverse (cdr (reverse lst))))
                            " \\f \"" fmt "\">%"
                        )
                    )
                )
            )
            (vl-cmdf "_.updatefield" (vlax-vla-object->ename att) "")
            (LM:endundo (LM:acdoc))
        )
    )
    (princ)
)

;; List Box  -  Lee Mac
;; Displays a DCL list box allowing the user to make a selection from the supplied data.
;; msg - [str] Dialog label
;; lst - [lst] List of strings to display
;; bit - [int] 1=allow multiple; 2=return indexes
;; Returns: [lst] List of selected items/indexes, else nil
 
(defun LM:listbox ( msg lst bit / dch des tmp rtn )
    (cond
        (   (not
                (and
                    (setq tmp (vl-filename-mktemp nil nil ".dcl"))
                    (setq des (open tmp "w"))
                    (write-line
                        (strcat "listbox:dialog{label=\"" msg "\";spacer;:list_box{key=\"list\";multiple_select="
                            (if (= 1 (logand 1 bit)) "true" "false") ";width=50;height=15;}spacer;ok_cancel;}"
                        )
                        des
                    )
                    (not (close des))
                    (< 0 (setq dch (load_dialog tmp)))
                    (new_dialog "listbox" dch)
                )
            )
            (prompt "\nError Loading List Box Dialog.")
        )
        (   t     
            (start_list "list")
            (foreach itm lst (add_list itm))
            (end_list)
            (setq rtn (set_tile "list" "0"))
            (action_tile "list" "(setq rtn $value)")
            (setq rtn
                (if (= 1 (start_dialog))
                    (if (= 2 (logand 2 bit))
                        (read (strcat "(" rtn ")"))
                        (mapcar '(lambda ( x ) (nth x lst)) (read (strcat "(" rtn ")")))
                    )
                )
            )
        )
    )
    (if (< 0 dch)
        (unload_dialog dch)
    )
    (if (and tmp (setq tmp (findfile tmp)))
        (vl-file-delete tmp)
    )
    rtn
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
        "\n:: LengthFieldToAttribute.lsp | Version 1.0 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"L2A\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;