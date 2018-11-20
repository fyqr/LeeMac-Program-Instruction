;;---------------------=={ Sum Attribute Values }==---------------------;;
;;                                                                      ;;
;;  This program allows the user to sum numerical attribute values      ;;
;;  held by a selection of block references, with the results displayed ;;
;;  in an AutoCAD table.                                                ;;
;;                                                                      ;;
;;  Upon issuing the command syntax 'attsum' at the AutoCAD             ;;
;;  command-line, the user is first prompted to make a selection of     ;;
;;  attributed blocks to process. This selection may include standard   ;;
;;  or dynamic attributed block references.                             ;;
;;                                                                      ;;
;;  Following a valid response, the program will iterate over the       ;;
;;  selection and will sum all numerical attribute values, grouping the ;;
;;  values by common attribute tags.                                    ;;
;;                                                                      ;;
;;  If multiple attribute tags hold numerical values, the user is       ;;
;;  presented with a dialog interface and prompted to select which      ;;
;;  attribute tags should be displayed in the resulting table, before   ;;
;;  being prompted to specify an insertion point for the table.         ;;
;;                                                                      ;;
;;  The program will then automatically construct an AutoCAD table      ;;
;;  with properties inherited from the active Table Style. Each row     ;;
;;  of the table will display an attribute tag alongside the total of   ;;
;;  all numerical values held by that tag in the selection.             ;;
;;                                                                      ;;
;;  If the program is configured to use field expressions in the table, ;;
;;  the various totals will be automatically updated following valid    ;;
;;  modifications to the attribute values referenced by the fields.     ;;
;;  Note that this does not include the addition of new attributed      ;;
;;  blocks to the drawing, or the removal of attributed blocks          ;;
;;  referenced by the table fields.                                     ;;
;;                                                                      ;;
;;  If field expressions are used, the formatting of the results will   ;;
;;  be dependent on the field formatting code set in the program;       ;;
;;  otherwise, the formatting of the totals displayed in the table      ;;
;;  will be dependent on the current unit & precision settings          ;;
;;  (that is, the values held by the LUNITS & LUPREC system variables   ;;
;;  respectively).                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2011  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2011-01-10                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2015-12-21                                      ;;
;;                                                                      ;;
;;  - Program entirely rewritten.                                       ;;
;;  - User may choose which attribute tags to display in table.         ;;
;;  - Numerical values of constant attributes now included.             ;;
;;  - Fields may be used to link attribute values to table.             ;;
;;----------------------------------------------------------------------;;

(defun c:attsum ( / *error* fld fmt fun hed idx ins lst obj sel spc tag ttl val )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (setq

;;----------------------------------------------------------------------;;
;;                          Program Parameters                          ;;
;;----------------------------------------------------------------------;;

        ;; Table title (e.g. "Attribute Sum") nil for none
        ttl nil 

        ;; Table Column Headings
        hed '("Tag" "Total") 

        ;; Use Field Expressions in Table? (t=yes; nil=no)
        fld t  

        ;; Field formatting
        fmt "%lu6" 

;;----------------------------------------------------------------------;;

    )
    
    (LM:startundo (LM:acdoc))
    (if (= 1 (getvar 'cvport))
        (setq spc (vla-get-paperspace (LM:acdoc)))
        (setq spc (vla-get-modelspace (LM:acdoc)))
    )
    (cond
        (   (not (vlax-method-applicable-p spc 'addtable))
            (princ "\nThis version of AutoCAD does not support tables.")
        )
        (   (= 4 (logand 4 (cdr (assoc 70 (tblsearch "layer" (getvar 'clayer))))))
            (princ "\nThe current layer is locked.")
        )
        (   (not (setq sel (LM:ssget "\nSelect attributed blocks: " '(((0 . "INSERT")))))))
        (   (progn
                (if fld
                    (setq fun (lambda ( obj val ) (strcat "+%<\\AcObjProp Object(%<\\_ObjId " (LM:objectid obj) ">%).TextString>%")))
                    (setq fun (lambda ( obj val ) val))
                )
                (repeat (setq idx (sslength sel))
                    (setq obj (vlax-ename->vla-object (ssname sel (setq idx (1- idx)))))
                    (foreach att
                        (append
                            (vlax-invoke obj 'getattributes)
                            (vlax-invoke obj 'getconstantattributes)
                        )
                        (if (setq val (distof (vla-get-textstring att)))
                            (setq lst (attsum:assoc++ (strcase (vla-get-tagstring att)) (fun att val) lst))
                        )
                    )
                )
                (null (setq lst (vl-sort lst '(lambda ( a b ) (< (car a) (car b))))))
            )
            (princ "\nNo numerical attribute data found.")
        )
        (   (and (setq tag (if (cdr lst) (LM:listbox "Select Tags to Display" (mapcar 'car lst) 1) (mapcar 'car lst)))
                 (setq ins (getpoint "\nSpecify point for table: "))
            )
            (if fld
                (setq fun 
                    (lambda ( x )
                        (list (car x)
                            (strcat
                                "%<\\AcExpr "
                                (substr (apply 'strcat (cdr x)) 2)
                                " \\f \"" fmt "\">%"
                            )
                        )
                    )
                )
                (setq fun (lambda ( x ) (list (car x) (rtos (apply '+ (cdr x))))))
            )
            (LM:addtable spc (trans ins 1 0) ttl
                (cons hed (mapcar 'fun (vl-remove-if-not '(lambda ( x ) (member (car x) tag)) lst)))
                nil
            )
        )
    )
    (LM:endundo (LM:acdoc))
    (princ)
)

(defun attsum:assoc++ ( key val lst / itm )
    (if (setq itm (assoc key lst))
        (subst (vl-list* key val (cdr itm)) itm lst)
        (cons  (list key val) lst)
    )
)

;; ssget  -  Lee Mac
;; A wrapper for the ssget function to permit the use of a custom selection prompt
;; msg - [str] selection prompt
;; arg - [lst] list of ssget arguments

(defun LM:ssget ( msg arg / sel )
    (princ msg)
    (setvar 'nomutt 1)
    (setq sel (vl-catch-all-apply 'ssget arg))
    (setvar 'nomutt 0)
    (if (not (vl-catch-all-error-p sel)) sel)
)

;; ObjectID  -  Lee Mac
;; Returns a string containing the ObjectID of a supplied VLA-Object
;; Compatible with 32-bit & 64-bit systems
 
(defun LM:objectid ( obj )
    (eval
        (list 'defun 'LM:objectid '( obj )
            (if (vlax-method-applicable-p (vla-get-utility (LM:acdoc)) 'getobjectidstring)
                (list 'vla-getobjectidstring (vla-get-utility (LM:acdoc)) 'obj ':vlax-false)
               '(itoa (vla-get-objectid obj))
            )
        )
    )
    (LM:objectid obj)
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

;; Add Table  -  Lee Mac
;; Generates a table at the given point, populated with the given data and optional title.
;; spc - [vla] VLA Block object
;; ins - [lst] WCS insertion point for table
;; ttl - [str] [Optional] Table title
;; lst - [lst] Matrix list of table cell data
;; eqc - [bol] If T, columns are of equal width
;; Returns: [vla] VLA Table Object

(defun LM:addtable ( spc ins ttl lst eqc / dif hgt i j obj stn sty wid )
    (setq sty
        (vlax-ename->vla-object
            (cdr
                (assoc -1
                    (dictsearch (cdr (assoc -1 (dictsearch (namedobjdict) "acad_tablestyle")))
                        (getvar 'ctablestyle)
                    )
                )
            )
        )
    )
    (setq hgt (vla-gettextheight sty acdatarow))
    (if (LM:annotative-p (setq stn (vla-gettextstyle sty acdatarow)))
        (setq hgt (/ hgt (cond ((getvar 'cannoscalevalue)) (1.0))))
    )
    (setq wid
        (mapcar
           '(lambda ( col )
                (apply 'max (mapcar '(lambda ( str ) (LM:addtable:textwidth str hgt stn)) col))
            )
            (apply 'mapcar (cons 'list lst))
        )
    )
    (if (and  ttl (< 0.0 (setq dif (/ (- (LM:addtable:textwidth ttl hgt stn) (apply '+ wid)) (length wid)))))
        (setq wid (mapcar '(lambda ( x ) (+ x dif)) wid))
    )
    (setq obj
        (vla-addtable spc
            (vlax-3D-point ins)
            (1+ (length lst))
            (length (car lst))
            (* 2.0 hgt)
            (if eqc
                (apply 'max wid)
                (/ (apply '+ wid) (float (length (car lst))))
            )
        )
    )
    (vla-put-regeneratetablesuppressed obj :vlax-true)
    (vla-put-stylename obj (getvar 'ctablestyle))
    (setq i -1)
    (if (null eqc)
        (foreach col wid
            (vla-setcolumnwidth obj (setq i (1+ i)) col)
        )
    )
    (if ttl
        (progn
            (vla-settext obj 0 0 ttl)
            (setq i 1)
        )
        (progn
            (vla-deleterows obj 0 1)
            (setq i 0)
        )
    )
    (foreach row lst
        (setq j 0)
        (foreach val row
            (vla-settext obj i j val)
            (setq j (1+ j))
        )
        (setq i (1+ i))
    )
    (vla-put-regeneratetablesuppressed obj :vlax-false)
    obj
)

(defun LM:addtable:textwidth ( str hgt sty / box obj tmp )
    (if
        (and (wcmatch str "*%<*>%*")
            (setq tmp
                (entmakex
                    (list
                       '(00 . "TEXT")
                       '(10 0.0 0.0 0.0)
                        (cons 01 str)
                        (cons 40 hgt)
                        (cons 07 sty)
                    )
                )
            )
        )
        (progn
            (setq obj (vlax-ename->vla-object tmp))
            (vla-put-textstring obj "")
            (vla-put-textstring obj str)
            (setq str (vla-get-textstring obj))
            (entdel tmp)
        )
    )
    (if
        (setq box
            (textbox
                (list
                    (cons 01 str)
                    (cons 40 hgt)
                    (cons 07 sty)
                )
            )
        )
        (+ (* 2.5 hgt) (- (caadr box) (caar box)))
        0.0
    )
)

;; Annotative-p  -  Lee Mac
;; Returns T if the given Textstyle is annotative

(defun LM:annotative-p ( sty )
    (and (setq sty (tblobjname "style" sty))
         (setq sty (cadr (assoc -3 (entget sty '("acadannotative")))))
         (= 1 (cdr (assoc 1070 (reverse sty))))
    )
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
        "\n:: SumAttributes.lsp | Version 1.1 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"attsum\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;