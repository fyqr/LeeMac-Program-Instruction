;;----------------------------------------------------------------------;;
;; Length Field Commands                                                ;;
;;----------------------------------------------------------------------;;

(defun c:lf  ( ) (lengthfield nil "%lu6"))            ;; Current units
(defun c:lfm ( ) (lengthfield nil "%lu6%ct8[0.001]")) ;; Current units with 0.001 conversion factor (mm->m)

;;----------------------------------------------------------------------;;
;; Area Field Commands                                                  ;;
;;----------------------------------------------------------------------;;

(defun c:af  ( ) (areafield nil "%lu6%qf1"))           ;; Current units
(defun c:afm ( ) (areafield nil "%lu6%qf1%ct8[1e-6]")) ;; Current units with 1e-6 (0.000001) conversion factor (mm2->m2)

;;----------------------------------------------------------------------;;

;;----------------------=={ Length & Area Field }==---------------------;;
;;                                                                      ;;
;;  This program offers two commands to allow a user to generate a      ;;
;;  field expression referencing either the area or the                 ;;
;;  length/perimeter/circumference of one or more selected objects.     ;;
;;  In the case of selecting multiple objects, the field expression     ;;
;;  will reference the sum of the areas or lengths of all objects in    ;;
;;  the selection.                                                      ;;
;;                                                                      ;;
;;  The user may opt to specify a point at which to create a new        ;;
;;  multiline text object housing the field expression, pick a table    ;;
;;  cell in which the field should be inserted, or select an existing   ;;
;;  single-line text, multiline text, multileader, or attribute to      ;;
;;  be populated with the field expression.                             ;;
;;                                                                      ;;
;;  Upon issuing the command syntax 'LF' (Length Field) at the AutoCAD  ;;
;;  command-line, the program first prompts the user to make a          ;;
;;  selection of objects for which to return the length summation.      ;;
;;                                                                      ;;
;;  At this prompt, the user may select any number of Arcs, Circles,    ;;
;;  Lines, 2D Polylines (light or heavy), or 3D Polylines.              ;;
;;                                                                      ;;
;;  Alternatively, upon issuing the command syntax 'AF' (Area Field)    ;;
;;  at the AutoCAD command-line, the program will prompt the user to    ;;
;;  make a selection of objects for which to return the area summation. ;;
;;                                                                      ;;
;;  At this prompt, the user may select any number of Arcs, Circles,    ;;
;;  Ellipses, Hatches, 2D Polylines (light or heavy), Regions, or       ;;
;;  Splines. If the selected object is open, the area is computed as    ;;
;;  though a straight line connects the start point and endpoint.       ;;
;;                                                                      ;;
;;  The user is then prompted to specify a point or table cell to       ;;
;;  insert a field expression referencing the summation of the lengths  ;;
;;  or areas of the selected objects.                                   ;;
;;                                                                      ;;
;;  At this prompt, the user may also choose the 'Object' option in     ;;
;;  order to populate the content of an existing annotation object      ;;
;;  with the field expression.                                          ;;
;;                                                                      ;;
;;  Upon choosing this option, the user may select any single-line      ;;
;;  text (DText), multiline text (MText), single-line or multiline      ;;
;;  attribute, attributed block, or multileader (MLeader) with either   ;;
;;  multiline text or attributed block content.                         ;;
;;                                                                      ;;
;;  If the user selects an attributed block or attributed multileader   ;;
;;  with more than one attribute, the user is presented with a dialog   ;;
;;  interface listing the available attributes, and is prompted to      ;;
;;  select a destination for the field expression.                      ;;
;;                                                                      ;;
;;  The user may optionally predefine the target block/multileader      ;;
;;  attribute by specifying the attribute tag where noted at the top    ;;
;;  of the program source code.                                         ;;
;;                                                                      ;;
;;  The resulting field expression will display the sum of the lengths  ;;
;;  or areas of the selected objects, formatted using the field         ;;
;;  formatting code specified at the top of each command definition.    ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2017  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2017-08-06                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2017-08-06                                      ;;
;;                                                                      ;;
;;  - Program modified to account for selection of existing annotation  ;;
;;    objects which already contain a field expression.                 ;;
;;----------------------------------------------------------------------;;
;;  Version 1.2    -    2018-10-29                                      ;;
;;                                                                      ;;
;;  - Restructured program to use standard LM:outputtext function.      ;;
;;  - Incorporated Area Field functionality and added new 'AF' command. ;;
;;----------------------------------------------------------------------;;
;;  Version 1.3    -    2018-11-04                                      ;;
;;                                                                      ;;
;;  - Changed 'c:lf' and 'c:af' commands to functions accepting an      ;;
;;    optional attribute tag and field formatting argument to enable    ;;
;;    the user to create multiple commands with varying parameters.     ;;
;;----------------------------------------------------------------------;;

(defun lengthfield ( tag fmt / *error* idx lst obj prp sel )
    
    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (and msg (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    
    (LM:startundo (LM:acdoc))
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
        (setq sel
            (LM:ssget "\nSelect objects to obtain total length <exit>: "
                (list
                    (list
                       '(000 . "ARC,CIRCLE,LINE,*POLYLINE")
                       '(-04 . "<NOT")
                           '(-04 . "<AND")
                               '(000 . "POLYLINE") '(-04 . "&") '(070 . 80)
                           '(-04 . "AND>")
                       '(-04 . "NOT>")
                        (if (= 1 (getvar 'cvport))
                            (cons 410 (getvar 'ctab))
                           '(410 . "Model")
                        )
                    )
                )
            )
        )
        (LM:outputtext tag
            (if (= 1 (sslength sel))
                (progn
                    (setq obj (vlax-ename->vla-object (ssname sel 0)))
                    (strcat
                        "%<\\AcObjProp Object(%<\\_ObjId "
                        (LM:objectid obj)
                        ">%)." (cdr (assoc (vla-get-objectname obj) prp))
                        (if (and fmt (/= "" fmt)) (strcat " \\f \"" fmt "\">%") ">%")
                    )
                )
                (progn
                    (repeat (setq idx (sslength sel))
                        (setq obj (vlax-ename->vla-object (ssname sel (setq idx (1- idx))))
                              lst
                            (vl-list*
                                "%<\\AcObjProp Object(%<\\_ObjId "
                                (LM:objectid obj)
                                ">%)." (cdr (assoc (vla-get-objectname obj) prp)) ">%" " + "
                                lst
                            )
                        )
                    )
                    (strcat
                        "%<\\AcExpr "
                        (apply 'strcat (reverse (cdr (reverse lst))))
                        (if (and fmt (/= "" fmt)) (strcat " \\f \"" fmt "\">%") ">%")
                    )
                )
            )
        )
    )
    (*error* nil) (princ)
)

;;----------------------------------------------------------------------;;

(defun areafield ( tag fmt / *error* idx lst sel )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (and msg (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    
    (LM:startundo (LM:acdoc))

    (if
        (setq sel
            (LM:ssget "\nSelect objects to obtain total area <exit>: "
                (list
                    (list
                       '(000 . "ARC,CIRCLE,ELLIPSE,HATCH,*POLYLINE,REGION,SPLINE")
                       '(-04 . "<NOT")
                           '(-04 . "<AND")
                               '(000 . "POLYLINE") '(-04 . "&") '(070 . 88)
                           '(-04 . "AND>")
                       '(-04 . "NOT>")
                        (if (= 1 (getvar 'cvport))
                            (cons 410 (getvar 'ctab))
                           '(410 . "Model")
                        )
                    )
                )
            )
        )
        (LM:outputtext tag
            (if (= 1 (sslength sel))
                (strcat
                    "%<\\AcObjProp Object(%<\\_ObjId "
                    (LM:objectid (vlax-ename->vla-object (ssname sel 0)))
                    ">%).Area"
                    (if (and fmt (/= "" fmt)) (strcat " \\f \"" fmt "\">%") ">%")
                )
                (progn
                    (repeat (setq idx (sslength sel))
                        (setq lst
                            (vl-list*
                                "%<\\AcObjProp Object(%<\\_ObjId "
                                (LM:objectid (vlax-ename->vla-object (ssname sel (setq idx (1- idx)))))
                                ">%).Area>%" " + "
                                lst
                            )
                        )
                    )
                    (strcat
                        "%<\\AcExpr "
                        (apply 'strcat (reverse (cdr (reverse lst))))
                        (if (and fmt (/= "" fmt)) (strcat " \\f \"" fmt "\">%") ">%")
                    )
                )
            )
        )
    )
    (*error* nil) (princ)
)

;; Output Text  -  Lee Mac
;; Prompts the user to specify a point at which to create an MText object containing the supplied string or to
;; select a table cell, text, mtext, multileader, attribute, or attributed block to be populated with the supplied string.
;; tag - [str] Optional target attribute tag
;; str - [str] Field expression or other text content

(defun LM:outputtext ( tag str / ent enx flg idx obj oid sel tab tmp typ )
    (if
        (setq tmp
            (ssget "_X"
                (list '(0 . "ACAD_TABLE")
                    (if (= 1 (getvar 'cvport))
                        (cons 410 (getvar 'ctab))
                       '(410 . "Model")
                    )
                )
            )
        )
        (repeat (setq idx (sslength tmp))
            (setq tab (cons (vlax-ename->vla-object (ssname tmp (setq idx (1- idx)))) tab))
        )
    )
    (while
        (not
            (progn
                (if flg
                    (progn
                        (setvar 'errno 0)
                        (initget "Point eXit")
                        (setq sel (nentsel "\nSelect text, mtext, mleader, attribute or attributed block [Point/eXit] <eXit>: "))
                    )
                    (progn
                        (initget "Object eXit")
                        (setq sel (getpoint "\nSpecify point or table cell [Object/eXit] <eXit>: "))
                    )
                )
                (cond
                    (   (= 7 (getvar 'errno))
                        (prompt "\nMissed, try again.")
                    )
                    (   (or (null sel) (= "eXit" sel)))
                    (   (= "Point" sel)
                        (setq flg nil)
                    )
                    (   (= "Object" sel)
                        (not (setq flg t))
                    )
                    (   flg
                        (setq ent (car sel)
                              enx (entget ent)
                              typ (cdr (assoc 0 enx))
                              obj (vlax-ename->vla-object ent)
                        )
                        (cond
                            (   (and (= 2 (length sel)) (wcmatch typ "TEXT,MTEXT"))
                                (if (vlax-write-enabled-p obj)
                                    (LM:outputtext:puttextstring obj str)
                                    (prompt "\nThe selected text object is on a locked layer.")
                                )
                            )
                            (   (and (= "ATTRIB" typ)
                                     (/= 'str (type tag))
                                )
                                (if (vlax-write-enabled-p obj)
                                    (progn
                                        (LM:outputtext:puttextstring obj str)
                                        (if (wcmatch (strcase str t) "*%<\\ac*>%*") (LM:outputtext:updatefield ent))
                                    )
                                    (prompt "\nThe selected attribute is on a locked layer.")
                                )
                            )
                            (   (and
                                    (or
                                        (and (= "ATTRIB" typ)
                                             (setq tmp (cdr (assoc 330 enx)))
                                        )
                                        (and (setq tmp (last (cadddr sel)))
                                             (= "INSERT" (cdr (assoc 0 (entget tmp))))
                                        )
                                    )
                                    (setq tmp (vlax-invoke (vlax-ename->vla-object tmp) 'getattributes))
                                    (or
                                        (and (= 'str (type tag))
                                             (setq idx (vl-position (strcase tag) (mapcar 'vla-get-tagstring tmp)))
                                             (setq obj (nth idx tmp))
                                        )
                                        (and (not (cdr tmp))
                                             (setq obj (car tmp))
                                        )
                                        (and (setq idx (LM:listbox "Choose Attribute" (mapcar 'vla-get-tagstring tmp) 2))
                                             (setq obj (nth (car idx) tmp))
                                        )
                                    )
                                )
                                (if (vlax-write-enabled-p obj)
                                    (progn
                                        (LM:outputtext:puttextstring obj str)
                                        (if (wcmatch (strcase str t) "*%<\\ac*>%*") (LM:outputtext:updatefield (vlax-vla-object->ename obj)))
                                    )
                                    (prompt "\nThe selected attribute is on a locked layer.")
                                )
                            )
                            (   (and (= 2 (length sel)) (= "MULTILEADER" typ))
                                (setq typ (cdr (assoc 172 (reverse enx))))
                                (cond
                                    (   (and (<= acblockcontent typ acmtextcontent) (not (vlax-write-enabled-p obj)))
                                        (prompt "\nThe selected multileader is on a locked layer.")
                                    )
                                    (   (= acmtextcontent typ)
                                        (LM:outputtext:puttextstring obj str)
                                        (if (wcmatch (strcase str t) "*%<\\ac*>%*") (vla-regen (LM:acdoc) acactiveviewport))
                                        t
                                    )
                                    (   (and
                                            (= acblockcontent typ)
                                            (setq tmp (LM:getmleaderattributes obj))
                                            (or
                                                (and (= 'str (type tag))
                                                     (setq oid (cdr (assoc (strcase tag) tmp)))
                                                )
                                                (and (not (cdr tmp))
                                                     (setq oid (cdar tmp))
                                                )
                                                (and (setq idx (LM:listbox "Choose Attribute" (mapcar 'car tmp) 2))
                                                     (setq oid (cdr (nth (car idx) tmp)))
                                                )
                                            )
                                        )
                                        (LM:setmleaderattributevalue obj oid str)
                                        (if (wcmatch (strcase str t) "*%<\\ac*>%*") (vla-regen (LM:acdoc) acactiveviewport))
                                        t
                                    )
                                    (   (prompt "\nThe select multileader has no editable content."))
                                )
                            )
                            (   (prompt "\nThe selected object is not text, mtext, multileader, attribute or attributed block."))
                        )
                    )
                    (   (setq tmp (LM:getcell tab (trans sel 1 0)))
                        (if (vlax-write-enabled-p (car tmp))
                            (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-settext (append tmp (list str)))))
                            (prompt "\nThe selected table cell belongs to a table on a locked layer.")
                        )
                    )
                    (   (vla-addmtext
                            (vlax-get-property (LM:acdoc) (if (= 1 (getvar 'cvport)) 'paperspace 'modelspace))
                            (vlax-3D-point (trans sel 1 0))
                            0.0
                            str
                        )
                    )
                )
            )
        )
    )
)

(defun LM:outputtext:puttextstring ( obj str )
    (vla-put-textstring obj "") ;; To clear any existing field
    (vla-put-textstring obj str)
    t
)

(defun LM:outputtext:updatefield ( ent / cmd rtn )
    (setq cmd (getvar 'cmdecho))
    (setvar 'cmdecho 0)
    (setq rtn (vl-cmdf "_.updatefield" ent ""))
    (setvar 'cmdecho cmd)
    rtn
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

;; Get MLeader Attributes  -  Lee Mac
;; Returns an association list of attribute tags & object IDs for all attributes held by an mleader block
;; mld - [vla] MLeader vla-object
;; Returns: [lst] List of ((<Attribute Tag> . <Object ID>) ... )

(defun LM:getmleaderattributes ( mld / rtn )
    (vlax-for obj (vla-item (vla-get-blocks (vla-get-document mld)) (vla-get-contentblockname mld))
        (if
            (and
                (= "AcDbAttributeDefinition" (vla-get-objectname obj))
                (= :vlax-false (vla-get-constant obj))
            )
            (setq rtn (cons (cons (strcase (vla-get-tagstring obj)) (LM:intobjectid obj)) rtn))
        )
    )
    (reverse rtn)
)

;; Object ID (integer)  -  Lee Mac
;; Returns an integer representing the ObjectID of a supplied VLA-Object
;; Compatible with 32-bit & 64-bit systems

(defun LM:intobjectid ( obj )
    (if (vlax-property-available-p obj 'objectid32)
        (defun LM:intobjectid ( obj ) (vla-get-objectid32 obj))
        (defun LM:intobjectid ( obj ) (vla-get-objectid   obj))
    )
    (LM:intobjectid obj)
)

;; Set MLeader Attribute Value  -  Lee Mac
;; obj - [vla] MLeader vla-object
;; idx - [int] Attribute Definition Object ID
;; str - [str] Attribute value

(defun LM:setmleaderattributevalue ( obj idx str )
    (if (vlax-method-applicable-p obj 'setblockattributevalue32)
        (defun LM:setmleaderattributevalue ( obj idx str ) (vla-setblockattributevalue32 obj idx str))
        (defun LM:setmleaderattributevalue ( obj idx str ) (vla-setblockattributevalue   obj idx str))
    )
    (LM:setmleaderattributevalue obj idx str)
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

;; Get Cell  -  Lee Mac
;; If the supplied point lies within a cell boundary,
;; returns a list of: (<VLA Table Object> <Row> <Col>)
 
(defun LM:getcell ( lst pnt / dir )
    (setq dir (vlax-3D-point (trans (getvar 'viewdir) 1 0))
          pnt (vlax-3D-point pnt)
    )
    (vl-some
       '(lambda ( tab / row col )
            (if (= :vlax-true (vla-hittest tab pnt dir 'row 'col))
                (list tab row col)
            )
        )
        lst
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
        "\n:: LengthAreaField.lsp | Version 1.3 | \\U+00A9 Lee Mac "
        ((lambda ( y ) (if (= y (menucmd "m=$(edtime,0,yyyy)")) y (strcat y "-" (menucmd "m=$(edtime,0,yyyy)")))) "2017")
        " www.lee-mac.com ::"
        "\n:: \"LF\" for Length Field | \"AF\" for Area Field ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;