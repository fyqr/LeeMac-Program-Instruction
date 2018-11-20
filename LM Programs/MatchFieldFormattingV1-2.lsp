;;--------------------=={ Match Field Formatting }==--------------------;;
;;                                                                      ;;
;;  This program enables the user to copy the formatting from a         ;;
;;  selected source field to multiple destination fields in a drawing.  ;;
;;                                                                      ;;
;;  Upon issuing the command syntax 'mff' at the AutoCAD command-line,  ;;
;;  the user is prompted to select an annotation object (Text, MText,   ;;
;;  Attribute, Multileader, Dimension) containing a field expression    ;;
;;  with formatting applied. If the selected object contains more than  ;;
;;  one field, the first field expression containing formatting will    ;;
;;  be used.                                                            ;;
;;                                                                      ;;
;;  The user may then apply the formatting extracted from the source    ;;
;;  field to multiple selected destination fields in the drawing.       ;;
;;                                                                      ;;
;;  The program is also compatible with nested fields and fields with   ;;
;;  no formatting applied. If a nested field has formatting applied,    ;;
;;  this formatting will be replaced by the formatting from the source  ;;
;;  field; however, only primary fields which contain no formatting     ;;
;;  will have new formatting applied - this program is designed not to  ;;
;;  add field formatting to nested fields which did not previously      ;;
;;  contain formatting.                                                 ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2013  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2013-07-12                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2017-06-13                                      ;;
;;                                                                      ;;
;;  - Updated LM:fieldcode function to account for field expressions    ;;
;;    greater than 250 characters in length.                            ;;
;;----------------------------------------------------------------------;;
;;  Version 1.2    -    2018-01-15                                      ;;
;;                                                                      ;;
;;  - Updated replaceformatting function to retain any text following   ;;
;;    the last field expression held by the selected annotation object. ;;
;;----------------------------------------------------------------------;;

(defun c:mff ( / *error* ent fld gr1 gr2 msg obj src )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (redraw) (princ)
    )
    
    (while
        (progn (setvar 'errno 0) (setq ent (nentsel "\nSelect source field: "))
            (cond
                (   (= 7 (getvar 'errno))
                    (princ "\nMissed, try again.")
                )
                (   (= 'list (type ent))
                    (cond
                        (   (progn
                                (if (= 4 (length ent))
                                    (setq ent (last (last ent)))
                                    (setq ent (car ent))
                                )
                                (not (wcmatch (cdr (assoc 0 (entget ent))) "TEXT,MTEXT,ATTRIB,MULTILEADER,*DIMENSION"))
                            )
                            (princ "\nInvalid object selected.")
                        )
                        (   (null (setq fld (LM:fieldcode ent)))
                            (princ "\nSelected object does not contain a field.")
                        )
                        (   (null (setq src (car (fieldformatting fld))))
                            (princ "\nSelected field has no formatting.")
                        )
                    )
                )
            )
        )
    )
    (if (= 'str (type src))
        (progn
            (LM:startundo (LM:acdoc))
            (princ (setq msg "\nSelect destination field <Exit>: "))
            (while
                (member
                    (setq gr1 (grread t 13 2)
                          gr2 (cadr gr1)
                          gr1 (car  gr1)
                    )
                   '(3 5)
                )
                (if (= 5 gr1)
                    (progn
                        (redraw)
                        (LM:grbrush (trans gr2 1 0))
                    )
                    (cond
                        (   (null (setq ent (nentselp gr2)))
                            (princ (strcat "\nMissed, try again." msg))
                        )
                        (   (progn
                                (if (= 4 (length ent))
                                    (setq ent (last (last ent)))
                                    (setq ent (car ent))
                                )
                                (not (wcmatch (cdr (assoc 0 (entget ent))) "TEXT,MTEXT,ATTRIB,MULTILEADER,*DIMENSION"))
                            )
                            (princ "\nInvalid object selected.")
                        )
                        (   (null (vlax-write-enabled-p (setq obj (vlax-ename->vla-object ent))))
                            (princ (strcat "\nSelected object is on a locked layer." msg))
                        )
                        (   (null (setq fld (LM:fieldcode ent)))
                            (princ (strcat "\nSelected object does not contain a field." msg))
                        )
                        (   (vlax-property-available-p obj 'textoverride t)
                            (vla-put-textoverride obj "")
                            (null (vla-put-textoverride obj (replaceformatting fld src 0)))
                        )
                        (   (vlax-property-available-p obj 'textstring t)
                            (vla-put-textstring obj "")
                            (null (vla-put-textstring obj (replaceformatting fld src 0)))
                        )
                    )
                )
            )
            (LM:endundo (LM:acdoc))
            (redraw)
        )
    )
    (princ)
)

(defun fieldformatting ( fld / pos )
    (if
        (and
            (setq pos (vl-string-search "\\f \"" fld))
            (setq fld (substr fld (+ 5 pos))
                  pos (vl-string-search "\">%" fld)
            )
        )
        (cons (substr fld 1 pos) (fieldformatting (substr fld (+ 3 pos))))
    )
)

(defun replaceformatting ( str new idx / p q r )
    (setq p (vl-string-search "%<" str)
          q (vl-string-search ">%" str)
    )
    (if (or (and p q (< p q)) (and p (not q)))
        (strcat (substr str 1 (+ p 2)) (replaceformatting (substr str (+ p 3)) new (1+ idx)))
        (if q
            (cond
                (   (and (setq r (vl-string-search "\\f \"" str)) (< r q))
                    (strcat (substr str 1 (+ r 4)) new "\">%" (replaceformatting (substr str (+ q 3)) new (1- idx)))
                )
                (   (= 1 idx)
                    (strcat (substr str 1 q) " \\f \"" new "\">%" (replaceformatting (substr str (+ q 3)) new (1- idx)))
                )
                (   (strcat (substr str 1 (+ q 2)) (replaceformatting (substr str (+ q 3)) new (1- idx))))
            )
            str
        )
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

(defun LM:ObjectID ( obj )
    (eval
        (list 'defun 'LM:ObjectID '( obj )
            (if
                (and
                    (vl-string-search "64" (getenv "PROCESSOR_ARCHITECTURE"))
                    (vlax-method-applicable-p (vla-get-utility (LM:acdoc)) 'getobjectidstring)
                )
                (list 'vla-getobjectidstring (vla-get-utility (LM:acdoc)) 'obj ':vlax-false)
               '(itoa (vla-get-objectid obj))
            )
        )
    )
    (LM:ObjectID obj)
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

;; grvecs Brush  -  Lee Mac
;; Displays the Match Properties Brush icon.

(defun LM:grbrush ( p / r )
    (setq r (/ (getvar 'viewsize) (cadr (getvar 'screensize)))
          p (trans p 0 2)
    )
    (grvecs
       '(
            251 (20 -10) (21 -10) 253 (22 -10) (22 -10)
            251 (19 -11) (19 -11) 007 (20 -11) (20 -11)
            253 (21 -11) (21 -12) 251 (22 -11) (22 -12)
            251 (14 -12) (16 -12) 251 (18 -12) (18 -12)
            007 (19 -12) (19 -12) 253 (20 -12) (20 -13)
            251 (13 -13) (13 -13) 007 (14 -13) (16 -13)
            251 (17 -13) (17 -13) 007 (18 -13) (18 -13)
            253 (19 -13) (19 -14) 251 (21 -13) (21 -13)
            034 (11 -14) (14 -14) 007 (15 -14) (15 -14)
            253 (16 -14) (17 -14) 251 (18 -14) (18 -14)
            251 (20 -14) (20 -14) 034 (09 -15) (14 -15)
            251 (15 -15) (15 -15) 007 (16 -15) (16 -15)
            253 (17 -15) (18 -15) 251 (19 -15) (19 -15)
            034 (06 -16) (15 -16) 251 (16 -16) (16 -16)
            253 (17 -16) (19 -16) 034 (06 -17) (16 -17)
            251 (17 -17) (17 -17) 253 (18 -17) (19 -17)
            034 (07 -18) (14 -18) 251 (15 -18) (15 -18)
            034 (16 -18) (16 -18) 251 (17 -18) (18 -18)
            253 (19 -18) (19 -18) 034 (08 -19) (15 -19)
            251 (16 -19) (16 -20) 034 (17 -19) (17 -19)
            251 (18 -19) (18 -19) 034 (09 -20) (09 -20)
            251 (10 -20) (10 -20) 034 (11 -20) (13 -20)
            251 (14 -20) (14 -20) 034 (15 -20) (15 -20)
            034 (10 -21) (10 -21) 251 (11 -21) (11 -21)
            034 (12 -21) (12 -21) 251 (13 -21) (13 -21)
            034 (14 -21) (14 -21) 251 (15 -21) (15 -21)
        )
        (list
            (list r 0.0 0.0 (car  p))
            (list 0.0 r 0.0 (cadr p))
            (list 0.0 0.0 r 0.0)
           '(0.0 0.0 0.0 1.0)
        )
    )
)

;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: MatchFieldFormatting.lsp | Version 1.2 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"mff\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;