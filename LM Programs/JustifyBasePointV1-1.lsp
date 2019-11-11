;;-------------------=={ Justify Block Base Point  }==------------------;;
;;                                                                      ;;
;;  This program allows the user to change the base point location      ;;
;;  for multiple blocks to one of nine standard justifications.         ;;
;;                                                                      ;;
;;  Upon issuing the command syntax 'JBP' (Justify Base Point) at the   ;;
;;  AutoCAD command-line, the user is prompted to make a selection of   ;;
;;  blocks for which to modify the base point location.                 ;;
;;                                                                      ;;
;;  As the program will modify the block definition for each distinct   ;;
;;  block in the selection, the user is only required to select one     ;;
;;  reference of a given block in order to change the base point for    ;;
;;  all other references of the block - the program will automatically  ;;
;;  ignore multiple references of the same block in the selection.      ;;
;;                                                                      ;;
;;  Following a valid selection, the user is presented with a dialog    ;;
;;  interface through which one of nine standard justification          ;;
;;  positions may be selected (Top Left, Top Center, Top Right,         ;;
;;  Middle Left, Middle Center, Middle Right, Bottom Left,              ;;
;;  Bottom Center, Bottom Right).                                       ;;
;;                                                                      ;;
;;  The user may also choose whether the modification to the base       ;;
;;  point should retain the visual position of the block references,    ;;
;;  or retain the coordinates of the block insertion points.            ;;
;;                                                                      ;;
;;  Upon clicking the 'OK' button to submit the selections, the         ;;
;;  program will modify the block definition of each distinct block in  ;;
;;  the selection accordingly. If one or more of the selected blocks    ;;
;;  is attributed, an ATTSYNC operation will be performed to ensure     ;;
;;  all attributes are in the correct positions relative to the new     ;;
;;  base points.                                                        ;;
;;                                                                      ;;
;;  Finally, the drawing is regenerated to reflect the changes across   ;;
;;  all references of the modified blocks.                              ;;
;;                                                                      ;;
;;  Please Note: A REGEN is required if the UNDO command is used to     ;;
;;  undo the operations performed by this program.                      ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright ?2015  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2015-08-01                                      ;;
;;                                                                      ;;
;;  First release.                                                      ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2019-09-08                                      ;;
;;                                                                      ;;
;;  Program updated to account for blocks nested within nested xrefs.   ;;
;;----------------------------------------------------------------------;;

(defun c:jbp

    (
        /
        *error*
        att
        bln bnl bpt
        cmd
        dch dcl def des
        ent enx
        fun
        idx
        jus
        ky1 ky2
        lck
        ret
        sel
    )

    (defun *error* ( msg )
        (if (= 'file (type des))
            (close des)
        )
        (if (and (= 'int (type dch)) (< 0 dch))
            (unload_dialog dch)
        )
        (if (and (= 'str (type dcl)) (setq dcl (findfile dcl)))
            (vl-file-delete dcl)
        )
        (if (= 'int (type cmd))
            (setvar 'cmdecho cmd)
        )
        (foreach lay lck
            (vla-put-lock lay :vlax-true)
        )
        (LM:endundo (LM:acdoc))
        (if (and msg (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (setq ky1 "LMac\\jbp-ret"
          ky2 "LMac\\jbp-jus"
    )
    
    (if
        (not
            (and
                (setq ret (getenv ky1))
                (member (setq ret (strcase ret t)) '("rb1" "rb2"))
            )
        )
        (setq ret (setenv ky1 "rb1"))
    )
    (if
        (not
            (and
                (setq jus (getenv ky2))
                (wcmatch (setq jus (strcase jus)) "[TMB][LCR]")
            )
        )
        (setq jus (setenv ky2 "BL"))
    )
    
    (LM:startundo (LM:acdoc))
    (cond
        (   (not (setq sel (jbp:ssget "\nSelect blocks: " '(((0 . "INSERT")))))))
        (   (not
                (and
                    (setq dcl (vl-filename-mktemp "jbp.dcl"))
                    (setq des (open dcl "w"))
                    (foreach str
                       '(
                            "jbp : dialog"
                            "{"
                            "    label = \"Justify Base Point V1.0\";"
                            "    spacer_1;"
                            "    : boxed_column"
                            "    {"
                            "        label = \"Justification\";"
                            "        : image_button"
                            "        {"
                            "            key = \"jus\";"
                            "            width = 33.5;"
                            "            height = 11.62;"
                            "            fixed_width = true;"
                            "            fixed_height = true;"
                            "            alignment = centered;"
                            "            color = dialog_background;"
                            "        }"
                            "        spacer;"
                            "    }"
                            "    : boxed_radio_column"
                            "    {"
                            "        label = \"Modification Method\";"
                            "        : radio_button"
                            "        {"
                            "            key = \"rb1\";"
                            "            label = \"Retain visual position\";"
                            "        }"
                            "        : radio_button"
                            "        {"
                            "            key = \"rb2\";"
                            "            label = \"Retain insertion point position\";"
                            "        }"
                            "        spacer;"
                            "    }"
                            "    spacer;"
                            "    ok_cancel;"
                            "}"
                        )
                        (write-line str des)
                    )
                    (not (setq des (close des)))
                    (< 0 (setq dch (load_dialog dcl)))
                )
            )
            (princ "\nUnable to write DCL file.")
        )
        (   (not (new_dialog "jbp" dch))
            (princ "\nUnable to load dialog.")
        )
        (   (progn
                (jbp:justification "jus" jus)
                (action_tile "jus"
                    (vl-prin1-to-string
                       '(
                            (lambda ( / tmp )
                                (if (setq tmp (jbp:pixel->justification (list $x $y)))
                                    (jbp:justification $key (setq jus tmp))
                                )
                            )
                        )
                    )
                )

                (set_tile ret "1")
                (foreach key '("rb1" "rb2")
                    (action_tile key "(setq ret $key)")
                )
                (zerop (start_dialog))
            )
            (princ "\n*Cancel*")
        )
        (   t
            (setenv ky1 ret)
            (setenv ky2 jus)

            (setq fun
                (eval
                    (nth (vl-position jus '("BL" "BR" "TR" "TL" "BC" "MC" "ML" "MR" "TC"))
                       '(
                            car
                            cadr
                            caddr
                            cadddr
                            (lambda ( lst ) (jbp:mid (car   lst) (cadr   lst)))
                            (lambda ( lst ) (jbp:mid (car   lst) (caddr  lst)))
                            (lambda ( lst ) (jbp:mid (car   lst) (cadddr lst)))
                            (lambda ( lst ) (jbp:mid (cadr  lst) (caddr  lst)))
                            (lambda ( lst ) (jbp:mid (caddr lst) (cadddr lst)))
                        )
                    )
                )
            )
            (vlax-for lay (vla-get-layers (LM:acdoc))
                (if (= :vlax-true (vla-get-lock lay))
                    (progn
                        (vla-put-lock lay :vlax-false)
                        (setq lck (cons lay lck))
                    )
                )
            )
            (repeat (setq idx (sslength sel))
                (setq enx (entget (ssname sel (setq idx (1- idx))))
                      bln (strcase (jbp:name->effectivename (cdr (assoc 2 enx))))
                )
                (if (not (assoc bln bnl))
                    (progn
                        (if (setq def (vla-item (LM:acblk) bln)
                                  bpt (fun (LM:blockdefinitionboundingbox def))
                            )
                            (vlax-for obj def (vlax-invoke obj 'move bpt '(0.0 0.0 0.0)))
                        )
                        (setq bnl (cons (cons bln bpt) bnl))
                        (if (= 1 (cdr (assoc 66 enx))) (setq att (cons bln att)))
                    )
                )
            )
            (if (= "rb1" ret)
                (vlax-for blk (LM:acblk)
                    (if (= :vlax-false (vla-get-isxref blk))
                        (vlax-for obj blk
                            (if
                                (and
                                    (= "AcDbBlockReference" (vla-get-objectname obj))
                                    (setq ent (vlax-vla-object->ename obj))
                                    (setq bpt (cdr (assoc (strcase (jbp:name->effectivename (cdr (assoc 2 (entget ent))))) bnl)))
                                    (vlax-write-enabled-p obj)
                                )
                                (vlax-invoke obj 'move '(0.0 0.0 0.0) (mxv (car (refgeom ent)) bpt))
                            )
                        )
                    )
                )
            )
            (if att
                (progn
                    (setq cmd (getvar 'cmdecho))
                    (setvar 'cmdecho 0)
                    (foreach blk att (vl-cmdf "_.attsync" "_N" blk))
                    (setvar 'cmdecho cmd)
                )
            )
            (vla-regen (LM:acdoc) acallviewports)
        )
    )
    (*error* nil)
    (princ)
)

;;----------------------------------------------------------------------;;

(defun jbp:mid ( a b )
    (mapcar '(lambda ( a b ) (/ (+ a b) 2.0)) a b)
)

;;----------------------------------------------------------------------;;

(defun jbp:ssget ( msg arg / sel )
    (princ msg)
    (setvar 'nomutt 1)
    (setq sel (vl-catch-all-apply 'ssget arg))
    (setvar 'nomutt 0)
    (if (not (vl-catch-all-error-p sel)) sel)
)

;;----------------------------------------------------------------------;;

(defun jbp:name->effectivename ( blk / rep )
    (if
        (and (wcmatch blk "`**")
            (setq rep
                (cdadr
                    (assoc -3
                        (entget
                            (cdr (assoc 330 (entget (tblobjname "block" blk))))
                           '("AcDbBlockRepBTag")
                        )
                    )
                )
            )
            (setq rep (handent (cdr (assoc 1005 rep))))
        )
        (cdr (assoc 2 (entget rep)))
        blk
    )
)

;;----------------------------------------------------------------------;;

(defun jbp:pixel->justification ( cpx )
    (vl-some '(lambda ( a b ) (if (apply 'and (mapcar '< a cpx (mapcar '+ a '(16 16)))) b))
       '(
            (012 009)
            (092 009)
            (172 009)
            (012 067)
            (092 067)
            (172 067)
            (012 125)
            (092 125)
            (172 125)
        )
       '("TL" "TC" "TR" "ML" "MC" "MR" "BL" "BC" "BR")
    )
)

;;----------------------------------------------------------------------;;

(defun jbp:justification ( key jus )
    (eval
        (list 'defun 'jbp:justification '( key jus )
           '(start_image key)
            (list 'fill_image 0 0 (dimx_tile key) (dimy_tile key) -15)
           '(mapcar 'vector_image
               '(012 028 028 012 028 092 108 108 092 108 172 188 188 172 020 012 028 028 012 020 012 028 028 012
                 100 092 108 108 092 100 092 108 108 092 180 172 188 188 172 180 172 188 188 172 028 108 028 108)
               '(141 141 125 125 133 141 141 125 125 133 141 141 125 125 125 083 083 067 067 067 025 025 009 009
                 125 083 083 067 067 067 025 025 009 009 125 083 083 067 067 067 025 025 009 009 075 075 017 017)
               '(028 028 012 012 092 108 108 092 092 172 188 188 172 172 020 028 028 012 012 020 028 028 012 012
                 100 108 108 092 092 100 108 108 092 092 180 188 188 172 172 180 188 188 172 172 092 172 092 172)
               '(141 125 125 141 133 141 125 125 141 133 141 125 125 141 083 083 067 067 083 025 025 009 009 025
                 083 083 067 067 083 025 025 009 009 025 083 083 067 067 083 025 025 009 009 025 075 075 017 017)
               '(008 008 008 008 008 008 008 008 008 008 008 008 008 008 008 008 008 008 008 008 008 008 008 008
                 008 008 008 008 008 008 008 008 008 008 008 008 008 008 008 008 008 008 008 008 008 008 008 008)
            )
           '(mapcar 'vector_image
               '(155 155 045 045 042 020 065 144 125 067 086 105 107 045 047 045 045)
               '(107 043 043 107 017 056 056 107 107 107 107 107 067 090 107 071 052)
               '(155 045 045 155 020 065 042 155 155 092 150 155 131 092 111 073 054)
               '(043 043 107 107 056 056 017 096 077 082 043 057 043 043 043 043 043)
               '(086 086 086 086 001 001 001 096 096 096 096 096 096 096 096 096 096)
            )
            (list 'mapcar ''(lambda ( x ) (apply 'vector_image (append x x '(5)))) (list 'quote (jbp:circlepixels 155 108 25)))
           '(apply 'fill_image
                (append
                    (cdr
                        (assoc (strcase jus)
                           '(
                                ("TL" 013 010)
                                ("TC" 093 010)
                                ("TR" 173 010)
                                ("ML" 013 068)
                                ("MC" 093 068)
                                ("MR" 173 068)
                                ("BL" 013 126)
                                ("BC" 093 126)
                                ("BR" 173 126)
                            )
                        )
                    )
                   '(015 015 001)
                )
            )
           '(end_image)
        )
    )
    (jbp:justification key jus)
)

;;----------------------------------------------------------------------;;

;; Bresenhams Algorithm
;; https://en.wikipedia.org/wiki/Midpoint_circle_algorithm

(defun jbp:circlepixels ( cx cy r / e l x y )
    (setq e (- r)
          x r
          y 0
    )
    (while (<= y x)
        (setq l (append l (jbp:plot8points cx cy x y))
              e (+ e y)
              y (1+  y)
              e (+ e y)
        )
        (if (<= 0 e)
            (setq x (1- x)
                  e (- e x)
                  e (- e x)
            )
        )
    )
    l
)

;;----------------------------------------------------------------------;;

(defun jbp:plot8points ( cx cy x y )
    (append
        (jbp:plot4points cx cy x y)
        (if (/= x y) (jbp:plot4points cx cy y x))
    )
)

;;----------------------------------------------------------------------;;

(defun jbp:plot4points ( cx cy x y )
    (append
        (list (list (+ cx x) (+ cy y)))
        (if (/= x 0) (list (list (- cx x) (+ cy y))))
        (if (/= y 0) (list (list (+ cx x) (- cy y))))
        (if (and (/= x 0) (/= y 0)) (list (list (- cx x) (- cy y))))
    )
)

;;----------------------------------------------------------------------;;

;; Block Reference Bounding Box  -  Lee Mac
;; Returns a WCS point list describing a rectangular frame bounding all geometry of a supplied block reference.
;; Excludes Text, MText & Attribute Definitions.
;; ref - [vla] Block Reference Object

(defun LM:blockreferenceboundingbox ( ref )
    (
        (lambda ( lst )
            (apply
                (function
                    (lambda ( m v )
                        (mapcar (function (lambda ( p ) (mapcar '+ (mxv m p) v))) lst)
                    )
                )
                (refgeom (vlax-vla-object->ename ref))
            )
        )
        (LM:blockdefinitionboundingbox (vla-item (LM:acblk) (vla-get-name ref)))
    )
)

;;----------------------------------------------------------------------;;

;; Block Definition Bounding Box  -  Lee Mac
;; Returns a WCS point list describing a rectangular frame bounding all geometry of a supplied block definition.
;; Excludes Text, MText & Attribute Definitions.
;; def - [vla] Block Definition Object

(defun LM:blockdefinitionboundingbox ( def / llp lst urp )
    (vlax-for obj def
        (cond
            (   (= :vlax-false (vla-get-visible obj)))
            (   (= "AcDbBlockReference" (vla-get-objectname obj))
                (setq lst (append lst (LM:blockreferenceboundingbox obj)))
            )
            (   (and (not (wcmatch (vla-get-objectname obj) "AcDbAttributeDefinition,AcDb*Text"))
                     (vlax-method-applicable-p obj 'getboundingbox)
                     (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list obj 'llp 'urp))))
                )
                (setq lst (vl-list* (vlax-safearray->list llp) (vlax-safearray->list urp) lst))
            )
        )
    )
    (LM:points->boundingbox lst)
)

;;----------------------------------------------------------------------;;

;; Points to Bounding Box  -  Lee Mac
;; Returns the rectangular extents of a supplied point list

(defun LM:points->boundingbox ( lst )
    (   (lambda ( l )
            (mapcar '(lambda ( a ) (mapcar '(lambda ( b ) ((eval b) l)) a))
               '(
                    (caar   cadar  caddar)
                    (caadr  cadar  caddar)
                    (caadr cadadr  caddar)
                    (caar  cadadr  caddar)
                )
            )
        )
        (mapcar '(lambda ( f ) (apply 'mapcar (cons f lst))) '(min max))
    )
)

;;----------------------------------------------------------------------;;

;; RefGeom (gile)
;; Returns a list which first item is a 3x3 transformation matrix (rotation, scales, normal)
;; and second item the object insertion point in its parent (xref, block or space)
;; Argument : an ename

(defun refgeom ( ent / ang ang mat ocs )
    (setq enx (entget ent)
          ang (cdr (assoc 050 enx))
          ocs (cdr (assoc 210 enx))
    )
    (list
        (setq mat
            (mxm
                (mapcar '(lambda ( v ) (trans v 0 ocs t))
                   '(
                        (1.0 0.0 0.0)
                        (0.0 1.0 0.0)
                        (0.0 0.0 1.0)
                    )
                )
                (mxm
                    (list
                        (list (cos ang) (- (sin ang)) 0.0)
                        (list (sin ang) (cos ang)     0.0)
                       '(0.0 0.0 1.0)
                    )
                    (list
                        (list (cdr (assoc 41 enx)) 0.0 0.0)
                        (list 0.0 (cdr (assoc 42 enx)) 0.0)
                        (list 0.0 0.0 (cdr (assoc 43 enx)))
                    )
                )
            )
        )
        (mapcar '- (trans (cdr (assoc 10 enx)) ocs 0)
            (mxv mat (cdr (assoc 10 (tblsearch "block" (cdr (assoc 2 enx))))))
        )
    )
)

;;----------------------------------------------------------------------;;

;; Matrix x Vector - Vladimir Nesterovsky
;; Args: m - nxn matrix, v - vector in R^n

(defun mxv ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)

;;----------------------------------------------------------------------;;

;; Matrix Transpose - Doug Wilson
;; Args: m - nxn matrix

(defun trp ( m )
    (apply 'mapcar (cons 'list m))
)

;;----------------------------------------------------------------------;;

;; Matrix x Matrix - Vladimir Nesterovsky
;; Args: m,n - nxn matrices

(defun mxm ( m n )
    ((lambda ( a ) (mapcar '(lambda ( r ) (mxv a r)) m)) (trp n))
)

;;----------------------------------------------------------------------;;

(defun LM:startundo ( doc )
    (LM:endundo doc)
    (vla-startundomark doc)
)

;;----------------------------------------------------------------------;;

(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;;----------------------------------------------------------------------;;

(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)

;;----------------------------------------------------------------------;;

(defun LM:acblk nil
    (eval (list 'defun 'LM:acblk 'nil (vla-get-blocks (LM:acdoc))))
    (LM:acblk)
)

;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: JustifyBasePoint.lsp | Version 1.1 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"jbp\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;
