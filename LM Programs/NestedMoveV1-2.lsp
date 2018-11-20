;;-------------------------=={ Nested Move }==--------------------------;;
;;                                                                      ;;
;;  This program enables the user to move multiple selected objects     ;;
;;  nested within a block or xref (nested to any depth), without        ;;
;;  opening the xref source drawing.                                    ;;
;;                                                                      ;;
;;  Upon calling the program with 'nmove' at the command-line, the      ;;
;;  user is continuously prompted to select objects nested within a     ;;
;;  block or xref to be moved. Similar to the standard move command,    ;;
;;  the user is then prompted to specify a base point & displacement    ;;
;;  for the move operation.                                             ;;
;;                                                                      ;;
;;  The program will then proceed to move the selected object within    ;;
;;  the parent block definition or the xref source drawing, utilising   ;;
;;  an ObjectDBX interface should the xref source drawing be unopened   ;;
;;  in the current drawing session.                                     ;;
;;                                                                      ;;
;;  Upon moving the object: in the case of a block parent, the active   ;;
;;  drawing is regenerated to reflect the modification to the block     ;;
;;  definition across all block references in the drawing; for xrefs,   ;;
;;  the xref source drawing is saved and the xref is reloaded in the    ;;
;;  active drawing.                                                     ;;
;;                                                                      ;;
;;  The program will account for the position, scale, rotation &        ;;
;;  orientation of the parent block or xref relative to the base point  ;;
;;  and displacement vector and will perform successfully under all     ;;
;;  UCS & View settings.                                                ;;
;;                                                                      ;;
;;  Please Note:                                                        ;;
;;  ------------------------------                                      ;;
;;  The act of modifying objects within an xref source drawing involves ;;
;;  saving the external drawing remotely - this action cannot be        ;;
;;  undone within the current drawing and any changes to the external   ;;
;;  drawing must be reset manually.                                     ;;
;;                                                                      ;;
;;  Note that when saving drawings through ObjectDBX, drawing files     ;;
;;  will be saved to the highest version available and drawing file     ;;
;;  thumbnails will be lost until the next manual save.                 ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2013  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    10-08-2013                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    26-08-2013                                      ;;
;;                                                                      ;;
;;  - Modified program to move the 'outermost' nested object within     ;;
;;    the parent block or xref.                                         ;;
;;  - Added ability to select multiple nested objects to be moved.      ;;
;;  - Added selection highlighting for visual feedback of selected      ;;
;;    nested objects.                                                   ;;
;;----------------------------------------------------------------------;;
;;  Version 1.2    -    31-08-2013                                      ;;
;;                                                                      ;;
;;  - Fixed bug causing the program to crash when a nested attributed   ;;
;;    block or nested polyline is selected.                             ;;
;;----------------------------------------------------------------------;;

(defun c:nmove ( / *error* app dbx def doc dwg dwl han hil lst obj own par pt1 pt2 sel tmp vec vrs )

    (defun *error* ( msg )
        (if (and (= 'vla-object (type dbx)) (not (vlax-object-released-p dbx)))
            (vlax-release-object dbx)
        )
        (foreach ent hil
            (if (and (= 'ename (type ent)) (entget ent))
                (entdel ent)
            )
        )
        (LM:endundo (LM:acdoc))
        (if (and msg (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    (LM:startundo (LM:acdoc))
    
    (while
        (progn (setvar 'errno 0) (setq sel (nentselp "\nSelect nested objects to move <Done>: "))
            (cond
                (   (= 7 (getvar 'errno))
                    (princ "\nMissed, try again.")
                )
                (   (null sel)
                    nil
                )
                (   (or (= 2 (length sel))
                        (/= "INSERT" (cdr (assoc 0 (entget (setq own (last (last sel)))))))
                    )
                    (princ "\nObject is not nested within a block or xref.")
                )
                (   (= :vlax-true (vla-get-isdynamicblock (vlax-ename->vla-object own)))
                    (princ "\nThis program is not compatible with dynamic blocks - sorry.")
                )
                (   (and (= 'ename (type par)) (not (eq par own)))
                    (princ "\nObject is nested within a different parent.")
                )
                (   t
                    (if (null par)
                        (setq par own)
                    )
                    (if (wcmatch (cdr (assoc 0 (entget (car sel)))) "ATTRIB,VERTEX")
                        (setq sel (cons (cdr (assoc 330 (entget (car sel)))) (cdr sel)))
                    )
                    (if (cdr (last sel))
                        (setq lst (cons (cons (cadr (reverse (last sel))) (car (revrefgeom par))) lst))
                        (setq lst (cons (cons (car sel) (mapcar 'rcdr (rcdr (invm (caddr sel))))) lst))
                    )
                    (if (setq tmp (nmove:copynested (caar lst)))
                        (progn
                            (vla-transformby (vlax-ename->vla-object tmp)
                                (vlax-tmatrix
                                    (if (cdr (last sel))
                                        (append
                                            (apply 'mapcar
                                                (cons '(lambda ( r v ) (append r (list v)))
                                                    (refgeom par)
                                                )
                                            )
                                           '((0.0 0.0 0.0 1.0))
                                        )
                                        (caddr sel)
                                    )
                                )
                            )
                            (redraw tmp 3)
                            (setq hil (cons tmp hil))
                        )
                    )
                    t
                )
            )
        )
    )
    (cond
        (   (not
                (and
                    (= 'list (type lst))
                    (setq pt1 (getpoint "\nSpecify base point: "))
                    (setq pt2 (getpoint "\nSpecify next point: " pt1))
                    (setq vec (mapcar '- (trans pt2 1 0) (trans pt1 1 0)))
                )
            )
        )
        (   (zerop (logand 4 (cdr (assoc 70 (setq def (tblsearch "block" (LM:blockname (vlax-ename->vla-object par))))))))
            (foreach itm lst
                (if (vlax-write-enabled-p (setq obj (vlax-ename->vla-object (car itm))))
                    (vla-move obj
                        (vlax-3D-point 0 0)
                        (vlax-3D-point (mxv (cdr itm) vec))
                    )
                    (princ (strcat "\nObject with handle \"" (vla-get-handle obj) "\" is on a locked layer."))
                )
            )
            (vla-regen (LM:acdoc) acallviewports)
        )
        (   (progn
                (setq dbx
                    (vl-catch-all-apply 'vla-getinterfaceobject
                        (list (setq app (vlax-get-acad-object))
                            (if (< (setq vrs (atoi (getvar 'acadver))) 16)
                                "objectdbx.axdbdocument" (strcat "objectdbx.axdbdocument." (itoa vrs))
                            )
                        )
                    )
                )
                (or (null dbx) (vl-catch-all-error-p dbx))
            )
            (prompt "\nUnable to interface with ObjectDBX.")
        )
        (   (not
                (and (setq dwg (findfile (cdr (assoc 1 def))))
                    (or
                        (progn
                            (vlax-for doc (vla-get-documents app)
                                (setq dwl (cons (cons (strcase (vla-get-fullname doc)) doc) dwl))
                            )
                            (setq doc (cdr (assoc (strcase dwg) dwl)))
                        )
                        (and (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-open (list dbx dwg))))
                             (setq doc dbx)
                        )
                    )
                )
            )
            (princ "\nUnable to interface with xref source drawing.")
        )
        (   t
            (foreach itm lst
                (cond
                    (   (vl-catch-all-error-p
                            (setq obj
                                (vl-catch-all-apply 'vla-handletoobject
                                    (list doc (setq han (cdr (assoc 5 (entget (car itm))))))
                                )
                            )
                        )
                        (princ (strcat "\nUnable to locate object with handle \"" han "\" in xref source drawing."))
                    )
                    (   (vlax-write-enabled-p obj)
                        (vla-move obj
                            (vlax-3D-point 0 0)
                            (vlax-3D-point (mxv (cdr itm) vec))
                        )
                    )
                    (   (princ (strcat "\nObject with handle \"" han "\" is on a locked layer in xref source drawing.")))
                )
            )
            (vla-saveas doc dwg)
            (vla-reload (vla-item (vla-get-blocks (LM:acdoc)) (cdr (assoc 2 def))))
        )
    )
    (*error* nil)
    (princ)
)

(defun nmove:copynested ( ent / enx )
    (if (= 1 (cdr (assoc 66 (setq enx (entget ent)))))
        (progn
            (nmove:entmakex enx)
            (setq ent (entnext ent)
                  enx (entget  ent)
            )
            (while (/= "SEQEND" (cdr (assoc 0 enx)))
                (nmove:entmakex enx)
                (setq ent (entnext ent)
                      enx (entget  ent)
                )
            )
            (cdr (assoc 330 (entget (nmove:entmakex enx))))
        )
        (nmove:entmakex enx)
    )
)

(defun nmove:entmakex ( enx )
    (entmakex
        (append
            (vl-remove-if
                (function
                    (lambda ( x )
                        (or (member (car x) '(005 006 008 039 048 062 102 370))
                            (= 'ename (type (cdr x)))
                        )
                    )
                )
                enx
            )
           '(
                (006 . "CONTINUOUS")
                (008 . "0")
                (039 . 0.0)
                (048 . 1.0)
                (062 . 7)
                (370 . 0)
            )
        )
    )
)

;; RefGeom (gile)
;; Returns a list whose first item is a 3x3 transformation matrix and
;; second item the object insertion point in its parent (xref, block or space)

(defun refgeom ( ent / ang enx mat ocs )
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

;; RevRefGeom (gile)
;; The inverse of RefGeom

(defun revrefgeom ( ent / ang enx mat ocs )
    (setq enx (entget ent)
          ang (cdr (assoc 050 enx))
          ocs (cdr (assoc 210 enx))
    )
    (list
        (setq mat
            (mxm
                (list
                    (list (/ 1.0 (cdr (assoc 41 enx))) 0.0 0.0)
                    (list 0.0 (/ 1.0 (cdr (assoc 42 enx))) 0.0)
                    (list 0.0 0.0 (/ 1.0 (cdr (assoc 43 enx))))
                )
                (mxm
                    (list
                        (list (cos ang)     (sin ang) 0.0)
                        (list (- (sin ang)) (cos ang) 0.0)
                       '(0.0 0.0 1.0)
                    )
                    (mapcar '(lambda ( v ) (trans v ocs 0 t))
                        '(
                             (1.0 0.0 0.0)
                             (0.0 1.0 0.0)
                             (0.0 0.0 1.0)
                         )
                    )
                )
            )
        )
        (mapcar '- (cdr (assoc 10 (tblsearch "block" (cdr (assoc 2 enx)))))
            (mxv mat (trans (cdr (assoc 10 enx)) ocs 0))
        )
    )
)

;; Matrix Inverse  -  gile & Lee Mac
;; Uses Gauss-Jordan Elimination to return the inverse of a non-singular nxn matrix.
;; Args: m - nxn matrix

(defun invm ( m / c f p r )
    
    (defun f ( p m )
        (mapcar '(lambda ( x ) (mapcar '(lambda ( a b ) (- a (* (car x) b))) (cdr x) p)) m)
    )
    (setq m (mapcar 'append m (imat (length m))))
    (while m
        (setq c (mapcar '(lambda ( x ) (abs (car x))) m))
        (repeat (vl-position (apply 'max c) c)
            (setq m (append (cdr m) (list (car m))))
        )
        (if (equal 0.0 (caar m) 1e-14)
            (setq m nil
                  r nil
            )
            (setq p (mapcar '(lambda ( x ) (/ (float x) (caar m))) (cdar m))
                  m (f p (cdr m))
                  r (cons p (f p r))
            )
        )
    )
    (reverse r)
)

;; Identity Matrix  -  Lee Mac
;; Args: n - matrix dimension

(defun imat ( n / i j l m )
    (repeat (setq i n)
        (repeat (setq j n)
            (setq l (cons (if (= i j) 1.0 0.0) l) j (1- j))
        )
        (setq m (cons l m) l nil i (1- i))
    )
    m
)

;; Reverse cdr  -  Lee Mac
;; Returns all but the last item of a list

(defun rcdr ( l )
    (reverse (cdr (reverse l)))
)

;; Matrix x Vector  -  Vladimir Nesterovsky
;; Args: m - nxn matrix, v - vector in R^n

(defun mxv ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)

;; Matrix x Matrix  -  Vladimir Nesterovsky
;; Args: m,n - nxn matrices

(defun mxm ( m n )
    ((lambda ( a ) (mapcar '(lambda ( r ) (mxv a r)) m)) (trp n))
)

;; Matrix Transpose  -  Doug Wilson
;; Args: m - nxn matrix

(defun trp ( m )
    (apply 'mapcar (cons 'list m))
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
        "\n:: NestedMove.lsp | Version 1.2 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,$(getvar,date),YYYY)")
        " www.lee-mac.com ::"
        "\n:: Type \"nmove\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;