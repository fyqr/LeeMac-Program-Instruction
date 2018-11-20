;;-----------------------=={ Viewport Outline }==-----------------------;;
;;                                                                      ;;
;;  This program allows the user to automatically generate a polyline   ;;
;;  in modelspace representing the outline of a selected paperspace     ;;
;;  viewport.                                                           ;;
;;                                                                      ;;
;;  The command is only available in paperspace (that is, when a        ;;
;;  layout tab other than the Model tab is the current layout, and no   ;;
;;  viewports are active).                                              ;;
;;                                                                      ;;
;;  Upon issuing the command syntax 'VPO' at the AutoCAD command-line,  ;;
;;  the user is prompted to select a viewport for which to construct    ;;
;;  the viewport outline in modelspace.                                 ;;
;;                                                                      ;;
;;  Following a valid selection, the boundary of the selected viewport  ;;
;;  is transformed appropriately to account for the position, scale,    ;;
;;  rotation, & orientation of the modelspace view displayed through    ;;
;;  the selected viewport, and a 2D polyline (LWPolyline) representing  ;;
;;  this transformed boundary is constructed in modelspace.             ;;
;;                                                                      ;;
;;  The program is compatible for use with all Rectangular, Polygonal & ;;
;;  Clipped Viewports (including those with Arc segments), and with all ;;
;;  views & construction planes.                                        ;;
;;                                                                      ;;
;;  The program also offers the ability to optionally offset the        ;;
;;  polyline outline to the interior of the viewport boundary by a      ;;
;;  predetermined number of paperspace units specified in the           ;;
;;  'Program Parameters' section of the program source code.            ;;
;;                                                                      ;;
;;  The program may also be configured to automatically apply a         ;;
;;  predefined set of properties (e.g. layer, colour, linetype, etc.)   ;;
;;  to the resulting polyline outline - these properties are also       ;;
;;  listed within the 'Program Parameters' section of the source code.  ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2015  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2015-01-02                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2016-08-11                                      ;;
;;                                                                      ;;
;;  - Program modified to account for polygonal viewports represented   ;;
;;    by 2D (Heavy) Polylines.                                          ;;
;;----------------------------------------------------------------------;;
;;  Version 1.2    -    2017-09-03                                      ;;
;;                                                                      ;;
;;  - Added the ability to specify an optional interior offset          ;;
;;    (relative to Paperspace Viewport dimensions).                     ;;
;;  - Added default polyline properties.                                ;;
;;----------------------------------------------------------------------;;

(defun c:vpo ( / *error* cen dpr ent lst ocs ofe off tmp vpe vpt )

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

        ;; Optional Interior Offset
        ;; Set this parameter to nil or 0.0 for no offset
        off 0.0

        ;; Default Polyline Properties
        ;; Omitted properties will use current settings when the program is run
        dpr
       '(
            (006 . "BYLAYER")   ;; Linetype (must be loaded)
           ;(008 . "VPOutline") ;; Layer (automatically created if not present in drawing)
            (039 . 0.0)         ;; Thickness
            (048 . 1.0)         ;; Linetype Scale
            (062 . 256)         ;; Colour (0 = ByBlock, 256 = ByLayer)
            (370 . -1)          ;; Lineweight (-1 = ByLayer, -2 = ByBlock, -3 = Default, 0.3 = 30 etc.)
        )
        
;;----------------------------------------------------------------------;;

    )
    
    (LM:startundo (LM:acdoc))
    (cond
        (   (/= 1 (getvar 'cvport))
            (princ "\nCommand not available in Modelspace.")
        )
        (   (setq vpt (LM:ssget "\nSelect viewport: " '("_+.:E:S" ((0 . "VIEWPORT")))))
            (setq vpt (entget (ssname vpt 0)))
            (if (setq ent (cdr (assoc 340 vpt)))
                (setq lst (vpo:polyvertices ent))
                (setq cen (mapcar 'list (cdr (assoc 10 vpt))
                              (list
                                  (/ (cdr (assoc 40 vpt)) 2.0)
                                  (/ (cdr (assoc 41 vpt)) 2.0)
                              )
                          )
                      lst (mapcar
                             '(lambda ( a ) (cons (mapcar 'apply a cen) '(42 . 0.0)))
                             '((- -) (+ -) (+ +) (- +))
                          )
                )
            )
            (if (not (LM:listclockwise-p (mapcar 'car lst)))
                (setq lst (reverse (mapcar '(lambda ( a b ) (cons (car a) (cons 42 (- (cddr b))))) lst (cons (last lst) lst))))
            )
            (if (and (numberp off) (not (equal 0.0 off 1e-8)))
                (cond
                    (   (null
                            (setq tmp
                                (entmakex
                                    (append
                                        (list
                                           '(000 . "LWPOLYLINE")
                                           '(100 . "AcDbEntity")
                                           '(100 . "AcDbPolyline")
                                            (cons 90 (length lst))
                                           '(070 . 1)
                                        )
                                        (apply 'append (mapcar '(lambda ( x ) (list (cons 10 (car x)) (cdr x))) lst))
                                    )
                                )
                            )
                        )
                        (princ "\nUnable to generate Paperspace outline for offset.")
                    )
                    (   (vl-catch-all-error-p (setq ofe (vl-catch-all-apply 'vlax-invoke (list (vlax-ename->vla-object tmp) 'offset off))))
                        (princ (strcat "\nViewport dimensions too small to offset outline by " (rtos off) " units."))
                        (entdel tmp)
                    )
                    (   (setq ofe (vlax-vla-object->ename (car ofe))
                              lst (vpo:polyvertices ofe)
                        )
                        (entdel ofe)
                        (entdel tmp)
                    )
            	)
            )
            (setq vpe (cdr (assoc -1 vpt))
                  ocs (cdr (assoc 16 vpt))
            )
            (entmakex
                (append
                    (list
                       '(000 . "LWPOLYLINE")
                       '(100 . "AcDbEntity")
                       '(100 . "AcDbPolyline")
                        (cons 90 (length lst))
                       '(070 . 1)
                       '(410 . "Model")
                    )
                    (if (and (setq ltp (assoc 6 dpr)) (not (tblsearch "ltype" (cdr ltp))))
                        (progn
                            (princ  (strcat "\n\"" (cdr ltp) "\" linetype not loaded - linetype set to \"ByLayer\"."))
                            (subst '(6 . "BYLAYER") ltp dpr)
                        )
                        dpr
                    )
                    (apply 'append (mapcar '(lambda ( x ) (list (cons 10 (trans (pcs2wcs (car x) vpe) 0 ocs)) (cdr x))) lst))
                    (list (cons 210 ocs))
                )
            )
        )
    )
    (LM:endundo (LM:acdoc))
    (princ)
)

(defun vpo:polyvertices ( ent )
    (apply '(lambda ( foo bar ) (foo bar))
        (if (= "LWPOLYLINE" (cdr (assoc 0 (entget ent))))
            (list
                (lambda ( enx )
                    (if (setq enx (member (assoc 10 enx) enx))
                        (cons (cons  (cdr (assoc 10 enx)) (assoc 42 enx)) (foo (cdr enx)))
                    )
                )
                (entget ent)
            )
            (list
                (lambda ( ent / enx )
                    (if (= "VERTEX" (cdr (assoc 0 (setq enx (entget ent)))))
                        (cons (cons (cdr (assoc 10 enx)) (assoc 42 enx)) (foo (entnext ent)))
                    )
            	)
                (entnext ent)
            )
        )
    )
)

;; List Clockwise-p  -  Lee Mac
;; Returns T if the point list is clockwise oriented

(defun LM:listclockwise-p ( lst )
    (minusp
        (apply '+
            (mapcar
                (function
                    (lambda ( a b )
                        (- (* (car b) (cadr a)) (* (car a) (cadr b)))
                    )
                )
                lst (cons (last lst) lst)
            )
        )
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

;; PCS2WCS (gile)
;; Translates a PCS point to WCS based on the supplied Viewport
;; (PCS2WCS pt vp) is the same as (trans (trans pt 3 2) 2 0) when vp is active
;; pnt : PCS point
;; ent : Viewport ename

(defun PCS2WCS ( pnt ent / ang enx mat nor scl )
    (setq pnt (trans pnt 0 0)
          enx (entget ent)
          ang (- (cdr (assoc 51 enx)))
          nor (cdr (assoc 16 enx))
          scl (/ (cdr (assoc 45 enx)) (cdr (assoc 41 enx)))
          mat (mxm
                  (mapcar (function (lambda ( v ) (trans v 0 nor t)))
                     '(   (1.0 0.0 0.0)
                          (0.0 1.0 0.0)
                          (0.0 0.0 1.0)
                      )
                  )
                  (list
                      (list (cos ang) (- (sin ang)) 0.0)
                      (list (sin ang)    (cos ang)  0.0)
                     '(0.0 0.0 1.0)
                  )
              )
    )
    (mapcar '+
        (mxv mat
            (mapcar '+
                (vxs pnt scl)
                (vxs (cdr (assoc 10 enx)) (- scl))
                (cdr (assoc 12 enx))
            )
        )
        (cdr (assoc 17 enx))
    )
)

;; Matrix Transpose  -  Doug Wilson
;; Args: m - nxn matrix

(defun trp ( m )
    (apply 'mapcar (cons 'list m))
)

;; Matrix x Matrix  -  Vladimir Nesterovsky
;; Args: m,n - nxn matrices

(defun mxm ( m n )
    ((lambda ( a ) (mapcar '(lambda ( r ) (mxv a r)) m)) (trp n))
)

;; Matrix x Vector  -  Vladimir Nesterovsky
;; Args: m - nxn matrix, v - vector in R^n

(defun mxv ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)

;; Vector x Scalar  -  Lee Mac
;; Args: v - vector in R^n, s - real scalar

(defun vxs ( v s )
    (mapcar '(lambda ( n ) (* n s)) v)
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

(princ
    (strcat
        "\n:: VPOutline.lsp | Version 1.2 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"vpo\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;