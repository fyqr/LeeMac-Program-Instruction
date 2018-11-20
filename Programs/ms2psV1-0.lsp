;;-------------------=={ Modelspace to Paperspace }==-------------------;;
;;                                                                      ;;
;;  This program allows the user to copy a selection of objects from    ;;
;;  Modelspace to Paperspace through an active viewport.                ;;
;;                                                                      ;;
;;  This functionality is similar to the standard CHSPACE command,      ;;
;;  however, this program will copy the selection of objects as         ;;
;;  opposed to moving the objects from Modelspace to Paperspace.        ;;
;;                                                                      ;;
;;  Upon issuing the command 'ms2ps' at the AutoCAD command-line, the   ;;
;;  user is prompted to make a selection of objects to copy.            ;;
;;                                                                      ;;
;;  Following a valid selection, the program will copy the objects to   ;;
;;  the active Paperspace layout, before performing the necessary       ;;
;;  matrix transformations to maintain the visual appearance of the     ;;
;;  objects as displayed through the active viewport.                   ;;
;;                                                                      ;;
;;  The program is compatible with rectangular & polygonal viewports,   ;;
;;  under all UCS & view settings, and with objects constructed in      ;;
;;  any UCS construction plane.                                         ;;
;;                                                                      ;;
;;  Note that the command is only available when a Paperspace Layout    ;;
;;  is set current, with a viewport active.                             ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2014  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2014-02-16                                      ;;
;;                                                                      ;;
;;  First release.                                                      ;;
;;----------------------------------------------------------------------;;
 
(defun c:ms2ps ( / *error* ang doc enx idx lst mat nor scl sel )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (LM:startundo (LM:acdoc))
    (cond
        (   (= 1 (getvar 'tilemode))
            (prompt "\nCommand only available in Paperspace.")
        )
        (   (= 1 (getvar 'cvport))
            (prompt "\nPlease activate a viewport.")
        )
        (   (setq sel (ssget '((410 . "Model"))))
            (repeat (setq idx (sslength sel))
                (setq lst (cons (vlax-ename->vla-object (ssname sel (setq idx (1- idx)))) lst))
            )
            (setq enx (entget (ssname (ssget "_X" (list '(0 . "VIEWPORT") (cons 69 (getvar 'cvport)))) 0))
                  ang (cdr (assoc 51 enx))
                  nor (cdr (assoc 16 enx))
                  scl (/ (cdr (assoc 41 enx)) (cdr (assoc 45 enx))) 
            )
            (setq mat
                (vlax-tmatrix
                    (append
                        (mapcar '(lambda ( a b ) (append a (list b)))
                            (setq mat ;; The following is adapted from gile's WCS2PCS function:
                                (mxm
                                    (list
                                        (list (cos ang) (- (sin ang)) 0.0)
                                        (list (sin ang)    (cos ang)  0.0)
                                       '(0.0 0.0 1.0)
                                    )
                                    (mapcar (function (lambda ( v ) (vxs (trans v nor 0 t) scl)))
                                       '(
                                            (1.0 0.0 0.0)
                                            (0.0 1.0 0.0)
                                            (0.0 0.0 1.0)
                                        )
                                    )
                                )
                            )
                            (mapcar '+
                                (mxv mat (mapcar '- (cdr (assoc 17 enx))))
                                (vxs (cdr (assoc 12 enx)) (- scl))
                                (cdr (assoc 10 enx))
                            )
                        )
                       '((0.0 0.0 0.0 1.0))
                    )
                )
            )
            (foreach obj
                (vlax-invoke (setq doc (vla-get-activedocument (vlax-get-acad-object))) 'copyobjects lst
                    (vla-get-block
                        (vla-item
                            (vla-get-layouts doc)
                            (getvar 'ctab)
                        )
                    )
                )
                (vla-transformby obj mat)
            )
        )
    )
    (LM:endundo (LM:acdoc))
    (princ)
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

(vl-load-com)
(princ
    (strcat
        "\n:: ms2ps.lsp | Version 1.0 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"ms2ps\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;