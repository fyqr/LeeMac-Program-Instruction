;;---------------------=={ Extract Nested Block }==---------------------;;
;;                                                                      ;;
;;  This program allows the user to convert a nested block into a       ;;
;;  primary block inserted at the same position for each reference of   ;;
;;  the nested block, retaining the same scale, rotation, orientation   ;;
;;  and all other properties possessed by the original nested block.    ;;
;;                                                                      ;;
;;  Upon selection of a nested block, the program will convert all      ;;
;;  references of the nested block and then proceed to remove the       ;;
;;  nested block from the primary block definition in which it resides. ;;
;;                                                                      ;;
;;  The program is compatible with uniformly-scaled nested standard     ;;
;;  blocks, nested dynamic blocks & nested xrefs, nested to any depth.  ;;
;;                                                                      ;;
;;  However, the program is incompatible with blocks nested within      ;;
;;  dynamic blocks or xrefs, and the program will ignore                ;;
;;  non-uniformly-scaled block references.                              ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2013  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    04-04-2012                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    09-09-2013                                      ;;
;;                                                                      ;;
;;  - Program modified to prohibit selection of nested blocks residing  ;;
;;    on locked layers or blocks nested within dynamic blocks or xrefs. ;;
;;                                                                      ;;
;;  - Program will now ignore non-uniformly scaled block references     ;;
;;    without returning an error.                                       ;;
;;----------------------------------------------------------------------;;
 
(defun c:enb ( / sel )
    (while
        (progn (setvar 'errno 0) (setq sel (nentselp "\nSelect nested block: "))
            (cond
                (   (= 7 (getvar 'errno))
                    (princ "\nMissed, try again.")
                )
                (   (null sel) nil)
                (   (null (cadr (setq sel (last sel))))
                    (princ "\nObject is not a nested block.")
                )
                (   (= 4 (logand 4 (cdr (assoc 70 (tblsearch "layer" (cdr (assoc 8 (entget (car sel)))))))))
                    (princ "\nThe selected block is on a locked layer.")
                )
                (   (vl-some 'enb:isxref (cdr sel))
                    (princ "\nThis program is not compatible with blocks nested within xrefs - sorry.")
                )
                (   (vl-some 'enb:isdynamic (cdr sel))
                    (princ "\nThis program is not compatible with blocks nested within dynamic blocks - sorry.")
                )
            )
        )
    )
    (if sel
        (progn
            (foreach lst (enb:getreferences (cdr (assoc 2 (entget (cadr sel)))))
                (apply
                   '(lambda ( mat vec / obj )
                        (foreach ent (cdr (reverse lst))
                            (apply
                                (function
                                    (lambda ( m v )
                                        (setq vec (mapcar '+ vec (mxv mat v))
                                              mat (mxm mat m)
                                        )
                                    )
                                )
                                (refgeom ent)
                            )
                        )
                        (if
                            (vl-catch-all-error-p
                                (vl-catch-all-apply 'vla-transformby
                                    (list (setq obj (enb:copy (car sel) (last lst)))
                                        (vlax-tmatrix
                                            (append
                                                (mapcar '(lambda ( m v ) (append m (list v))) mat vec)
                                               '((0.0 0.0 0.0 1.0))
                                            )
                                        )
                                    )
                                )
                            )
                            (vla-delete obj)
                        )
                    )
                    (refgeom (last lst))
                )
            )
            (vla-delete (vlax-ename->vla-object (car sel)))
            (vla-regen  (LM:acdoc) acactiveviewport)
        )
    )
    (princ)
)

(defun enb:copy ( ent par )
    (eval
        (list 'defun 'enb:copy '( ent par )
            (list 'car
                (list 'vlax-invoke (LM:acdoc) ''copyobjects '(list (vlax-ename->vla-object ent))
                    (if (vlax-method-applicable-p (LM:acdoc) 'objectidtoobject32)
                        (list 'vla-objectidtoobject32 (LM:acdoc) '(vla-get-ownerid32 (vlax-ename->vla-object par)))
                        (list 'vla-objectidtoobject   (LM:acdoc) '(vla-get-ownerid   (vlax-ename->vla-object par)))
                    )
                )
            )
        )
    )
    (enb:copy ent par)
)

(defun enb:getreferences ( blk / ent enx lst )
    (if (setq ent (tblobjname "block" blk))
        (foreach dxf (entget (cdr (assoc 330 (entget ent))))
            (if
                (and
                    (= 331 (car dxf))
                    (setq ent (cdr dxf))
                    (setq enx (entget ent))
                    (setq enx (entget (cdr (assoc 330 enx))))
                )
                (if (wcmatch (strcase (setq blk (cdr (assoc 2 enx)))) "`**_SPACE")
                    (setq lst (cons (list ent) lst))
                    (setq lst (append (mapcar '(lambda ( l ) (cons ent l)) (enb:getreferences blk)) lst))
                )
            )
        )
    )
    lst
)

(defun enb:isxref ( ent )
    (= 4 (logand 4 (cdr (assoc 70 (tblsearch "block" (cdr (assoc 2 (entget ent))))))))
)

(defun enb:isdynamic ( ent / obj )
    (and (vlax-property-available-p (setq obj (vlax-ename->vla-object ent)) 'isdynamicblock)
         (= :vlax-true (vla-get-isdynamicblock obj))
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
        "\n:: ExtractNestedBlock.lsp | Version 1.1 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"ENB\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;