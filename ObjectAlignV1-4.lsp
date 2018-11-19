;;--------------------------=={ Object Align }==------------------------;;
;;                                                                      ;;
;;  This program will enable the user to dynamically align a selection  ;;
;;  of objects to a selected curve, with intuitive placement controls.  ;;
;;                                                                      ;;
;;  Upon starting the program with the command syntax 'OA', the user is ;;
;;  prompted to make a selection of objects to be aligned. Following a  ;;
;;  valid selection, the user is prompted to specify a base point to    ;;
;;  use during alignment; at this prompt, the program will use the      ;;
;;  center of the bounding box of the selection of objects by default.  ;;
;;                                                                      ;;
;;  The user is then prompted to select a curve object (this may be a   ;;
;;  Line, Polyline, Arc, Circle, Ellipse, XLine, Spline etc.) to which  ;;
;;  the objects are to be aligned. The selected curve may be a primary  ;;
;;  object, or nested with a Block or XRef to any level. After          ;;
;;  selection, the program offers several controls to aid with object   ;;
;;  placement displayed at the command line:                            ;;
;;                                                                      ;;
;;  [+/-] for [O]ffset | [</>] for [R]otation | <[E]xit>:               ;;
;;                                                                      ;;
;;  The offset of the objects from the curve may be controlled          ;;
;;  incrementally by a tenth of the object height using the '+' / '-'   ;;
;;  keys, or a specific offset may be entered upon pressing the 'O' or  ;;
;;  'o' key.                                                            ;;
;;                                                                      ;;
;;  The set of objects may be rotated anti-clockwise or clockwise by    ;;
;;  45 degrees relative to the curve by pressing the '<' or '>' keys    ;;
;;  respectively; alternatively, the user may enter a specific rotation ;;
;;  by pressing the 'R' or 'r' key.                                     ;;
;;                                                                      ;;
;;  Finally, the user may place the objects and exit the program by     ;;
;;  either clicking the left or right mouse buttons, pressing Enter or  ;;
;;  Space, or by pressing the 'E' or 'e' keys.                          ;;
;;                                                                      ;;
;;  The program should perform successfully in all UCS & Views, and in  ;;
;;  all versions of AutoCAD that have Visual LISP functions available   ;;
;;  (AutoCAD 2000 onwards running on a Windows OS).                     ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2010  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2010-05-01                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2011-05-07                                      ;;
;;----------------------------------------------------------------------;;
;;  Version 1.2    -    2012-12-11                                      ;;
;;----------------------------------------------------------------------;;
;;  Version 1.3    -    2012-12-14                                      ;;
;;----------------------------------------------------------------------;;
;;  Version 1.4    -    2018-05-06                                      ;;
;;                                                                      ;;
;;  - Program modified to enable compatibility with all UCS & Views.    ;;
;;----------------------------------------------------------------------;;

(defun c:oa

    (
        /
        *error*
        bb1 bb2 blk bnm bpt
        def dis
        ent
        fac
        gr1 gr2
        idx inc
        llp lst
        mat msg
        obj ocs oss
        pi2 pt1 pt2 pt3 pt4
        sel
        tma tmp trm
        urp uxa
        vec 
    )

    (defun *error* ( msg )
        (if (and (= 'list (type trm)) (= 'ename (type ent)) (entget ent))
            (entdel ent)
        )
        (if (and (= 'vla-object (type blk)) (not (vlax-erased-p blk)))
            (vl-catch-all-apply 'vla-delete (list blk))
        )
        (if (and (= 'vla-object (type def)) (not (vlax-erased-p def)))
            (vl-catch-all-apply 'vla-delete (list def))
        )
        (foreach obj lst
            (if (not (vlax-erased-p obj))
                (vl-catch-all-apply 'vla-delete (list obj))
            )
        )
        (oa:endundo (oa:acdoc))
        (if (and msg (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    
    (oa:startundo (oa:acdoc))
    (if (null oa|rot) (setq oa|rot 0.0))
    (if (null oa|off) (setq oa|off 0.0))
    (cond
        (   (or (oa:layerlocked (getvar 'clayer))
                (oa:layerlocked "0")
            )
            (princ "\nThe current layer or layer \"0\" is locked - please unlock these layers before using this program.")
        )
        (   (null (setq oss (oa:ssget "\nSelect objects to align: " '("_:L" ((0 . "~VIEWPORT"))))))
            (princ "\n*Cancel*")
        )
        (   (progn
                (setq bpt (getpoint "\nSpecify basepoint <center>: "))
                (while
                    (progn
                        (setvar 'errno 0)
                        (setq sel (nentselp "\nSelect curve to align objects <exit>: "))
                        (cond
                            (   (= 7 (getvar 'errno))
                                (princ "\nMissed, try again.")
                            )
                            (   (= 'ename (type (car sel)))
                                (if
                                    (not
                                        (or (= "VERTEX" (cdr (assoc 0 (entget (car sel)))))
                                            (not (vl-catch-all-error-p (vl-catch-all-apply 'vlax-curve-getendparam (list (car sel)))))
                                        )
                                    )
                                    (princ "\nInvalid object selected.")
                                )
                            )
                        )
                    )
                )
                (while (/= 5 (car (setq pt1 (grread t 13 1)))))
                (null sel)
            )
        )
        (   (not
                (or
                    (and
                        (setq trm (caddr sel))
                        (setq ent (oa:copynested (car sel) trm))
                    )
                    (and
                        (= "VERTEX" (cdr (assoc 0 (entget (car sel)))))
                        (setq ent (cdr (assoc 330 (entget (car sel)))))
                    )
                    (setq ent (car sel))
                )
            )
            (princ "\nUnable to recreate nested entity.")
        )
        (   (progn
                (setq ocs (trans '(0 0 1) 1 0 t)
                      uxa (angle '(0.0 0.0) (trans (getvar 'ucsxdir) 0 ocs t))
                      mat (mxm
                              (list
                                  (list (cos uxa)     (sin uxa) 0.0)
                                  (list (- (sin uxa)) (cos uxa) 0.0)
                                 '(0.0 0.0 1.0)
                              )
                              (mapcar '(lambda ( a ) (trans a ocs 0 t))
                                 '(
                                      (1.0 0.0 0.0)
                                      (0.0 1.0 0.0)
                                      (0.0 0.0 1.0)
                                  )
                              )
                          )
                      vec (mapcar '- (mxv mat (trans '(0.0 0.0 0.0) ocs 0)))
                      tma (vlax-tmatrix (append (mapcar 'append mat (mapcar 'list vec)) '((0.0 0.0 0.0 1.0))))
                )
                (repeat (setq idx (sslength oss))
                    (setq idx (1- idx)
                          obj (vla-copy (vlax-ename->vla-object (ssname oss idx)))
                          lst (cons obj lst)
                    )
                    (vla-transformby obj tma)
                    (if (and (vlax-method-applicable-p obj 'getboundingbox)
                             (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list obj 'llp 'urp))))
                        )
                        (setq bb1 (cons (vlax-safearray->list llp) bb1)
                              bb2 (cons (vlax-safearray->list urp) bb2)
                        )
                    )
                    (vla-put-visible obj :vlax-false)
                )
                (not (and bb1 bb2))
            )
            (*error* nil)
            (princ "\nUnable to calculate bounding box for the selection.")
        )
        (   t
            (setq bb1 (apply 'mapcar (cons 'min bb1))
                  bb2 (apply 'mapcar (cons 'max bb2))
                  bpt (cond ( bpt (mapcar '+ (mxv mat (trans bpt 1 0)) vec)) ((mapcar '(lambda ( a b ) (/ (+ a b) 2.0)) bb1 bb2)))
                  fac (/ (- (cadr bb2) (cadr bb1)) 2.0)
                  pi2 (/ pi -2.0)
                  inc 0
            )
            (while (tblsearch "block" (setq bnm (strcat "$tmp" (itoa (setq inc (1+ inc)))))))
            (foreach obj lst (vla-put-visible obj :vlax-true))
            (vla-copyobjects (oa:acdoc)
                (vlax-make-variant
                    (vlax-safearray-fill
                        (vlax-make-safearray vlax-vbobject (cons 0 (1- (length lst))))
                        lst
                    )
                )
                (setq def (vla-add (vla-get-blocks (oa:acdoc)) (vlax-3D-point bpt) bnm))
            )
            (foreach obj lst (vla-delete obj))
            (setq lst nil
                  blk
                (vla-insertblock
                    (vlax-get-property (oa:acdoc) (if (= 1 (getvar 'cvport)) 'paperspace 'modelspace))
                    (vlax-3D-point (trans (cadr pt1) 1 0))
                    bnm 1.0 1.0 1.0 0.0
                )
            )
            (vla-put-layer  blk "0")
            (vla-put-normal blk (vlax-3D-point ocs))
            (setq msg (princ "\n[+/-] for [O]ffset | [</>] for [R]otation | <[E]xit>: "))

            (while
                (progn
                    (setq gr1 (grread t 15 0)
                          gr2 (cadr gr1)
                          gr1 (car  gr1)
                    )
                    (cond
                        (   (member gr1 '(3 5))
                            (setq pt2 (trans gr2 1 0)
                                  pt1 (vlax-curve-getclosestpointtoprojection ent pt2 ocs)
                                  pt3 (oa:2d (trans pt1 0 ocs))
                                  pt4 (oa:2d (trans pt2 0 ocs))
                            )
                            (if (not (equal pt3 pt4 1e-8))
                                (progn
                                    (setq dis (/ (* fac oa|off) (distance pt3 pt4)))
                                    (vla-put-insertionpoint blk
                                        (vlax-3D-point
                                            (trans
                                                (append
                                                    (mapcar '(lambda ( a b ) (+ a (* (- b a) dis))) pt3 pt4)
                                                    (list (caddr (trans pt1 0 ocs)))
                                                )
                                                ocs 0
                                            )
                                        )
                                    )
                                    (vla-put-rotation blk (+ (angle (trans pt1 0 ocs) (trans gr2 1 ocs)) oa|rot pi2))
                                )
                            )
                            (= 5 gr1)
                        )
                        (   (= 2 gr1)
                            (cond
                                (   (member gr2 '(043 061))
                                    (setq oa|off (+ oa|off 0.1))
                                )
                                (   (member gr2 '(045 095))
                                    (setq oa|off (- oa|off 0.1))
                                )
                                (   (member gr2 '(044 060))
                                    (setq oa|rot (+ oa|rot (/ pi 4.0)))
                                )
                                (   (member gr2 '(046 062))
                                    (setq oa|rot (- oa|rot (/ pi 4.0)))
                                )
                                (   (member gr2 '(013 032 069 101))
                                    nil
                                )
                                (   (member gr2 '(082 114))
                                    (if (setq tmp (getangle (strcat "\nSpecify Rotation <" (angtos oa|rot) ">: ")))
                                        (setq oa|rot tmp)
                                    )
                                    (princ msg)
                                )
                                (   (member gr2 '(079 111))
                                    (if (setq tmp (getdist (strcat "\nSpecify Offset <" (rtos (* fac oa|off)) ">: ")))
                                        (setq oa|off (/ tmp fac))
                                    )
                                    (princ msg)
                                )
                                (   t   )
                            )
                        )
                        (   (member gr1 '(011 025))
                            nil
                        )
                        (   t   )
                    )
                )
            )
            (if trm (entdel ent))
            (vla-explode blk)
            (vla-delete  blk)
            (vla-delete  def)
            (oa:endundo (oa:acdoc))
        )
    )
    (princ)
)

;;----------------------------------------------------------------------;;

(defun oa:2d ( x ) (list (car x) (cadr x)))

;;----------------------------------------------------------------------;;

(defun oa:layerlocked ( lay / def )
    (and
        (setq def (tblsearch "layer" lay))
        (= 4 (logand 4 (cdr (assoc 70 def))))
    )
)

;;----------------------------------------------------------------------;;

(defun oa:copynested ( ent mat / enx tmp )
    (if (= 1 (cdr (assoc 66 (setq enx (entget ent)))))
        (progn
            (oa:entmakex enx)
            (setq ent (entnext ent)
                  enx (entget  ent)
            )
            (while (/= "SEQEND" (cdr (assoc 0 enx)))
                (oa:entmakex enx)
                (setq ent (entnext ent)
                      enx (entget  ent)
                )
            )
            (setq tmp (cdr (assoc 330 (entget (oa:entmakex enx)))))
        )
        (setq tmp (oa:entmakex enx))
    )
    (if tmp (vla-transformby (vlax-ename->vla-object tmp) (vlax-tmatrix mat)))
    tmp
)

;;----------------------------------------------------------------------;;

(defun oa:entmakex ( enx )
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

;;----------------------------------------------------------------------;;

(defun oa:ssget ( msg arg / sel )
    (princ msg)
    (setvar 'nomutt 1)
    (setq sel (vl-catch-all-apply 'ssget arg))
    (setvar 'nomutt 0)
    (if (not (vl-catch-all-error-p sel)) sel)
)

;;----------------------------------------------------------------------;;

(defun oa:startundo ( doc )
    (oa:endundo doc)
    (vla-startundomark doc)
)

;;----------------------------------------------------------------------;;

(defun oa:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;;----------------------------------------------------------------------;;

(defun oa:acdoc nil
    (eval (list 'defun 'oa:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (oa:acdoc)
)

;;----------------------------------------------------------------------;;

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
    
;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: ObjectAlign.lsp | Version 1.4 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"oa\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;