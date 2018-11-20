;;------------------=={ Offset LWPolyline Section }==-------------------;;
;;                                                                      ;;
;;  This program prompts the user to specify an offset distance and to  ;;
;;  select an LWPolyline. The user is then prompted to specify two      ;;
;;  points on the LWPolyline enclosing the section to be offset. The    ;;
;;  progam will proceed to offset all segments between the two given    ;;
;;  points to both sides by the specified distance.                     ;;
;;                                                                      ;;
;;  The program is compatible with LWPolylines of constant or varying   ;;
;;  width, with straight and/or arc segments, and defined in any UCS    ;;
;;  construction plane.                                                 ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2013  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    27-12-2012                                      ;;
;;                                                                      ;;
;;  First release.                                                      ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    05-04-2013                                      ;;
;;                                                                      ;;
;;  Fixed bug when offsetting polyline arc segments.                    ;;
;;----------------------------------------------------------------------;;

(defun c:offsec ( / d e h l m n o p q w x z )
    (if (null *off*)
        (setq *off* 1.0)
    )
    (initget 6)
    (if (setq d (getdist (strcat "\nSpecify Offset <" (rtos *off*) ">: ")))
        (setq *off* d)
        (setq d *off*)
    )
    (while
        (progn (setvar 'errno 0) (setq e (car (entsel "\nSelect LWPolyline: ")))
            (cond
                (   (= 7 (getvar 'errno))
                    (princ "\nMissed, try again.")
                )
                (   (null e) nil)
                (   (/= "LWPOLYLINE" (cdr (assoc 0 (entget e))))
                    (princ "\nObject is not an LWPolyline.")
                )
                (   (setq p (getpoint "\nSpecify 1st Point: "))
                    (setq p (vlax-curve-getclosestpointto e (trans p 1 0)))
                    (while
                        (and
                            (setq  q (getpoint (trans p 0 1) "\nSpecify 2nd Point: "))
                            (equal p (setq q (vlax-curve-getclosestpointto e (trans q 1 0))) 1e-8)
                        )
                        (princ "\nPoints must be distinct.")
                    )
                    (if q
                        (progn
                            (if (> (setq m (vlax-curve-getparamatpoint e p))
                                   (setq n (vlax-curve-getparamatpoint e q))
                                )
                                (mapcar 'set '(m n p q) (list n m q p))
                            )
                            (setq e (entget e)
                                  h (reverse (member (assoc 39 e) (reverse e)))
                                  h (subst (cons 70 (logand (cdr (assoc 70 h)) (~ 1))) (assoc 70 h) h)
                                  l (LM:LWVertices e)
                                  z (assoc 210 e)
                            )
                            (repeat (fix m)
                                (setq l (cdr l))
                            )
                            (if (not (equal m (fix m) 1e-8))
                                (setq x (car l)
                                      w (cdr (assoc 40 x))
                                      l
                                    (cons
                                        (list
                                            (cons  10 (trans p 0 (cdr z)))
                                            (cons  40 (+ w (* (- m (fix m)) (- (cdr (assoc 41 x)) w))))
                                            (assoc 41 x)
                                            (cons  42
                                                (tan
                                                    (*  (- (min n (1+ (fix m))) m)
                                                        (atan (cdr (assoc 42 x)))
                                                    )
                                                )
                                            )
                                        )
                                        (cdr l)
                                    )
                                )
                            )
                            (setq l (reverse l))
                            (repeat (+ (length l) (fix m) (- (fix n)) -1)
                                (setq l (cdr l))
                            )
                            (if (not (equal n (fix n) 1e-8))
                                (setq x (car l)
                                      w (cdr (assoc 40 x))
                                      l
                                    (vl-list*
                                        (list
                                            (cons 10 (trans q 0 (cdr z)))
                                           '(40 . 0.0)
                                           '(41 . 0.0)
                                           '(42 . 0.0)
                                        )
                                        (list
                                            (assoc 10 x)
                                            (assoc 40 x)
                                            (cons  41
                                                (+ w
                                                    (*  (/ (- n (max m (fix n))) (- (1+ (fix n)) (max m (fix n))))
                                                        (- (cdr (assoc 41 x)) w)
                                                    )
                                                )
                                            )
                                            (cons  42
                                                (tan
                                                    (*  (if (< (fix n) m) 1.0 (- n (fix n)))
                                                        (atan (cdr (assoc 42 x)))
                                                    )
                                                )
                                            )
                                        )
                                        (cdr l)
                                    )
                                )
                            )
                            (setq o
                                (vlax-ename->vla-object
                                    (entmakex (append h (apply 'append (reverse l)) (list z)))
                                )
                            )
                            (vl-catch-all-apply 'vla-offset (list o d))
                            (vl-catch-all-apply 'vla-offset (list o (- d)))
                            (vla-delete o)
                        )
                    )
                )
            )
        )
    )
    (princ)
)

;; Tangent  -  Lee Mac
;; Args: x - real
 
(defun tan ( x )
    (if (not (equal 0.0 (cos x) 1e-8))
        (/ (sin x) (cos x))
    )
)
 
;; LW Vertices  -  Lee Mac
;; Returns a list of lists in which each sublist describes the position,
;; starting width, ending width and bulge of a vertex of an LWPolyline
 
(defun LM:LWVertices ( e )
    (if (setq e (member (assoc 10 e) e))
        (cons
            (list
                (assoc 10 e)
                (assoc 40 e)
                (assoc 41 e)
                (assoc 42 e)
            )
            (LM:LWVertices (cdr e))
        )
    )
)

;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: OffsetSection.lsp | Version 1.1 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"offsec\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;