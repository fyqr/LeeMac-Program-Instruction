;;------------------=={ Isometric Polygon }==-----------------;;
;;                                                            ;;
;;  Enables the user to construct a regular polygon projected ;;
;;  in the active isometric plane.                            ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2012 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;

(defun c:isopoly ( / a b c i l m p r s )
    (if (null n) (setq n 6))
    (while
        (< (setq n (cond ((getint (strcat "\nSpecify Number of Sides <" (itoa n) ">: "))) ( n ))) 3)
        (princ "\nRequires an integer greater than or equal to 3.")
    )
    (if (and (setq c (getpoint "\nSpecify Center of Polygon: "))
             (progn
                 (initget 6)
                 (setq r (getdist c "\nSpecify Radius: "))
             )
        )
        (progn
            (if (= 0 (getvar 'snapstyl))
                (setq a 1.0
                      b 1.0
                )
                (setq a (/ (sqrt 3.0) (sqrt 2.0))
                      b (/ 1.0 (sqrt 2.0))
                )
            )
            (setq i (/ (+ pi pi) n)
                  c (trans c 1 0)
                  p (/ pi 3.0)
                  s 0.0
            )
            (repeat n
                (setq l (cons (list (* a r (cos s)) (* b r (sin s))) l)
                      s (+ i s)
                )
            )
            (if (= 0 (getvar 'snapstyl))
                (setq m '((1.0 0.0) (0.0 1.0)))
                (setq m
                    (cdr
                        (assoc (getvar 'snapisopair)
                            (list
                                (list 0 (list (cos p) (sin p)) (list (sin (- p)) (cos p)))
                               '(1 (1.0 0.0) (0.0 1.0))
                                (list 2 (list (cos p) (sin (- p))) (list (sin p) (cos p)))
                            )
                        )
                    )
                )
            )
            (entmake
                (append
                    (list
                       '(0 . "LWPOLYLINE")
                       '(100 . "AcDbEntity")
                       '(100 . "AcDbPolyline")
                        (cons 90 n)
                       '(70 . 1)
                    )
                    (mapcar '(lambda ( p ) (cons 10 (mapcar '+ (mxv m p) c))) l)
                )
            )
        )
    )
    (princ)
)

;; Matrix x Vector  -  Vladimir Nesterovsky
;; Args: m - nxn matrix, v - vector in R^n

(defun mxv ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)
(princ)