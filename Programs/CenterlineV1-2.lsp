;;--------------------=={ Centerline }==----------------------;;
;;                                                            ;;
;;  Creates a pair of centerlines for Arcs, Circles, Ellipses ;;
;;  and Elliptical arcs with a user-defined length and angle. ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2013 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Version 1.2    -    13-08-2013                            ;;
;;------------------------------------------------------------;;

(defun c:cline ( / ang dis ent pt1 pt2 )
    (if
        (and
            (progn
                (while
                    (progn (setvar 'errno 0) (setq ent (car (entsel "\nSelect Arc, Circle or Ellipse: ")))
                        (cond
                            (   (= 7 (getvar 'errno))
                                (princ "\nMissed, try again.")
                            )
                            (   (= 'ename (type ent))
                                (if (not (wcmatch (cdr (assoc 0 (entget ent))) "CIRCLE,ARC,ELLIPSE"))
                                    (princ "\nInvalid Object Selected.")
                                )
                            )
                        )
                    )
                )
                (= 'ename (type ent))
            )
            (setq pt1 (trans (cdr (assoc 10 (entget ent))) ent 1))
            (setq pt2 (getpoint "\nSpecify Length & Direction of Centreline: " pt1))
            (setq ang (angle pt1 pt2)
                  dis (distance pt1 pt2)
            )
        )
        (repeat 2
            (entmake
                (list
                   '(0 . "LINE")
                    (cons 10 (trans (polar pt1 ang dis) 1 0))
                    (cons 11 (trans (polar pt1 ang (- dis)) 1 0))
                )
            )
            (setq ang (+ ang (/ pi 2.0))
                  dis (- dis)
            )
        )
    )
    (princ)
)
(princ)