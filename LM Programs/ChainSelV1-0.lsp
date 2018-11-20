;;--------------------=={ Chain Selection }==-----------------;;
;;                                                            ;;
;;  Prompts the user to select an object and generates a      ;;
;;  selection chain of all objects sharing endpoints with     ;;
;;  objects in the accumulative selection.                    ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2012 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;

(defun c:cs ( / en fl in l1 l2 s1 s2 sf vl )
    (setq sf
        (list
           '(-4 . "<OR")
               '(0 . "LINE,ARC")
               '(-4 . "<AND")
                   '(0 . "LWPOLYLINE,SPLINE")
                   '(-4 . "<NOT")
                       '(-4 . "&=")
                       '(70 . 1)
                   '(-4 . "NOT>")
               '(-4 . "AND>")
               '(-4 . "<AND")
                   '(0 . "POLYLINE")
                   '(-4 . "<NOT")
                       '(-4 . "&")
                       '(70 . 89)
                   '(-4 . "NOT>")
                   '(-4 . "AND>")
               '(-4 . "<AND")
                   '(0 . "ELLIPSE")
                   '(-4 . "<OR")
                       '(-4 . "<>")
                       '(41 . 0.0)
                       '(-4 . "<>")
                        (cons 42 (+ pi pi))
                   '(-4 . "OR>")
               '(-4 . "AND>")
           '(-4 . "OR>")
            (if (= 1 (getvar 'cvport))
                (cons 410 (getvar 'ctab))
               '(410 . "Model")
            )
        )
    )
    (if (setq s1 (ssget "_X" sf))
        (if (setq en (ssget "_+.:E:S" sf))
            (progn
                (setq s2 (ssadd)
                      en (ssname en 0)
                      l1 (list (vlax-curve-getstartpoint en) (vlax-curve-getendpoint en))
                )
                (repeat (setq in (sslength s1))
                    (setq en (ssname s1 (setq in (1- in)))
                          vl (cons (list (vlax-curve-getstartpoint en) (vlax-curve-getendpoint en) en) vl)
                    )
                )
                (while
                    (progn
                        (foreach v vl
                            (if (vl-some '(lambda ( p ) (or (equal (car v) p 1e-8) (equal (cadr v) p 1e-8))) l1)
                                (setq s2 (ssadd (caddr v) s2)
                                      l1 (vl-list* (car v) (cadr v) l1)
                                      fl t
                                )
                                (setq l2 (cons v l2))
                            )
                        )
                        fl
                    )
                    (setq vl l2 l2 nil fl nil)
                )
            )
        )
        (princ "\nNo valid objects found.")
    )
    (sssetfirst nil s2)
    (princ)
)
(vl-load-com) (princ)