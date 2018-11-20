;;---------------------=={ Total Area }==---------------------;;
;;                                                            ;;
;;  Displays the total area of selected objects at the        ;;
;;  command line. The precision of the printed result is      ;;
;;  dependent on the setting of the LUPREC system variable.   ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright ? 2013 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;

(defun c:tarea ( / a i s )
    (if (setq s
            (ssget
               '(   (0 . "CIRCLE,ELLIPSE,*POLYLINE,SPLINE")
                    (-4 . "<NOT")
                        (-4 . "<AND")
                            (0 . "POLYLINE") (-4 . "&") (70 . 80)
                        (-4 . "AND>")
                    (-4 . "NOT>")
                )
            )
        )
        (progn
            (setq a 0.0)
            (repeat (setq i (sslength s))
                (setq a (+ a (vlax-curve-getarea (ssname s (setq i (1- i))))))
            )
            (princ "\nTotal Area: ")
            (princ (rtos a 2))
        )
    )
    (princ)
)
(vl-load-com) (princ)