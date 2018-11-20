;;--------------------=={ Total Length }==--------------------;;
;;                                                            ;;
;;  Displays the total length of selected objects at the      ;;
;;  command line. The units and precision format of the       ;;
;;  printed result is dependent upon the settings of the      ;;
;;  LUNITS & LUPREC system variables respectively.            ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright ? 2013 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;

(defun c:tlen ( / e i l s )
    (if (setq s
            (ssget
               '(   (0 . "ARC,CIRCLE,ELLIPSE,LINE,*POLYLINE,SPLINE")
                    (-4 . "<NOT")
                        (-4 . "<AND")
                            (0 . "POLYLINE") (-4 . "&") (70 . 80)
                        (-4 . "AND>")
                    (-4 . "NOT>")
                )
            )
        )
        (progn
            (setq l 0.0)
            (repeat (setq i (sslength s))
                (setq e (ssname s (setq i (1- i)))
                      l (+ l (vlax-curve-getdistatparam e (vlax-curve-getendparam e)))
                )
            )
            (princ "\nTotal Length: ")
            (princ (rtos l))
        )
    )
    (princ)
)
(vl-load-com) (princ)