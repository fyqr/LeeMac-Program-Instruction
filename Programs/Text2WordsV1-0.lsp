;;--------------------=={ Text to Words }==-------------------;;
;;                                                            ;;
;;  Prompts the user for a selection of Text objects and      ;;
;;  converts each object in the selection into separate Text  ;;
;;  objects for each word, retaining all properties of the    ;;
;;  original objects.                                         ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2013 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;

(defun c:t2w ( / _splitwords _textwidth ang dxf ent enx fun inc lst pnt sel spc tot wid )

    (defun _splitwords ( str / pos )
        (if (setq pos (vl-string-position 32 str))
            (cons (cons 1 (substr str 1 pos)) (_splitwords (substr str (+ pos 2))))
            (list (cons 1 str))
        )
    )

    (defun _textwidth ( enx )
        ((lambda ( lst ) (- (caadr lst) (caar lst))) (textbox enx))
    )

    (if (setq sel (ssget "_:L" '((0 . "TEXT") (-4 . "<NOT") (-4 . "<OR") (72 . 3) (72 . 4) (72 . 5) (-4 . "OR>") (-4 . "NOT>"))))
        (repeat (setq inc (sslength sel))
            (setq ent (ssname sel (setq inc (1- inc)))
                  enx (entget ent)
                  tot 0.0
                  lst nil
            )
            (foreach item (_splitwords (cdr (assoc 1 enx)))
                (if (= "" (cdr item))
                    (setq lst (cons '(nil . 0.0) lst))
                    (setq dxf (entget (entmakex (subst item (assoc 1 enx) enx)))
                          wid (_textwidth dxf)
                          tot (+ tot wid)
                          lst (cons (cons dxf wid) lst)
                    )
                )
            )
            (if (< 1 (length lst))
                (progn
                    (setq wid (_textwidth enx)
                          spc (/ (- wid tot) (float (1- (length lst))))
                          lst (reverse lst)
                          ang (cdr (assoc 50 enx))
                    )
                    (if
                        (and
                            (= 0 (cdr (assoc 72 enx)))
                            (= 0 (cdr (assoc 73 enx)))
                        )
                        (setq pnt (cdr (assoc 10 enx)))
                        (setq pnt (cdr (assoc 11 enx)))
                    )
                    (cond
                        (   (= (cdr (assoc 72 enx)) 0)
                            (setq fun (lambda ( a b ) (+ spc (cdr a))))
                        )
                        (   (= (cdr (assoc 72 enx)) 1)
                            (setq fun (lambda ( a b ) (+ spc (/ (+ (cdr a) (cdr b)) 2.0)))
                                  pnt (polar pnt (+ ang pi) (/ (- wid (cdar lst)) 2.0))
                            )
                        )
                        (   (= (cdr (assoc 72 enx)) 2)
                            (setq fun (lambda ( a b ) (+ spc (cdr b)))
                                  pnt (polar pnt (+ ang pi) (- wid (cdar lst)))
                            )
                        )
                    )
                    (mapcar
                        (function
                            (lambda ( a b / dxf )
                                (if (setq dxf (car a))
                                    (entmod
                                        (subst (cons 10 pnt) (assoc 10 dxf)
                                            (subst (cons 11 pnt) (assoc 11 dxf) dxf)
                                        )
                                    )
                                )
                                (setq pnt (polar pnt ang (fun a b)))
                            )
                        )
                        lst (append (cdr lst) '((nil . 0.0)))
                    )
                    (entdel ent)
                )
            )
        )
    )
    (princ)
)
(princ)