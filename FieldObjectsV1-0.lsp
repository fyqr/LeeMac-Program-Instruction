;;--------------------=={ Display Field Objects  }==--------------------;;
;;                                                                      ;;
;;  This program enables the user to easily view the object or set of   ;;
;;  objects referenced by a selected Field.                             ;;
;;                                                                      ;;
;;  Upon issuing the command syntax 'fieldobjects' at the AutoCAD       ;;
;;  command-line, the program will prompt the user to select an         ;;
;;  annotation object (Text, MText or Attribute) containing one or      ;;
;;  more field expressions referencing one or more objects in the       ;;
;;  active drawing.                                                     ;;
;;                                                                      ;;
;;  Following a valid selection, the program will enclose the selected  ;;
;;  annotation object containing the field(s) with a green rectangular  ;;
;;  text box, with links to every referenced object, each of which      ;;
;;  is surrounded by a red rectangular bounding box. The size of the    ;;
;;  rectangular frames relative to the enclosed objects is dependent    ;;
;;  on the current zoom level, and will automatically update as the     ;;
;;  user zooms &amp; pans around the drawing area.                      ;;
;;                                                                      ;;
;;  The diagrammatic display is retained until the user clicks the      ;;
;;  mouse or presses any key to exit the program.                       ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2011  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2011-11-04                                      ;;
;;                                                                      ;;
;;  First release.                                                      ;;
;;----------------------------------------------------------------------;;

(defun c:fieldobjects

    (
        /
        *error*
        _corners->list
        _offsetoutside
        _midpoint
        _inters-box-point
        _outline
        _fieldobjects
        _selectif

        a b c d e f
    )

    (defun *error* ( m ) (redraw) (princ))

    (defun _corners->list ( a b )
        (mapcar
            (function
                (lambda ( a b ) (list (car a) (cadr b)))
            )
            (list a b b a) (list a a b b)
        )
    )

    (defun _offsetoutside ( a b )
        (mapcar
            (function
                (lambda ( a c )
                    (mapcar
                        (function
                            (lambda ( a c ) ((eval a) c b))
                        )
                        a c
                    )
                )
            )
           '((- -) (+ -) (+ +) (- +)) a
        )
    )

    (defun _midpoint ( a b )
        (mapcar
            (function
                (lambda ( a b ) (/ (+ a b) 2.))
            )
            a b
        )
    )

    (defun _inters-box-point ( a b c )
        (vl-some
            (function
                (lambda ( d e ) (inters b c d e))
            )
            a (cons (last a) a)
        )
    )

    (defun _outline ( a b c d e / f g )
        (mapcar
            (function
                (lambda ( a b ) (grdraw a b e 1))
            )
            a (cons (last a) a)
        )
        (if
            (and c
                (setq f (_inters-box-point a b d))
                (setq g (_inters-box-point c d b))
            )
            (grdraw f g 2 1)
        )
    )

    (defun _fieldobjects ( e / _getfieldobjects )

        (defun _getfieldobjects ( a )
            (apply 'append
                (mapcar
                   '(lambda ( a )
                        (if (= 360 (car a))
                            (_getfieldobjects (cdr a))
                            (if (= 331 (car a)) (list (cdr a)))
                        )
                    )
                    (entget a)
                )
            )
        )
        
        (if (and (wcmatch  (cdr (assoc 0 (setq e (entget e)))) "TEXT,MTEXT,ATTRIB")
                 (setq e (cdr (assoc 360 e)))
                 (setq e (dictsearch e "acad_field"))
                 (setq e (dictsearch (cdr (assoc -1 e)) "text"))
            )
            (_getfieldobjects (cdr (assoc -1 e)))
        )
    )

    (defun _selectif ( a b / c d ) (setq b (eval b))
        (while
            (progn (setvar 'errno 0) (setq c (car (nentsel a)))
                (cond
                    (   (= 7 (getvar 'errno))
                        (princ "\nMissed, try again.")
                    )
                    (   (null c) nil)
                    (   (not (setq d (b c))) (princ "\nInvalid object."))
                )
            )
        )
        (if c (cons c d))
    )

    (if
        (setq a
            (mapcar
                (function
                    (lambda ( a )
                        (vla-getboundingbox (vlax-ename->vla-object a) 'b 'c)
                        (setq b (vlax-safearray->list b)
                              c (vlax-safearray->list c)
                        )
                        (list (_corners->list b c) (_midpoint b c))
                    )
                )
                (_selectif "\nSelect field: " '_fieldobjects)
            )
        )
        (progn
            (princ "\nPress any key to exit...")
            (while (= 5 (car (setq b (grread t 9))))
                (redraw)
                (_outline
                    (setq c (cadar a)
                          f (/ (getvar 'viewsize) 50.0)
                          d (_offsetoutside (caar a) f)
                    )
                    nil nil nil 3
                )
                (foreach e (cdr a)
                    (_outline (_offsetoutside (car e) f) (cadr e) d c 1)
                )
            )
        )
    )
    (redraw) (princ)
)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;