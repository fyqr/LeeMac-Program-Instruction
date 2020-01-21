;;------------------------=={ Chain Selection }==-----------------------;;
;;                                                                      ;;
;;  This program prompts the user to select an object and generates a   ;;
;;  'chain selection' of all objects sharing endpoints with objects in  ;;
;;  the accumulative selection.                                         ;;
;;                                                                      ;;
;;  To explain further: when prompted, the user may select either a     ;;
;;  Line, Circular or Elliptical Arc, Open LWPolyline or Spline, or a   ;;
;;  2D Polyline. The program will then return a selection of all        ;;
;;  objects whose endpoints coincide with the endpoints of the selected ;;
;;  object, including those objects whose endpoints coincide with any   ;;
;;  object already in the selection, forming a 'chain' of selected      ;;
;;  objects.                                                            ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2012  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2012-11-07                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2019-11-24                                      ;;
;;                                                                      ;;
;;  - Program modified to exclude objects residing on frozen or locked  ;;
;;    layers, or on layers that are turned off.                         ;;
;;----------------------------------------------------------------------;;

(defun c:cs ( / df en ex fl fz in l1 l2 s1 s2 sf vl )

    (setq fz 1e-8) ;; Point comparison tolerance
    
    (while (setq df (tblnext "layer" (not df)))
        (if
            (or
                (minusp (cdr (assoc 62 df)))
                (< 0 (logand 5 (cdr (assoc 70 df))))
            )
            (setq ex (cons (cons 8 (LM:escapewildcards (cdr (assoc 2 df)))) ex))
        )
    )
    
    (setq sf
        (append
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
            (if ex
                (append
                   '(
                        (-4 . "<NOT")
                        (-4 . "<OR")
                    )
                    ex
                   '(
                        (-4 . "OR>")
                        (-4 . "NOT>")
                    )
                )
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
                            (if (vl-some '(lambda ( p ) (or (equal (car v) p fz) (equal (cadr v) p fz))) l1)
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

;; Escape Wildcards  -  Lee Mac
;; Escapes wildcard special characters in a supplied string

(defun LM:escapewildcards ( str )
    (vl-list->string
        (apply 'append
            (mapcar
               '(lambda ( c )
                    (if (member c '(35 64 46 42 63 126 91 93 45 44))
                        (list 96 c)
                        (list c)
                    )
                )
                (vl-string->list str)
            )
        )
    )
)
        
;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: ChainSel.lsp | Version 1.1 | \\U+00A9 Lee Mac "
        ((lambda ( y ) (if (= y (menucmd "m=$(edtime,0,yyyy)")) y (strcat y "-" (menucmd "m=$(edtime,0,yyyy)")))) "2012")
        " www.lee-mac.com ::"
        "\n:: Type \"cs\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;