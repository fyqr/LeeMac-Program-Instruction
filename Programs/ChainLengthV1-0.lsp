;;-------------------------=={ Chain Length }==-------------------------;;
;;                                                                      ;;
;;  This program enables the user to calculate the total object length  ;;
;;  between two points along a chain of connected objects.              ;;
;;                                                                      ;;
;;  Upon issuing the command syntax 'chainlen' at the AutoCAD           ;;
;;  command-line, the user is first prompted to make a selection of     ;;
;;  objects; this selection may consist of any set of open geometric    ;;
;;  objects, and may contain multiple chains of connected objects, or   ;;
;;  indeed, no connected objects.                                       ;;
;;                                                                      ;;
;;  Following a valid selection, the user may then specify two          ;;
;;  distinct points located on any two objects in the selection, or     ;;
;;  with both points located on the same object.                        ;;
;;                                                                      ;;
;;  The program will then proceed to calculate the total length along   ;;
;;  the series of connected objects between the two given points, and   ;;
;;  will print the result to the AutoCAD command-line. The printed      ;;
;;  length will be formatted in line with the current units & precision ;;
;;  settings (i.e. the LUNITS & LUPREC system variables).               ;;
;;                                                                      ;;
;;  A set of distinct objects will be considered a 'chain' if each      ;;
;;  endpoint coincides with the endpoint of another object in the       ;;
;;  selection.                                                          ;;
;;                                                                      ;;
;;  The user will be notified if the two given points do not lie on     ;;
;;  the same chain of objects in the selection.                         ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2015  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2015-11-01                                      ;;
;;                                                                      ;;
;;  First release.                                                      ;;
;;----------------------------------------------------------------------;;

(defun c:chainlen ( / len lst pt1 pt2 sel tmp )
    (if
        (and
            (setq sel
                (ssget
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
            )
            (setq pt1 (getpoint "\nSpecify 1st point: "))
            (setq pt2 (getpoint "\nSpecify 2nd point: " pt1))
        )
        (if
            (setq tmp
                (vl-member-if
                    (function
                        (lambda ( itm / tmp )
                            (cond
                                (   (equal pt1 (setq tmp (vlax-curve-getclosestpointto (cadr itm) pt1)) 1e-3)
                                    (setq  pt1 tmp)
                                )
                                (   (equal pt2 (setq tmp (vlax-curve-getclosestpointto (cadr itm) pt2)) 1e-3)
                                    (mapcar 'set '(pt1 pt2) (list tmp pt1))
                                )
                            )
                        )
                    )
                    (LM:sortedchainselection sel)
                )
                lst
                (vl-member-if
                    (function
                        (lambda ( itm / tmp )
                            (if (equal pt2 (setq tmp (vlax-curve-getclosestpointto (cadr itm) pt2)) 1e-3)
                                (setq  pt2 tmp)
                            )
                        )
                    )
                    (reverse tmp)
                )
            )
            (progn
                (if (cdr lst)
                    (setq len
                        (+
                            (abs
                                (- (vlax-curve-getdistatpoint (cadar tmp) pt1)
                                   (vlax-curve-getdistatpoint (cadar tmp) (caddar tmp))
                                )
                            )
                            (abs
                                (- (vlax-curve-getdistatpoint (cadar lst) pt2)
                                   (vlax-curve-getdistatpoint (cadar lst) (caar lst))
                                )
                            )
                        )
                    )
                    (setq len
                        (abs
                            (-  (vlax-curve-getdistatpoint (cadar lst) pt1)
                                (vlax-curve-getdistatpoint (cadar lst) pt2)
                            )
                        )
                    )
                )
                (foreach itm (cdr (reverse (cdr lst)))
                    (setq len (+ len (vlax-curve-getdistatparam (cadr itm) (vlax-curve-getendparam (cadr itm)))))
                )
                (princ (strcat "\nLength: " (rtos len)))
            )
            (princ "\nThe selected points do not lie on the same chain of objects.")
        )
    )
    (princ)
)
 
(defun LM:sortedchainselection ( sel / end ent flg idx lst rtn tmp )
    (repeat (setq idx (sslength sel))
        (setq ent (ssname sel (setq idx (1- idx)))
              lst (cons (list (vlax-curve-getstartpoint ent) ent (vlax-curve-getendpoint ent)) lst)
        )
    )
    (setq end (list (caar lst) (caddar lst))
          rtn (list (car lst))
          lst (cdr lst)
    )
    (while
        (progn
            (foreach itm lst
                (cond
                    (   (equal (car itm) (car end) 1e-8)
                        (setq end (cons (caddr itm) (cdr end))
                              rtn (cons (reverse itm) rtn)
                              flg t
                        )
                    )
                    (   (equal (car itm) (cadr end) 1e-8)
                        (setq end (list (car end) (caddr itm))
                              rtn (append rtn (list itm))
                              flg t
                        )
                    )
                    (   (equal (caddr itm) (car end) 1e-8)
                        (setq end (cons (car itm) (cdr end))
                              rtn (cons itm rtn)
                              flg t
                        )
                    )
                    (   (equal (caddr itm) (cadr end) 1e-8)
                        (setq end (list (car end) (car itm))
                              rtn (append rtn (list (reverse itm)))
                              flg t
                        )
                    )
                    (   (setq tmp (cons itm tmp)))
                )
            )
            flg
        )
        (setq lst tmp tmp nil flg nil)
    )
    rtn
)

;;----------------------------------------------------------------------;;
 
(vl-load-com)
(princ
    (strcat
        "\n:: ChainLength.lsp | Version 1.0 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"chainlen\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;