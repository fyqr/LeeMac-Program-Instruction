;;-------------------------=={ Segment Curve }==------------------------;;
;;                                                                      ;;
;;  This program enables the user convert every object in a selection   ;;
;;  into a polyline approximating the object using a given number of    ;;
;;  linear segments.                                                    ;;
;;                                                                      ;;
;;  Upon calling the program with 'segs' at the AutoCAD command-line,   ;;
;;  the user is prompted to make a selection of planar curve objects    ;;
;;  (Arcs, Circles, Ellipses, Lines, Splines, or 2D Polylines) and      ;;
;;  then to specify the number of segments for the resulting polyline.  ;;
;;                                                                      ;;
;;  The program will then proceed to create a 2D polyline with          ;;
;;  properties identical to each selected object, approximating the     ;;
;;  selected object using the specified number of linear segments.      ;;
;;                                                                      ;;
;;  If a selected object has a coordinate with non-zero Z-component,    ;;
;;  the resultant polyline will have an elevation equal to the          ;;
;;  Z-component of the first point on the curve.                        ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2011  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2011-04-18                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2016-04-24                                      ;;
;;                                                                      ;;
;;  - Program entirely rewritten.                                       ;;
;;----------------------------------------------------------------------;;

(defun c:segs ( / *error* dis ent idx inc lst num pnt sel tmp )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (if
        (not
            (and
                (setq num (getenv "LMac\\segs"))
                (setq num (atoi num))
                (< 0  num)
            )
        )
        (setenv "LMac\\segs" (itoa (setq num 10)))
    )
    (if (setq sel
            (ssget "_:L"
               '(
                    (0 . "ARC,CIRCLE,ELLIPSE,LINE,*POLYLINE,SPLINE")
                    (-4 . "<NOT")
                        (-4 . "<AND")
                            (0 . "POLYLINE") (-4 . "&") (70 . 88)
                        (-4 . "AND>")
                    (-4 . "NOT>")
                )
            )
        )
        (progn
            (initget 6)
            (if (setq tmp (getint (strcat "\nSpecify number of segments <" (itoa num) ">: ")))
                (setenv "LMac\\segs" (itoa (setq num tmp)))
            )
            (LM:startundo (LM:acdoc))
            (repeat (setq idx (sslength sel))
                (setq ent (ssname sel (setq idx (1- idx)))
                      inc (/ (vlax-curve-getdistatparam ent (vlax-curve-getendparam ent)) (float num))
                      dis 0.0
                      lst nil
                )
                (repeat (1+ num)
                    (if (setq pnt (vlax-curve-getpointatdist ent dis))
                        (setq lst (cons (cons 10 (trans pnt 0 ent)) lst))
                    )
                    (setq dis (+ dis inc))
                )
                (if (not (equal (vlax-curve-getendpoint ent) (trans (cdar lst) ent 0) 1e-6))
                    (setq lst (cons (cons 10 (trans (vlax-curve-getendpoint ent) 0 ent)) lst))
                )
                (if (entmake
                        (append
                            (list
                               '(000 . "LWPOLYLINE")
                               '(100 . "AcDbEntity")
                               '(100 . "AcDbPolyline")
                                (cons 90 (length lst))
                                (cons 38 (last (car lst)))
                                (cons 70 (if (vlax-curve-isclosed ent) 1 0))
                            )
                            (LM:defaultprops (entget ent))
                            (reverse lst)
                            (list (assoc 210 (entget ent)))
                        )
                	)
                    (entdel ent)
                )
            )
            (LM:endundo (LM:acdoc))
        )
    )
    (princ)
)

;; Default Properties  -  Lee Mac
;; Returns a list of DXF properties for the supplied DXF data,
;; substituting default values for absent DXF groups
 
(defun LM:defaultprops ( enx )
    (mapcar '(lambda ( x ) (cond ((assoc (car x) enx)) ( x )))
       '(
            (006 . "BYLAYER")
            (008 . "0")
            (039 . 0.0)
            (048 . 1.0)
            (062 . 256)
            (370 . -1)
        )
    )
)

;; Start Undo  -  Lee Mac
;; Opens an Undo Group.

(defun LM:startundo ( doc )
    (LM:endundo doc)
    (vla-startundomark doc)
)

;; End Undo  -  Lee Mac
;; Closes an Undo Group.

(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;; Active Document  -  Lee Mac
;; Returns the VLA Active Document Object

(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)

;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: SegmentCurve.lsp | Version 1.1 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"segs\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;