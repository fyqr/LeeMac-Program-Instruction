;;-------------------------=={ Object Break }==-------------------------;;
;;                                                                      ;;
;;  This program enables the user to break a selected object at two     ;;
;;  specified points or at two points of intersection with another      ;;
;;  selected object, and apply a set of properties to the selected      ;;
;;  portion of the object.                                              ;;
;;                                                                      ;;
;;  The motivation for creating the program was to streamline the       ;;
;;  procedure of displaying hidden sections of geometry. Previously,    ;;
;;  an object would need to be broken separately at two points, before  ;;
;;  selecting the resulting section and applying a set of properties    ;;
;;  to indicate hidden detail.                                          ;;
;;                                                                      ;;
;;  This manual method is suitable for polylines and open objects       ;;
;;  (such as lines, arcs & elliptical arcs); however, since an arc or   ;;
;;  elliptical arc cannot span a full circle, circles & ellipses        ;;
;;  cannot be broken at a single point and hence the hidden section     ;;
;;  would need to be recreated.                                         ;;
;;                                                                      ;;
;;  This program offers two commands to facilitate this operation:      ;;
;;                                                                      ;;
;;  ------------------------------------------------------------------  ;;
;;  BRK    -    Break at two points                                     ;;
;;  ------------------------------------------------------------------  ;;
;;                                                                      ;;
;;  Upon issuing this command, the user may select either a Line,       ;;
;;  Circle, Arc, Elliptical Arc or LWPolyline at the section to be      ;;
;;  hidden, and then select two break points. The selected object is    ;;
;;  then broken between the two points and a set of properties are      ;;
;;  applied to the selected section.                                    ;;
;;                                                                      ;;
;;  ------------------------------------------------------------------  ;;
;;  BRKO   -    Break with object                                       ;;
;;  ------------------------------------------------------------------  ;;
;;                                                                      ;;
;;  This command will prompt the user to select a compatible object     ;;
;;  at the section to be broken, and then to select a second object     ;;
;;  which intersects the first in at least two places.                  ;;
;;                                                                      ;;
;;  The program will then break the first selected object at the two    ;;
;;  closest points of intersection located either side of the point of  ;;
;;  selection, before applying the set of properties to the selected    ;;
;;  section.                                                            ;;
;;                                                                      ;;
;;  ------------------------------------------------------------------  ;;
;;                                                                      ;;
;;  The set of properties (Layer, Linetype, Lineweight etc.) to be      ;;
;;  applied to the hidden section of geometry are listed below and may  ;;
;;  be altered to suit the requirements of the user. If properties      ;;
;;  are omitted from the list, the hidden geometry will inherit these   ;;
;;  properties from the selected object.                                ;;
;;                                                                      ;;
;;  Finally, this program will operate successfully in all UCS & Views, ;;
;;  with objects constructed in any UCS plane.                          ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2013  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    02-06-2013                                      ;;
;;                                                                      ;;
;;  First release.                                                      ;;
;;----------------------------------------------------------------------;;

;;----------------------------------------------------------------------;;
;;                      Hidden Section Properties                       ;;
;;----------------------------------------------------------------------;;

;; Omit properties to match those of the selected object

(setq breakobject:dxf
   '(
        (006 . "BYLAYER") ;; Linetype (must be loaded)
        (008 . "HIDDEN")  ;; Layer
        (039 . 0.0)       ;; Thickness
        (048 . 1.0)       ;; Linetype Scale
        (062 . 256)       ;; Colour (0 = ByBlock, 256 = ByLayer)
        (370 . -1)        ;; Lineweight (-1 = ByLayer, -2 = ByBlock, -3 = Default, 0.3 = 30 etc.)
    )
)

;;----------------------------------------------------------------------;;
;;  BRK    -    Break at two points                                     ;;
;;----------------------------------------------------------------------;;

(defun c:brk ( / *error* pt1 pt2 sel )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (if
        (and
            (setq sel (breakobject:selection))
            (setq pt1 (getpoint "\nPick 1st break point: "))
            (progn
                (while (equal pt1 (setq pt2 (getpoint "\nPick 2nd break point: ")) 1e-8)
                    (princ "\nPoints must be distinct.")
                )
                pt2
            )
        )
        (progn
            (LM:startundo (LM:acdoc))
            (LM:breakobject (car sel) (trans pt1 1 0) (trans pt2 1 0) (trans (cadr sel) 1 0) breakobject:dxf)
            (LM:endundo   (LM:acdoc))
        )
    )
    (princ)
)

;;----------------------------------------------------------------------;;
;;  BRKO   -    Break with object                                       ;;
;;----------------------------------------------------------------------;;

(defun c:brko ( / *error* ent ls1 ls2 lst obj pa1 par sel tmp )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (if (setq sel (breakobject:selection))
        (progn
            (while
                (progn (setvar 'errno 0) (setq obj (car (entsel "\nSelect intersecting object: ")))
                    (cond
                        (   (= 7 (getvar 'errno))
                            (princ "\nMissed, try again.")
                        )
                        (   (= 'ename (type obj))
                            (setq obj (vlax-ename->vla-object obj)
                                  ent (car sel)
                            )
                            (cond
                                (   (not (vlax-method-applicable-p obj 'intersectwith))
                                    (princ "\nObject type is not supported.")
                                )
                                (   (not (cdddr (setq tmp (vlax-invoke obj 'intersectwith (vlax-ename->vla-object ent) acextendnone))))
                                    (princ "\nObject does not intersect object to be broken in two places.")
                                )
                                (   t
                                    (repeat (/ (length tmp) 3)
                                        (setq lst (cons (vlax-curve-getclosestpointto ent (list (car tmp) (cadr tmp) (caddr tmp))) lst)
                                              tmp (cdddr tmp)
                                        )
                                    )
                                    (if (< 2 (length lst))
                                        (progn
                                            (setq par (vlax-curve-getparamatpoint ent (vlax-curve-getclosestpointto ent (trans (cadr sel) 1 0))))
                                            (foreach pnt lst
                                                (if (minusp (setq pa1 (- par (vlax-curve-getparamatpoint ent pnt))))
                                                    (setq ls1 (cons (list pa1 pnt) ls1))
                                                    (setq ls2 (cons (list pa1 pnt) ls2))
                                                )
                                            )
                                            (setq ls1 (vl-sort ls1 '(lambda ( a b ) (> (car a) (car b))))
                                                  ls2 (vl-sort ls2 '(lambda ( a b ) (< (car a) (car b))))
                                                  lst (list (cond ((cadar ls1)) ((cadr (last ls2)))) (cond ((cadar ls2)) ((cadr (last ls1)))))
                                            )
                                        )
                                    )
                                    (LM:startundo (LM:acdoc))
                                    (LM:breakobject ent (car lst) (cadr lst) (trans (cadr sel) 1 0) breakobject:dxf)
                                    (LM:endundo   (LM:acdoc))
                                )
                            )
                        )
                    )
                )
            )
        )
    )
    (princ)
)

;; Break Object Selection Function  -  Lee Mac
;; Prompts the user to select a compatible object at the section to be hidden.

(defun breakobject:selection ( / sel )
    (while
        (progn (setvar 'errno 0) (setq sel (entsel "\nSelect object at section to be hidden: "))
            (cond
                (   (= 7 (getvar 'errno))
                    (princ "\nMissed, try again.")
                )
                (   sel
                    (if (not (wcmatch (cdr (assoc 0 (entget (car sel)))) "ARC,LINE,CIRCLE,ELLIPSE,LWPOLYLINE"))
                        (princ "\nObject type is not supported.")
                    )
                )
            )
        )
    )
    sel
)

;; Break Object  -  Lee Mac
;; Breaks an object at two given points and applies a set of properties to the section containing a third point.
;; ent     - Entity to break (Arc, Circle, Ellipse, Elliptical Arc, Line, LWPolyline)
;; pt1,pt2 - WCS break points
;; bpt     - WCS point on section on which to apply properties
;; dxf     - DXF list of properties
            
(defun LM:breakobject ( ent pt1 pt2 bpt dxf / bul cen def enx fnc hed lst ocs pa1 pa2 typ vl1 vl2 vtx vx1 wid )
    (setq enx (entget ent)
          pt1 (vlax-curve-getclosestpointto ent pt1)
          pt2 (vlax-curve-getclosestpointto ent pt2)
          pa1 (vlax-curve-getparamatpoint ent pt1)
          pa2 (vlax-curve-getparamatpoint ent pt2)
          def (LM:defaultprops enx)
          dxf (mapcar '(lambda ( x ) (cond ((assoc (car x) dxf)) ( x ))) def)
    )
    (if (< pa2 pa1)
        (mapcar 'set '(pt1 pt2 pa1 pa2) (list pt2 pt1 pa2 pa1))
    )
    (if (< pa1 (vlax-curve-getparamatpoint ent (vlax-curve-getclosestpointto ent bpt)) pa2)
        (setq dxf (list (LM:defaultprops enx) dxf))
        (setq dxf (list dxf (LM:defaultprops enx)))
    )
    (if
        (cond
            (   (= "CIRCLE" (setq typ (cdr (assoc 0 enx))))
                (setq cen (cdr (assoc 10 enx))
                      lst (list (angle cen (trans pt1 0 ent)) (angle cen (trans pt2 0 ent)))
                      lst (cons (cadr lst) lst)
                      enx (vl-remove-if '(lambda ( x ) (member (car x) '(-1 0 5 6 8 39 48 62 100 102 330 370))) enx)
                )
                (setq fnc
                    (lambda ( )
                        (entmake (append '((0 . "ARC")) (car dxf) (mapcar 'cons '(50 51) lst) enx))
                    )
                )
            )
            (   (= "ELLIPSE" typ)
                (setq pa1 (LM:ang0-2pi (cdr (assoc 41 enx)))
                      pa2 (LM:ang0-2pi (cdr (assoc 42 enx)))
                      lst (list (LM:point->param enx pt1) (LM:point->param enx pt2))
                      enx (vl-remove-if '(lambda ( x ) (member (car x) '(-1 5 6 8 39 41 42 48 62 102 330 370))) enx)
                )
                (if (equal (+ pi pi) (- pa2 pa1) 1e-8)
                    (setq lst (cons (cadr lst) lst))
                    (setq lst (append (list pa1) lst (list pa2)))
                )
                (setq fnc
                    (lambda ( )
                        (if (not (equal (car lst) (cadr lst) 1e-8))
                            (entmake (append enx (car dxf) (mapcar 'cons '(41 42) lst)))
                        )
                    )
                )
            )
            (   (= "LINE" typ)
                (setq lst (list (cdr (assoc 10 enx)) pt1 pt2 (cdr (assoc 11 enx))))
                (setq fnc
                    (lambda ( )
                        (if (not (equal (car lst) (cadr lst) 1e-8))
                            (entmake (append '((0 . "LINE")) (car dxf) (mapcar 'cons '(10 11) lst)))
                        )
                    )
                )
            )
            (   (= "ARC" typ)
                (setq cen (cdr (assoc 10 enx))
                      lst (list (cdr (assoc 50 enx)) (angle cen (trans pt1 0 ent)) (angle cen (trans pt2 0 ent)) (cdr (assoc 51 enx)))
                      enx (vl-remove-if '(lambda ( x ) (member (car x) '(-1 5 6 8 39 48 62 102 330 370))) enx)
                )
                (setq fnc
                    (lambda ( )
                        (if (not (equal (car lst) (cadr lst) 1e-8))
                            (entmake (append enx (car dxf) (mapcar 'cons '(50 51) lst)))
                        )
                    )
                )
            )
            (   (= "LWPOLYLINE" typ)
                (setq hed (reverse (member (assoc 39 enx) (reverse enx)))
                      hed (subst (cons 70 (logand (cdr (assoc 70 hed)) (~ 1))) (assoc 70 hed) hed)
                      hed (vl-remove-if '(lambda ( x ) (member (car x) '(-1 5 6 8 39 48 62 102 330 370))) hed)
                      vtx (LM:LWVertices enx)
                      ocs (assoc 210 enx)
                )
                (repeat (fix pa1)
                    (setq vl1 (cons (car vtx) vl1)
                          vtx (cdr vtx)
                    )
                )
                (if (not (equal pa1 (fix pa1) 1e-8))
                    (setq
                        vx1 (car vtx)
                        wid (cdr (assoc 40 vx1))
                        wid (+ wid (* (- pa1 (fix pa1)) (- (cdr (assoc 41 vx1)) wid)))
                        bul (atan (cdr (assoc 42 vx1)))
                        vl1
                        (vl-list*
                            (list
                                (cons 10 (trans pt1 0 (cdr ocs)))
                               '(40 . 0.0)
                               '(41 . 0.0)
                               '(42 . 0.0)
                            )
                            (list
                                (assoc 10 vx1)
                                (assoc 40 vx1)
                                (cons  41 wid)
                                (cons  42 (tan (* (- pa1 (fix pa1)) bul)))
                            )
                            vl1
                        )
                        vtx
                        (cons
                            (list
                                (cons  10 (trans pt1 0 (cdr ocs)))
                                (cons  40 wid)
                                (assoc 41 vx1)
                                (cons  42 (tan (* (- (min pa2 (1+ (fix pa1))) pa1) bul)))
                            )
                            (cdr vtx)
                        )
                    )
                    (setq vl1 (cons (car vtx) vl1))
                )
                (setq vtx (reverse vtx))
                (repeat (+ (length vtx) (fix pa1) (- (fix pa2)) -1)
                    (setq vl2 (cons (car vtx) vl2)
                          vtx (cdr vtx)
                    )
                )
                (if (not (equal pa2 (fix pa2) 1e-8))
                    (setq
                        vx1 (car vtx)
                        wid (cdr (assoc 40 vx1))
                        wid (+ wid (* (/ (- pa2 (max pa1 (fix pa2))) (- (1+ (fix pa2)) (max pa1 (fix pa2)))) (- (cdr (assoc 41 vx1)) wid)))
                        bul (atan (cdr (assoc 42 vx1)))
                        vl2
                        (cons
                            (list
                                (cons  10 (trans pt2 0 (cdr ocs)))
                                (cons  40 wid)
                                (assoc 41 vx1)
                                (cons  42 (tan (* (/ (- (1+ (fix pa2)) pa2) (if (< (fix pa2) pa1) (- pa2 pa1) 1.0)) bul)))
                            )
                            vl2
                        )
                        vtx
                        (vl-list*
                            (list
                                (cons 10 (trans pt2 0 (cdr ocs)))
                               '(40 . 0.0)
                               '(41 . 0.0)
                               '(42 . 0.0)
                            )
                            (list
                                (assoc 10 vx1)
                                (assoc 40 vx1)
                                (cons  41 wid)
                                (cons  42 (tan (* (if (< (fix pa2) pa1) 1.0 (- pa2 (fix pa2))) bul)))
                            )
                            (cdr vtx)
                        )
                    )
                    (setq vl2 (cons (car vtx) vl2))
                )
                (if (vlax-curve-isclosed ent)
                    (setq lst (list (append vl2 (reverse vl1)) (reverse vtx)))
                    (setq lst (list (reverse vl1) (reverse vtx) vl2))
                )
                (setq fnc
                    (lambda ( )
                        (if (cdar lst)
                            (entmake
                                (append
                                    (subst (cons 90 (length (car lst))) (assoc 90 hed) hed)
                                    (car dxf)
                                    (apply 'append (car lst))
                                    (list ocs)
                                )
                            )
                        )
                    )
                )
            )
        )
        (progn
            (repeat (if (vlax-curve-isclosed ent) 2 3)
                (fnc)
                (setq dxf (reverse dxf)
                      lst (cdr lst)
                )
            )
            (entdel ent)
        )
    )
    (princ)
)

;; Point -> Ellipse Parameter  -  Lee Mac
;; Returns the ellipse parameter for the given point
;; dxf  -  Ellipse DXF data (DXF groups 10, 11, 40, 210)
;; pnt  -  WCS Point on Ellipse
;; Uses relationship: ratio*tan(param) = tan(angle)

(defun LM:point->param ( dxf pnt / ang ocs )
    (setq ocs (cdr (assoc 210 dxf))
          ang (- (angle (trans (cdr (assoc 10 dxf)) 0 ocs) (trans pnt 0 ocs))
                 (angle '(0.0 0.0) (trans (cdr (assoc 11 dxf)) 0 ocs))
              )
    )
    (LM:ang0-2pi (atan (sin ang) (* (cdr (assoc 40 dxf)) (cos ang))))
)

;; Ang0-2pi  -  Lee Mac
;; Returns the supplied angle in the range 0-2pi rads

(defun LM:ang0-2pi ( a )
    (cond
        (   (equal (+ pi pi) a 1e-8) a)
        (   (minusp a) (LM:ang0-2pi (+ a pi pi)))
        (   (rem a (+ pi pi)))
    )
)

;; LW Vertices  -  Lee Mac
;; Returns a list of lists in which each sublist describes the position,
;; starting width, ending width and bulge of a vertex of an LWPolyline
 
(defun LM:LWVertices ( e )
    (if (setq e (member (assoc 10 e) e))
        (cons
            (list
                (assoc 10 e)
                (assoc 40 e)
                (assoc 41 e)
                (assoc 42 e)
            )
            (LM:LWVertices (cdr e))
        )
    )
)

;; Default Properties  -  Lee Mac
;; Returns a list of DXF properties for the supplied DXF data,
;; substituting default values for absent DXF groups
 
(defun LM:defaultprops ( elist )
    (mapcar
        (function
            (lambda ( pair )
                (cond ((assoc (car pair) elist)) ( pair ))
            )
        )
       '(
            (008 . "0")
            (006 . "BYLAYER")
            (039 . 0.0)
            (048 . 1.0)
            (062 . 256)
            (370 . -1)
        )
    )
)

;; Tangent  -  Lee Mac
;; Args: x - real
 
(defun tan ( x )
    (if (not (equal 0.0 (cos x) 1e-8))
        (/ (sin x) (cos x))
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
        "\n:: ObjectBreak.lsp | Version 1.0 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,$(getvar,date),YYYY)")
        " www.lee-mac.com ::"
        "\n:: Available Commands:"
        "\n::    \"brk\"   -  Break at two points"
        "\n::    \"brko\"  -  Break at intersections with another object"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;