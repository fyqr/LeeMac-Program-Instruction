;;--------------=={ Minimum Enclosing Circle }==--------------;;
;;                                                            ;;
;; Prompts the user to select an object, then constructs the  ;;
;; Minimum Enclosing Circle containing the selected object.   ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;

(defun c:mec ( / en lst )
    (while
        (progn (setvar 'ERRNO 0) (setq en (car (entsel)))
            (cond
                (   (= 7 (getvar 'ERRNO))
                    (princ "\nMissed, try again.")
                )
                (   (eq 'ENAME (type en))
                    (if (not (setq lst (LM:Entity->PointList en)))
                        (princ "\nInvalid Object Selected.")
                    )
                )
            )
        )
    )
    (if (setq lst (LM:MinEncCircle lst))
        (entmake
            (list
               '(0 . "CIRCLE")
                (cons 10 (car  lst))
                (cons 40 (cadr lst))
            )
        )
    )
    (princ)
)

;;---------=={ Minimum Enclosing Circle (Multiple) }==--------;;
;;                                                            ;;
;; Prompts the user to select multiple objects, then          ;;
;; constructs the Minimum Enclosing Circle containing the     ;;
;; selected objects.                                          ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;

(defun c:mecm ( / in l1 l2 ss )
    (if
        (and
            (setq ss (ssget '((0 . "ARC,CIRCLE,ELLIPSE,LINE,SPLINE,*POLYLINE,POINT"))))
            (progn
                (repeat (setq in (sslength ss))
                    (if (setq l1 (LM:Entity->PointList (ssname ss (setq in (1- in)))))
                        (setq l2 (LM:ConvexHull (append l2 l1)))
                    )
                )
                (setq l2 (LM:MinEncCircle l2))
            )
        )
        (entmake
            (list
               '(0 . "CIRCLE")
                (cons 10 (car  l2))
                (cons 40 (cadr l2))
            )
        )
    )
    (princ)
)

;; Minimum Enclosing Circle  -  Lee Mac
;; Implements the algorithm by Pr.Chrystal (1885) using the Convex Hull
;; to determine the Minimum Enclosing Circle of a point set.

(defun LM:MinEncCircle ( lst / _sub )

    (defun _sub ( p1 p2 l1 / a1 a2 l2 p3 p4 )
        (setq l2 (LM:RemoveWithFuzz (list p1 p2) l1 1e-8)
              p3 (car l2)
              a1 (LM:GetInsideAngle p1 p3 p2)
        )
        (foreach p4 (cdr l2)
            (if (< (setq a2 (LM:GetInsideAngle p1 p4 p2)) a1)
                (setq p3 p4 a1 a2)
            )
        )
        (cond
            (   (<= (/ pi 2.0) a1)
                (list (mid p1 p2) (/ (distance p1 p2) 2.0))
            )
            (   (vl-some
                    (function
                        (lambda ( a b c )
                            (if (< (/ pi 2.0) (LM:GetInsideAngle a b c)) (_sub a c l1))
                        )
                    )
                    (list p1 p1 p2) (list p2 p3 p1) (list p3 p2 p3)
                )
            )
            (   (LM:3PCircle p1 p2 p3)   )
        )
    )

    (
        (lambda ( lst )
            (cond
                (   (< (length lst) 2)
                    nil
                )
                (   (< (length lst) 3)
                    (list (apply 'mid lst) (/ (apply 'distance lst) 2.0))
                )
                (   (_sub (car lst) (cadr lst) lst)   )
            )
        )
        (LM:ConvexHull lst)
    )
)

;; Remove With Fuzz  -  Lee Mac
;; Removes items from a list which are equal to a supplied tolerance

(defun LM:RemoveWithFuzz ( l1 l2 fz )
    (vl-remove-if
        (function
            (lambda ( a )
                (vl-some
                    (function (lambda ( b ) (equal a b fz)))
                    l1
                )
            )
        )
        l2
    )
)

;; Get Inside Angle  -  Lee Mac
;; Returns the smaller angle subtended by three points with vertex at p2

(defun LM:GetInsideAngle ( p1 p2 p3 )
    (   (lambda ( a ) (min a (- (+ pi pi) a)))
        (rem (+ pi pi (- (angle p2 p1) (angle p2 p3))) (+ pi pi))
    )
)

;; 3-Point Circle  -  Lee Mac
;; Returns the Center and Radius of the Circle defined by
;; the supplied three points.

(defun LM:3PCircle ( p1 p2 p3 / cn m1 m2 )
    (setq m1 (mid p1 p2)
          m2 (mid p2 p3)
    )
    (list
        (setq cn
            (inters
                m1 (polar m1 (+ (angle p1 p2) (/ pi 2.)) 1.0)
                m2 (polar m2 (+ (angle p2 p3) (/ pi 2.)) 1.0)
                nil
            )
        )
        (distance cn p1)
    )
)

;; Midpoint - Lee Mac
;; Returns the midpoint of two points

(defun mid ( a b )
    (mapcar (function (lambda ( a b ) (/ (+ a b) 2.0))) a b)
)

;; Convex Hull  -  Lee Mac
;; Implements the Graham Scan Algorithm to determine the
;; Convex Hull of a list of points.

(defun LM:ConvexHull ( lst / hul p0 )
    (cond
        (   (< (length lst) 4)
            lst
        )
        (   t
            (setq p0 (car lst))
            (foreach p1 (cdr lst)
                (if (or (< (cadr p1) (cadr p0))
                        (and (equal (cadr p1) (cadr p0) 1e-8) (< (car p1) (car p0)))
                    )
                    (setq p0 p1)
                )
            )
            (setq lst
                (vl-sort lst
                    (function
                        (lambda ( a b / c d )
                            (if (equal (setq c (angle p0 a)) (setq d (angle p0 b)) 1e-8)
                                (< (distance p0 a) (distance p0 b))
                                (< c d)
                            )
                        )
                    )
                )
            )
            (setq hul (list (caddr lst) (cadr lst) (car lst)))
            (foreach pt (cdddr lst)
                (setq hul (cons pt hul))
                (while (and (caddr hul) (LM:Clockwise-p (caddr hul) (cadr hul) pt))
                    (setq hul (cons pt (cddr hul)))
                )
            )
            hul
        )
    )
)

;; Clockwise-p  -  Lee Mac
;; Returns T if p1,p2,p3 are clockwise oriented or collinear
                 
(defun LM:Clockwise-p ( p1 p2 p3 )
    (<  (-  (* (- (car  p2) (car  p1)) (- (cadr p3) (cadr p1)))
            (* (- (cadr p2) (cadr p1)) (- (car  p3) (car  p1)))
        )
        1e-8
    )
)

;;----------------=={ Entity to Point List }==----------------;;
;;                                                            ;;
;;  Returns a list of points describing or approximating the  ;;
;;  supplied entity, else nil if the entity is not supported. ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  ent - Entity for which to return Point List.              ;;
;;------------------------------------------------------------;;
;;  Returns:  List of Points describing/approximating entity  ;;
;;------------------------------------------------------------;;

(defun LM:Entity->PointList ( ent / der di1 di2 di3 elst fun inc lst par rad )
    (setq elst (entget ent))
    (cond
        (   (eq "POINT" (cdr (assoc 0 elst)))
            (list (cdr (assoc 10 elst)))
        )
        (   (eq "LINE" (cdr (assoc 0 elst)))
            (list (cdr (assoc 10 elst)) (cdr (assoc 11 elst)))
        )
        (   (member (cdr (assoc 0 elst)) '("CIRCLE" "ARC"))
            (setq di1 0.0
                  di2 (vlax-curve-getdistatparam ent (vlax-curve-getendparam ent))
                  inc (/ di2 (1+ (fix (* 35.0 (/ di2 (cdr (assoc 40 elst)) (+ pi pi))))))
                  fun (if (vlax-curve-isclosed ent) < <=)
            )
            (while (fun di1 di2)
                (setq lst (cons (vlax-curve-getpointatdist ent di1) lst)
                      di1 (+ di1 inc)
                )
            )
            lst
        )
        (   (or (eq (cdr (assoc 0 elst)) "LWPOLYLINE")
                (and (eq (cdr (assoc 0 elst)) "POLYLINE") (zerop (logand (cdr (assoc 70 elst)) 80)))
            )
            (setq par 0)
            (repeat (fix (1+ (vlax-curve-getendparam ent)))
                (if (setq der (vlax-curve-getsecondderiv ent par))
                    (if (equal der '(0.0 0.0 0.0) 1e-8)
                        (setq lst (cons (vlax-curve-getpointatparam ent par) lst))
                        (if (setq rad (distance '(0.0 0.0) (vlax-curve-getfirstderiv ent par))
                                  di1 (vlax-curve-getdistatparam ent par)
                                  di2 (vlax-curve-getdistatparam ent (1+ par))
                            )
                            (progn
                                (setq inc (/ (- di2 di1) (1+ (fix (* 35.0 (/ (- di2 di1) rad (+ pi pi)))))))
                                (while (< di1 di2)
                                    (setq lst (cons (vlax-curve-getpointatdist ent di1) lst)
                                          di1 (+ di1 inc)
                                    )
                                )
                            )
                        )
                    )
                )
                (setq par (1+ par))
            )
            (if (or (vlax-curve-isclosed ent) (equal '(0.0 0.0 0.0) der 1e-8))
                lst
                (cons (vlax-curve-getendpoint ent) lst)
            )
        )
        (   (eq (cdr (assoc 0 elst)) "ELLIPSE")
            (setq di1 (vlax-curve-getdistatparam ent (vlax-curve-getstartparam ent))
                  di2 (vlax-curve-getdistatparam ent (vlax-curve-getendparam   ent))
                  di3 (* di2 (/ (+ pi pi) (abs (- (vlax-curve-getendparam ent) (vlax-curve-getstartparam ent)))))
            )
            (while (< di1 di2)
                (setq lst (cons (vlax-curve-getpointatdist ent di1) lst)
                      der (distance '(0.0 0.0) (vlax-curve-getsecondderiv ent (vlax-curve-getparamatdist ent di1)))
                      di1 (+ di1 (/ di3 (1+ (fix (/ 35.0 (/ di3 der (+ pi pi)))))))
                )
            )
            (if (vlax-curve-isclosed ent)
                lst
                (cons (vlax-curve-getendpoint ent) lst)
            )
        )
        (   (eq (cdr (assoc 0 elst)) "SPLINE")
            (setq di1 (vlax-curve-getdistatparam ent (vlax-curve-getstartparam ent))
                  di2 (vlax-curve-getdistatparam ent (vlax-curve-getendparam   ent))
                  inc (/ di2 25.0)
            )
            (while (< di1 di2)
                (setq lst (cons (vlax-curve-getpointatdist ent di1) lst)
                      der (/ (distance '(0.0 0.0) (vlax-curve-getsecondderiv ent (vlax-curve-getparamatdist ent di1))) inc)
                      di1 (+ di1 (if (equal 0.0 der 1e-10) inc (min inc (/ 1.0 der (* 10. inc)))))
                )
            )
            (if (vlax-curve-isclosed ent)
                lst
                (cons (vlax-curve-getendpoint ent) lst)
            )
        )
    )
)

(vl-load-com) (princ)

;;------------------------------------------------------------;;
;;                         End of File                        ;;
;;------------------------------------------------------------;;