;; Single Strikethrough

(defun c:strike ( / i s )
    (if (setq s (ssget '((0 . "TEXT,MTEXT"))))
        (repeat (setq i (sslength s))
            (LM:strikethrough (ssname s (setq i (1- i)))
               '(
                    (0.0 0.1)
                )
            )
        )
    )
    (princ)
)

;; Double Strikethrough

(defun c:strike2 ( / i s )
    (if (setq s (ssget '((0 . "TEXT,MTEXT"))))
        (repeat (setq i (sslength s))
            (LM:strikethrough (ssname s (setq i (1- i)))
               '(
                    ( 0.15 0.1)
                    (-0.15 0.1)
                )
            )
        )
    )
    (princ)
)

;; Triple Strikethrough

(defun c:strike3 ( / i s )
    (if (setq s (ssget '((0 . "TEXT,MTEXT"))))
        (repeat (setq i (sslength s))
            (LM:strikethrough (ssname s (setq i (1- i)))
               '(
                    ( 0.2 0.1)
                    ( 0.0 0.1)
                    (-0.2 0.1)
                )
            )
        )
    )
    (princ)
)

;; Underline

(defun c:under ( / i s )
    (if (setq s (ssget '((0 . "TEXT,MTEXT"))))
        (repeat (setq i (sslength s))
            (LM:strikethrough (ssname s (setq i (1- i)))
               '(
                    (-0.8 0.1)
                )
            )
        )
    )
    (princ)
)

;; Double Underline

(defun c:under2 ( / i s )
    (if (setq s (ssget '((0 . "TEXT,MTEXT"))))
        (repeat (setq i (sslength s))
            (LM:strikethrough (ssname s (setq i (1- i)))
               '(
                    (-0.8  0.05)
                    (-1.0  0.05)
                )
            )
        )
    )
    (princ)
)

;; Double Overline & Underline

(defun c:overunder2 ( / i s )
    (if (setq s (ssget '((0 . "TEXT,MTEXT"))))
        (repeat (setq i (sslength s))
            (LM:strikethrough (ssname s (setq i (1- i)))
               '(
                    ( 1.0 0.05)
                    ( 0.8 0.05)
                    (-0.8 0.05)
                    (-1.0 0.05)
                )
            )
        )
    )
    (princ)
)

;; Strikethrough Text  -  Lee Mac
;; Generates polylines through the supplied text object, with spacing & width given by the supplied parameter list.
;; ent - [ent] Text or MText entity
;; par - [lst] List of ((<Spacing Factor> <Width Factor>) ... ) for each polyline
;; Returns: [lst] List of created polyline entities

(defun LM:strikethrough ( ent par / ang enx hgt lst md1 md2 rtn )
    (if (setq lst (mytextbox (setq enx (entget ent))))
        (progn
            (setq hgt (cdr (assoc 40 enx))
                  md1 (mid   (car  lst) (last  lst))
                  md2 (mid   (cadr lst) (caddr lst))
                  ang (angle (car  lst) (last  lst))
            )
            (foreach itm par
                (setq rtn
                    (cons
                        (entmakex
                            (append
                               '(   (000 . "LWPOLYLINE")
                                    (100 . "AcDbEntity")
                                    (100 . "AcDbPolyline")
                                    (090 . 2)
                                    (070 . 0)
                                )
                                (LM:defaultprops enx)
                                (list
                                    (cons  043 (* (cadr itm) hgt))
                                    (cons  038 (caddar lst))
                                    (cons  010 (polar md1 ang (* (car itm) hgt)))
                                    (cons  010 (polar md2 ang (* (car itm) hgt)))
                                    (assoc 210 enx)
                                )
                            )
                        )
                        rtn
                    )
                )
            )
        )
    )
    rtn
)

;; Midpoint  -  Lee Mac
;; Returns the midpoint of two points

(defun mid ( a b )
    (mapcar (function (lambda ( a b ) (/ (+ a b) 2.0))) a b)
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

;; Text Box  -  gile / Lee Mac
;; Returns an OCS point list describing a rectangular frame surrounding the supplied Text or MText entity
;; enx - [lst] Text or MText DXF data list

(defun mytextbox ( enx / bpt hgt jus lst ocs org rot wid )
    (cond
        (   (= "TEXT" (cdr (assoc 00 enx)))
            (setq bpt (cdr (assoc 10 enx))
                  rot (cdr (assoc 50 enx))
                  lst (textbox enx)
                  lst (list (car lst) (list (caadr lst) (cadar lst)) (cadr lst) (list (caar lst) (cadadr lst)))
            )
        )
        (   (= "MTEXT" (cdr (assoc 00 enx)))
            (setq ocs  (cdr (assoc 210 enx))
                  bpt  (trans (cdr (assoc 10 enx)) 0 ocs)
                  rot  (angle '(0.0 0.0) (trans (cdr (assoc 11 enx)) 0 ocs))
                  wid  (cdr (assoc 42 enx))
                  hgt  (cdr (assoc 43 enx))
                  jus  (cdr (assoc 71 enx))
                  org  (list (cond ((member jus '(2 5 8)) (/ wid -2.0)) ((member jus '(3 6 9)) (- wid))      (0.0))
                             (cond ((member jus '(1 2 3)) (- hgt))      ((member jus '(4 5 6)) (/ hgt -2.0)) (0.0))
                       )
                  lst  (list org (mapcar '+ org (list wid 0)) (mapcar '+ org (list wid hgt)) (mapcar '+ org (list 0 hgt)))
            )
        )
    )
    (if lst
        (   (lambda ( m ) (mapcar '(lambda ( p ) (mapcar '+ (mxv m p) bpt)) lst))
            (list
                (list (cos rot) (sin (- rot)) 0.0)
                (list (sin rot) (cos rot)     0.0)
               '(0.0 0.0 1.0)
            )
        )
    )
)

;; Matrix x Vector  -  Vladimir Nesterovsky
;; Args: m - nxn matrix, v - vector in R^n

(defun mxv ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)
(princ)