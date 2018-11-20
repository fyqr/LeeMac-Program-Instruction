;;---------------------=={ Total Length Polyline }==--------------------;;
;;                                                                      ;;
;;  This program allows the user to dynamically construct a             ;;
;;  straight-segmented polyline whose overall length may optionally be  ;;
;;  limited to a given value.                                           ;;
;;                                                                      ;;
;;  Upon issuing the command syntax 'tlpline' at the AutoCAD            ;;
;;  command-line, the program will first prompt the user to specify     ;;
;;  an optional length limit. At this prompt, the user may press        ;;
;;  'Enter' to dismiss the prompt and proceed without restricting the   ;;
;;  polyline length.                                                    ;;
;;                                                                      ;;
;;  The user may then proceed to dynamically construct a polyline with  ;;
;;  linear segments, with the overall polyline length automatically     ;;
;;  restricted to the specified limit. After three vertices have been   ;;
;;  specified, the user may choose to close the polyline, with the      ;;
;;  length remaining within the given limit.                            ;;
;;                                                                      ;;
;;  Whilst constructing the polyline, the overall polyline length is    ;;
;;  displayed in real-time beside the AutoCAD cursor as a visual aid.   ;;
;;                                                                      ;;
;;  The program also utilises my GrSnap utility to enable Object Snap   ;;
;;  functionality in conjunction with the dynamic visual effect.        ;;
;;                                                                      ;;
;;  The program is also designed to perform successfully under all      ;;
;;  UCS and View settings.                                              ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2016  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2016-06-23                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;

(defun c:tlpline ( / *error* cls col dis gr1 gr2 len lim lst msg ocs ort osf osm pt1 pt2 str tmp ucx )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (redraw) (princ)
    )
    
    (initget 6)
    (setq lim (cond ((getdist "\nSpecify length limit <none>: ")) (1e308)))

    (if (setq pt1 (getpoint "\nSpecify first point: "))
        (progn
            (setq msg (lambda ( ) (if (cddr lst) "\nSpecify next point [Close] <done>: " "\nSpecify next point <done>: "))
                  ucx (trans  (getvar 'ucsxdir) 0 1 t)
                  ocs (trans '(0.0 0.0 1.0)     1 0 t)
                  osf (LM:grsnap:snapfunction)
                  ort (getvar 'orthomode)
                  osm (getvar 'osmode)
                  col (LM:cecolor)
                  lst (list pt1)
                  len 0.0
                  str ""
                  cls 0
            )
            (princ (msg))
            (while
                (progn
                    (setq gr1 (grread t 15 0)
                          gr2 (cadr gr1)
                          gr1 (car  gr1)
                    )
                    (cond
                        (   (= 5 gr1)
                            (redraw)
                            (osf gr2 osm)
                            (if (= 1 ort)
                                (setq gr2 (LM:orthopoint (car lst) gr2 ucx))
                            )
                            (if (< lim (setq dis (distance (car lst) gr2)))
                                (progn
                                    (setq pt2 (polar (car lst) (angle (car lst) gr2) lim))
                                    (grdraw (car lst) pt2 col)
                                    (grdraw pt2 gr2 1 1)
                                    (LM:displaygrtext gr2 (LM:grtext (rtos (+ len lim))) 253 15 -31)
                                )
                                (progn
                                    (grdraw (car lst) gr2 col)
                                    (LM:displaygrtext gr2 (LM:grtext (rtos (+ len dis))) 253 15 -31)
                                )
                            )
                            (mapcar '(lambda ( a b ) (grdraw a b col)) lst (cdr lst)) t
                        )
                        (   (= 3 gr1)
                            (if (and (equal gr2 (setq gr2 (osf gr2 osm)) 1e-8) (= 1 ort))
                                (setq gr2 (LM:orthopoint (car lst) gr2 ucx))
                            )
                            (setq dis (distance (car lst) gr2)
                                  osm (getvar 'osmode)
                            )
                            (if (< dis lim)
                                (progn
                                    (setq len (+ len dis)
                                          lim (- lim dis)
                                          lst (cons gr2 lst)
                                    )
                                    (princ (msg))
                                )
                                (not (setq lst (cons (polar (car lst) (angle (car lst) gr2) lim) lst)))
                            )
                        )
                        (   (= 2 gr1)
                            (cond
                                (   (= 6 gr2)
                                    (if (zerop (logand 16384 (setq osm (setvar 'osmode (boole 6 16384 (getvar 'osmode))))))
                                        (princ "\n<Osnap on>")
                                        (princ "\n<Osnap off>")
                                    )
                                    (princ (msg))
                                )
                                (   (= 8 gr2)
                                    (if (< 0 (strlen str))
                                        (progn
                                            (princ "\010\040\010")
                                            (setq str (substr str 1 (1- (strlen str))))
                                        )
                                    )
                                    t
                                )
                                (   (= 15 gr2)
                                    (if (zerop (setq ort (setvar 'orthomode (- 1 ort))))
                                        (princ "\n<Ortho off>")
                                        (princ "\n<Ortho on>")
                                    )
                                    (princ (msg))
                                )
                                (   (< 32 gr2 127)
                                    (setq str (strcat str (princ (chr gr2))))
                                )
                                (   (member gr2 '(13 32))
                                    (cond
                                        (   (= "" str) nil)
                                        (   (and (cddr lst) (wcmatch "close" (strcat (strcase str t) "*")))
                                            (if (< (distance pt1 (car lst)) lim)
                                                (setq cls 1)
                                                (setq lst (cons (polar (car lst) (angle (car lst) pt1) lim) lst))
                                            )
                                            nil
                                        )
                                        (   (setq gr2 (LM:grsnap:parsepoint pt1 str))
                                            (if (< (setq dis (distance (car lst) gr2)) lim)
                                                (progn
                                                    (setq osm (getvar 'osmode)
                                                          len (+ len dis)
                                                          lim (- lim dis)
                                                          lst (cons gr2 lst)
                                                          str ""
                                                    )
                                                    (princ (msg))
                                                )
                                                (not (setq lst (cons (polar (car lst) (angle (car lst) gr2) lim) lst)))
                                            )
                                        )
                                        (   (setq tmp (LM:grsnap:snapmode str))
                                            (setq osm tmp
                                                  str ""
                                            )
                                        )
                                        (   (setq str "")
                                            (princ (strcat "\n2D / 3D Point Required." (msg)))
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
            (if (cdr lst)
                (progn
                    (LM:startundo (LM:acdoc))
                    (entmake
                        (append
                            (list
                               '(000 . "LWPOLYLINE")
                               '(100 . "AcDbEntity")
                               '(100 . "AcDbPolyline")
                                (cons 090 (length lst))
                                (cons 070 (logior cls (* (getvar 'plinegen) 128)))
                                (cons 210 ocs)
                            )
                            (mapcar '(lambda ( x ) (cons 10 (trans x 1 ocs))) (reverse lst))
                        )
                    )
                    (LM:endundo (LM:acdoc))
                )
            )
        )
    )
    (redraw) (princ)
)

;; Current Entity Color  -  Lee Mac
;; Returns the ACI colour equivalent to the current entity colour

(defun LM:cecolor ( / col pos )
    (cond
        (   (= "BYLAYER" (setq col (strcase (getvar 'cecolor))))
            (abs (cdr (assoc 62 (tblsearch "layer" (getvar 'clayer)))))
        )
        (   (= "BYBLOCK" col)
            7
        )
        (   (wcmatch col "RGB:*")
            (cond
                (   (apply 'LM:RGB->ACI (read (strcat "(" (vl-string-translate "," " " (substr col 5)) ")"))))
                (   7   )
            )
        )
        (   (setq pos (vl-string-position 36 col))
            (cond
                (   (LM:colorbook->ACI (substr col 1 pos) (substr col (+ 2 pos))))
                (   7   )
            )
        )
        (   (atoi col))
    )
)

;; Object Snap for grread: Snap Function  -  Lee Mac
;; Returns: [fun] A function requiring two arguments:
;; p - [lst] UCS Point to be snapped
;; o - [int] Object Snap bit code
;; The returned function returns either the snapped point (displaying an appropriate snap symbol)
;; or the supplied point if the snap failed for the given Object Snap bit code.

(defun LM:grsnap:snapfunction ( )
    (eval
        (list 'lambda '( p o / q )
            (list 'if '(zerop (logand 16384 o))
                (list 'if
                   '(setq q
                        (cdar
                            (vl-sort
                                (vl-remove-if 'null
                                    (mapcar
                                        (function
                                            (lambda ( a / b )
                                                (if (and (= (car a) (logand (car a) o)) (setq b (osnap p (cdr a))))
                                                    (list (distance p b) b (car a))
                                                )
                                            )
                                        )
                                       '(
                                            (0001 . "_end")
                                            (0002 . "_mid")
                                            (0004 . "_cen")
                                            (0008 . "_nod")
                                            (0016 . "_qua")
                                            (0032 . "_int")
                                            (0064 . "_ins")
                                            (0128 . "_per")
                                            (0256 . "_tan")
                                            (0512 . "_nea")
                                            (2048 . "_app")
                                            (8192 . "_par")
                                        )
                                    )
                                )
                               '(lambda ( a b ) (< (car a) (car b)))
                            )
                        )
                    )
                    (list 'LM:grsnap:displaysnap '(car q)
                        (list 'cdr
                            (list 'assoc '(cadr q)
                                (list 'quote
                                    (LM:grsnap:snapsymbols
                                        (atoi (cond ((getenv "AutoSnapSize")) ("5")))
                                    )
                                )
                            )
                        )
                        (cond
                            (   (LM:OLE->ACI
                                    (if (= 1 (getvar 'cvport))
                                        (atoi (cond ((getenv "Layout AutoSnap Color")) ("117761")))
                                        (atoi (cond ((getenv  "Model AutoSnap Color")) ("104193")))
                                    )
                                )
                            )
                            (   94   )
                        )
                    )
                )
            )
           '(cond ((car q)) (p))
        )
    )
)

;; Ortho Point  -  Lee Mac
;; Returns a point transformed relative to a basepoint to account for activation of Orthomode in the current UCS.
;; bpt - [lst] Basepoint for transformation (UCS)
;; pnt - [lst] Point to be transformed (UCS)

(defun LM:orthopoint ( bpt pnt ucx / ang tmp )
    (setq tmp (mapcar '- (trans pnt 0 ucx) (trans bpt 0 ucx))
          ang (angle '(0.0 0.0 0.0) ucx)
    )
    (if (< (abs (caddr tmp)) (abs (car tmp)))
        (polar bpt (+ ang (/ pi 2.0)) (car tmp))
        (polar bpt ang (caddr tmp))
    )
)

;; Object Snap for grread: Display Snap  -  Lee Mac
;; pnt - [lst] UCS point at which to display the symbol
;; lst - [lst] grvecs vector list
;; col - [int] ACI colour for displayed symbol
;; Returns nil

(defun LM:grsnap:displaysnap ( pnt lst col / scl )
    (setq scl (/ (getvar 'viewsize) (cadr (getvar 'screensize)))
          pnt (trans pnt 1 2)
    )
    (grvecs (cons col lst)
        (list
            (list scl 0.0 0.0 (car  pnt))
            (list 0.0 scl 0.0 (cadr pnt))
            (list 0.0 0.0 scl 0.0)
           '(0.0 0.0 0.0 1.0)
        )
    )
)

;; Object Snap for grread: Snap Symbols  -  Lee Mac
;; p - [int] Size of snap symbol in pixels
;; Returns: [lst] List of vector lists describing each Object Snap symbol

(defun LM:grsnap:snapsymbols ( p / -p -q -r a c i l q r )
    (setq -p (- p) q (1+  p)
          -q (- q) r (+ 2 p)
          -r (- r) i (/ pi 6.0)
           a 0.0
    )
    (repeat 12
        (setq l (cons (list (* r (cos a)) (* r (sin a))) l)
              a (- a i)
        )
    )
    (setq c (apply 'append (mapcar 'list (cons (last l) l) l)))
    (list
        (list 1
            (list -p -p) (list p -p) (list p -p) (list p p) (list p p) (list -p p) (list -p p) (list -p -p)
            (list -q -q) (list q -q) (list q -q) (list q q) (list q q) (list -q q) (list -q q) (list -q -q)
        )
        (list 2
            (list -r -q) (list 0  r) (list 0  r) (list r -q)
            (list -p -p) (list p -p) (list p -p) (list 0  p) (list 0  p) (list -p -p)
            (list -q -q) (list q -q) (list q -q) (list 0  q) (list 0  q) (list -q -q)
        )
        (cons 4 c)
        (vl-list* 8 (list -r -r) (list r r) (list r -r) (list -r r) c)
        (list 16
            (list p 0) (list 0 p) (list 0 p) (list -p 0) (list -p 0) (list 0 -p) (list 0 -p) (list p 0)
            (list q 0) (list 0 q) (list 0 q) (list -q 0) (list -q 0) (list 0 -q) (list 0 -q) (list q 0)
            (list r 0) (list 0 r) (list 0 r) (list -r 0) (list -r 0) (list 0 -r) (list 0 -r) (list r 0)
        )
        (list 32
            (list  r r) (list -r -r) (list  r q) (list -q -r) (list  q r) (list -r -q)
            (list -r r) (list  r -r) (list -q r) (list  r -q) (list -r q) (list  q -r)
        )
        (list 64
            '( 0  1) (list  0  p) (list  0  p) (list -p  p) (list -p  p) (list -p -1) (list -p -1) '( 0 -1)
            '( 0 -1) (list  0 -p) (list  0 -p) (list  p -p) (list  p -p) (list  p  1) (list  p  1) '( 0  1)
            '( 1  2) (list  1  q) (list  1  q) (list -q  q) (list -q  q) (list -q -2) (list -q -2) '(-1 -2)
            '(-1 -2) (list -1 -q) (list -1 -q) (list  q -q) (list  q -q) (list  q  2) (list  q  2) '( 1  2)
        )
        (list 128
            (list (1+ -p) 0) '(0 0) '(0 0) (list 0 (1+ -p))
            (list (1+ -p) 1) '(1 1) '(1 1) (list 1 (1+ -p))
            (list -p q) (list -p -p) (list -p -p) (list q -p)
            (list -q q) (list -q -q) (list -q -q) (list q -q)
        )
        (vl-list* 256 (list -r r)  (list r r) (list -r (1+ r)) (list r (1+ r)) c)
        (list 512
            (list -p -p) (list  p -p) (list -p  p) (list p p) (list -q -q) (list  q -q)
            (list  q -q) (list -q  q) (list -q  q) (list q q) (list  q  q) (list -q -q)
        )
        (list 2048
            (list   -p     -p) (list    p      p) (list   -p      p) (list    p     -p)
            (list (+ p 05) -p) (list (+ p 06) -p) (list (+ p 05) -q) (list (+ p 06) -q)
            (list (+ p 09) -p) (list (+ p 10) -p) (list (+ p 09) -q) (list (+ p 10) -q)
            (list (+ p 13) -p) (list (+ p 14) -p) (list (+ p 13) -q) (list (+ p 14) -q)
            (list -p -p) (list p -p) (list p -p) (list p p) (list p p) (list -p p) (list -p p) (list -p -p)
            (list -q -q) (list q -q) (list q -q) (list q q) (list q q) (list -q q) (list -q q) (list -q -q)
        )
        (list 8192 (list r 1) (list -r -q) (list r 0) (list -r -r) (list r q) (list -r -1) (list r r) (list -r 0))
    )
)

;; Object Snap for grread: Parse Point  -  Lee Mac
;; bpt - [lst] Basepoint for relative point input, e.g. @5,5
;; str - [str] String representing point input
;; Returns: [lst] Point represented by the given string, else nil

(defun LM:grsnap:parsepoint ( bpt str / str->lst lst )
 
    (defun str->lst ( str / pos )
        (if (setq pos (vl-string-position 44 str))
            (cons (substr str 1 pos) (str->lst (substr str (+ pos 2))))
            (list str)
        )
    )

    (if (wcmatch str "`@*")
        (setq str (substr str 2))
        (setq bpt '(0.0 0.0 0.0))
    )           

    (if
        (and
            (setq lst (mapcar 'distof (str->lst str)))
            (vl-every 'numberp lst)
            (< 1 (length lst) 4)
        )
        (mapcar '+ bpt lst)
    )
)

;; Object Snap for grread: Snap Mode  -  Lee Mac
;; str - [str] Object Snap modifier
;; Returns: [int] Object Snap bit code for the given modifier, else nil

(defun LM:grsnap:snapmode ( str )
    (vl-some
        (function
            (lambda ( x )
                (if (wcmatch (car x) (strcat (strcase str t) "*"))
                    (progn
                        (princ (cadr x)) (caddr x)
                    )
                )
            )
        )
       '(
            ("endpoint"      " of " 00001)
            ("midpoint"      " of " 00002)
            ("center"        " of " 00004)
            ("node"          " of " 00008)
            ("quadrant"      " of " 00016)
            ("intersection"  " of " 00032)
            ("insert"        " of " 00064)
            ("perpendicular" " to " 00128)
            ("tangent"       " to " 00256)
            ("nearest"       " to " 00512)
            ("appint"        " of " 02048)
            ("parallel"      " to " 08192)
            ("none"          ""     16384)
        )
    )
)

;; OLE -> ACI  -  Lee Mac
;; Args: c - [int] OLE Colour

(defun LM:OLE->ACI ( c )
    (apply 'LM:RGB->ACI (LM:OLE->RGB c))
)

;; OLE -> RGB  -  Lee Mac
;; Args: c - [int] OLE Colour

(defun LM:OLE->RGB ( c )
    (mapcar '(lambda ( x ) (lsh (lsh (fix c) x) -24)) '(24 16 8))
)

;; RGB -> ACI  -  Lee Mac
;; Args: r,g,b - [int] Red, Green, Blue values

(defun LM:RGB->ACI ( r g b / c o )
    (setq c
        (vl-catch-all-apply
           '(lambda nil
                (setq o (vla-getinterfaceobject (LM:acapp) (strcat "autocad.accmcolor." (substr (getvar 'acadver) 1 2))))
                (vla-setrgb o r g b)
                (vla-get-colorindex o)
            )
        )
    )
    (if (= 'vla-object (type o)) (vlax-release-object o))
    (if (not (vl-catch-all-error-p c)) c)
)

;; Color Book -> ACI  -  Lee Mac
;; b - [str] Color Book
;; n - [str] Color Name

(defun LM:colorbook->ACI ( b n / c o )
    (setq c
        (vl-catch-all-apply
           '(lambda nil
                (setq o (vla-getinterfaceobject (LM:acapp) (strcat "autocad.accmcolor." (substr (getvar 'acadver) 1 2))))
                (vla-setcolorbookcolor o b n)
                (vla-get-colorindex o)
            )
        )
    )
    (if (= 'vla-object (type o)) (vlax-release-object o))
    (if (not (vl-catch-all-error-p c)) c)
)

;; Display GrText  -  Lee Mac
;; pnt  -  cursor point in UCS
;; vec  -  GrText vector list
;; col  -  Text Colour (ACI Colour)
;; xof  -  x-offset from cursor in pixels
;; yof  -  y-offset from cursor in pixels
 
(defun LM:displaygrtext ( pnt vec col xof yof / scl )
    (setq scl (/ (getvar 'viewsize) (cadr (getvar 'screensize)))
          pnt (trans pnt 1 2)
    )
    (grvecs (cons col vec)
        (list
            (list scl 0.0 0.0 (+ (car  pnt) (* xof scl)))
            (list 0.0 scl 0.0 (+ (cadr pnt) (* yof scl)))
            (list 0.0 0.0 scl 0.0)
           '(0.0 0.0 0.0 1.0)
        )
    )
)

;;-----------------------=={ GrText }==-----------------------;;
;;                                                            ;;
;;  Returns a grvecs pixel vector list relative to the origin ;;
;;  encoding the supplied string.                             ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2013 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  With thanks to ElpanovEvgeniy for the method of vector    ;;
;;  encoding to save me a lot of typing.                      ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  str - String to be expressed in vector list format.       ;;
;;------------------------------------------------------------;;
;;  Returns:  GrVecs Pixel Vector List relative to the Origin ;;
;;------------------------------------------------------------;;
;;  Version 1.1    -    26-03-2011                            ;;
;;------------------------------------------------------------;;

(defun LM:grtext ( str / asc lst vec xco yco )
    (setq vec
       '(
            (033 045 045 065 135)
            (034 104 134 107 137)
            (035 043 063 046 066 084 094 087 097 115 135 118 138 072 078 103 109)
            (036 025 035 052 052 043 047 058 078 083 087 092 112 123 127 118 118 135 135)
            (037 052 052 063 063 074 074 085 085 096 096 107 107 118 118 129 129 047 048 067 068 056 056 059 059 113 114 133 134 122 122 125 125)
            (038 043 046 049 049 052 072 057 058 067 068 076 076 079 079 083 083 085 085 094 094 103 123 134 136 127 127)
            (039 105 135)
            (040 017 017 026 036 045 105 116 126 137 137)
            (041 014 014 025 035 046 106 115 125 134 134)
            (042 073 074 076 077 084 086 092 098 104 106 113 114 116 117)
            (043 055 115 082 084 086 088)
            (044 034 035 045 046 055 057)
            (045 083 088)
            (046 045 046 055 056)
            (047 052 052 063 063 074 074 085 085 096 096 107 107 118 118 129 129)
            (048 044 047 134 137 053 123 058 128)
            (049 044 048 124 125 056 136)
            (050 043 048 053 053 064 064 075 075 086 086 097 097 108 128 134 137 123 123)
            (051 053 053 044 047 058 088 095 097 108 128 134 137 123 123)
            (052 046 048 057 137 078 078 073 076 083 083 094 094 105 115 126 126)
            (053 053 053 044 047 058 088 094 097 093 133 134 138)
            (054 044 047 058 088 095 097 084 084 053 113 124 124 135 137)
            (055 044 054 065 075 086 096 107 117 128 138 133 137 123 123)
            (056 044 047 094 097 134 137 053 083 058 088 103 123 108 128)
            (057 044 046 057 057 068 128 097 097 084 086 134 137 093 123)
            (058 045 046 055 056 095 096 105 106)
            (059 034 035 045 046 055 057 095 096 105 106)
            (060 047 047 056 056 065 065 074 074 083 083 094 094 105 105 116 116 127 127)
            (061 073 078 093 098)
            (062 043 043 054 054 065 065 076 076 087 087 096 096 105 105 114 114 123 123)
            (063 045 045 065 075 086 086 097 097 108 128 134 137 123 123)
            (064 034 038 043 043 052 112 123 123 134 137 128 128 079 119 068 068 065 066 105 106 077 107 074 094)
            (065 041 043 047 049 052 062 058 068 073 077 083 093 087 097 104 114 106 116 125 135 133 134)
            (066 042 047 053 123 058 088 108 128 094 097 132 137)
            (067 044 047 053 053 058 058 062 112 123 123 134 136 127 127 108 138)
            (068 042 046 057 057 127 127 132 136 068 118 053 123)
            (069 042 048 058 058 094 095 086 106 132 137 128 138 053 123)
            (070 042 045 094 095 086 106 132 137 128 138 053 123)
            (071 044 047 053 053 058 078 086 089 062 112 123 123 134 136 127 127 108 138)
            (072 041 043 047 049 131 133 137 139 093 097 052 122 058 128)
            (073 043 047 133 137 055 125)
            (074 052 062 043 046 057 127 135 139)
            (075 042 044 048 049 132 134 136 138 053 123 084 085 095 095 106 116 127 127 076 076 067 067 058 058)
            (076 042 047 048 058 053 123 132 135)
            (077 041 043 047 049 052 122 058 128 131 132 138 139 103 113 107 117 084 094 086 096 065 075)
            (078 041 044 131 132 136 139 052 122 048 128 113 113 094 104 085 085 066 076 057 057)
            (079 044 046 053 053 057 057 123 123 127 127 134 136 062 112 068 118)
            (080 042 045 084 087 132 137 053 123 098 128)
            (081 134 136 123 123 127 127 112 062 118 068 053 053 057 057 044 046 035 036 023 024 027 028)
            (082 042 044 048 049 132 137 123 053 128 098 084 087 076 076 067 067 058 058)
            (083 042 062 053 053 044 047 058 078 086 087 093 095 102 122 133 136 127 127 118 138)
            (084 043 047 055 125 132 138 131 121 139 129)
            (085 044 046 052 053 057 058 062 122 068 128 131 133 137 139)
            (086 045 055 064 074 066 076 083 103 087 107 112 122 118 128 131 133 137 139)
            (087 043 063 047 067 072 092 074 094 076 096 078 098 101 121 105 115 109 129 131 132 138 139)
            (088 041 043 047 049 131 133 137 139 052 052 058 058 063 063 067 067 074 074 076 076 085 095 104 104 106 106 113 113 117 117 122 122 128 128)
            (089 043 047 055 085 094 094 096 096 103 113 107 117 122 122 128 128 131 133 137 139)
            (090 122 122 058 058 132 138 042 048 128 128 052 052 063 063 074 074 085 095 106 106 117 117)
            (091 015 017 135 137 025 125)
            (092 122 122 113 113 104 104 095 095 086 086 077 077 068 068 059 059)
            (093 014 016 134 136 026 126)
            (094 102 102 113 113 124 124 135 135 126 126 117 117 108 108)
            (095 021 029)
            (096 125 125 134 134)
            (097 043 046 048 048 052 072 057 097 083 086 103 106)
            (098 042 043 045 046 054 054 057 058 068 098 097 097 105 106 094 094 132 132 053 133)
            (099 044 046 053 053 057 058 052 092 093 093 104 106 097 098 108 108)
            (100 044 045 047 048 052 092 053 053 056 056 093 093 104 105 096 096 136 136 057 137)
            (101 044 046 053 053 057 058 052 092 093 093 104 106 097 098 088 088 073 078)
            (102 043 046 054 124 093 093 095 096 135 137 128 128)
            (103 013 016 022 032 027 097 107 108 066 066 096 096 054 055 104 105 063 063 093 093 062 092)
            (104 042 044 046 048 057 097 053 133 132 132 094 094 105 106)
            (105 043 047 055 105 103 104 135 135)
            (106 022 022 013 015 026 106 104 105 136 136)
            (107 042 044 046 048 053 133 132 132 057 057 066 066 074 075 085 085 096 106 107 108)
            (108 043 047 055 135 133 134)
            (109 041 043 045 046 048 049 052 102 055 105 058 108 101 101 093 093 104 104 096 096 107 107)
            (110 042 044 046 048 053 103 057 097 102 102 094 094 105 106)
            (111 044 046 104 106 053 053 057 057 093 093 097 097 052 092 058 098)
            (112 012 015 023 103 102 102 054 054 094 094 045 046 105 106 057 058 097 098 068 088)
            (113 015 018 027 107 108 108 056 056 096 096 044 045 104 105 052 053 092 093 062 082)
            (114 042 046 054 104 102 103 095 095 106 108 099 099)
            (115 052 052 043 047 058 068 073 077 082 092 103 107 098 098)
            (116 045 047 058 058 054 124 102 103 105 107)
            (117 102 102 106 106 053 103 056 056 044 045 047 107 048 048)
            (118 045 045 054 064 056 066 073 083 077 087 092 092 098 098 101 103 107 109)
            (119 043 053 047 057 062 092 064 084 066 086 068 098 101 103 095 105 107 109)
            (120 042 044 046 048 102 104 106 108 053 053 057 057 093 093 097 097 064 064 066 066 084 084 086 086 075 075)
            (121 012 013 024 024 035 045 054 064 056 066 073 083 077 087 092 092 098 098 101 103 107 109)
            (122 092 092 058 058 102 108 042 048 097 097 086 086 075 075 064 064 053 053)
            (123 016 017 025 065 073 074 085 125 136 137)
            (124 015 135)
            (125 014 015 026 066 077 078 086 126 134 135)
            (126 112 122 133 134 125 125 116 117 128 138)
            (145 114 116 125 126 136 137)
            (146 114 115 125 126 135 137)
            (161 045 115 135 135)
            (162 026 036 045 047 058 058 054 054 053 093 094 094 098 098 105 107 116 126)
            (163 043 048 054 074 083 086 094 094 103 123 134 136 117 127)
            (164 083 083 088 088 133 133 138 138 094 097 124 127 104 114 107 117)
            (165 044 046 055 075 081 089 094 094 096 096 101 103 107 109 113 113 117 117 122 122 128 128 131 133 137 139)
            (166 015 055 095 135)
            (167 042 042 032 036 047 047 056 057 065 065 074 074 083 083 092 102 068 078 087 087 096 096 105 105 113 114 123 123 134 138 128 128)
            (168 134 134 137 137)
            (169 054 057 063 063 068 068 072 122 079 129 133 133 138 138 144 147 075 076 087 087 084 114 125 126 117 117)
            (170 063 067 084 086 088 088 093 103 097 127 114 116 134 136)
            (171 055 055 064 064 073 073 082 082 093 093 104 104 115 115 058 058 067 067 076 076 085 085 096 096 107 107 118 118)
            (172 068 098 092 097)
            (173 083 088)
            (174 054 057 063 063 068 068 072 122 079 129 133 133 138 138 144 147 074 124 095 096 125 126 077 087 107 117)
            (175 151 159)
            (176 105 106 114 124 117 127 135 136)
            (177 042 048 092 098 065 085 105 125)
            (178 084 087 095 095 106 106 117 127 135 136 124 124)
            (179 094 094 085 086 097 107 116 116 127 127 135 136 124 124)
            (180 125 125 136 136)
            (181 012 012 023 113 044 047 049 049 058 118)
            (182 045 045 049 049 048 128 046 126 133 139 122 125 112 115 102 105 092 095 083 085)
            (183 085 086 095 096)
            (184 014 015 026 026 035 035)
            (185 084 086 124 124 095 135)
            (186 063 067 084 086 134 136 093 123 097 127)
            (187 052 052 063 063 074 074 085 085 094 094 103 103 112 112 055 055 066 066 077 077 088 088 097 097 106 106 115 115)
            (188 048 098 059 059 055 057 065 065 076 076 087 087 083 133 122 122 052 052 063 063 074 074 085 085 096 096 107 107 118 118 129 129)
            (189 046 049 057 057 068 068 079 089 097 098 086 086 083 133 122 122 052 052 063 063 074 074 085 085 096 096 107 107 118 118 129 129)
            (190 048 098 059 059 055 057 065 065 076 076 087 087 092 092 083 084 095 105 114 114 125 125 133 134 122 122 052 052 063 063 074 074 085 085 096 096 107 107 118 118 129 129)
            (191 044 047 058 058 053 073 084 084 095 095 106 116 136 136)
            (192 041 043 047 049 052 062 058 068 073 077 083 093 087 097 104 114 106 116 125 135 133 134 155 155 164 164)
            (193 041 043 047 049 052 062 058 068 073 077 083 093 087 097 104 114 106 116 125 135 133 134 155 155 166 166)
            (194 041 043 047 049 052 062 058 068 073 077 083 093 087 097 104 114 106 116 125 135 133 134 154 154 165 165 156 156)
            (195 041 043 047 049 052 062 058 068 073 077 083 093 087 097 104 114 106 116 125 135 133 134 152 152 163 165 155 157 168 168)
            (196 041 043 047 049 052 062 058 068 073 077 083 093 087 097 104 114 106 116 125 135 133 134 163 163 167 167)
            (197 041 043 047 049 052 062 058 068 073 077 083 093 087 097 104 114 106 116 125 135 133 134 145 145 154 154 165 165 156 156)
            (198 041 043 045 049 052 062 073 093 104 114 125 125 084 085 059 059 056 126 097 098 088 088 108 108 134 139 129 129)
            (199 044 047 053 053 058 058 062 112 123 123 134 136 127 127 108 138 014 015 026 026 035 035)
            (200 042 048 058 058 094 095 086 106 132 137 128 138 053 123 156 156 165 165)
            (201 042 048 058 058 094 095 086 106 132 137 128 138 053 123 155 155 166 166)
            (202 042 048 058 058 094 095 086 106 132 137 128 138 053 123 154 154 165 166 157 157)
            (203 042 048 058 058 094 095 086 106 132 137 128 138 053 123 164 164 167 167)
            (204 043 047 133 137 055 125 155 155 164 164)
            (205 043 047 133 137 055 125 155 155 166 166)
            (206 043 047 133 137 055 125 154 154 165 165 156 156)
            (207 043 047 133 137 055 125 163 163 167 167)
            (208 042 046 057 057 127 127 132 136 068 118 053 123 091 092 094 095)
            (209 041 044 131 132 137 139 052 122 048 128 113 113 094 104 085 085 066 076 057 057 152 152 163 165 155 157 168 168)
            (210 044 046 053 053 057 057 123 123 127 127 134 136 062 112 068 118 155 155 164 164)
            (211 044 046 053 053 057 057 123 123 127 127 134 136 062 112 068 118 155 155 166 166)
            (212 044 046 053 053 057 057 123 123 127 127 134 136 062 112 068 118 154 154 165 165 156 156)
            (213 044 046 053 053 057 057 123 123 127 127 134 136 062 112 068 118 152 152 163 165 155 157 168 168)
            (214 044 046 053 053 057 057 123 123 127 127 134 136 062 112 068 118 163 163 167 167)
            (215 052 052 063 063 074 074 085 085 096 096 107 107 118 118 058 058 067 067 076 076 094 094 103 103 112 112)
            (216 044 046 053 053 057 057 123 123 127 127 134 136 062 112 068 118 043 043 064 074 085 095 106 116 137 137)
            (217 044 046 052 053 057 058 062 122 068 128 131 133 137 139 155 155 164 164)
            (218 044 046 052 053 057 058 062 122 068 128 131 133 137 139 155 155 166 166)
            (219 044 046 052 053 057 058 062 122 068 128 131 133 137 139 154 154 165 165 156 156)
            (220 044 046 052 053 057 058 062 122 068 128 131 133 137 139 163 163 167 167)
            (221 044 046 055 085 094 094 096 096 103 113 107 117 122 122 128 128 131 133 137 139 145 155 166 166)
            (222 042 044 132 132 053 133 074 077 104 107 088 098)
            (223 042 042 043 123 134 136 107 127 095 096 087 087 058 078 045 047)
            (224 043 046 048 048 052 072 057 097 083 086 103 106 125 125 134 134)
            (225 043 046 048 048 052 072 057 097 083 086 103 106 125 125 136 136)
            (226 043 046 048 048 052 072 057 097 083 086 103 106 124 124 135 135 126 126)
            (227 043 046 048 048 052 072 057 097 083 086 103 106 122 122 133 134 125 126 137 137)
            (228 043 046 048 048 052 072 057 097 083 086 103 106 133 133 137 137)
            (229 043 046 048 048 052 072 057 097 083 086 103 106 125 125 134 134 145 145 136 136)
            (230 042 044 046 048 059 059 051 071 082 084 102 104 055 095 076 079 089 099 106 108)
            (231 014 015 026 026 035 035 044 046 053 053 057 058 052 092 093 093 104 106 097 098 108 108)
            (232 044 046 053 053 057 058 052 092 093 093 104 106 097 098 088 088 073 078 125 125 134 134)
            (233 044 046 053 053 057 058 052 092 093 093 104 106 097 098 088 088 073 078 125 125 136 136)
            (234 044 046 053 053 057 058 052 092 093 093 104 106 097 098 088 088 073 078 124 124 135 135 126 126)
            (235 044 046 053 053 057 058 052 092 093 093 104 106 097 098 088 088 073 078 133 133 137 137)
            (236 043 047 055 105 103 104 125 125 134 134)
            (237 043 047 055 105 103 104 124 124 135 135)
            (238 043 047 055 105 103 104 124 124 135 135 126 126)
            (239 043 047 055 105 103 104 133 133 136 136)
            (240 044 046 053 053 057 057 052 082 058 088 083 083 087 107 094 096 116 116 113 114 125 125 134 134 136 137)
            (241 042 044 046 048 053 103 057 097 102 102 094 094 105 106 122 122 133 134 125 126 137 137)
            (242 044 046 104 106 053 053 057 057 093 093 097 097 052 092 058 098 125 125 134 134)
            (243 044 046 104 106 053 053 057 057 093 093 097 097 052 092 058 098 125 125 136 136)
            (244 044 046 104 106 053 053 057 057 093 093 097 097 052 092 058 098 124 124 135 135 126 126)
            (245 044 046 104 106 053 053 057 057 093 093 097 097 052 092 058 098 122 122 133 135 125 127 138 138)
            (246 044 046 104 106 053 053 057 057 093 093 097 097 052 092 058 098 133 133 137 137)
            (247 055 055 115 115 082 088)
            (248 044 046 104 106 053 053 057 057 093 093 097 097 052 092 058 098 042 042 064 064 075 075 086 086 108 108)
            (249 102 102 106 106 053 103 056 056 044 045 047 107 048 048 125 125 134 134)
            (250 102 102 106 106 053 103 056 056 044 045 047 107 048 048 125 125 136 136)
            (251 102 102 106 106 053 103 056 056 044 045 047 107 048 048 124 124 135 135 126 126)
            (252 102 102 106 106 053 103 056 056 044 045 047 107 048 048 133 133 137 137)
            (253 012 013 024 024 035 045 054 064 056 066 073 083 077 087 092 092 098 098 101 103 107 109 125 125 136 136)
            (254 012 015 132 132 023 133 054 054 045 046 057 058 068 088 094 094 105 106 097 098)
            (255 012 013 024 024 035 045 054 064 056 066 073 083 077 087 092 092 098 098 101 103 107 109 133 133 137 137)
        )
    )
    (eval
        (list 'defun 'LM:GrText '( str / asc lst vec xco yco )
            (list 'setq 'vec
                (list 'quote
                    (mapcar
                        (function
                            (lambda ( b )
                                (cons (car b)
                                    (mapcar
                                        (function
                                            (lambda ( a )
                                                (list (rem a 10) (/ a 10))
                                            )
                                        )
                                        (cdr b)
                                    )
                                )
                            )
                        )
                        vec
                    )
                )
            )
           '(setq xco 0 yco 0)
           '(repeat (strlen str)
                (setq asc (ascii str)
                      str (substr str 2)
                )
                (cond
                    (   (= 32 asc)
                        (setq xco (+ xco 09))
                    )
                    (   (= 09 asc)
                        (setq xco (+ xco 36))
                    )
                    (   (= 10 asc)
                        (setq xco 0
                              yco (- yco 16)
                        )
                    )
                    (   (setq lst
                            (cons
                                (mapcar
                                    (function
                                        (lambda ( a )
                                            (list (+ (car a) xco) (+ (cadr a) yco))
                                        )
                                    )
                                    (cdr (assoc asc vec))
                                )
                                lst
                            )
                        )
                        (setq xco (+ xco 9))
                    )
                )
            )
           '(apply 'append lst)
        )
    )
    (LM:grtext str)
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

;; Application Object  -  Lee Mac
;; Returns the VLA Application Object

(defun LM:acapp nil
    (eval (list 'defun 'LM:acapp 'nil (vlax-get-acad-object)))
    (LM:acapp)
)

;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: TotalLengthPolyline.lsp | Version 1.0 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"tlpline\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;