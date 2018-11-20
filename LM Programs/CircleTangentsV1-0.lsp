;;-----------------------=={ Circle Tangents  }==-----------------------;;
;;                                                                      ;;
;;  This program allows the user to dynamically construct two circles   ;;
;;  connected with a pair of lines meeting the circumference of each    ;;
;;  circle at a tangent, resulting in a belt or cam shape.              ;;
;;                                                                      ;;
;;  Upon issuing the command syntax 'ctan' at the AutoCAD               ;;
;;  command-line, the program will issue four successive prompts: the   ;;
;;  user is prompted to specify the center of the first circle, the     ;;
;;  radius of the first circle, followed by the center & radius of      ;;
;;  the second circle.                                                  ;;
;;                                                                      ;;
;;  During each of these prompts, the circles and adjoining lines are   ;;
;;  displayed dynamically in real-time relative to the position of the  ;;
;;  AutoCAD cursor.                                                     ;;
;;                                                                      ;;
;;  Following valid responses to all prompts, the program will          ;;
;;  construct the resulting shape using a 2D polyline (LWPolyline).     ;;
;;                                                                      ;;
;;  However, if the radius of the second circle is greater than the     ;;
;;  combination of the distance between the circle centers & radius of  ;;
;;  the first circle, the program will instead construct a circle       ;;
;;  centered at the second given center, with radius equal to this      ;;
;;  maximum limit.                                                      ;;
;;                                                                      ;;
;;  Similarly, if the distance between the two circle centers is less   ;;
;;  than the radius of the first circle, the program will construct     ;;
;;  only the first circle.                                              ;;
;;                                                                      ;;
;;  Although the dynamic visual effect is dependent on heavy use of     ;;
;;  the AutoLISP grread function, this program utilises my GrSnap       ;;
;;  utility to enable full Object Snap functionality during the         ;;
;;  dynamic prompts. The latest version and full documentation for      ;;
;;  this utility may be found at: http://www.lee-mac.com/grsnap.html    ;;
;;                                                                      ;;
;;  Finally, this program has been designed to perform successfully     ;;
;;  under all UCS & View settings.                                      ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2014  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2014-08-25                                      ;;
;;                                                                      ;;
;;  First release.                                                      ;;
;;----------------------------------------------------------------------;;

(defun c:ctan ( / *error* grcircle grarc grgetpoint an1 an2 cn1 cn2 di1 di2 ocs rd1 rd2 tmp )

    (setq ctan:res 40 ;; arc resolution (int > 0)
          ctan:2pi (+ pi pi)
          ctan:inc (/ ctan:2pi ctan:res)
    )
    
    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (and msg (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*")))
            (princ (strcat "\nError: " msg))
        )
        (redraw) (princ)
    )

    (defun grcircle ( cen rad / ang )
        (setq ang 0.0)
        (repeat ctan:res
            (grdraw (polar cen ang rad) (polar cen (setq ang (+ ang ctan:inc)) rad) 1)
        )
    )

    (defun grarc ( cen pt1 pt2 / ang rad )
        (setq ang (angle cen pt1)
              rad (distance cen pt1)
        )
        (repeat (fix (/ (rem (+ (- (angle cen pt2) ang) ctan:2pi) ctan:2pi) ctan:inc))
            (grdraw pt1 (setq pt1 (polar cen (setq ang (+ ang ctan:inc)) rad)) 1)
        )
        (grdraw pt1 pt2 1)
    )

    (defun grgetpoint ( msg bpt flg fun / gr1 gr2 osf osm rtn str tmp )
        (setq osf (LM:grsnap:snapfunction)
              osm (getvar 'osmode)
              fun (eval fun)
              str ""
        )
        (princ msg)
        (while
            (progn
                (setq gr1 (grread t 15 0)
                      gr2 (cadr gr1)
                      gr1 (car  gr1)
                )
                (cond
                    (   (= 5 gr1) (redraw)
                        (osf gr2 osm)
                        (fun gr2)
                        t
                    )
                    (   (= 3 gr1) nil)
                    (   (= 2 gr1)
                        (cond
                            (   (= 6 gr2)
                                (if (zerop (logand 16384 (setq osm (setvar 'osmode (boole 6 16384 (getvar 'osmode))))))
                                    (princ "\n<Osnap on>")
                                    (princ "\n<Osnap off>")
                                )
                                (princ msg)
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
                            (   (< 32 gr2 127)
                                (setq str (strcat str (princ (chr gr2))))
                            )
                            (   (member gr2 '(13 32))
                                (cond
                                    (   (= "" str) nil)
                                    (   (setq gr2 (LM:grsnap:parsepoint bpt str))
                                        (setq osm 16384)
                                        nil
                                    )
                                    (   (setq tmp (LM:grsnap:snapmode str))
                                        (setq osm tmp
                                              str ""
                                        )
                                    )
                                    (   (and  flg (distof str))
                                        (setq gr2 (mapcar '+ bpt (list (distof str) 0.0 0.0))
                                              osm 16384
                                        )
                                        nil
                                    )
                                    (   (setq str "")
                                        (princ (strcat "\n2D / 3D Point Required." msg))
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
        (if (listp gr2) (osf gr2 osm))
    )

    (LM:startundo (LM:acdoc))
    (if (setq cn1 (getpoint "\nSpecify center of 1st circle: "))
        (progn
            (while
                (and
                    (setq tmp
                        (grgetpoint "\nSpecify 1st radius: " cn1 t
                            (function
                                (lambda ( gr2 )
                                    (grcircle cn1 (distance cn1 gr2))
                                )
                            )
                        )
                    )
                    (equal 0.0 (setq rd1 (distance cn1 tmp)) 1e-8)
                )
                (princ "\nRadius cannot be zero.")
            )
            (if
                (and tmp
                    (setq cn2
                        (grgetpoint "\nSpecify center of 2nd circle: " cn1 nil
                            (function
                                (lambda ( gr2 / an1 an2 di1 pt1 pt2 )
                                    (if (< rd1 (setq di1 (distance cn1 gr2)))
                                        (progn
                                            (setq an1 (angle cn1 gr2)
                                                  an2 (atan (sqrt (- (* di1 di1) (* rd1 rd1))) rd1)
                                                  pt1 (polar cn1 (+ an1 an2) rd1)
                                                  pt2 (polar cn1 (- an1 an2) rd1)
                                            )
                                            (grarc  cn1 pt1 pt2)
                                            (grdraw gr2 pt1 1)
                                            (grdraw gr2 pt2 1)
                                        )
                                        (grcircle cn1 rd1)
                                    )
                                )
                            )
                        )
                    )
                    (setq di1 (distance cn1 cn2)
                          an1 (angle cn1 cn2)
                          ocs (trans '(0.0 0.0 1.0) 1 0 t)
                    )
                )
                (if (< rd1 di1)
                    (if
                        (setq tmp
                            (grgetpoint "\nSpecify 2nd radius: " cn2 t
                                (function
                                    (lambda ( gr2 / an2 pt1 pt2 pt3 pt4 )
                                        (if (< (abs (setq di2 (- rd1 (setq rd2 (distance cn2 gr2))))) di1)
                                            (progn
                                                (setq an2 (atan (sqrt (- (* di1 di1) (* di2 di2))) di2)
                                                      pt1 (polar cn1 (+ an1 an2) rd1)
                                                      pt2 (polar cn1 (- an1 an2) rd1)
                                                      pt3 (polar cn2 (- an1 an2) rd2)
                                                      pt4 (polar cn2 (+ an1 an2) rd2)
                                                )
                                                (grarc  cn1 pt1 pt2)
                                                (grarc  cn2 pt3 pt4)
                                                (grdraw pt1 pt4 1)
                                                (grdraw pt2 pt3 1)
                                            )
                                            (grcircle cn2 (+ di1 rd1))
                                        ) 
                                    )
                                )
                            )
                        )
                        (if (< (abs (setq di2 (- rd1 (setq rd2 (distance cn2 tmp))))) di1)
                            (progn
                                (setq an2 (atan (sqrt (- (* di1 di1) (* di2 di2))) di2))
                                (entmake
                                    (list
                                       '(000 . "LWPOLYLINE")
                                       '(100 . "AcDbEntity")
                                       '(100 . "AcDbPolyline")
                                       '(090 . 40)
                                       '(070 . 01)
                                        (cons 010 (trans (polar cn1 (+ an1 an2) rd1) 1 ocs))
                                        (cons 042 (/ (sin (/ (- pi an2) 2.0)) (cos (/ (- pi an2) 2.0))))
                                        (cons 010 (trans (polar cn1 (- an1 an2) rd1) 1 ocs))
                                        (cons 010 (trans (polar cn2 (- an1 an2) rd2) 1 ocs))
                                        (cons 042 (/ (sin (/ an2 2.0)) (cos (/ an2 2.0))))
                                        (cons 010 (trans (polar cn2 (+ an1 an2) rd2) 1 ocs))
                                        (cons 210 ocs)
                                    )
                                )
                            )
                            (entmake
                                (list
                                   '(000 . "CIRCLE")
                                    (cons 010 (trans cn2 1 ocs))
                                    (cons 040 (+ di1 rd1))
                                    (cons 210 ocs)
                                )
                            )
                        )
                    )
                    (entmake
                        (list
                           '(000 . "CIRCLE")
                            (cons 010 (trans cn1 1 ocs))
                            (cons 040 rd1)
                            (cons 210 ocs)
                        )
                    )
                )
            )
        )
    )
    (*error* nil)
    (princ)
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
                        (LM:OLE->ACI
                            (if (= 1 (getvar 'cvport))
                                (atoi (cond ((getenv "Layout AutoSnap Color")) ("117761")))
                                (atoi (cond ((getenv  "Model AutoSnap Color")) ("104193")))
                            )
                        )
                    )
                )
            )
           '(cond ((car q)) (p))
        )
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
    (if (setq o (vla-getinterfaceobject (LM:acapp) (strcat "autocad.accmcolor." (substr (getvar 'acadver) 1 2))))
        (progn
            (setq c (vl-catch-all-apply '(lambda ( ) (vla-setrgb o r g b) (vla-get-colorindex o))))
            (vlax-release-object o)
            (if (vl-catch-all-error-p c)
                (prompt (strcat "\nError: " (vl-catch-all-error-message c)))
                c
            )
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

;; Application Object  -  Lee Mac
;; Returns the VLA Application Object

(defun LM:acapp nil
    (eval (list 'defun 'LM:acapp 'nil (vlax-get-acad-object)))
    (LM:acapp)
)

(vl-load-com)
(princ
    (strcat
        "\n:: CircleTangents.lsp | Version 1.0 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"ctan\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;