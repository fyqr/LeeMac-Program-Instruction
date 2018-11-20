;;---------------------=={ Polyline Information }==---------------------;;
;;                                                                      ;;
;;  This program provides the user with detailed information about      ;;
;;  every segment of a selected polyline in the form of either an       ;;
;;  AutoCAD Table (if available), Text file, or CSV file.               ;;
;;                                                                      ;;
;;  Upon calling the program with the command syntax 'polyinfo' at the  ;;
;;  AutoCAD command-line, the user is prompted to select an LWPolyline  ;;
;;  to be queried from the active drawing. At this prompt the user      ;;
;;  also has the option to choose the form of output for the            ;;
;;  information harvested by the program; this output format will be    ;;
;;  remembered between drawing sessions to enable streamlined repeated  ;;
;;  program usage.                                                      ;;
;;                                                                      ;;
;;  The program will output LWPolyline segment data to either an        ;;
;;  AutoCAD Table Object created in the active drawing (if such object  ;;
;;  is available in the version of AutoCAD in which the program is      ;;
;;  being executed), or a tab-delimited Text file or CSV file           ;;
;;  automatically created (streamlining the program to minimise         ;;
;;  prompts) in the working directory of the active drawing.            ;;
;;                                                                      ;;
;;  For every segment of the selected LWPolyline, the program will      ;;
;;  extract the following information:                                  ;;
;;                                                                      ;;
;;      • Segment Number                                                ;;
;;      • Segment Start Vertex Coordinate                               ;;
;;      • Segment End Vertex Coordinate                                 ;;
;;      • Segment Start Width                                           ;;
;;      • Segment End Width                                             ;;
;;      • Segment Length                                                ;;
;;      • Arc Centre (if arc segment)                                   ;;
;;      • Arc Radius (if arc segment)                                   ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2014  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2012-07-10                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2012-07-16                                      ;;
;;                                                                      ;;
;;  - Added Table & Text file output options.                           ;;
;;  - Removed basic LWPolyline properties.                              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.2    -    2014-06-14                                      ;;
;;                                                                      ;;
;;  - Fixed bug causing final segment to be omitted from output data    ;;
;;    when processing closed polylines.                                 ;;
;;----------------------------------------------------------------------;;
;;  Version 1.3    -    2015-04-13                                      ;;
;;                                                                      ;;
;;  - Fixed bug causing the program to crash when processing polylines  ;;
;;    containing arc segments.                                          ;;
;;----------------------------------------------------------------------;;

(defun c:polyinfo ( / *error* ent enx flg ins lst out seg tmp )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (= 'str (type out)) (setenv "LMac\\PolyInfo" out))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
   
    (if (null (setq out (getenv "LMac\\PolyInfo")))
        (setq out "TXT")
    )
    (princ
        (strcat "\nOutput Format: "
            (cond
                (   (= out "TXT") "Text File")
                (   (= out "CSV") "CSV File")
                (   "AutoCAD Table"   )
            )
        )
    )

    (while
        (progn
            (setvar 'errno 0)
            (initget "Output")
            (setq ent (entsel "\nSelect polyline [Output]: "))
            (cond
                (   (= 7 (getvar 'errno))
                    (princ "\nMissed, try again.")
                )
                (   (null ent)
                    nil
                )
                (   (= "Output" ent)
                    (polyinfo:chooseoutput  'out)
                    (setenv "LMac\\PolyInfo" out)
                    (princ
                        (strcat "\nOutput Format: "
                            (cond
                                (   (= out "TXT") "Text File")
                                (   (= out "CSV") "CSV File")
                                (   "AutoCAD Table"   )
                            )
                        )
                    )
                )
                (   (/= "LWPOLYLINE" (cdr (assoc 0 (entget (setq ent (car ent))))))
                    (princ "\nSelected object is not an LWPolyline.")
                )
            )
        )
    )
    (cond
        (   (and
                (= 'ename (type ent))
                (= "Table" out)
                (= 4 (logand 4 (cdr (assoc 70 (tblsearch "layer" (getvar 'clayer))))))
            )
            (princ "\nCurrent layer locked.")
        )
        (   (= 'ename (type ent))
            (setq seg 0
                  enx (entget ent)
                  lst (LM:lwvertices enx)
                  lst
                (cons
                    (append '("SEG." "START X" "START Y" "END X" "END Y" "WIDTH 1" "WIDTH 2" "LENGTH")
                        (if (setq flg (vl-some '(lambda ( x ) (not (zerop (cdr (assoc 42 x))))) lst))
                           '("CENTRE X" "CENTRE Y" "RADIUS")
                        )
                    )
                    (mapcar
                        (function
                            (lambda ( l1 l2 / b p q )
                                (setq p (cdr (assoc 10 l1))
                                      q (cdr (assoc 10 l2))
                                      b (cdr (assoc 42 l1))
                                )
                                (append
                                    (list (itoa (setq seg (1+ seg))))
                                    (mapcar 'rtos p)
                                    (mapcar 'rtos q)
                                    (list
                                        (rtos (cdr (assoc 40 l1)))
                                        (rtos (cdr (assoc 41 l1)))
                                    )
                                    (if (zerop b)
                                        (cons (rtos (distance p q)) (if flg '("" "" "")))
                                        (append
                                            (list (rtos (abs (* (LM:bulgeradius p q b) (atan b) 4))))
                                            (mapcar 'rtos (LM:bulgecentre p q b))
                                            (list (rtos (LM:bulgeradius p q b)))
                                        )
                                    )
                                )
                            )
                        )
                        lst
                        (if (= 1 (logand 1 (cdr (assoc 70 enx))))
                            (append (cdr lst) (list (car lst)))
                            (cdr lst)
                        )
                    )
                )
            )
            (cond
                (   (= out "TXT")
                    (if (LM:writetxt lst (setq tmp (vl-filename-mktemp (cdr (assoc 5 enx)) (getvar 'dwgprefix) ".txt")))
                        (startapp "explorer" tmp)
                    )
                )
                (   (= out "CSV")
                    (if (LM:writecsv lst (setq tmp (vl-filename-mktemp (cdr (assoc 5 enx)) (getvar 'dwgprefix) ".csv")))
                        (startapp "explorer" tmp)
                    )
                )
                (   (setq ins (getpoint "\nSpecify point for table: "))
                    (LM:startundo (LM:acdoc))
                    (LM:addtable  (vlax-get-property (LM:acdoc) (if (= 1 (getvar 'cvport)) 'paperspace 'modelspace)) (trans ins 1 0) nil lst nil)
                    (LM:endundo   (LM:acdoc))
                )
            )
        )
    )
    (princ)
)

;; Add Table  -  Lee Mac
;; Generates a table at the given point, populated with the given data and optional title.
;; spc - [vla] VLA Block object
;; ins - [lst] WCS insertion point for table
;; ttl - [str] [Optional] Table title
;; lst - [lst] Matrix list of table cell data
;; eqc - [bol] If T, columns are of equal width
;; Returns: [vla] VLA Table Object

(defun LM:addtable ( spc ins ttl lst eqc / dif hgt i j obj stn sty wid )
    (setq sty
        (vlax-ename->vla-object
            (cdr
                (assoc -1
                    (dictsearch (cdr (assoc -1 (dictsearch (namedobjdict) "acad_tablestyle")))
                        (getvar 'ctablestyle)
                    )
                )
            )
        )
    )
    (setq hgt (vla-gettextheight sty acdatarow))
    (if (LM:annotative-p (setq stn (vla-gettextstyle sty acdatarow)))
        (setq hgt (/ hgt (cond ((getvar 'cannoscalevalue)) (1.0))))
    )
    (setq wid
        (mapcar
           '(lambda ( col )
                (apply 'max
                    (mapcar
                       '(lambda ( str )
                            (   (lambda ( box ) (if box (+ (* 2.5 hgt) (- (caadr box) (caar box))) 0.0))
                                (textbox
                                    (list
                                        (cons 01 str)
                                        (cons 40 hgt)
                                        (cons 07 stn)
                                    )
                                )
                            )
                        )
                        col
                    )
                )
            )
            (apply 'mapcar (cons 'list lst))
        )
    )
    (if 
        (and ttl
            (< 0.0
                (setq dif
                    (/
                        (-
                            (   (lambda ( box ) (if box (+ (* 2.5 hgt) (- (caadr box) (caar box))) 0.0))
                                (textbox
                                    (list
                                        (cons 01 ttl)
                                        (cons 40 hgt)
                                        (cons 07 stn)
                                    )
                                )
                            )
                            (apply '+ wid)
                        )
                        (length wid)
                    )
                )
            )
        )
        (setq wid (mapcar '(lambda ( x ) (+ x dif)) wid))
    )
    (setq obj
        (vla-addtable spc
            (vlax-3D-point ins)
            (1+ (length lst))
            (length (car lst))
            (* 2.0 hgt)
            (if eqc
                (apply 'max wid)
                (/ (apply '+ wid) (float (length (car lst))))
            )
        )
    )
    (vla-put-regeneratetablesuppressed obj :vlax-true)
    (vla-put-stylename obj (getvar 'ctablestyle))
    (setq i -1)
    (if (null eqc)
        (foreach col wid
            (vla-setcolumnwidth obj (setq i (1+ i)) col)
        )
    )
    (if ttl
        (progn
            (vla-settext obj 0 0 ttl)
            (setq i 1)
        )
        (progn
            (vla-deleterows obj 0 1)
            (setq i 0)
        )
    )
    (foreach row lst
        (setq j 0)
        (foreach val row
            (vla-settext obj i j val)
            (setq j (1+ j))
        )
        (setq i (1+ i))
    )
    (vla-put-regeneratetablesuppressed obj :vlax-false)
    obj
)

;; Write CSV  -  Lee Mac
;; Writes a matrix list of cell values to a CSV file.
;; lst - [lst] list of lists, sublist is row of cell values
;; csv - [str] filename of CSV file to write
;; Returns T if successful, else nil

(defun LM:writecsv ( lst csv / des sep )
    (if (setq des (open csv "w"))
        (progn
            (setq sep (cond ((vl-registry-read "HKEY_CURRENT_USER\\Control Panel\\International" "sList")) (",")))
            (foreach row lst (write-line (LM:lst->csv row sep) des))
            (close des)
            t
        )
    )
)

;; List -> CSV  -  Lee Mac
;; Concatenates a row of cell values to be written to a CSV file.
;; lst - [lst] list containing row of CSV cell values
;; sep - [str] CSV separator token

(defun LM:lst->csv ( lst sep )
    (if (cdr lst)
        (strcat (LM:csv-addquotes (car lst) sep) sep (LM:lst->csv (cdr lst) sep))
        (LM:csv-addquotes (car lst) sep)
    )
)

(defun LM:csv-addquotes ( str sep / pos )
    (cond
        (   (wcmatch str (strcat "*[`" sep "\"]*"))
            (setq pos 0)    
            (while (setq pos (vl-string-position 34 str pos))
                (setq str (vl-string-subst "\"\"" "\"" str pos)
                      pos (+ pos 2)
                )
            )
            (strcat "\"" str "\"")
        )
        (   str   )
    )
)

;; Write Text File  -  Lee Mac
;; Writes a matrix of values to a tab-delimited Text file.
;; lst - [lst] list of lists, sublist is line of text values
;; txt - [str] filename of Text file to write
;; Returns T if successful, else nil

(defun LM:writetxt ( lst txt / des )
    (if (setq des (open txt "w"))
        (progn
            (foreach itm lst (write-line (LM:lst->str itm "\t") des))
            (close des)
            t
        )
    )
)

;; List to String  -  Lee Mac
;; Concatenates each string in a supplied list, separated by a given delimiter
;; lst - [lst] List of strings to concatenate
;; del - [str] Delimiter string to separate each item

(defun LM:lst->str ( lst del )
    (if (cdr lst)
        (strcat (car lst) del (LM:lst->str (cdr lst) del))
        (car lst)
    )
)

;; Annotative-p  -  Lee Mac
;; Predicate function to determine whether a Text Style is annotative.
;; sty - [str] Name of Text Style

(defun LM:annotative-p ( sty )
    (and (setq sty (tblobjname "style" sty))
         (setq sty (cadr (assoc -3 (entget sty '("AcadAnnotative")))))
         (= 1 (cdr (assoc 1070 (reverse sty))))
    )
)

;; LW Vertices  -  Lee Mac
;; Returns a list of lists in which each sublist describes
;; the position, starting width, ending width and bulge of the
;; vertex of a supplied LWPolyline

(defun LM:lwvertices ( e )
    (if (setq e (member (assoc 10 e) e))
        (cons
            (list
                (assoc 10 e)
                (assoc 40 e)
                (assoc 41 e)
                (assoc 42 e)
            )
            (LM:lwvertices (cdr e))
        )
    )
)

;; Bulge Radius  -  Lee Mac
;; p1 - start vertex
;; p2 - end vertex
;; b  - bulge
;; Returns the radius of the arc described by the given bulge and vertices

(defun LM:bulgeradius ( p1 p2 b )
    (/ (* (distance p1 p2) (1+ (* b b))) 4 (abs b))
)

;; Bulge Centre  -  Lee Mac
;; p1 - start vertex
;; p2 - end vertex
;; b  - bulge
;; Returns the centre of the arc described by the given bulge and vertices

(defun LM:bulgecentre ( p1 p2 b )
    (polar p1
        (+ (angle p1 p2) (- (/ pi 2) (* 2 (atan b))))
        (/ (* (distance p1 p2) (1+ (* b b))) 4 b)
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

(vl-load-com)
(eval
    (append
        (list 'defun 'polyinfo:chooseoutput '( sym ))
        (if (vlax-method-applicable-p (vla-get-modelspace (LM:acdoc)) 'addtable)
            (list
               '(initget "Table TXT CSV")
               '(set sym (cond ((getkword (strcat "\nChoose Output [Table/TXT/CSV] <" (eval sym) ">: "))) ((eval sym))))
            )
            (list
               '(initget "TXT CSV")
               '(set sym (cond ((getkword (strcat "\nChoose Output [TXT/CSV] <" (eval sym) ">: "))) ((eval sym))))
            )
        )
    )
)

(princ
    (strcat
        "\n:: PolyInfo.lsp | Version 1.3 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"polyinfo\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;