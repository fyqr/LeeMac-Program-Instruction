;;------------------------=={ Label }==-----------------------;;
;;                                                            ;;
;;  Allows the user to create MText labels dynamically        ;;
;;  aligned to a selected object, with additional placement   ;;
;;  controls available.                                       ;;
;;                                                            ;;
;;  Works with primary & nested objects (uniformly scaled),   ;;
;;  and in all UCS/Views.                                     ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2012 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Version 1.1    -    15-01-2012                            ;;
;;  Fixed intermittent divide by zero error.                  ;;
;;------------------------------------------------------------;;

(defun c:label
     
    (
        /
        *error*
        _StartUndo
        _EndUndo
        _CurveObject-p
        _FixDXFData
        _CopyNested
        _SelectIf
        _MakeReadable

        acdoc acspc d di ent factor g1 g2 g3 gr mat msg obj p1 p2 sel str ucsnm ucsxa
    )

    (setq factor (/ (getvar 'textsize) (cond ((getvar 'cannoscalevalue)) (1.0)))
          *off*  (cond (*off*) (1.0))
          *per*  (cond (*per*) ((/ pi 2.0)))
          *bak*  (cond (*bak*) (:vlax-false))
    )

    (defun *error* ( msg )
        (if (and mat ent) (entdel ent))
        (if (and obj (not (vlax-erased-p obj))) (vla-delete obj))
        (if acdoc (_EndUndo acdoc))
        (if (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (defun _StartUndo ( doc )
        (_EndUndo doc)
        (vla-StartUndoMark doc)
    )

    (defun _EndUndo ( doc )
        (while (= 8 (logand 8 (getvar 'UNDOCTL)))
            (vla-EndUndoMark doc)
        )
    )

    (defun _CurveObject-p ( ent )
        (null
            (vl-catch-all-error-p
                (vl-catch-all-apply 'vlax-curve-getendparam (list ent))
            )
        )
    )

    (defun _FixDXFData ( elst )
        (vl-remove-if '(lambda ( pair ) (member (car pair) '(5 6 8 102 330))) elst)
    )

    (defun _CopyNested ( ent mat / elst )
        (setq elst (entget ent))
        (cond
            (   (setq ent
                    (cond
                        (   (eq "VERTEX" (cdr (assoc 0 elst)))
                            (entmakex (_FixDXFData (entget (setq ent (cdr (assoc 330 elst))))))
                            (while (not (eq "SEQEND" (cdr (assoc 0 (setq elst (entget (setq ent (entnext ent))))))))
                                (entmakex (_FixDXFData elst))
                            )
                            (cdr (assoc 330 (entget (entmakex (_FixDXFData elst)))))
                        )
                        (   (entmakex (_FixDXFData elst))   )
                    )
                )
                (if mat (vla-transformby (vlax-ename->vla-object ent) (vlax-tmatrix mat)))
                ent
            )
        )
    )
    
    (defun _SelectIf ( msg pred )
        (
            (lambda ( f / sel )
                (while
                    (progn (setvar 'ERRNO 0) (setq sel (nentselp msg))
                        (cond
                            (   (= 7 (getvar 'ERRNO))
                                (princ "\nMissed, try again.")
                            )
                            (   (eq 'ENAME (type (car sel)))
                                (if (and f (null (f (car sel))))
                                    (princ "\nInvalid Object.")
                                )
                            )
                        )
                    )
                )
                sel
            )
            (eval pred)
        )
    )

    (defun _MakeReadable ( a )
        (
            (lambda ( a )
                (if (and (< (/ pi 2.0) a) (<= a (/ (* 3.0 pi) 2.0)))
                    (+ a pi)
                    a
                )
            )
            (rem (+ a pi pi) (+ pi pi))
        )
    )

    (setq acdoc (vla-get-activedocument (vlax-get-acad-object))
          acspc (vlax-get-property acdoc (if (= 1 (getvar 'cvport)) 'paperspace 'modelspace))
          ucsnm (trans '(0. 0. 1.) 1 0 t)
          ucsxa (angle '(0. 0. 0.) (trans (getvar 'UCSXDIR) 0 ucsnm))
    )
    (_StartUndo acdoc)
    (cond
        (   (= 4 (logand 4 (cdr (assoc 70 (tblsearch "LAYER" (getvar 'clayer))))))
            (princ "\nCurrent Layer Locked.")
        )
        (   (null
                (and
                    (setq *str*
                        (cond
                            (   (eq ""
                                    (setq str
                                        (getstring t
                                            (strcat "\nSpecify Label"
                                                (if *str* (strcat " <" *str* ">: ") ": ")
                                            )
                                        )
                                    )
                                )
                                *str*
                            )
                            (   str   )
                        )
                    )
                    (setq sel
                        (_SelectIf "\nSelect Object to Label: "
                            (function
                                (lambda ( x )
                                    (or
                                        (eq "VERTEX" (cdr (assoc 0 (entget x))))
                                        (_CurveObject-p x)
                                    )
                                )
                            )
                        )
                    )
                )
            )
            (princ "\n*Cancel*")
        )
        (   (null
                (or
                    (and
                        (setq mat (caddr sel))
                        (setq ent (_CopyNested (car sel) mat))
                    )
                    (and
                        (eq "VERTEX" (cdr (assoc 0 (entget (car sel)))))
                        (setq ent (cdr (assoc 330 (entget (car sel)))))
                    )
                    (setq ent (car sel))
                )
            )
            (princ "\nUnable to Recreate Nested Entity.")
        )
        (   t
            (setq obj
                (vla-addmtext acspc
                    (vlax-3D-point
                        (vlax-curve-getclosestpointto ent (trans (cadr sel) 1 0))
                    )
                    0.0 *str*
                )
            )
            (vla-put-attachmentpoint obj acattachmentpointmiddlecenter)
            (vla-put-backgroundfill  obj *bak*)
            (setq msg (princ "\nAlign Label: [+/-] for [O]ffset, [P]erpendicular, [B]ackground Mask"))

            (while
                (progn
                    (setq gr (grread 't 15 0)
                          g1 (car  gr)
                          g2 (cadr gr)
                    )
                    (cond
                        (   (member g1 '(5 3))
                            (setq p2 (trans g2 1 0)
                                  p1 (vlax-curve-getclosestpointto ent p2)
                            )
                            (if (not (equal p1 p2 1e-10))
                                (progn
                                    (setq di (/ (* factor *off*) (distance p1 p2)))
                                    (vla-put-insertionpoint obj (vlax-3D-point (mapcar '(lambda ( a b ) (+ a (* (- b a) di))) p1 p2)))
                                    (vla-put-rotation obj (_MakeReadable (+ (angle (trans p1 0 1) g2) *per*)))
                                )
                            )
                            (= 5 g1)
                        )
                        (   (= 2 g1)
                            (cond
                                (   (member g2 '(80 112)) ; P/p
                                    (setq *per* (- (/ pi 2.) *per*))
                                )
                                (   (member g2 '(45  95)) ; -/_
                                    (setq *off* (- *off* 0.1))
                                )
                                (   (member g2 '(43  61)) ; +/=
                                    (setq *off* (+ *off* 0.1))
                                )
                                (   (member g2 '(66  98)) ; B/b
                                    (vlax-put obj 'backgroundfill (setq *bak* (~ (vlax-get obj 'backgroundfill))))
                                    (if (zerop *bak*)
                                        (princ "\n<Background Mask Off>")
                                        (princ "\n<Background Mask On>")
                                    )
                                    (princ msg)
                                )
                                (   (member g2 '(79 111)) ; O/o
                                    (setq *off*
                                        (cond
                                            (   (setq d (getdist (strcat "\nSpecify Label Offset <" (rtos (* *off* factor)) "> : ")))
                                                (/ d factor)
                                            )
                                            (   *off*   )
                                        )
                                    )
                                    (princ msg)
                                )
                                (   (member g2 '(13 32))
                                    nil
                                )
                                (   t   )
                            )
                        )
                    )
                )
            )            
            (if mat (entdel ent))
        )
    )
    (_EndUndo acdoc)
    (princ)
)
(vl-load-com)
(princ
    (strcat
        "\n:: Label.lsp | Version 1.1 | © Lee Mac "
        (menucmd "m=$(edtime,$(getvar,DATE),YYYY)")
        " www.lee-mac.com ::"
        "\n:: Type \"Label\" to Invoke ::"
    )
)
(princ)