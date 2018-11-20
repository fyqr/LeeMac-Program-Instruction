;;-------------------=={ Centered Measure }==-----------------;;
;;                                                            ;;
;;  Emulates the behaviour of the standard 'Measure' command  ;;
;;  however centering the divisions along the selected object ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;

(defun c:cmeasure ( / *error* _StartUndo _EndUndo _SelectIf _IsCurveObject acdoc al bl d0 di en mx nm pt )

  (defun *error* ( msg )
    (if acdoc (_EndUndo acdoc))
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ)
  )

  (defun _StartUndo ( doc ) (_EndUndo doc)
    (vla-StartUndoMark doc)
  )

  (defun _EndUndo ( doc )
    (if (= 8 (logand 8 (getvar 'UNDOCTL)))
      (vla-EndUndoMark doc)
    )
  )

  (defun _SelectIf ( msg pred func / sel ) (setq pred (eval pred))  
    (while
      (progn (setvar 'ERRNO 0) (setq sel (car (func msg)))
        (cond
          ( (= 7 (getvar 'ERRNO))
            (princ "\nMissed, Try again.")
          )
          ( (eq 'ENAME (type sel))
            (if (and pred (not (pred sel)))
              (princ "\nInvalid Object Selected.")
            )
          )
        )
      )
    )
    sel
  )

  (defun _IsCurveObject ( entity / param )
    (and
      (not
        (vl-catch-all-error-p
          (setq param
            (vl-catch-all-apply 'vlax-curve-getendparam (list entity))
          )
        )
      )
      param
    )
  )

  (setq acdoc (vla-get-activedocument (vlax-get-acad-object))
        nm    (trans '(0. 0. 1.) 1 0 t)
  )
  (if (setq en (_SelectIf "\nSelect Object to Measure: " '_isCurveObject entsel))
    (progn
      (initget 7 "Block")
      (setq di (getdist "\nSpecify length of segment or [Block]: "))
      
      (if (eq "Block" di)
        (progn
          (while
            (progn (setq bl (getstring t "\nEnter name of block to insert: "))
              (cond
                ( (not (snvalid bl))
                  (princ "\nInvalid block name.")
                )
                ( (not (tblsearch "BLOCK" bl))
                  (princ (strcat "\nCannot find block \"" bl "\"."))
                )
              )
            )
          )
          (initget "Yes No")
          (setq al (not (eq "No" (getkword "\nAlign block with object? [Yes/No] <Y>: "))))
          (initget 7)
          (setq di (getdist "\nSpecify length of segment: "))
        )
      )
      (setq mx (vlax-curve-getdistatparam en (vlax-curve-getendparam en))
            d0 (- (/ (- mx (* di (fix (/ mx di)))) 2.) di)
      )
      (_StartUndo acdoc)
      (while (and (<= (setq d0 (+ d0 di)) mx) (setq pt (vlax-curve-getpointatdist en d0)))
        (if bl
          (entmakex
            (list
              (cons 0 "INSERT")
              (cons 2 bl)
              (cons 10 (trans pt 0 nm))
              (cons 50
                (if al
                  (angle '(0. 0. 0.)
                    (trans
                      (vlax-curve-getfirstderiv en (vlax-curve-getparamatpoint en pt)) 0 nm
                    )
                  )
                  0.
                )
              )
              (cons 210 nm)
            )
          )
          (entmakex (list (cons 0 "POINT") (cons 10 pt)))
        )
      )
      (_EndUndo acdoc)
    )
    (princ "\n*Cancel*")
  )
  (princ)
)
(vl-load-com) (princ)

;;------------------------------------------------------------;;
;;                         End of File                        ;;
;;------------------------------------------------------------;;