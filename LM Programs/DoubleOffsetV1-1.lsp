;;--------------------=={ Double Offset }==-------------------;;
;;                                                            ;;
;;  Offsets each object in a selection to both sides by a     ;;
;;  specified distance. With additional controls for erasure  ;;
;;  of source object and offset layer.                        ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Version: 1-1 20100912                                     ;;
;;------------------------------------------------------------;;

(defun c:DOff nil (c:DoubleOffset))

(defun c:DoubleOffset ( / *error* _StartUndo _EndUndo DoubleOffset doc exitflag layer mpoint obj object of point sel symbol value )

  (defun *error* ( msg )    
    (and doc (_EndUndo doc))
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ)
  )

  (defun _StartUndo ( doc ) (vla-StartUndoMark doc))

  (defun _EndUndo   ( doc ) (if (= 8 (logand 8 (getvar 'UNDOCTL))) (vla-EndUndomark doc)))
  
  (defun DoubleOffset ( object offset layer )
    (mapcar
      (function
        (lambda ( o )
          (if
            (and
              (not
                (vl-catch-all-error-p
                  (setq o
                    (vl-catch-all-apply
                      (function vlax-invoke) (list object 'Offset o)
                    )
                  )
                )
              )
              layer
            )
            (mapcar
              (function
                (lambda ( o )
                  (vla-put-layer o (getvar 'CLAYER))
                )
              )
              o
            )
          )
        )
      )
      (list offset (- offset))
    )
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  (mapcar
    '(lambda ( symbol value ) (or (boundp symbol) (set symbol value)))
    '(*dOff:Erase *dOff:Layer) '("No" "Source")
  )

  (if
    (progn
      (while
        (progn
          (princ
            (strcat
              "\nCurrent Settings: Erase source="
              *dOff:Erase
              "  Layer="
              *dOff:Layer
              "  OFFSETGAPTYPE="
              (itoa (getvar 'OFFSETGAPTYPE))
            )
          )
          (initget 6 "Through Erase Layer")
          (setq of
            (getdist
              (strcat "\nSpecify Offset Distance [Through/Erase/Layer] <"
                (if (minusp (getvar 'OFFSETDIST)) "Through"  (rtos (getvar 'OFFSETDIST))) "> : "
              )
            )
          )
          (cond
            (
              (null of) (not (setq of (getvar 'OFFSETDIST)))
            )
            (
              (eq "Through" of) (setq of (setvar 'OFFSETDIST -1)) nil
            )
            (
              (eq "Erase" of) (initget "Yes No")

              (setq *dOff:Erase
                (cond
                  (
                    (getkword
                      (strcat "\nErase source object after offsetting? [Yes/No] <" *doff:Erase "> : ")
                    )
                  )
                  ( *dOff:Erase )
                )
              )
            )
            (
              (eq "Layer" of) (initget "Current Source")

              (setq *dOff:Layer
                (cond
                  (
                    (getkword
                      (strcat "\nEnter layer option for offset objects [Current/Source] <" *dOff:Layer "> : ")
                    )
                  )
                  ( *dOff:Layer )
                )
              )
            )
            ( of (setvar 'OFFSETDIST of) nil )
          )
        )
      )
      of
    )
    (while
      (progn
        (or ExitFlag
          (progn (initget "Exit")
            (setq sel (entsel "\nSelect object to offset or [Exit] <Exit> : "))
          )
        )
        
        (cond
          (
            (or ExitFlag (null sel) (eq sel "Exit")) nil
          )
          ( (vl-consp sel)

            (_EndUndo doc) (_StartUndo doc)

            (if (and (wcmatch (cdr (assoc 0 (entget (car sel)))) "ARC,CIRCLE,ELLIPSE,SPLINE,LWPOLYLINE,XLINE,LINE")
                     (setq obj (vlax-ename->vla-object (car sel))))

              (if (minusp of)
                (if
                  (progn (initget "Exit Multiple")
                    (and
                      (setq point (getpoint "\nSpecify through point or [Exit/Multiple] <Exit> : "))
                      (not (eq "Exit" point))
                    )
                  )
                  (if (eq "Multiple" point)
                    (while
                      (progn (initget "Exit")
                        (setq mpoint (getpoint "\nSpecify through point or [Exit] <next object> : "))

                        (cond
                          (
                            (eq "Exit" mpoint)

                            (if (eq "Yes" *dOff:Erase) (vla-delete obj))

                            (not (setq ExitFlag t))
                          )
                          (
                            (null mpoint)

                            (if (eq "Yes" *dOff:Erase) (vla-delete obj))

                            nil
                          )
                          (
                            (listp mpoint)
                           
                            (DoubleOffset obj
                              (distance (trans mpoint 1 0)
                                (vlax-curve-getClosestPointto (car sel) (trans mpoint 1 0) t)
                              )
                              (eq "Current" *dOff:Layer)
                            )
                           t
                          )
                        )
                      )
                    )
                    (progn
                      (DoubleOffset obj
                        (distance (trans point 1 0)
                          (vlax-curve-getClosestPointto (car sel) (trans point 1 0) t)
                        )
                        (eq "Current" *dOff:Layer)
                      )
                      (if (eq "Yes" *dOff:Erase) (vla-delete obj))
                     t
                    )
                  )
                  (setq ExitFlag t)
                )
                (progn
                  (DoubleOffset obj of (eq "Current" *dOff:Layer))

                  (if (eq "Yes" *dOff:Erase) (vla-delete obj))
                )
              )
              (princ "\n** Cannot Offset that Object **")
            )
           t
          )
        )
      )
    )
  )  
  (_EndUndo doc) (princ)
)   

(vl-load-com) (princ)
(princ "\n:: DoubleOffset.lsp | Version 1.1 | © Lee Mac 2011 www.lee-mac.com ::")
(princ "\n:: Type \"DoubleOffset\" or \"DOff\" to invoke ::")
(princ)

;;------------------------------------------------------------;;
;;                         End of File                        ;;
;;------------------------------------------------------------;;