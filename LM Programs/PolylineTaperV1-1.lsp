;;-------------------------=={ Polyline Taper }==-----------------------;;
;;                                                                      ;;
;;  This program allows the user to apply a tapered width across all    ;;
;;  vertices of a selected polyline or all polylines in a selection.    ;;
;;                                                                      ;;
;;  Upon issuing the command syntax 'ptaper' at the AutoCAD             ;;
;;  command-line the user may specify a starting width and an ending    ;;
;;  width, with the width increasing uniformly along all vertices of    ;;
;;  the selected polyline.                                              ;;
;;                                                                      ;;
;;  The program is compatible with 2D polylines containing linear       ;;
;;  segments and/or arc segments, constructed in any UCS.               ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2016  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2016-01-01                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2018-03-11                                      ;;
;;                                                                      ;;
;;  - Restructured program to perform taper operation using a separate  ;;
;;    function, allowing an option for multiple selection to be added.  ;;
;;----------------------------------------------------------------------;;

(defun c:ptaper ( / enx idx sel tmp )   
    (if (null ptaper:swd) (setq ptaper:swd 1.0))
    (if (null ptaper:ewd) (setq ptaper:ewd 1.0))
    (initget 4)
    (if (setq tmp (getdist (strcat "\nSpecify start width <" (rtos ptaper:swd) ">: ")))
        (setq ptaper:swd tmp)
    )
    (initget 4)
    (if (setq tmp (getdist (strcat "\nSpecify end width <" (rtos ptaper:ewd) ">: ")))
        (setq ptaper:ewd tmp)
    )
    
    (while
        (not
            (progn
                (initget "Multiple eXit")
                (setvar 'errno 0)
                (setq sel (entsel "\nSelect polyline [Multiple/eXit] <eXit>: "))
                (cond
                    (   (= 7 (getvar 'errno))
                        (prompt "\nMissed, try again.")
                    )
                    (   (or (null sel) (= "eXit" sel)))
                    (   (= "Multiple" sel)
                        (if (setq sel (ssget "_:L" '((0 . "LWPOLYLINE"))))
                            (repeat (setq idx (sslength sel))
                                (ptaper:taperpolyline (ssname sel (setq idx (1- idx))) ptaper:swd ptaper:ewd)
                            )
                        )
                    )
                    (   (/= "LWPOLYLINE" (cdr (assoc 0 (setq enx (entget (car sel))))))
                        (prompt "\nThe selected object is not a polyline.")
                    )
                    (   (= 4 (logand 4 (cdr (assoc 70 (tblsearch "layer" (cdr (assoc 8 enx)))))))
                        (prompt "\nThe selected polyline is on a locked layer.")
                    )
                    (   (ptaper:taperpolyline (car sel) ptaper:swd ptaper:ewd))
                )
            )
        )
    )
    (princ)
)

(defun ptaper:taperpolyline ( ent swd ewd / blg del enx hed len lst ocs vt1 vt2 vtx )
    (setq enx (entget ent)
          hed (reverse (member (assoc 39 enx) (reverse enx)))
          ocs (assoc 210 enx)
    )
    (while
        (setq vtx (assoc 10 enx)
              enx (cdr (member vtx enx))
        )
        (setq lst (cons (list vtx (assoc 42 enx)) lst))
    )
    (setq lst
        (mapcar
           '(lambda ( vt1 vt2 / blg )
                (append vt1
                    (list
                        (if vt2
                            (if (equal 0.0 (setq blg (cdadr vt1)) 1e-8)
                                (distance (cdar vt1) (cdar vt2))
                                (*  (/ (distance (cdar vt1) (cdar vt2)) (* 2.0 (sin (* 2.0 (atan blg)))))
                                    (* (atan blg) 4.0)
                                )
                            )
                            0.0
                        )
                    )
                )
            )
            (setq lst (reverse lst))
            (append (cdr lst) (list (if (= 1 (logand 1 (cdr (assoc 70 hed)))) (car lst))))
        )
    )
    (setq del (- ewd swd)
          len (apply '+ (mapcar 'caddr lst))
    )
    (entmod
        (append (apply 'append (subst nil (list (assoc 43 hed)) (mapcar 'list hed)))
            (apply 'append
                (mapcar
                   '(lambda ( x )
                        (list
                            (car x)
                            (cadr x)
                            (cons 40 swd)
                            (cons 41 (setq swd (+ swd (* del (/ (caddr x) len)))))
                        )
                    )
                    lst
                )
            )
            (list ocs)
        )
    )
)

(princ)