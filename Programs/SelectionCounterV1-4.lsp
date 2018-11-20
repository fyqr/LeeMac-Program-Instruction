;;----------------------=={ Selection Counter  }==----------------------;;
;;                                                                      ;;
;;  A very short & simple snippet of code to automatically display the  ;;
;;  number of objects in the active selection in the status bar.        ;;
;;                                                                      ;;
;;  The selection reactor will be enabled on loading by default, but    ;;
;;  may be subsequently enabled or disabled manually using the          ;;
;;  SELCOUNTON & SELCOUNTOFF commands respectively.                     ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2014  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2014-11-13                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2014-11-15                                      ;;
;;                                                                      ;;
;;  - Program modified to fix bug when using the UNDO command.          ;;
;;----------------------------------------------------------------------;;
;;  Version 1.2    -    2016-03-25                                      ;;
;;                                                                      ;;
;;  - Program modified to store the value of the MODEMACRO system       ;;
;;    variable as a global variable on loading, and to use this stored  ;;
;;    value as a fail-safe for cases in which the pickfirstmodified     ;;
;;    event callback function cannot reset the system variable.         ;;
;;----------------------------------------------------------------------;;
;;  Version 1.3    -    2016-06-09                                      ;;
;;                                                                      ;;
;;  - Program modified to account for the use of the QSELECT and        ;;
;;    SELECTSIMILAR AutoCAD commands.                                   ;;
;;----------------------------------------------------------------------;;
;;  Version 1.4    -    2016-06-20                                      ;;
;;                                                                      ;;
;;  - Program modified to retain object selection count for selected    ;;
;;    objects following grip-editing operations.                        ;;
;;  - Corrected selection count of non-rectangular viewports.           ;;
;;----------------------------------------------------------------------;;

(defun c:selcounton nil
    (selcount:remove)
    (vlr-miscellaneous-reactor "selcount" '((:vlr-pickfirstmodified . selcount:callback)))
    (vlr-command-reactor       "selcount" '((:vlr-commandended      . selcount:comended)))
    (setq selcount:oldmodemacro (getvar 'modemacro))
    (princ "\nSelection Counter enabled.")
    (princ)
)
(defun c:selcountoff nil
    (selcount:remove)
    (setq selcount:oldmodemacro nil)
    (princ "\nSelection Counter disabled.")
    (princ)
)
(defun selcount:remove nil
    (foreach obj (apply 'append (mapcar 'cdr (vlr-reactors :vlr-miscellaneous-reactor :vlr-command-reactor)))
        (if (= "selcount" (vlr-data obj)) (vlr-remove obj))
    )
)
(defun selcount:callback ( obj arg / ent enx idx int sel vpt )
    (if
        (and (setq sel (cadr (ssgetfirst)))
            (progn
                (repeat (setq idx (sslength sel))
                    (if (and (setq ent (ssname sel (setq idx (1- idx))))
                             (setq enx (member '(102 . "{ACAD_REACTORS") (entget ent)))
                             (setq vpt (cdr (assoc 330 enx)))
                             (= "VIEWPORT" (cdr (assoc 0 (entget vpt))))
                        )
                        (ssdel ent sel)
                    )
                )
                (< 0 (setq int (sslength sel)))
            )
        )
        (progn
            (if (null selcount:modemacro)
                (setq selcount:modemacro (getvar 'modemacro))
            )
            (setvar 'modemacro (strcat (itoa int) " object" (if (= 1 int) "" "s") " selected."))
        )
        (progn
            (if (= 'str (type selcount:modemacro))
                (setvar 'modemacro selcount:modemacro)
            )
            (setq selcount:modemacro nil)
        )
    )
    (princ)
)
(defun selcount:comended ( obj arg )
    (cond
        (   (wcmatch (strcase (car arg) t) "qselect,selectsimilar")
            (selcount:callback nil nil)
        )
        (   (and (null (cadr (ssgetfirst))) (wcmatch (getvar 'modemacro) "*object*selected.") (= 'str (type selcount:oldmodemacro)))
            (setvar 'modemacro selcount:oldmodemacro)
        )
    )
    (princ)
)
(vl-load-com) (c:selcounton)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;