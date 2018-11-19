;;--------------------------=={  Dynamic Offset  }==-----------------------------;;
;;                                                                               ;;
;;  Allows the user to dynamically offset multiple objects simultaneously, by    ;;
;;  either specifying the offset distance via mouse-click, or at the             ;;
;;  command-line. The command-line default will remember the last offset entered ;;
;;  by the user.                                                                 ;;
;;                                                                               ;;
;;  Object Snap functionality is retained, and the on-screen offset distance is  ;;
;;  determined by the nearest object in the offset selection set.                ;;
;;                                                                               ;;
;;  The user can use the +/- keys to change the number of offsets, or,           ;;
;;  alternatively, the user can press 'N' and specify the number of offsets      ;;
;;  directly.                                                                    ;;
;;                                                                               ;;
;;  The user can also press 'S' during object offset to change the layer,        ;;
;;  linetype, lineweight, and colour of the resultant offset objects.            ;;
;;                                                                               ;;
;;  By pressing TAB the user can switch modes between offsetting both sides or   ;;
;;  just one side of an object.                                                  ;;
;;                                                                               ;;
;;  The toggle for retaining or deleting the original Objects can be altered by  ;;
;;  pressing 'D'. The user can hold SHIFT to highlight the offset entities.      ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  FUNCTION SYNTAX:  DynOff                                                     ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  Author: Lee Mac, Copyright © October 2009 - www.lee-mac.com                  ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  Version:                                                                     ;;
;;                                                                               ;;
;;  1.0:  17/10/2009  -  First Release                                           ;;
;;-------------------------------------------------------------------------------;;
;;  1.1:  19/10/2009  -  Added Object Snap functionality                         ;;
;;-------------------------------------------------------------------------------;;
;;  1.2:  20/10/2009  -  Added Multiple Offset Option                            ;;
;;-------------------------------------------------------------------------------;;
;;  1.3:  21/10/2009  -  Updated Object Snap Coding                              ;;
;;-------------------------------------------------------------------------------;;
;;  1.4:  23/10/2009  -  Updated Offset Number Options                           ;;
;;                    -  Added option to specify Offset factor.                  ;;
;;-------------------------------------------------------------------------------;;
;;  1.5:  29/10/2009  -  Added Settings Dialog.                                  ;;
;;-------------------------------------------------------------------------------;;
;;  1.6:  08/12/2009  -  Added option to only offset object on one side.         ;;
;;-------------------------------------------------------------------------------;;
;;  1.7:  10/12/2009  -  Added option to delete original objects.                ;;
;;-------------------------------------------------------------------------------;;
;;  1.8:  14/12/2009  -  Added ability to offset to the center of two objects.   ;;
;;-------------------------------------------------------------------------------;;
;;  1.9:  16/12/2009  -  Modified method for spacing multiple offsets.           ;;
;;                    -  Improved OSnap Coding.                                  ;;
;;-------------------------------------------------------------------------------;;
;;  2.0:  18/12/2009  -  Added colour change for offset entities.                ;;
;;                   -   Fixed Bug when using Offset Factor.                     ;;
;;-------------------------------------------------------------------------------;;
;;  2.1:  21/12/2009  -  Updated code to check for Express Tools.                ;;
;;-------------------------------------------------------------------------------;;

(defun c:DynOff (/    ;; --=={ Local Functions }==--

                       *error* clean DynOff_Sub txt2num

                      ;; --=={ Local Variables }==--

                       CODE CPT
                       DATA DCFNAME DCTITLE DOC DRFT
                       E1 E2 ELST ENTLST ET EXFLAG
                       GR
                       I IOBJ
                       K MSG
                       OBJ OBJLST OFF OOBJ OPT OSGRV OSTR
                       PROP
                       RLST
                       SS
                       X

                      ;; --=={ Global Variables }==--

                       ; *dynOff  ~  Default Offset Distance
                       ; *dynNum  ~  Default Offset Number
                       ; *dynFac  ~  Default Offset Factor
                       ; *dynMod  ~  Default Offset Mode (bit-coded)
                       ; *dynDel  ~  Default Delete Option

                       ; *DynOffDefaults*  ~ Default Dialog Settings

                 )

  (setq dcfname "LMAC_DynOff_V2.1.dcl"
        dcTitle "Dynamic Offset V2.1 Settings")
  
  (or *dynOff (setq *dynOff 10.0))
  (or *dynNum (setq *dynNum  1  ))
  (or *dynFac (setq *dynFac  1.0))
  (or *dynMod (setq *dynMod  2  ))
  (or *dynDel (setq *dynDel  nil))

  (or *DynOffDefaults*
      (setq *DynOffDefaults* '("1" "1" 256 256 "*Source*" "*Source*"   ;; Layer
                                               "ByLayer"  "ByLayer"    ;; Linetype
                                               "ByLayer"  "ByLayer"))) ;; Lineweight

  (setq k -1)
  (setq *DynOffDefaults*
    (mapcar
      (function
        (lambda (value)
          (cond (  (<= 4 (setq k (1+ k)) 5)
                   (if (tblsearch "LAYER" value) value "*Source*"))

                (  (<= 6 k 7)
                   (if (tblsearch "LTYPE" value) value "ByLayer"))

                (t value)))) *DynOffDefaults*))

  (setq dynMode (nth *dynMod '(1 2 3)))
  

  ;; --=={ Error Handler }==--

  (defun *error* (err)
    (setq eLst (clean eLst))
    (and doc (vla-EndUndoMark doc))
    (or (wcmatch (strcase err) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " err " **"))) (redraw) (princ))

  ;;-------------------------------------------------------------------------------;;
  

  (defun clean (lst / lst)
    (cond (lst (mapcar
                 (function
                   (lambda (object)
                     (if (not (vlax-erased-p object))
                       (vla-delete object)))) lst)
           (setq lst nil))))


  (defun DynOff_Sub (obj off / rLst oObj iObj i)
    
    (if (vl-catch-all-error-p
          (vl-catch-all-apply
            (function
              (lambda ( )                
                (if (= 1 (logand 1 dynmode))
                  (progn
                    (setq i  3 oObj (vlax-invoke obj 'Offset off))
                    (if (or (and et (not (acet-sys-shift-down))) (not et)) ;; -- Mod 2.0 --
                      (foreach prop '(color layer linetype lineweight)
                        (mapcar
                          (function
                            (lambda (object / val)
                              (if (or (vl-position (setq val (nth i *DynOffDefaults*)) '(256 "*Source*"))
                                      (and (= i 3) (= "1" (cadr *DynOffDefaults*))))
                                (vlax-put-property object prop (vlax-get-property obj prop))
                                (vlax-put-property object prop (if (< i 9) val (eval (read (strcat "acLnWt" val)))))))) oObj)
                        (setq i (+ i 2))))))
                
                (if (= 2 (logand 2 dynmode))
                  (progn
                    (setq i 2 iObj (vlax-invoke obj 'Offset (- off)))
                    (if (or (and et (not (acet-sys-shift-down))) (not et)) ;; -- Mod 2.0 --
                      (foreach prop '(color layer linetype lineweight)
                        (mapcar
                          (function
                            (lambda (object / val)
                              (if (or (vl-position (setq val (nth i *DynOffDefaults*)) '(256 "*Source*"))
                                      (and (= i 2) (= "1" (car *DynOffDefaults*)))) 
                                (vlax-put-property object prop (vlax-get-property obj prop))
                                (vlax-put-property object prop (if (< i 8) val (eval (read (strcat "acLnWt" val)))))))) iObj)
                        (setq i (+ i 2))))))
                
                (setq rLst (append oObj iObj))))))
      
          (clean (append oObj iObj))
      
      rLst))
  

  (defun txt2num  (txt)
    (cond ((distof txt 5)) ((distof txt 2)) ((distof txt 1)) ((distof txt 4)) ((distof txt 3))))

  (defun GetOffLen (num fac / result)
    (setq result 0.)
    (while (not (minusp (setq num (1- num))))
      (setq result (+ result (expt fac num))))
    
    result)

  (setq et (not (vl-catch-all-error-p
                  (vl-catch-all-apply (function (lambda nil (acet-sys-shift-down)))))))
  (or et (princ "\nExpress Tools not Loaded - Shift option not Available."))
  

  (setq doc   (vla-get-ActiveDocument
                (vlax-get-acad-object))
        
        drft  (vla-get-drafting
                (vla-get-preferences
                  (vlax-get-acad-object)))
        
        osGrv (osmode-grvecs-lst
                (vla-get-AutoSnapMarkerColor drft)
                  (vla-get-AutoSnapMarkerSize drft)))
  

  (cond (  (not (DC_Write dcfname))
           (princ "\nDCL File could not be Written."))

        (t
  
          (if (setq ss (ssget "_:L" '((0 . "ARC,CIRCLE,ELLIPSE,*LINE"))))
            (progn
              
              (vla-StartUndoMark doc)
              (setq ObjLst (mapcar 'vlax-ename->vla-object
                             (setq EntLst (vl-remove-if 'listp
                                            (mapcar 'cadr (ssnamex ss))))) oStr "")
              
              (setq msg '(strcat "\n[TAB] Mode, [+/-] Offset [N]umber, Offset [F]actor"
                                 "\n[C]enter, [S]ettings"
                          (if et ", [SHIFT] Show Offsets" "") ", [D]elete Original = "
                          (if *dynDel "Yes" "No")
                                 "\nSpecify Offset <" (vl-princ-to-string *dynOff) "> : "))
              (princ (eval msg))
              
              (while
                (progn
                  (setq gr (grread 't 15 0) code (car gr) data (cadr gr))
                  (redraw)
                  
                  (cond (  (and (= 5 code) (listp data))
                           
                           (setq EntLst (vl-sort EntLst
                                          (function
                                            (lambda (a b)
                                              (< (distance data (vlax-curve-getClosestPointto a data))
                                                 (distance data (vlax-curve-getClosestPointto b data)))))))
                             
                           (setq cPt (vlax-curve-getClosestPointto (car entLst) data)

                                 off (/ (distance cPt data) (GetOffLen *dynNum *dynFac)))  ;; -- Mod 2.0 --
                         
                           (grdraw cPt data 3 1)

                           (setq eLst (clean eLst))

                           (if (and (< 0 (getvar "OSMODE") 16384)
                                    (setq oPt (vl-remove-if (function null)
                                                (mapcar
                                                  (function
                                                    (lambda (x / o)
                                                      (if (setq o (osnap data x))
                                                        (list (distance data o) o x data)))) (get_osmode)))))
                             
                             (setq oPt (cdar (vl-sort oPt (function (lambda (a b) (< (car a) (car b)))))))
                             (setq oPt nil))

                           (and oPt (OsMark oPt))

                           (foreach obj ObjLst (setq i -1 x 0.)
                             (repeat *dynNum                     
                               (setq eLst
                                 (append eLst (cond ((DynOff_Sub obj
                                                       (* (setq x (+ x (expt *dynFac (setq i (1+ i))))) off))) ((clean eLst)))))))
                         
                           (if (and eLst et (acet-sys-shift-down))
                             (mapcar
                               (function
                                 (lambda (x) (vla-put-color x acBlue))) eLst)) ;; -- Mod 2.0 --

                         t)

                        (  (= code 25) nil)
                        
                        (  (and (= code 3) (listp data))

                           (clean eLst)
                         
                           (setq *dynOff off)
                           (setq cPt (vlax-curve-getClosestPointto (car entLst) data))
                         
                           (if (and (< 0 (getvar "OSMODE") 16384)
                                    (setq oPt (vl-remove-if (function null)
                                                (mapcar
                                                  (function
                                                    (lambda (x / o)
                                                      (if (setq o (osnap data x))
                                                        (list (distance data o) o x data)))) (get_osmode)))))
                             
                             (setq oPt     (cdar (vl-sort oPt
                                                   (function (lambda (a b) (< (car a) (car b))))))
                                   
                                   data    (osnap (caddr oPt) (cadr oPt))

                                   *dynOff (/ (distance (vlax-curve-getClosestPointto (car entLst) data) data)

                                              (GetOffLen *dynNum *dynFac)))   ;; -- Mod 1.9 --
                            
                             (setq oPt nil))

                           (foreach obj ObjLst (setq i -1 x 0.)
                             (repeat *dynNum
                               (setq eLst
                                 (append eLst (cond ((DynOff_Sub obj
                                                       (* (setq x (+ x (expt *dynFac (setq i (1+ i))))) *dynoff))) ((clean eLst))))))) nil)                   
                        
                        (  (= code 2)

                           (cond (  (or (= 46 data) (< 47 data 58))
                                    (setq oStr (strcat oStr (chr data)))
                                    (princ (chr data)))

                                 (  (vl-position data '(67 99)) (clean eLst)

                                    (while
                                      (progn
                                        (setq e1 (entsel "\nSelect First Object: "))

                                        (cond (  (vl-consp e1)

                                                 (if (wcmatch (cdr (assoc 0 (entget (car e1)))) "ARC,CIRCLE,ELLIPSE,*LINE")
                                                   
                                                   (while
                                                     (progn
                                                       (setq e2 (car (entsel "\nSelect Second Object: ")))

                                                       (cond (  (eq 'ENAME (type e2))

                                                                (if (wcmatch (cdr (assoc 0 (entget e2))) "ARC,CIRCLE,ELLIPSE,*LINE")
                                                                  (progn

                                                                    (setq p1 (vlax-curve-getClosestPointto (car e1) (cadr e1))
                                                                          p2 (vlax-curve-getClosestPointto e2 p1))

                                                                    (setq *dynOff (/ (distance p1 p2)

                                                                                     (GetOffLen (1+ *dynNum) *dynFac)))  ;; -- Mod 2.0 --

                                                                    (foreach obj ObjLst (setq i -1 x 0.)
                                                                      (repeat *dynNum
                                                                        (setq eLst
                                                                          (append eLst

                                                                            (cond (  (DynOff_Sub obj

                                                                                       (* (setq x (+ x (expt *dynFac (setq i (1+ i))))) *dynoff)))

                                                                                  (  (clean eLst)))))))

                                                                    (setq exFlag nil))

                                                                  (princ "\nInvalid Object Selected.")))

                                                             (  (setq exFlag t) nil))))
                                                   
                                                   (princ "\nInvalid Object Selected.")))

                                              (  (setq exFlag t) nil))))

                                    exFlag)                                                                    

                                 (  (vl-position data '(43 61))
                                    (setq *dynNum (1+ *dynNum)))

                                 (  (vl-position data '(68 100))
                                    (setq *dynDel (not *dynDel))
                                 
                                    (princ (strcat "\nOriginal Objects "
                                                   (if *dynDel "will" "will not") " be Deleted" (eval msg))))

                                 (  (vl-position data '(78 110))
                                    (initget 6)
                                    (setq *dynNum
                                           
                                      (cond ((getint (strcat "\nSpecify Number of Offsets <" (itoa *dynNum) "> : ")))
                                            
                                            (*dynNum)))
                                  
                                    (princ (eval msg)))

                                 (  (= data 6)
                                    (cond (  (< 0 (getvar "OSMODE") 16384)
                                             (setvar "OSMODE" (+ 16384 (getvar "OSMODE")))
                                             (princ "\n<Osnap off>"))
                                          
                                          (t (setvar "OSMODE" (- (getvar "OSMODE") 16384))
                                             (princ "\n<Osnap on>")))
                                  
                                    (princ (eval msg)))

                                 (  (= data 9)

                                    (setq dynmode

                                      (cond (  (= 2 *dynMod)
                                               (setq *dynMod 0) 1)

                                            (  (nth (setq *dynMod (1+ *dynMod)) '(1 2 3))))))
                                 
                                 (  (vl-position data '(70 102))
                                    (initget 6)
                                    (setq *DynFac
                                           
                                      (cond ((getreal (strcat "\nSpecify Offset Factor <" (vl-princ-to-string *dynFac) "> : ")))
                                            
                                            (*dynFac)))

                                    (princ (eval msg)))

                                 (  (vl-position data '(83 115))
                                    (Off_Settings dcfname (vlax-ename->vla-object (car EntLst)))
                                  
                                  t)                            

                                 (  (= data 45)
                                  
                                    (cond (  (= 1 *dynNum)
                                             (princ (strcat "\nMinimum Offset Number Reached." (eval msg))))
                                          
                                          (t (setq *dynNum (1- *dynNum)))))

                                 (  (and (< 0 (strlen oStr)) (= data 8))
                                  
                                    (setq oStr (substr oStr 1 (1- (strlen oStr))))
                                    (princ (vl-list->string '(8 32 8))))

                                 (  (vl-position data '(13 32))

                                    (cond (  (zerop (strlen oStr))

                                             (clean eLst)
                                           
                                             (foreach obj ObjLst (setq i -1 x 0.)
                                               (repeat *dynNum
                                                 (setq eLst
                                                   (append eLst
                                                           
                                                     (cond (  (DynOff_Sub obj
                                                                
                                                                (* (setq x (+ x (expt *dynFac (setq i (1+ i))))) *dynoff)))

                                                           (  (clean eLst))))))) nil)                                     

                                          (  (setq off (txt2num oStr))

                                             (clean eLst)
                                           
                                             (setq *dynOff off)
                                             (foreach obj ObjLst (setq i -1 x 0.)
                                               (repeat *dynNum
                                                 (setq eLst
                                                   (append eLst

                                                     (cond (  (DynOff_Sub obj

                                                                (* (setq x (+ x (expt *dynFac (setq i (1+ i))))) off)))

                                                           (  (clean eLst))))))) nil)

                                          (t (princ "\nInvalid Offset Entered.")
                                             (princ (eval msg)) (setq oStr ""))))
                                 (t )))
                        (t ))))

              (if *dynDel (mapcar (function vla-delete) ObjLst))

              (vla-EndUndoMark doc)))))
  
  (princ))

(defun osMark (o / s)
  (setq s (/ (getvar "VIEWSIZE") (cadr (getvar "SCREENSIZE")))
        o (cons (trans (car o) 1 3) (cdr o)))

  (grvecs (cdr (assoc (cadr o) osGrv))
          (list (list s 0. 0. (caar o))
                (list 0. s 0. (cadar o))
                (list 0. 0. s 0.)
                (list 0. 0. 0. 1.))))

(defun get_osmode nil ; by Evgeniy Elpanov
  (mapcar
    (function cdr)
      (vl-remove-if
        (function (lambda (x) (zerop (logand (getvar "OSMODE") (car x)))))
        '((1    . "_end")
          (2    . "_mid")
          (4    . "_cen")
          (8    . "_nod")
          (16   . "_qua")
          (32   . "_int")
          (64   . "_ins")
          (128  . "_per")
          (256  . "_tan")
          (512  . "_nea")
          (2048 . "_app")))))

(defun osmode-grvecs-lst (col ass / -ASS ASS COL)
   ; By Evgeniy Elpanov (Modified by Lee Mac)
  
  (setq -ass (- ass))
  
  (list (list "_end"
              col (list -ass -ass) (list -ass  ass)
              col (list (1-  -ass) (1- -ass)) (list (1- -ass) (1+  ass))              
              col (list -ass  ass) (list  ass  ass)
              col (list (1-  -ass) (1+  ass)) (list (1+  ass) (1+  ass))              
              col (list  ass  ass) (list  ass -ass)
              col (list (1+   ass) (1+  ass)) (list (1+  ass) (1- -ass))              
              col (list  ass -ass) (list -ass -ass)
              col (list (1+   ass) (1- -ass)) (list (1- -ass) (1- -ass)))
        
        (list "_mid"
              col (list -ass -ass) (list    0. ass)
              col (list (1-  -ass) (1- -ass)) (list 0. (1+  ass))
              col (list    0. ass) (list  ass -ass)
              col (list 0. (1+  ass)) (list (1+  ass) (1- -ass))
              col (list  ass -ass) (list -ass -ass)
              col (list (1+   ass) (1- -ass)) (list (1- -ass) (1- -ass)))
        
        (list "_cen"
              7   (list (* -ass 0.2) 0.)  (list (*  ass 0.2) 0.)
              7   (list  0. (* -ass 0.2)) (list  0.  (*  ass 0.2))
              col (list    -ass   0.)     (list (* -ass 0.86) (* ass  0.5))
              col (list (* -ass 0.86) (* ass  0.5))  (list (* -ass  0.5) (* ass 0.86))
              col (list (* -ass  0.5) (* ass 0.86))  (list 0. ass)
              col (list 0. ass) (list (* ass 0.5)    (* ass 0.86))
              col (list (* ass 0.5)   (* ass 0.86))  (list (* ass 0.86) (* ass 0.5))
              col (list (* ass 0.86)  (* ass 0.5))   (list ass 0.)
              col (list ass 0.) (list (* ass 0.86)   (* -ass 0.5))
              col (list (* ass 0.86)  (* -ass 0.5))  (list (* ass 0.5) (* -ass 0.86))
              col (list (* ass 0.5)   (* -ass 0.86)) (list 0. -ass)
              col (list 0. -ass)(list (* -ass 0.5)   (* -ass 0.86))
              col (list (* -ass 0.5)  (* -ass 0.86)) (list (* -ass 0.86) (* -ass 0.5))
              col (list (* -ass 0.86) (* -ass 0.5))  (list -ass 0.))

        (list "_nod"
              col (list -ass -ass)    (list ass ass)
              col (list -ass ass)     (list ass -ass)
              col (list -ass 0.)      (list (* -ass 0.86) (* ass 0.5))
              col (list (* -ass 0.86) (* ass 0.5))   (list (* -ass 0.5) (* ass 0.86))
              col (list (* -ass 0.5)  (* ass 0.86))  (list 0. ass)
              col (list 0. ass) (list (* ass 0.5)    (* ass 0.86))
              col (list (* ass 0.5)   (* ass 0.86))  (list (* ass 0.86) (* ass 0.5))
              col (list (* ass 0.86)  (* ass 0.5))   (list ass 0.)
              col (list ass 0.) (list (* ass 0.86)   (* -ass 0.5))
              col (list (* ass 0.86)  (* -ass 0.5))  (list (* ass 0.5) (* -ass 0.86))
              col (list (* ass 0.5)   (* -ass 0.86)) (list 0. -ass)
              col (list 0. -ass)(list (* -ass 0.5)   (* -ass 0.86))
              col (list (* -ass 0.5)  (* -ass 0.86)) (list (* -ass 0.86) (* -ass 0.5))
              col (list (* -ass 0.86) (* -ass 0.5))  (list -ass 0.))

        (list "_qua"
              col (list 0. -ass)   (list -ass 0.)
              col (list 0. (1- -ass))   (list (1- -ass) 0.)
              col (list -ass 0.)   (list 0. ass)
              col (list (1- -ass) 0.)   (list 0. (1+ ass))
              col (list 0. ass)    (list ass 0.)
              col (list 0. (1+ ass))    (list (1+ ass) 0.)
              col (list ass 0.)    (list 0. -ass)
              col (list (1+ ass) 0.)    (list 0. (1- -ass)))

        (list "_int"
              col (list -ass -ass) (list ass ass)
              col (list -ass (1+ -ass)) (list ass (1+ ass))
              col (list (1+ -ass) -ass) (list (1+ ass) ass)
              col (list -ass ass)  (list ass -ass)
              col (list -ass (1+ ass))  (list ass (1+ -ass))
              col (list (1+ -ass) ass)  (list (1+ ass) -ass))

        (list "_ins"
              col (list (* -ass 0.1) (* -ass 0.1)) (list -ass (* -ass 0.1))
              col (list -ass (* -ass 0.1)) (list -ass ass)
              col (list -ass ass) (list (* ass 0.1) ass)
              col (list (* ass 0.1) ass)   (list (* ass 0.1) (* ass 0.1))
              col (list (* ass 0.1) (* ass 0.1))   (list ass (* ass 0.1))
              col (list ass (* ass 0.1))   (list ass -ass)
              col (list ass -ass) (list (* -ass 0.1) -ass)
              col (list (* -ass 0.1) -ass) (list (* -ass 0.1) (* -ass 0.1))
              col (list (1- (* -ass 0.1)) (1- (* -ass 0.1))) (list (1- -ass) (1- (* -ass 0.1)))
              col (list (1- -ass) (1- (* -ass 0.1))) (list (1- -ass) (1+ ass))
              col (list (1- -ass) (1+ ass)) (list (1+ (* ass 0.1)) (1+ ass))
              col (list (1+ (* ass 0.1)) (1+ ass)) (list (1+ (* ass 0.1)) (1+ (* ass 0.1)))
              col (list (1+ (* ass 0.1)) (1+ (* ass 0.1))) (list (1+ ass) (1+ (* ass 0.1)))
              col (list (1+ ass) (1+ (* ass 0.1)))   (list (1+ ass) (1- -ass))
              col (list (1+ ass) (1- -ass)) (list (1- (* -ass 0.1)) (1- -ass))
              col (list (1- (* -ass 0.1))   (1- -ass)) (list (1- (* -ass 0.1)) (1- (* -ass 0.1))))

        (list "_tan"
              col (list -ass ass) (list ass ass)
              col (list (1- -ass) (1+ ass)) (list (1+ ass) (1+ ass))
              col (list -ass 0.)  (list (* -ass 0.86) (* ass 0.5))
              col (list (* -ass 0.86) (* ass 0.5)) (list (* -ass 0.5) (* ass 0.86))
              col (list (* -ass 0.5) (* ass 0.86)) (list 0. ass)
              col (list 0. ass) (list  (* ass 0.5) (* ass 0.86))
              col (list (* ass 0.5)  (* ass 0.86)) (list (* ass 0.86) (* ass 0.5))
              col (list (* ass 0.86)  (* ass 0.5)) (list ass 0.)
              col (list ass 0.) (list (* ass 0.86) (* -ass 0.5))
              col (list (* ass 0.86) (* -ass 0.5)) (list (* ass 0.5) (* -ass 0.86))
              col (list (* ass 0.5) (* -ass 0.86)) (list 0. -ass)
              col (list 0. -ass)(list (* -ass 0.5) (* -ass 0.86))
              col (list (* -ass 0.5)(* -ass 0.86)) (list (* -ass 0.86) (* -ass 0.5))
              col (list (* -ass 0.86)(* -ass 0.5)) (list -ass 0.))

        (list "_per"
              col (list -ass -ass) (list -ass ass)
              col (list (1- -ass)  (1- -ass)) (list (1- -ass) (1+ ass))
              col (list ass -ass)  (list -ass -ass)
              col (list (1+ ass)   (1- -ass)) (list (1- -ass) (1- -ass))
              col (list -ass 0.)   (list 0. 0.)
              col (list -ass -1.)  (list 0. -1.)
              col (list 0. 0.)     (list 0. -ass)
              col (list -1. 0.)    (list -1. -ass))

        (list "_nea"
              col (list -ass -ass) (list ass ass)
              col (list -ass ass)  (list ass ass)
              col (list (1- -ass)  (1+ ass)) (list (1+ ass) (1+ ass))
              col (list -ass ass)  (list ass -ass)
              col (list ass -ass)  (list -ass -ass)
              col (list (1+ ass) (1- -ass)) (list (1- -ass) (1- -ass)))

        (list "_app"
              col (list -ass -ass) (list ass ass)
              col (list ass -ass)  (list -ass ass)
              col (list -ass -ass) (list -ass ass)
              col (list (1- -ass)  (1- -ass)) (list (1- -ass) (1+ ass))
              col (list -ass ass)  (list ass ass)
              col (list (1- -ass)  (1+ ass))  (list (1+ ass) (1+ ass))
              col (list ass ass)   (list ass -ass)
              col (list (1+ ass)   (1+ ass))  (list (1+ ass) (1- -ass))
              col (list ass -ass)  (list -ass -ass)
              col (list (1+ ass)   (1- -ass)) (list (1- -ass) (1- -ass)))))


;; ----=={ DCL Section }==----

(defun Off_Settings (fname obj / mklst img layCol mk_arc lays dcTag lLay lLin bsCol bsflag)

  (mapcar 'set '(*dyniCols *dynoCols *dyniCol *dynoCol  *dynilay
                 *dynolay  *dynilin  *dynolin *dyniwgt  *dynowgt) *DynOffDefaults*)          
  
  (defun mklst (key lst)
    (start_list key) (mapcar 'add_list lst) (end_list))

  (defun img (key col)    
    (start_image key)
    (fill_image 0 0 (dimx_tile key) (dimy_tile key) col)
    (end_image))

  (setq doc  (cond (doc) ((vla-get-ActiveDocument
                            (vlax-get-acad-object))))
        lays (vla-get-layers doc))

  (defun layCol (lay)
    (vla-get-color (vla-item lays lay)))

  (setq bsCol
    (cond (  (vl-position (setq bsCol (vla-get-Color obj)) '(256 0))
             (setq bsflag t)
             (layCol (vla-get-Layer obj)))

          (bsCol)))  

  (defun mk_arc nil
    (vec_arc (if (= "1" *dyniCols)
               (if bsFlag
                 (laycol
                   (if (eq "*Source*" *dynilay)
                     (vla-get-layer obj) *dynilay)) bsCol)
               (if (= 256 *dyniCol)
                 (if (eq "*Source*" *dynilay) bsCol
                   (laycol *dynilay))
                 *dyniCol))

             bsCol

             (if (= "1" *dynoCols)
               (if bsFlag
                 (laycol
                   (if (eq "*Source*" *dynolay)
                     (vla-get-layer obj) *dynolay)) bsCol)
               (if (= 256 *dynoCol)
                 (if (eq "*Source*" *dynolay) bsCol
                   (laycol *dynolay))
                 *dynoCol))))
  
  (cond (  (<= (setq dcTag (load_dialog fname)) 0)
           (princ "\nDialog File not Found."))

        (  (not (new_dialog "dsett" dcTag))
           (princ "\nSettings Dialog could not be Loaded."))

        (t
           (vlax-for lay (vla-get-layers doc)
             (setq lLay (cons (vla-get-Name lay) lLay)))
           (setq lLay (cons "*Source*" (acad_strlsort lLay)))

           (vlax-for lin (vla-get-linetypes doc)
             (setq lLin (cons (vla-get-Name lin) lLin)))
           (setq lLin (append '("*Source*" "ByLayer") (acad_strlsort (cddr (reverse lLin)))))

           (setq lWgt '("*Source*" "ByLayer"
                        "000" "005" "009" "013" "015" "018" "020" "025" "030"
                        "035" "040" "050" "053" "060" "070" "080" "090" "100"
                        "106" "120" "140" "158" "200" "211")) 

           (mapcar 'mklst '("ilay" "olay" "ilin" "olin" "olw" "ilw")
                   (list lLay lLay lLin lLin lWgt lWgt))

           (mapcar 'set_tile '("ilay" "olay" "ilin" "olin" "ilw" "olw")
                   (mapcar 'itoa
                           (mapcar 'vl-position
                                   (list *dyniLay *dynoLay *dyniLin *dynoLin *dyniWgt *dynoWgt)
                                   (list   lLay     lLay      lLin     lLin    lWgt     lWgt))))

           (set_tile "icols" *dyniCols)
           (set_tile "ocols" *dynoCols)

           (set_tile "dtitle" dcTitle)
         
           (mode_tile "icol" (atoi (get_tile "icols")))
           (mode_tile "ocol" (atoi (get_tile "ocols")))

           (logo)
           (mk_arc)
           (mapcar 'img '("icol" "ocol") (list *dyniCol *dynoCol))

           (action_tile "ilay"
             (vl-prin1-to-string
               (quote
                 (progn
                   (setq *dynilay (nth (atoi $value) lLay)) (mk_arc)))))

           (action_tile "olay"
             (vl-prin1-to-string
               (quote
                 (progn
                   (setq *dynolay (nth (atoi $value) lLay)) (mk_arc)))))

           (action_tile "ilin"
             (vl-prin1-to-string
               (quote
                 (progn
                   (setq *dyniLin (nth (atoi $value) lLin))))))

           (action_tile "olin"
             (vl-prin1-to-string
               (quote
                 (progn
                   (setq *dynoLin (nth (atoi $value) lLin))))))

           (action_tile "ilw"
             (vl-prin1-to-string
               (quote
                 (progn
                   (setq *dyniWgt (nth (atoi $value) lWgt))))))

           (action_tile "olw"
             (vl-prin1-to-string
               (quote
                 (progn
                   (setq *dynoWgt (nth (atoi $value) lWgt))))))

           (action_tile "icol"
             (vl-prin1-to-string
               (quote
                 (progn
                   (setq *dyniCol (cond ((acad_colordlg *dyniCol)) (*dyniCol)))
                   (img "icol" *dyniCol)
                   (mk_arc)))))
         
           (action_tile "ocol"
             (vl-prin1-to-string
               (quote
                 (progn
                   (setq *dynoCol (cond ((acad_colordlg *dynoCol)) (*dynoCol)))
                   (img "ocol" *dynoCol)
                   (mk_arc)))))

           (action_tile "icols"
             (vl-prin1-to-string
               (quote
                 (progn
                   (mode_tile "icol" (atoi (setq *dyniCols $value)))
                   (mk_arc)))))

           (action_tile "ocols"
             (vl-prin1-to-string
               (quote
                 (progn
                   (mode_tile "ocol" (atoi (setq *dynoCols $value)))
                   (mk_arc)))))
                   
           (action_tile "accept"
             (vl-prin1-to-string
               (quote
                 (progn
                   (setq *DynOffDefaults*
                          (list *dyniCols *dynoCols *dyniCol  *dynoCol  *dynilay
                                *dynolay  *dynilin  *dynolin  *dyniwgt  *dynowgt))
                   (done_dialog)))))
                   
           (action_tile "cancel" "(done_dialog)")

           (start_dialog)
           (unload_dialog dcTag))))


(defun DC_Write (fname / wPath ofile)

  (if (not (findfile fname))
    (if (setq wPath (findfile "ACAD.PAT"))
      (progn
        (setq wPath (vl-filename-directory wPath))
        (or (eq "\\" (substr wPath (strlen wPath)))
            (setq wPath (strcat wPath "\\")))
        
        (setq ofile (open (strcat wPath fname) "w"))
        
        (foreach str
                 
          '(
            "//-------------------=={ Dynamic Offset }==-------------------//"
            "//                                                            //"
            "//  DynOff.dcl for use in conjunction with DynOff.lsp         //"
            "//------------------------------------------------------------//"
            "//  Author: Lee Mac, Copyright © 2009 - www.lee-mac.com       //"
            "//------------------------------------------------------------//"
            ""
            "// Sub-Assembly Definitions"
            ""
            "pop : popup_list   { fixed_width = false; alignment = centered; }"
            "col : image_button { alignment = centered; height = 1.5; width = 4.0;"
            "                     fixed_width = true; fixed_height = true; color = 2; }"
            ""
            "// Main Dialog"
            ""
            "dsett : dialog { key = \"dtitle\";"
            "  spacer;"
            "  : row {"
            "    : boxed_column { label = \"Offset Preview\"; fixed_width = false; "
            "      : boxed_row { label = \"Outer Colour\"; fixed_width = true;"
            "                    alignment = centered;"
            "        spacer;"
            "        : col  { key = \"ocol\"; }"
            "        spacer;"
            "        : toggle { key = \"ocols\"; label = \"Source\"; }"
            "        spacer;"
            "      } // boxed_row"
            "      : image { key = \"dimage\"; alignment = centered;"
            "                width = 24.64 ; fixed_width  = true;"
            "                height = 11.39; fixed_height = true; color = -2; }"
            "      : boxed_row { label = \"Inner Colour\"; fixed_width = true;"
            "                    alignment = centered;"
            "        spacer;"
            "        : col  { key = \"icol\"; }"
            "        spacer;"
            "        : toggle { key = \"icols\"; label = \"Source\"; }"
            "        spacer;"
            "      } // boxed_row"
            "    } // column"
            "    : column { "
            "      : boxed_column { label = \"Outer Offset\";"
            "        : pop { label = \"Layer:\"; key = \"olay\"; }"
            "        : pop { label = \"Linetype:\"; key = \"olin\"; }"
            "        : pop { label = \"Lineweight:\"; key = \"olw\" ; }"
            "        spacer;"
            "      } // boxed_column"
            "      spacer;"
            "      : boxed_column { label = \"Inner Offset\";"
            "        : pop { label = \"Layer:\"; key = \"ilay\"; }"
            "        : pop { label = \"Linetype:\"; key = \"ilin\"; }"
            "        : pop { label = \"Lineweight:\"; key = \"ilw\" ; }"
            "        spacer;"
            "      } // boxed_column"
            "    } // column"
            "  } // row"
            "  spacer;"
            "  : row {"
            "    : spacer { width = 19.33; height = 3.18; }"
            "    ok_cancel;"
            "    spacer;"
            "    : image { key = \"logo\" ; alignment = centered;"
            "              width = 19.33; fixed_width  = true;"
            "              height = 3.18; fixed_height = true; color = -15; }"
            "  } // row"
            "} // dialog"
            ""
            "//------------------------------------------------------------//"
            "//                         End of File                        //"
            "//------------------------------------------------------------//"
            )

          (write-line str ofile))
        (close ofile)
      t)
  nil)
t))

(defun logo nil

  (start_image "logo")

  (mapcar 'vector_image
          
          '(24 21 19 18 17 16 15 14 1 1 0 0 17 8 0 0 1 1 1 1 8 8 7 7 7 7 7 7 33 33 35
            37 38 39 41 48 47 46 46 54 52 51 50 49 41 42 43 44 45 46 46 47 47 48 48 48
            48 48 49 49 49 49 48 48 48 47 47 44 46 47 48 49 49 50 50 51 52 52 53 53 53
            53 53 53 53 52 52 52 52 51 51 51 51 52 54 54 55 56 58 59 60 62 70 70 68 67
            65 64 62 61 59 58 57 55 64 63 63 63 63 62 61 71 70 69 69 69 69 69 68 69 69
            69 47 26 27 28 28 28 28 28 27 27 26 25 36 34 33 33 33 46 46 47 77 77 77 76
            75 74 73 73 73 72 72 72 72 72 72 72 72 73 73 74 75 79 76 76 76 75 75 75 75
            75 76 76 76 77 77 78 78 79 80 81 83 84 85 86 93 76 77 87 87 81 80 88 88 88
            88 86 86 94 93 92 92 92 92 92 92 93 113 113 112 111 109 108 107 105 109 111
            112 113 113 113 96 94 93 93 93 93 93 94 95 96 97 98 99 101 102 104 105 107
            108 104 103 101 100 100 99 99 98 98 98 98 98 99 100 102 103 104 101 99 98
            96 94 105 107 108 109 110 112 113 114 114 114 112 111 110 108 107 105 103 102)

          '(16 18 19 20 21 21 21 21 22 23 23 24 24 0 0 0 1 2 3 21 21 21 20 19 18 1 1 0 1
            1 2 2 3 4 5 5 4 3 3 1 1 2 3 4 5 6 7 9 10 12 12 13 15 16 18 19 21 23 25 25 27
            28 30 31 33 34 35 40 38 37 36 34 34 33 31 30 28 28 26 25 23 21 21 19 18 16 15
            14 13 11 10 10 9 8 7 6 5 4 3 3 2 2 0 0 0 0 0 0 0 0 0 0 0 0 1 20 21 22 22 23
            24 24 23 23 22 21 20 4 3 2 2 0 0 0 1 2 2 4 4 20 21 23 23 24 24 23 22 21 20 1
            1 0 4 5 6 7 8 9 11 12 13 15 16 18 20 20 22 23 25 26 27 29 30 34 30 29 27 26
            24 22 21 19 18 16 15 13 12 12 10 9 8 7 6 5 4 4 3 3 3 4 10 10 12 12 19 19 20
            21 22 22 21 21 20 19 6 6 5 3 13 14 15 16 17 18 19 19 20 19 18 17 15 14 5 9 11
            12 14 14 16 17 18 19 20 21 21 22 22 22 22 21 21 20 19 19 18 18 17 16 14 12 11
            10 8 7 6 6 5 5 3 3 3 3 3 5 5 6 6 6 7 7 7 7 6 5 4 4 3 3 3 3 3)

          '(21 19 18 17 16 15 14 8 1 0 0 17 24 0 0 1 1 1 1 1 8 7 7 7 7 7 7 8 33 35 37 38
            39 41 41 47 46 46 46 52 51 50 49 48 42 43 44 45 46 46 47 47 48 48 48 48 48 49
            49 49 49 48 48 48 47 47 44 46 47 48 49 49 50 50 51 52 52 53 53 53 53 53 53 53
            52 52 52 52 51 51 51 51 52 54 54 55 56 58 59 60 62 64 70 68 67 65 64 62 61 59
            58 57 55 54 63 63 63 63 62 61 71 70 69 69 69 69 69 68 69 69 69 70 26 27 28 28
            28 28 28 27 27 26 25 36 34 33 33 33 33 46 47 47 77 77 76 75 74 73 73 73 72 72
            72 72 72 72 72 72 73 73 74 75 79 76 76 76 75 75 75 75 75 76 76 76 77 77 78 78
            79 80 81 83 84 85 86 87 76 77 77 87 81 80 88 88 88 88 86 86 94 93 92 92 92 92
            92 92 93 93 113 112 111 109 108 107 105 104 111 112 113 113 113 113 94 93 93
            93 93 93 94 95 96 97 98 99 101 102 104 105 107 108 109 103 101 100 100 99 99
            98 98 98 98 98 99 100 102 103 104 105 99 98 96 94 96 107 108 109 110 112 113
            114 114 114 112 111 110 108 107 105 103 102 101)

          '(18 19 20 21 21 21 21 21 23 23 24 24 16 0 0 1 2 3 21 22 21 20 19 18 1 1 0 0 1 2
            2 3 4 5 5 4 3 3 1 1 2 3 4 5 6 7 9 10 12 12 13 15 16 18 19 21 23 25 25 27 28 30
            31 33 34 35 40 38 37 36 34 34 33 31 30 28 28 26 25 23 21 21 19 18 16 15 14 13
            11 10 10 9 8 7 6 5 4 3 3 2 2 1 0 0 0 0 0 0 0 0 0 0 0 1 20 21 22 22 23 24 24 23
            23 22 21 20 4 3 2 2 0 0 0 1 2 2 4 4 20 21 23 23 24 24 23 22 21 20 1 1 0 0 5 6 7
            8 9 11 12 13 15 16 18 20 20 22 23 25 26 27 29 30 34 30 29 27 26 24 22 21 19 18
            16 15 13 12 12 10 9 8 7 6 5 4 4 4 3 3 4 10 10 12 12 19 19 20 21 22 22 21 21 20
            19 6 6 5 3 3 14 15 16 17 18 19 19 20 19 18 17 15 14 13 9 11 12 14 14 16 17 18 19
            20 21 21 22 22 22 22 21 21 20 19 19 18 18 17 16 14 12 11 10 8 7 6 6 5 5 5 3 3 3
            3 5 5 6 6 6 7 7 7 7 6 5 4 4 3 3 3 3 3 3)

          '(14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14
            14 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166
            166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166
            166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166
            166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166
            166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166
            166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166
            166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166
            166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166
            166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166
            166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166
            166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166
            166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166
            166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166
            166 166 166 166))

  (end_image))

(defun vec_arc (iCol bCol oCol)

  (start_image "dimage")

  (mapcar 'vector_image

          '(0 48 48 48 48 47 46 45 44 43 42 40 39 37 35 33 31 29 26 24 21 19 16 14 11 8 5 2)
          '(0 146 143 141 138 135 132 130 127 124 122 119 117 115 113 111 109 107 105 104 103 101 100 99 99 98 98 97)
          '(0 48 48 48 47 46 45 44 43 42 40 39 37 35 33 31 29 26 24 21 19 16 14 11 8 5 2 0)
          '(0 143 141 138 135 132 130 127 124 122 119 117 115 113 111 109 107 105 104 103 101 100 99 99 98 98 97 97)

          (list iCol iCol iCol iCol iCol iCol iCol iCol iCol iCol iCol iCol iCol iCol iCol
                iCol iCol iCol iCol iCol iCol iCol iCol iCol iCol iCol iCol iCol))

  (mapcar 'vector_image

          '(0 42 39 35 32 28 25 21 18 14 10 7 3 42 39 35 32 28 25 21 18 14 10 7 3 71 69 66 63
            60 58 55 52 48 45 71 69 66 63 60 58 55 52 48 45 93 92 90 89 88 86 84 82 80 78 76
            74 93 92 90 89 88 86 84 82 80 78 76 74 97 97 97 97 96 96 95 94 97 97 97 97 96 96 95 94)

          '(0 58 57 55 54 53 52 51 50 50 49 49 49 58 57 55 54 53 52 51 50 50 49 49 49 80 77 75
            72 70 68 65 63 62 60 80 77 75 72 70 68 65 63 62 60 117 114 111 107 104 101 97 94 91
            88 85 82 117 114 111 107 104 101 97 94 91 88 85 82 146 143 139 135 132 128 125 121
            146 143 139 135 132 128 125 121)

          '(0 39 35 32 28 25 21 18 14 10 7 3 0 39 35 32 28 25 21 18 14 10 7 3 0 69 66 63 60 58
            55 52 48 45 42 69 66 63 60 58 55 52 48 45 42 92 90 89 88 86 84 82 80 78 76 74 71 92
            90 89 88 86 84 82 80 78 76 74 71 97 97 97 96 96 95 94 93 97 97 97 96 96 95 94 93)

          '(0 57 55 54 53 52 51 50 50 49 49 49 48 57 55 54 53 52 51 50 50 49 49 49 48 77 75 72
            70 68 65 63 62 60 58 77 75 72 70 68 65 63 62 60 58 114 111 107 104 101 97 94 91 88
            85 82 80 114 111 107 104 101 97 94 91 88 85 82 80 143 139 135 132 128 125 121 117 143
            139 135 132 128 125 121 117)

          (list bCol bCol bCol bCol bCol bCol bCol bCol bCol bCol bCol bCol bCol bCol bCol bCol
                bCol bCol bCol bCol bCol bCol bCol bCol bCol bCol bCol bCol bCol bCol bCol bCol
                bCol bCol bCol bCol bCol bCol bCol bCol bCol bCol bCol bCol bCol bCol bCol bCol
                bCol bCol bCol bCol bCol bCol bCol bCol bCol bCol bCol bCol bCol bCol bCol bCol
                bCol bCol bCol bCol bCol bCol bCol bCol bCol bCol bCol bCol bCol bCol bCol bCol
                bCol bCol bCol bCol bCol))

  (mapcar 'vector_image

          '(56 51 47 43 39 35 30 26 22 17 13 8 4 0 106 103 100 97 93 90 86 83 79 75 72 68 64 60
            146 146 146 146 145 144 144 143 142 141 139 138 137 135 133 131 129 127 125 123 120
            118 115 112 109)

          '(11 9 8 6 5 4 3 2 1 1 0 0 0 0 46 42 39 36 34 31 28 25 23 21 18 16 14 12 146 142 137 133
            129 124 120 115 111 107 103 98 94 90 86 82 78 74 70 67 63 59 56 52 49)

          '(51 47 43 39 35 30 26 22 17 13 8 4 0 0 103 100 97 93 90 86 83 79 75 72 68 64 60 56 146
            146 146 145 144 144 143 142 141 139 138 137 135 133 131 129 127 125 123 120 118 115 112 109 106)

          '(9 8 6 5 4 3 2 1 1 0 0 0 0 0 42 39 36 34 31 28 25 23 21 18 16 14 12 11 142 137 133 129
            124 120 115 111 107 103 98 94 90 86 82 78 74 70 67 63 59 56 52 49 46)

          (list oCol oCol oCol oCol oCol oCol oCol oCol oCol oCol oCol oCol oCol oCol oCol oCol
                oCol oCol oCol oCol oCol oCol oCol oCol oCol oCol oCol oCol oCol oCol oCol oCol
                oCol oCol oCol oCol oCol oCol oCol oCol oCol oCol oCol oCol oCol oCol oCol oCol
                oCol oCol oCol oCol oCol))

  (end_image))


(vl-load-com)
(princ "\n:: DynOff.lsp | Version 2.1 | © Lee Mac 2009 www.lee-mac.com ::")
(princ "\n:: Type \"DynOff\" to Invoke ::")
(princ)

;;-------------------------------------------------------------------------------;;
;;                                 End of File                                   ;;
;;-------------------------------------------------------------------------------;;