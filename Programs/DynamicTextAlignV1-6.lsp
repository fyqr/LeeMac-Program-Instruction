;;------------------------=={  Dynamic Text Align  }==---------------------------;;
;;                                                                               ;;
;;  Allows the user to dynamically align text to any angle. User is prompted to  ;;
;;  make a selection of Text or MText objects to align, and pick an alignment    ;;
;;  point, or select a text object to use for alignment. The selection of text   ;;
;;  is then aligned by either x or y coordinate, or dynamically stretched        ;;
;;  depending on the mode chosen.                                                ;;
;;                                                                               ;;
;;  The mode can be switched upon pressing TAB during alignment. Text and MText  ;;
;;  entities will be aligned in accordance with their respective justifications. ;;
;;                                                                               ;;
;;  The user can also specify a fixed text spacing, by pressing 'S' during text  ;;
;;  alignment. Holding Shift whilst aligning Text will alter Text Rotation, the  ;;
;;  user can also refine Rotation by pressing 'R' during text alignment. Text    ;;
;;  Justfication can be altered by pressing 'J' during text alignment.           ;;
;;                                                                               ;;
;;                                                                               ;;
;;  Object Alignment Mode:-                                                      ;;
;;  --------------------------                                                   ;;
;;  Text can be aligned to an object by pressing 'O' during text alignment. In   ;;
;;  this mode, the text spacing along the object can be adjusted by pressing     ;;
;;  'S' and the text offset from the object can also be altered by pressing 'O'. ;;
;;                                                                               ;;
;;  Text Rotation can be aligned to the tangent vector of the object at the      ;;
;;  point of alignment by holding Shift during text placement. The user can      ;;
;;  furthermore specify a text rotation by pressing 'R'.                         ;;
;;                                                                               ;;
;;  The order of the text entities along the object can be Reversed by pressing  ;;
;;  'V' during Text placement. The original order of these entities is           ;;
;;  determined by the drawing direction of the object.                           ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  FUNCTION SYNTAX:  TXALIGN                                                    ;;
;;                                                                               ;;
;;  Notes:-                                                                      ;;
;;  ---------                                                                    ;;
;;  Shift Functionality requires the user to have Express Tools installed.       ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  Author: Lee Mac, Copyright © October 2009 - www.lee-mac.com                  ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  Version:                                                                     ;;
;;                                                                               ;;
;;  1.0:  12/10/2009  -  First Release                                           ;;
;;-------------------------------------------------------------------------------;;
;;  1.1:  14/10/2009  -  Added ability to Specify fixed text spacing             ;;
;;-------------------------------------------------------------------------------;;
;;  1.2:  15/10/2009  -  Added Stretch Mode                                      ;;
;;                    -  Upgraded User messaging                                 ;;
;;-------------------------------------------------------------------------------;;
;;  1.3:  18/10/2009  -  Added Rotation Functionality                            ;;
;;-------------------------------------------------------------------------------;;
;;  1.4:  20/10/2009  -  Added functionality to align text to object.            ;;
;;-------------------------------------------------------------------------------;;
;;  1.5:  23/10/2009  -  Added Justification Options.                            ;;
;;-------------------------------------------------------------------------------;;
;;  1.6:  28/10/2009  -  Added Option to Select Text object at Alignment Point   ;;
;;                       prompt                                                  ;;
;;-------------------------------------------------------------------------------;;

(defun c:TxAlign (/ ;; --=={ Local Functions }==--

                      *error* GetProp Text_Rotation Text_Offset Text_Stretch

                    ;; --=={ Local Variables }==--

                      ANG BAR BDIS BPT BSANG BSDIS BSPT CANG CLST CMODE CODE
                      CODEC COL CPT DATA DATAC DER DIS DOC DSPC ENT ET FOO
                      FOOC GR GRC I IPT J JLST K MLST MSG MSGC OBJLST PLST
                      PROP PT PTO RANG RLST SS TMPLST

                    ;; --=={ Global Variables }==--

                    ; *txMode   ~  Mode Setting
                    ; *txSpc    ~  Default Text Spacing
                    ; *txRot    ~  Default Text Rotation
                    ; *txOff    ~  Default Text Curve Offset
                    ; *txJus    ~  Default Text Justification

                  )
  
  (vl-load-com)

  (defun *error* (err)
    (and doc (vla-EndUndoMark doc))
    (and rLst (mapcar
                (function
                  (lambda (values)
                    (vlax-put (car values) (cadr values) (cadddr values))
                    (vlax-put (car values) 'Rotation     (caddr  values)))) rLst))
    (or (wcmatch (strcase err) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\nError: " err)))
    (redraw)
    (princ))

  (defun GetProp (object_list)
    (mapcar
      (function
        (lambda (object / prop)
          (setq prop (if (eq "AcDbText" (vla-get-ObjectName object))
                       (if (eq acAlignmentLeft (vla-get-Alignment object))
                         'InsertionPoint 'TextAlignmentPoint)
                       'InsertionPoint))
          (list object prop (vlax-get object 'Rotation) (vlax-get object prop)))) object_list))

  (defun Text_Rotation (/ oStr msgR grR codeR dataR rPt rAng)
    (setq oStr "")

    (princ (setq msgR (strcat "\nSpecify Text Rotation [Reset] <" (vl-princ-to-string *txRot) "> : ")))

    (while
      (progn
        (setq grR (grread 't 15 0) codeR (car grR) dataR (cadr grR))
        (redraw)
        
        (cond (  (and (= codeR 5) (listp dataR))
                 (setq rPt (last (car (GetProp (list (car ObjLst))))))
               
                 (if (not (zerop (getvar "ORTHOMODE")))
                   (if (< (abs (- (car dataR) (car rPt))) (abs (- (cadr dataR) (cadr rPt))))
                     (setq dataR (list (car rPt) (cadr dataR) (caddr dataR)))
                     (setq dataR (list (car dataR) (cadr rPt) (caddr dataR)))))
               
                 (setq rAng (angle rPt dataR))
                 (mapcar
                   (function
                     (lambda (object) (vla-put-rotation object rAng))) ObjLst)
               
                 (grdraw rPt dataR 40 1) t)
              
              (  (and (= codeR 2) (< 46 dataR 123))
                 (princ (chr dataR))
                 (setq oStr (strcat oStr (chr dataR))))
              
              (  (and (= codeR 2) (= dataR 8) (< 0 (strlen oStr)))
                 (princ (vl-list->string '(8 32 8)))
                 (setq oStr (substr oStr 1 (1- (strlen oStr)))))
              
              (  (and (= codeR 2) (= 15 dataR))
                 (setvar "ORTHOMODE" (- 1 (getvar "ORTHOMODE"))))
              
              (  (or (and (= codeR 2) (vl-position dataR '(32 13)))
                     (= code 25))
               
                 (cond (  (< 0 (strlen oStr))
                      
                        (cond (  (vl-position oStr '("r" "R" "reset" "Reset" "RESET"))
                                 (setq rAng nil))
                            
                              (  (setq rAng (angtof oStr 0))
                                 (setq *txRot (* 180. (/ rAng pi))) nil)
                            
                              (  (princ "\nInvalid Angle Entered.")
                                 (setq oStr "")
                                 (princ msgR))))
                     
                       (t (setq rAng (* pi (/ *txRot 180.))) nil)))
              
              (  (and (= codeR 3) (listp dataR))
                 (setq *txRot (* 180. (/ rAng pi))) nil)
              
              (t (princ "\nInvalid Input.") (princ msgR)))))
    
      (if rAng
        (mapcar (function (lambda (object) (vla-put-rotation object rAng))) ObjLst)
        (mapcar (function (lambda (values) (vla-put-rotation (car values) (caddr values)))) rLst)))

  (defun Text_Offset (/ oStr BaseDis inc grLst tmpPt msgR grR codeR dataR cPt ang ptO der tmpOff k)
    (setq oStr "")

    (princ (setq msgR (strcat "\nSpecify Text Offset [Exit] <" (vl-princ-to-string *txOff) "> : ")))

    (setq BaseDis (vlax-curve-getDistatPoint ent
                    (vlax-curve-getClosestPointto ent
                      (vlax-get (caar pLst) (cadar pLst)))))

    (setq inc (/ (- (vlax-curve-getDistatPoint ent
                      (vlax-curve-getClosestPointto ent
                        (vlax-get (car (last pLst)) (cadr (last pLst))))) BaseDis) 50.))
    (while
      (progn
        (setq grR (grread 't 15 0) codeR (car grR) dataR (cadr grR))
        (redraw)
        
        (cond (  (and (= codeR 5) (listp dataR))                 

                 (setq cPt  (vlax-curve-getClosestPointto ent dataR) k -1 ang  (angle cPt dataR))
                 (grdraw cPt dataR 40 1)

                 (setq aFac (- (angle '(0 0 0) (vlax-curve-getFirstDeriv ent
                                                 (vlax-curve-getParamatPoint ent cPt))) ang))
                 (setq grLst nil i -1)
                 (repeat 50
                   (setq grLst (cons (polar (setq tmpPt (vlax-curve-getPointatDist ent (+ BaseDis (* (setq i (1+ i)) inc))))
                                            (if (vl-position (cdr (assoc 0 (entget ent))) '("XLINE" "LINE")) ang
                                              (- (setq der (angle '(0 0 0) (vlax-curve-getFirstDeriv ent
                                                                             (vlax-curve-getParamatPoint ent tmpPt)))) aFac))
                                            (distance cPt dataR)) grLst)))
                 (grvecs (append '(-91) grLst))  

                 (foreach Obj pLst
                   (setq ptO (vlax-curve-getClosestPointto ent (vlax-get (car Obj) (cadr Obj))))
                   (vlax-put (car Obj) (cadr Obj)
                             (polar ptO (if (vl-position (cdr (assoc 0 (entget ent))) '("XLINE" "LINE")) ang
                                          (- (setq der (angle '(0 0 0) (vlax-curve-getFirstDeriv ent
                                                                           (vlax-curve-getParamatPoint ent ptO)))) aFac))
                                    (setq tmpOff (distance cPt dataR)))))
               t)
                               
              (  (and (= codeR 2) (< 46 dataR 123))
                 (princ (chr dataR))
                 (setq oStr (strcat oStr (chr dataR))))
              
              (  (and (= codeR 2) (= dataR 8) (< 0 (strlen oStr)))
                 (princ (vl-list->string '(8 32 8)))
                 (setq oStr (substr oStr 1 (1- (strlen oStr)))))
              
              (  (and (= codeR 2) (= 15 dataR))
                 (setvar "ORTHOMODE" (- 1 (getvar "ORTHOMODE"))))
              
              (  (or (and (= codeR 2) (vl-position dataR '(32 13)))
                     (= code 25))
               
                 (cond (  (< 0 (strlen oStr))
                      
                          (cond (  (vl-position oStr '("e" "E" "EXIT" "Exit" "exit"))
                                   (setq tmpOff nil))
                            
                                (  (setq tmpOff (txt2num oStr))
                                   (setq *txOff tmpOff) nil)
                            
                                (  (princ "\nInvalid Distance Entered.")
                                   (setq oStr "")
                                   (princ msgR))))
                     
                       (t (setq tmpOff nil))))
              
              (  (and (= codeR 3) (listp dataR))
                 (setq *txOff tmpOff) nil)
              
              (t (princ "\nInvalid Input.") (princ msgR))))))

  (defun Text_Stretch (/ BaseDis BasePt oStr msgR grR codeR dataR cPt ang ptO der tmpspc k grLst i inc tmpPt)
    (setq oStr "")

    (princ (setq msgR (strcat "\nSpecify Text Spacing [Exit] <" (vl-princ-to-string dSpc) "> : ")))

    (setq BaseDis (vlax-curve-getDistatPoint ent
                    (setq BasePt
                      (vlax-curve-getClosestPointto ent
                        (vlax-get (caar pLst) (cadar pLst))))))
    (while
      (progn
        (setq grR (grread 't 15 0) codeR (car grR) dataR (cadr grR))
        (redraw)
        
        (cond (  (and (= codeR 5) (listp dataR))                 

                 (setq cPt    (vlax-curve-getClosestPointto ent dataR) k 0 ang (angle cPt dataR)
                       tmpspc (/ (* ((eval fooC) 0.)
                                    (- (vlax-curve-getDistatPoint ent cPt) BaseDis))
                                 (float (1- (length pLst)))))
               
                 (grdraw cPt dataR 40 1)
                 (setq aFac (- (angle '(0 0 0) (vlax-curve-getFirstDeriv ent
                                                 (vlax-curve-getParamatPoint ent cPt))) ang))
                 (grdraw BasePt (polar BasePt (if (vl-position (cdr (assoc 0 (entget ent))) '("XLINE" "LINE")) ang
                                                (- (setq der (angle '(0 0 0) (vlax-curve-getFirstDeriv ent
                                                                               (vlax-curve-getParamatPoint ent BasePt)))) aFac))
                                       (distance cPt dataR)) 40 1)
                 (vlax-put (caar pLst) (cadar pLst)
                           (polar BasePt (if (vl-position (cdr (assoc 0 (entget ent))) '("XLINE" "LINE")) ang
                                           (- (setq der (angle '(0 0 0) (vlax-curve-getFirstDeriv ent
                                                                          (vlax-curve-getParamatPoint ent BasePt)))) aFac)) *txOff))

                 (setq grLst nil i -1 inc (/ (- (vlax-curve-getDistatPoint ent cPt) BaseDis) 50.))
                 (repeat 50
                   (setq grLst (cons (polar (setq tmpPt (vlax-curve-getPointatDist ent (+ BaseDis (* (setq i (1+ i)) inc))))
                                            (if (vl-position (cdr (assoc 0 (entget ent))) '("XLINE" "LINE")) ang
                                              (- (setq der (angle '(0 0 0) (vlax-curve-getFirstDeriv ent
                                                                             (vlax-curve-getParamatPoint ent tmpPt)))) aFac))
                                            (distance cPt dataR)) grLst)))
                 (grvecs (append '(-91) grLst))                                            

                 (foreach Obj (cdr pLst)
                   (if (setq ptO (vlax-curve-getPointatDist ent (+ bDis (* (setq k ((eval fooC) k)) tmpspc))))
                     (vlax-put (car Obj) (cadr Obj)
                               (polar ptO (if (vl-position (cdr (assoc 0 (entget ent))) '("XLINE" "LINE")) ang
                                            (- (setq der (angle '(0 0 0) (vlax-curve-getFirstDeriv ent
                                                                           (vlax-curve-getParamatPoint ent ptO)))) aFac)) *txOff))))
               t)
                               
              (  (and (= codeR 2) (< 46 dataR 123))
                 (princ (chr dataR))
                 (setq oStr (strcat oStr (chr dataR))))
              
              (  (and (= codeR 2) (= dataR 8) (< 0 (strlen oStr)))
                 (princ (vl-list->string '(8 32 8)))
                 (setq oStr (substr oStr 1 (1- (strlen oStr)))))
              
              (  (and (= codeR 2) (= 15 dataR))
                 (setvar "ORTHOMODE" (- 1 (getvar "ORTHOMODE"))))
              
              (  (or (and (= codeR 2) (vl-position dataR '(32 13)))
                     (= code 25))
               
                 (cond (  (< 0 (strlen oStr))
                      
                          (cond (  (vl-position oStr '("e" "E" "EXIT" "Exit" "exit"))
                                   (setq tmpspc nil))
                            
                                (  (setq tmpspc (txt2num oStr))
                                   (setq dSpc tmpspc) nil)
                            
                                (  (princ "\nInvalid Distance Entered.")
                                   (setq oStr "")
                                   (princ msgR))))
                     
                       (t (setq tmpspc nil))))
              
              (  (and (= codeR 3) (listp dataR))
                 (setq dSpc tmpspc) nil)
              
              (t (princ "\nInvalid Input.") (princ msgR))))))

(defun txt2num  (txt)
  (cond ((distof txt 5)) ((distof txt 2))
        ((distof txt 1)) ((distof txt 4))
        ((distof txt 3))))

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  
  (and (not acet-sys-shift-down)
       (findfile "acetutil.arx")
       (arxload (findfile "acetutil.arx") "Failed to Load Express Tools"))
  (setq et  (not (vl-catch-all-error-p
                   (vl-catch-all-apply 'acet-sys-shift-down '( )))))

  (setq mLst '("HORIZONTAL" "VERTICAL" "STRETCH")
        cLst '("CURVE MOVE" "CURVE STRETCH" "CURVE OFFSET") cMode 0)
  (or *txMode (setq *txMode   0))
  (or *txRot  (setq *txRot  0.0))
  (or *txSpc  (setq *txSpc 10.0))
  (or *txOff  (setq *txOff  0.0))
  (or *txJus  (setq *txJus    1))

  (princ "\nSelect Text to Align...")
  (if (and (setq ss (ssget "_:L" '((0 . "*TEXT"))))
           (/= 1 (sslength ss)))
    (progn

      (while
        (progn
          (initget "Text")
          (or (vl-consp pt)
              (setq pt (getpoint "\nSpecify Alignment Point or [T]ext Object: ")))
          
          (cond (  (vl-consp pt) nil)
                
                (  (eq "Text" pt)

                   (while
                     (progn
                       (initget "Point")
                       (setq ent (entsel "\nSelect Text Object or [P]oint: "))

                       (cond (  (vl-consp ent)
                              
                                (if (wcmatch (cdr (assoc 0 (entget (car ent)))) "*TEXT")
                                  (not (setq pt (last (car (GetProp (list (vlax-ename->vla-object (car ent))))))))
                                  (princ "\nObject is not Text.")))

                             (  (eq "Point" ent) nil)

                             (t (princ "\nNothing Selected."))))) t))))

      (if (vl-consp pt)
        (progn
        
          (vla-StartUndoMark doc)

          (setq i -1 col 3)
          (while (setq ent (ssname ss (setq i (1+ i))))
            (setq ObjLst (cons (vlax-ename->vla-object ent) ObjLst)))
          (setq rLst (GetProp ObjLst))

          (or (and (= 1 *txMode) (setq foo 'car bar '<))
              (setq foo 'cadr bar '>))

          (setq ObjLst (mapcar 'car
                         (vl-sort rLst
                           (function
                             (lambda (a b)
                               ((eval bar) ((eval foo) (vlax-get (car a) (cadr a)))
                                           ((eval foo) (vlax-get (car b) (cadr b)))))))))

          (eval (setq msg '(princ (strcat "\n[TAB] to Change Mode, [S]pace Text, [SHIFT] Align Rotation"
                                          "\n[R]otation, [O]bject, [J]ustification"
                                          "\nCurrent Mode: " (nth *txMode MLst)))))
          
          (while
            (progn
              (setq gr (grread 't 15 0) code (car gr) data (cadr gr))
              (redraw)
              
              (cond (  (and (= 5 code) (listp data))

                       (setq bPt (cond ((= 2 *txMode) (last (car (GetProp (list (car ObjLst)))))) (pt)))
                     
                       (if (not (zerop (getvar "ORTHOMODE")))
                         (if (< (abs (- (car data) (car bPt))) (abs (- (cadr data) (cadr bPt))))
                           (setq data (list (car bPt) (cadr data) (caddr data)))
                           (setq data (list (car data) (cadr bPt) (caddr data)))))

                       (setq *tx (cond ((zerop *txMode) 0.) ((/ pi 2.))) j -1
                             ang (angle bPt data) dis (/ (distance bPt data) (1- (float (length ObjLst)))))

                       (if (and et (acet-sys-shift-down))
                         (mapcar (function (lambda (object) (vla-put-rotation object (+ ang (/ pi 2.))))) ObjLst))

                       (foreach obj ObjLst
                         (setq prop (if (eq "AcDbText" (vla-get-ObjectName obj))
                                      (if (eq acAlignmentLeft (vla-get-Alignment obj))
                                        'InsertionPoint 'TextAlignmentPoint)
                                      'InsertionPoint))

                         (cond (  (= 2 *txMode)
                                  (grdraw bPt data col 1)
                                  (vlax-put Obj prop (polar bPt ang (* (setq j (1+ j)) dis))))

                               (t (grdraw bPt data col 1)
                                  (setq bsPt (vlax-get obj prop))
                                  (if (setq iPt (inters bPt data (polar bsPt *tx 1) bsPt nil))
                                    (vlax-put Obj prop iPt)))))
                       t)

                    (  (= 2 code)
                     
                       (cond  (  (= 13 data) nil)
                              (  (= 32 data) nil)
                              (  (= 9  data)
                                 (cond ((= (1- (length mLst)) *txMode)
                                        (setq *txMode 0))
                                       ((setq *txMode (1+ *txMode))))
                                 (eval msg))
                              
                              (  (= 15 data) (setvar "ORTHOMODE" (- 1 (getvar "ORTHOMODE"))))
                              (  (vl-position data '(99 67)) (setq col (1+ (rem col 6))))
                              (  (vl-position data '(115 83))

                                 (if (= *txMode 2) (princ "\nText Cannot be Spaced in this Mode")
                                   (progn
                                     (initget 4)
                                     (setq *txSpc
                                       (cond ((getdist (strcat "\nSpecify Text Spacing <" (vl-princ-to-string *txSpc) "> : ")))
                                             (*txSpc)))

                                     (or (and (zerop *tx) (setq foo 'cadr bar '>))
                                         (setq foo 'car bar '<))

                                     (setq tmpLst (GetProp ObjLst))

                                     (setq ObjLst (mapcar 'car
                                                    (setq tmpLst (vl-sort tmpLst
                                                                   (function
                                                                     (lambda (a b)
                                                                       ((eval bar) ((eval foo) (vlax-get (car a) (cadr a)))
                                                                                   ((eval foo) (vlax-get (car b) (cadr b))))))))) j 0)

                                     (setq bsPt  (vlax-get (caar tmpLst) (cadar tmpLst))
                                           bsAng (angle (vlax-get (caar tmpLst) (cadar tmpLst))
                                                        (vlax-get (car (last tmpLst)) (cadr (last tmpLst)))))

                                     (foreach obj (cdr ObjLst)
                                       (setq prop (if (eq "AcDbText" (vla-get-ObjectName obj))
                                                    (if (eq acAlignmentLeft (vla-get-Alignment obj))
                                                      'InsertionPoint 'TextAlignmentPoint)
                                                    'InsertionPoint))

                                       (vlax-put Obj prop (polar bsPt bsAng (* (setq j (1+ j)) *txSpc))))))
                                   
                                     (eval msg))

                              (  (vl-position data '(114 82)) (Text_Rotation) (eval msg))

                              (  (vl-position data '(74 106))

                                 (setq jLst '("TL" "TC" "TR" "ML" "MC" "MR" "BL" "BC" "BR"))
                                 (initget "TL TC TR ML MC MR BL BC BR")
                                 (setq *txJus
                                   (1+
                                     (vl-position
                                       (cond
                                         ((getkword (strcat "\nSpecify Text Justifcation [TL/TC/TR/ML/MC/MR/BL/BC/BR] <"
                                                            (nth (1- *txJus) jLst) "> : ")))
                                         ((nth (1- *txJus) jLst))) jLst)))

                                 (mapcar
                                   (function
                                     (lambda (object / tmp)
                                       (if (eq "AcDbText" (vla-get-ObjectName object))
                                         (if (eq AcAlignmentLeft (vla-get-Alignment object))
                                           (progn
                                             (setq tmp (vla-get-InsertionPoint object))
                                             (vla-put-Alignment object (+ *txJus 5))
                                             (vla-put-TextAlignmentPoint object tmp))
                                           (vla-put-Alignment object (+ *txJus 5)))
                                         (vla-put-AttachmentPoint object *txJus)))) ObjLst)

                                 (eval msg))

                              (  (vl-position data '(79 111))

                                 (while
                                   (progn
                                     (setq ent (car (entsel "\nSelect Object to Align Text <Exit> : ")))

                                     (cond (  (eq 'ENAME (type ent))
                                            
                                              (if (vl-catch-all-error-p
                                                    (vl-catch-all-apply 'vlax-curve-getEndParam (list ent)))
                                                (princ "\nInvalid Object Type Selected.")))

                                           (t (eval msg) (setq ent nil)))))

                                 (if ent
                                   (progn

                                     (setq pLst (GetProp ObjLst) k 0 fooC '1+
                                           dSpc (/ (- (vlax-curve-getDistatParam ent (vlax-curve-getEndParam ent))
                                                      (vlax-curve-getDistatParam ent (vlax-curve-getStartParam ent)))
                                                   (* 2. (length ObjLst))))
                                   
                                     (vlax-put (caar pLst) (cadar pLst)
                                               (setq bsPt (vlax-curve-getClosestPointto ent
                                                            (vlax-get (caar pLst) (cadar pLst)))))
                                     (setq bsDis (vlax-curve-getDistatPoint ent bsPt))

                                     (foreach obj (cdr pLst)
                                       (if (setq ptO (vlax-curve-getPointatDist ent (+ (* (setq k ((eval fooC) k)) dSpc) bsDis)))
                                         (vlax-put (car obj) (cadr obj) ptO)))

                                     (princ (setq msgC "\n[E]xit, Re[V]erse, Text [O]ffset, [S]pace Text, [SHIFT] Align Rotation, [R]otation"))
                                     
                                     (while
                                       (progn
                                         (setq grC (grread 't 15 0) codeC (car grC) dataC (cadr grC))
                                         (redraw)

                                         (cond (  (and (= codeC 5) (listp dataC))

                                                  (setq cPt  (vlax-curve-getClosestPointto ent dataC) k 0
                                                        ang  (angle cPt dataC)
                                                        bDis (vlax-curve-getDistatPoint ent cPt))
                                                  (grdraw cPt dataC col 1)
                                                
                                                  (vlax-put (caar pLst) (cadar pLst) (polar cPt ang *txOff))
                                                  (if (and et (acet-sys-shift-down))
                                                    (vla-put-rotation (caar pLst) (- ang (/ pi 2.))))

                                                  (setq aFac (- (angle '(0 0 0) (vlax-curve-getFirstDeriv ent
                                                                                  (vlax-curve-getParamatPoint ent cPt))) ang))
                                                
                                                  (foreach Obj (cdr pLst)
                                                    (if (setq ptO (vlax-curve-getPointatDist ent (+ bDis (* (setq k ((eval fooC) k)) dSpc))))
                                                      (vlax-put (car Obj) (cadr Obj)
                                                                (polar ptO (setq cAng (if (vl-position (cdr (assoc 0 (entget ent))) '("XLINE" "LINE")) ang
                                                                                        (- (setq der (angle '(0 0 0) (vlax-curve-getFirstDeriv ent
                                                                                                                       (vlax-curve-getParamatPoint ent ptO))))
                                                                                           aFac)))
                                                                       *txOff)))                                                  
                                                    
                                                    (if (and et (acet-sys-shift-down))
                                                      (vla-put-rotation (car Obj) (- cAng (/ pi 2.)))))
                                                t)

                                               (  (= codeC 2)

                                                  (cond (  (vl-position dataC '(114 82)) (Text_Rotation) (princ msgC))

                                                        (  (vl-position dataC '(99 67)) (setq col (1+ (rem col 6))))

                                                        (  (vl-position dataC '(118 86))
                                                           (setq fooC (cond ((eq fooC '1+) '1-) ('1+))))

                                                        (  (vl-position dataC '(79 111)) (Text_Offset)  (princ msgC))

                                                        (  (vl-position dataC '(83 115)) (Text_Stretch) (princ msgC))

                                                        (  (vl-position dataC '(13 32)) nil)

                                                        (  (vl-position dataC '(69 101)) (eval msg) nil)

                                                        (t )))

                                               (  (and (= codeC 3) (listp dataC)) nil)

                                               (  (= codeC 25) nil)

                                               (t ))))
                                     
                                     (cond ((vl-position dataC '(69 101))))) t))
               
                              (t )))

                    (  (= 25 code) nil)

                    (  (and (= 3 code) (listp data)) nil)

                    (t ))))

          (vla-EndUndoMark doc))

        (princ "\nNo Alignment Point Specified."))))

  (redraw)
  (princ))

(vl-load-com)
(princ "\n:: TxAlign.lsp | Version 1.6 | © Lee Mac 2009 www.lee-mac.com ::")
(princ "\n:: Type \"TxAlign\" to Invoke ::")
(princ)

;;-------------------------------------------------------------------------------;;
;;                                 End of File                                   ;;
;;-------------------------------------------------------------------------------;;