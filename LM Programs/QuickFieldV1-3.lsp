;;======================================================================;;
;;  Quick Field Program  -  Lee Mac                                     ;;
;;======================================================================;;
;;                                                                      ;;
;;  Program Description                                                 ;;
;;  -------------------------------                                     ;;
;;                                                                      ;;
;;  This function is designed to enable the user to quickly create      ;;
;;  custom  programs for inserting multiple fields into a drawing.      ;;
;;                                                                      ;;
;;  This could be for use in situations in which the user is required   ;;
;;  to create many fields in a drawing, with each field referencing     ;;
;;  the same object property, and continued use of the Field Command    ;;
;;  Dialog can become tedious.                                          ;;
;;                                                                      ;;
;;  Custom programs can be created by calling the 'LM:quickfield'       ;;
;;  function with a string describing the object property to be         ;;
;;  referenced by the field; a string describing the field formatting;  ;;
;;  and an integer to determine how the field will be created.          ;;
;;                                                                      ;;
;;  When creating the custom field programs, the user is advised to     ;;
;;  first use the Field Command dialog with all settings set to create  ;;
;;  the desired field, then make note of the Field Expression           ;;
;;  displayed at the bottom of the dialog. The required QuickField      ;;
;;  parameters may then be read directly from this Field Expression.    ;;
;;                                                                      ;;
;;  Each parameter is described in more detail below.                   ;;
;;                                                                      ;;
;;======================================================================;;
;;  Notes on QuickField Parameters                                      ;;
;;======================================================================;;
;;                                                                      ;;
;;  'prop'    [STR]                                                     ;;
;;                                                                      ;;
;;  This parameter is a string describing the object property to be     ;;
;;  referenced by the field.                                            ;;
;;                                                                      ;;
;;  Example:                                                            ;;
;;  ------------------                                                  ;;
;;  Field Expression displayed in Field Command Dialog:                 ;;
;;                                                                      ;;
;;  %<\AcObjProp Object(%<\_ObjId 2129673136>%).Area \f "%lu6%qf1">%    ;;
;;                                                                      ;;
;;  For the above expression, the object property is "Area"             ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;                                                                      ;;
;;  'format'  [STR]                                                     ;;
;;                                                                      ;;
;;  This parameter is a string describing the field formatting for the  ;;
;;  object property that is to be displayed.                            ;;
;;                                                                      ;;
;;  If no formatting is to be used, 'format' should be an empty         ;;
;;  string ("").                                                        ;;
;;                                                                      ;;
;;  Example:                                                            ;;
;;  ------------------                                                  ;;
;;  Field Expression displayed in Field Command Dialog:                 ;;
;;                                                                      ;;
;;  %<\AcObjProp Object(%<\_ObjId 2129673136>%).Area \f "%lu6%qf1">%    ;;
;;                                                                      ;;
;;  For the above expression, the field formatting is "%lu6%qf1"        ;;
;;  This formatting string indicates that the Area property of the      ;;
;;  object will be displayed using the current units and precision in   ;;
;;  the drawing.                                                        ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;                                                                      ;;
;;  'mode'    [INT]                                                     ;;
;;                                                                      ;;
;;  This integer parameter determines how the field should be created   ;;
;;  in the drawing.                                                     ;;
;;                                                                      ;;
;;  mode = 1  :  Replace Existing Text / MText / Attribute string.      ;;
;;                                                                      ;;
;;               This mode will prompt the user to select an existing   ;;
;;               object to contain the field.                           ;;
;;                                                                      ;;
;;  mode = 2  :  Create Text Object                                     ;;
;;                                                                      ;;
;;               The user will be prompted to pick a point at which a   ;;
;;               Text Object containing the field will be created.      ;;
;;                                                                      ;;
;;  mode = 3  :  Create MText Object                                    ;;
;;                                                                      ;;
;;               This user will be prompted to pick a point at which    ;;
;;               an MText Object containing the field will be created.  ;;
;;                                                                      ;;
;;                                                                      ;;
;;======================================================================;;
;;  Example Custom Field Programs                                       ;;
;;======================================================================;;
;;                                                                      ;;
;;  Example Program #1:                                                 ;;
;;  -----------------------------------                                 ;;
;;                                                                      ;;
                                                                      
    (defun c:test1 ( ) (LM:QuickField "Area" "%lu6%qf1" 2))        
                                                                      
;;                                                                      ;;
;;  Here,                                                               ;;
;;                                                                      ;;
;;     prop   =  "Area"                                                 ;;
;;     format =  "%lu6%qf1"                                             ;;
;;     mode   =  2                                                      ;;
;;                                                                      ;;
;;  This program will hence prompt the user to select an object with    ;;
;;  the "Area" property, then prompt for a point at which to create a   ;;
;;  Text Object (mode=2) containing the field.                          ;;
;;                                                                      ;;
;;  The displayed Area will be formatted using the current settings     ;;
;;  for Units and Precision.                                            ;;
;;                                                                      ;;
;;======================================================================;;
;;                                                                      ;;
;;  Example Program #2:                                                 ;;
;;  -----------------------------------                                 ;;
;;                                                                      ;;

    (defun c:test2 ( ) (LM:QuickField "Length" "%lu2%pr3%ps[Length:,]%ct8[0.1]" 3))

;;                                                                      ;;
;;  Here,                                                               ;;
;;                                                                      ;;
;;     prop   =  "Length"                                               ;;
;;     format =  "%lu2%pr3%ps[Length:,]%ct8[0.1]"                       ;;
;;     mode   =  3                                                      ;;
;;                                                                      ;;
;;  This program will prompt the user to select an object with the      ;;
;;  "Length" property (Lines / LWPolylines / Polylines), then prompt    ;;
;;  for a point at which to create an MText Object (mode=3) containing  ;;
;;  the field.                                                          ;;
;;                                                                      ;;
;;  The Length value will be formatted...                               ;;
;;                                                                      ;;
;;     -  in Decimal Units (%lu2)                                       ;;
;;     -  to a Precision of 3 d.p. (%pr3)                               ;;
;;     -  with a Prefix of "Length:"  (%ps[Length:,])                   ;;
;;     -  with a Conversion Factor of 0.1  (%ct8[0.1])                  ;;
;;                                                                      ;;
;;======================================================================;;
;;                                                                      ;;
;;  Example Program #3:                                                 ;;
;;  -----------------------------------                                 ;;
;;                                                                      ;;

    (defun c:test3 ( ) (LM:QuickField "StyleName" "%tc1" 1))

;;                                                                      ;;
;;  Here,                                                               ;;
;;                                                                      ;;
;;     prop   =  "StyleName"                                            ;;
;;     format =  "%tc1"                                                 ;;
;;     mode   =  1                                                      ;;
;;                                                                      ;;
;;  This program will prompt the user to select an object with the      ;;
;;  "StyleName" property (Text / MText / ...anything with a Style),     ;;
;;  then prompt for an existing object (mode=1) to house the field.     ;;
;;                                                                      ;;
;;  The referenced StyleName property will be displayed as              ;;
;;  uppercase (%tc1).                                                   ;;
;;                                                                      ;;
;;======================================================================;;

;;--------------------------=={ Quick Field }==-------------------------;;
;;                                                                      ;;
;;  Creates a field using the method encoded by the 'mode' argument,    ;;
;;  using the supplied field property and format strings.               ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2011  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Arguments:                                                          ;;
;;  prop    -  Object Property to link to field (e.g. "Area")           ;;
;;  format  -  Field formatting string (use "" for none)                ;;
;;  mode    -  Integer to determine how the field is created            ;;
;;                                                                      ;;
;;             mode=1 : Replace Existing Text, MText, Attribute         ;;
;;             mode=2 : Create Text Object                              ;;
;;             mode=3 : Create MText Object                             ;;
;;----------------------------------------------------------------------;;
;;  Returns:  -None-                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2012-04-20                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.2    -    2015-10-10                                      ;;
;;                                                                      ;;
;;  - Program updated to account for selection of annotation objects    ;;
;;    which already contain a field expression.                         ;;
;;----------------------------------------------------------------------;;
;;  Version 1.3    -    2016-04-10                                      ;;
;;                                                                      ;;
;;  - Program updated to incorporate UPDATEFIELD command when inserting ;;
;;    a field into attribute content.                                   ;;
;;----------------------------------------------------------------------;;

(defun LM:quickfield ( prop format mode / ent ins obj str )  
    (if (setq str (LM:quickfield:constructfieldstring prop format))
        (cond
            (   (= 1 mode)
                (if (setq ent (LM:quickfield:selectifhasprop "Textstring" nentsel))
                    (progn
                        (setq obj (vlax-ename->vla-object ent))
                        (vla-put-textstring obj "") ;; To clear any existing field
                        (vla-put-textstring obj str)
                        (if (= "ATTRIB" (cdr (assoc 0 (entget ent))))
                            (vl-cmdf "_.updatefield" ent "")
                        )
                    )
                )
            )
            (   (= 2 mode)
                (if (setq ins (getpoint "\nSpecify point for text: "))
                    (vla-addtext
                        (vlax-get-property (LM:quickfield:acdoc)
                            (if (= 1 (getvar 'cvport))
                                'paperspace
                                'modelspace
                            )
                        )
                        str (vlax-3D-point (trans ins 1 0)) (getvar 'textsize)
                    )
                )
            )
            (   (= 3 mode)
                (if (setq ins (getpoint "\nSpecify point for mtext: "))
                    (vla-addmtext
                        (vlax-get-property (LM:quickfield:acdoc)
                            (if (= 1 (getvar 'cvport))
                                'paperspace
                                'modelspace
                            )
                        )
                        (vlax-3D-point (trans ins 1 0)) 0.0 str
                    )
                )
            )
        )
    )
    (princ)
)

(defun LM:quickfield:selectifhasprop ( prop func / ent )
    (while
        (progn
            (setvar 'errno 0)
            (setq ent (car (func (strcat "\nSelect object with " prop " property: "))))
            (cond
                (   (= 7 (getvar 'errno))
                    (princ "\nMissed, try again.")
                )
                (   (null ent)
                    nil
                )
                (   (not (vlax-property-available-p (vlax-ename->vla-object ent) prop))
                    (princ (strcat "\nSelected object does not have " prop " property."))
                )
            )
        )
    )
    ent
)

(defun LM:quickfield:acdoc nil
    (eval (list 'defun 'LM:quickfield:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:quickfield:acdoc)
)

(   (lambda nil (vl-load-com)
        (eval
            (list 'defun 'LM:quickfield:constructfieldstring '( prop format / ent )
                (list 'if '(setq ent (LM:quickfield:selectifhasprop prop entsel))
                    (list 'strcat "%<\\AcObjProp Object(%<\\_ObjId "
                        (if (vlax-method-applicable-p    (vla-get-utility (LM:quickfield:acdoc)) 'getobjectidstring)
                            (list 'vla-getobjectidstring (vla-get-utility (LM:quickfield:acdoc)) '(vlax-ename->vla-object ent) ':vlax-false)
                           '(itoa (vla-get-objectid (vlax-ename->vla-object ent)))
                        )
                        ">%)." 'prop '(if (/= "" format) (strcat " \\f \"" format "\">%") ">%")
                    )
                )
            )
        )
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;