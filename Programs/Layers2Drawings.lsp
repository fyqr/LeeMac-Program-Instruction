;;--------------------=={ Layers 2 DWGs }==-------------------;;
;;                                                            ;;
;;  WBlocks all active layers to separate drawings, saved to  ;;
;;  the specified directory                                   ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;

(defun c:Layers2DWGs ( / *error* _UniqueFilename _UniqueItem _LayerList doc docname SelSets Path ss )
  (vl-load-com)
  ;; © Lee Mac 2010

  (defun *error* ( msg )
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ)
  )

  (defun _UniqueFileName ( seed )
    (
      (lambda ( i / filename )
        (if (findfile (setq filename (strcat seed ".dwg")))
          (while (findfile (setq filename (strcat seed "(" (itoa (setq i (1+ i))) ").dwg"))))
        )
        filename
      )
      1
    )
  )

  (defun _UniqueItem ( collection seed )
    (
      (lambda ( i )
        (while (LM:Itemp collection (strcat seed (itoa (setq i (1+ i))))))
        (strcat seed (itoa i))
      )
      0
    )
  )

  (defun _LayerList ( doc / l )
    (vlax-for layer (vla-get-layers doc)
      (if
        (not
          (or
            (eq :vlax-false (vla-get-layeron layer))
            (wcmatch (vla-get-name layer) "*|*")
          )
        )
        (setq l (cons (vla-get-name layer) l))
      )
    )
    (reverse l)
  )

  (setq doc     (vla-get-ActiveDocument (vlax-get-acad-object))
        docname (vl-filename-base (vla-get-Name doc))
        SelSets (vla-get-SelectionSets doc))

  (if (setq Path (LM:DirectoryDialog "Select Directory for New Files" nil 0))
    (progn
      (setq ss (vla-Add SelSets (_UniqueItem SelSets "LayerSave")))
      
      (mapcar
        (function
          (lambda ( layer )
            (LM:DXF->Variants (list (cons 8 layer)) 'typ 'val)
            (vla-Select ss acSelectionSetAll nil nil typ val)

            (if (not (zerop (vla-get-Count ss)))
              (progn
                (vla-WBlock doc (_UniqueFilename (strcat Path "\\" docname "_" layer)) ss)
                (princ (strcat "\n--> Extracted Layer: " layer))
              )
              (princ (strcat "\n[ Nothing Found on Layer: " layer " ]"))
            )
            (vla-clear ss)            
          )
        )
        (_LayerList doc)
      )

      (vl-catch-all-apply 'vla-delete (list ss))
    )
    (princ "\n*Cancel*")
  )

  (princ)
)

;;-------------------=={ Directory Dialog }==-----------------;;
;;                                                            ;;
;;  Displays a dialog prompting the user to select a folder   ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  msg  - message to display at top of dialog                ;;
;;  dir  - root directory (or nil)                            ;;
;;  flag - bit coded flag specifying dialog display settings  ;;
;;------------------------------------------------------------;;
;;  Returns:  Selected folder filepath, else nil              ;;
;;------------------------------------------------------------;;

(defun LM:DirectoryDialog ( msg dir flag / Shell HWND Fold Self Path ac )
  (vl-load-com)
  ;; © Lee Mac 2010

  (setq Shell (vla-getInterfaceObject (setq ac (vlax-get-acad-object)) "Shell.Application")
        HWND  (vl-catch-all-apply 'vla-get-HWND (list ac))
        Fold  (vlax-invoke-method Shell 'BrowseForFolder (if (vl-catch-all-error-p HWND) 0 HWND)  msg flag dir))
  (vlax-release-object Shell)
  
  (if Fold
    (progn
      (setq Self (vlax-get-property Fold 'Self) Path (vlax-get-property Self 'Path))
      (vlax-release-object Self)
      (vlax-release-object Fold)      
      
      (and (= "\\" (substr Path (strlen Path)))
           (setq Path (substr Path 1 (1- (strlen Path)))))
    )
  )
  Path
)

;;------------------=={ Safearray Variant }==-----------------;;
;;                                                            ;;
;;  Creates a populated Safearray Variant of a specified      ;;
;;  data type                                                 ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  datatype - variant type enum (eg vlax-vbDouble)           ;;
;;  data     - list of static type data                       ;;
;;------------------------------------------------------------;;
;;  Returns:  VLA Variant Object of type specified            ;;
;;------------------------------------------------------------;;

(defun LM:SafearrayVariant ( datatype data )
  ;; © Lee Mac 2010
  (vlax-make-variant
    (vlax-safearray-fill
      (vlax-make-safearray datatype
        (cons 0 (1- (length data)))
      )
      data
    )    
  )
)

;;------------------=={ DXF->Variants }==---------------------;;
;;                                                            ;;
;;  Converts a DXF List to Type and Value Variants            ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  lst  - DXF List                                           ;;
;;  *typ - a quoted symbol (other than *typ) to house variant ;;
;;  *val - a quoted symbol (other than *val) to house variant ;;
;;------------------------------------------------------------;;

(defun LM:DXF->Variants ( lst *typ *val)
  ;; © Lee Mac 2010
  (set *typ (LM:SafearrayVariant vlax-vbInteger (mapcar 'car lst))) 

  (set *val
    (LM:SafearrayVariant vlax-vbVariant
      (mapcar
       '(lambda ( data )
          (if (listp (setq data (cdr data)))
            (vlax-3D-point data)
            (vlax-make-variant data)
          )
        )
       lst       
      )
    )
  )
)

;;-----------------------=={ Itemp }==------------------------;;
;;                                                            ;;
;;  Retrieves the item with index 'item' if present in the    ;;
;;  specified collection, else nil                            ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  coll - the VLA Collection Object                          ;;
;;  item - the index of the item to be retrieved              ;;
;;------------------------------------------------------------;;
;;  Returns:  the VLA Object at the specified index, else nil ;;
;;------------------------------------------------------------;;

(defun LM:Itemp ( coll item )
  ;; © Lee Mac 2010
  (if
    (not
      (vl-catch-all-error-p
        (setq item
          (vl-catch-all-apply
            (function vla-item) (list coll item)
          )
        )
      )
    )
    item
  )
)
