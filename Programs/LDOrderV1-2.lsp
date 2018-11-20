;;------------------=={ Layer Draw Order }==------------------;;
;;                                                            ;;
;;  Allows the user to control the draw order of all objects  ;;
;;  on a layer relative to other layers in the drawing.       ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Version 1.2    -    20-06-2011                            ;;
;;------------------------------------------------------------;;

(setq ldorderversion# "1.2")

(defun c:ldorder

  ( /

   *error*
   _arrowtiles
   _getsavepath
   _help
   _list->value
   _listbottom
   _listdown
   _listtop
   _listup
   _makelist
   _removeitems
   _value->list
   _writedcl

   acdoc
   dclfname
   dclid
   dcltitle
   dflag
   hlptitle
   layer1
   layer2
   layers
   savepath
   value1
   value2
   
  )

;;------------------------------------------------------------;;

  (defun *error* ( msg )
    (if (< 0 dclid) (unload_dialog dclid))
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ)
  )

;;------------------------------------------------------------;;

  (defun _GetSavePath ( / tmp )
    (cond      
      ( (setq tmp (getvar 'ROAMABLEROOTPREFIX))
        (strcat (vl-string-right-trim "\\" (vl-string-translate "/" "\\" tmp)) "\\Support")
      )
      ( (setq tmp (findfile "ACAD.pat"))
        (vl-string-right-trim "\\" (vl-string-translate "/" "\\" (vl-filename-directory tmp)))
      )
      ( (vl-string-right-trim "\\" (vl-filename-directory (vl-filename-mktemp))) )
    )
  )

;;------------------------------------------------------------;;

  (defun _WriteDCL ( fname / ofile )
    (cond
      ( (findfile fname) )
      
      ( (setq ofile (open fname "w"))
       
        (foreach line
         '(
            "//----------=={ Layer Draw Order Dialog Definition }==--------//"
            "//                                                            //"
            "//  DCL file to be used in conjunction with LDOrder.lsp       //"
            "//------------------------------------------------------------//"
            "//  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       //"
            "//------------------------------------------------------------//"
            ""
            "arrowbox : image_button { width = 4.17; height = 1.92; fixed_width = true; fixed_height = true; color = dialog_background; }"
            "arrowimg : image        { width = 4.17; height = 1.92; fixed_width = true; fixed_height = true; color = dialog_background; }"
            "listbox  : list_box     { width = 35; fixed_width = true; height = 20; fixed_height = true; multiple_select = true; }"
            "editbox  : edit_box     { width = 35; fixed_width = true; }"
            "button12 : button       { width = 12; fixed_width = true; }"
            "button20 : button       { width = 20; fixed_width = true; height = 2.1; fixed_height = true; }"
            "ctext    : text         { width = 80; fixed_width = true; alignment = centered; }"
            "ltext    : text         { width = 75; fixed_width = true; height = 1.3; fixed_height = true; alignment = centered; }"
            "btext    : image        { width = 19; fixed_width = true; height = 1.2; fixed_height = true; color = dialog_background; }"
            ""
            "ldorder : dialog { key = \"dcltitle\";"
            "  spacer;"
            "  : row {"
            "    : boxed_column { label = \"All Layers\";"
            "      : listbox { key = \"layer1\"; }"
            "      : editbox { key = \"filter\"; label = \"Filter: \"; }"
            "      spacer;"
            "    }"
            "    : column { fixed_height = true;"
            "      : arrowbox { key = \"add\"; }"
            "      : arrowbox { key = \"del\"; }"
            "    }"
            "    : boxed_column { label = \"Layers to Adjust Draw Order\";"
            "      : listbox { key = \"layer2\"; }"
            "      : editbox { key = \"pattern\"; label = \"Add Pattern:\"; }"
            "      spacer;"
            "    }"
            "    : column { fixed_height = true;"
            "      : arrowbox { key = \"top\";    }"
            "      : arrowbox { key = \"up\";     }"
            "      : arrowbox { key = \"down\";   }"
            "      : arrowbox { key = \"bottom\"; }"
            "    }"
            "  }"
            "  spacer;"
            "  : row { fixed_width = true; alignment = centered;"
            "    : button20 { label = \"Move to Top\";    key = \"totop\"; is_default = true; }"
            "    : button20 { label = \"Move to Bottom\"; key = \"tobottom\"; }"
            "  }"
            "  : row { fixed_width = true; alignment = centered;"
            "    : button12 { label = \"Cancel\"; key = \"cancel\"; is_cancel = true; alignment = centered; }"
            "    : button12 { label = \"Help\"  ; key = \"help\"; alignment = centered; }"
            "  }"
            "}"
            ""
            "help : dialog { key = \"dcltitle\";"
            "  : btext  { key   = \"title1\"; alignment = centered; }"
            "  : ctext  { value = \"Designed and Created by Lee Mac 2011\"; }"
            "  : button { key = \"weblink\"; label = \"www.lee-mac.com\"; fixed_width = true; alignment = centered;"
            "             action = \"(startapp \\\"explorer\\\" \\\"http://www.lee-mac.com\\\")\"; }"
            "  spacer;"
            "  : paragraph {"
            "    : text_part { value = \"This program allows the user to control the draw order of all objects on specific layers in a drawing.\"; }"
            "    spacer;"
            "    : text_part { value = \"Layers whose draw-order is to be adjusted may be selected from a list containing all non-XRef\";} "
            "    : text_part { value = \"dependent layers in the drawing, situated to the left of the dialog.\"; }"
            "    spacer;"
            "    : text_part { value = \"The selected layers may be added to the list of layers to be adjusted by pressing the arrow pointing to\";} "
            "    : text_part { value = \"the right. Similarly, layers may be removed from this list by making a selection and pressing the arrow\";}"
            "    : text_part { value = \"pointing to the left.\"; }"
            "    spacer;"
            "    : text_part { value = \"For ease of layer selection, the left-hand layer list may be filtered by specifying a wildcard pattern\"; }"
            "    : text_part { value = \"in the 'Filter' edit box and pressing 'Enter'. This filter may be subsequently removed by clearing the\"; }"
            "    : text_part { value = \"filter edit box and again pressing 'Enter'.\"; }"
            "    spacer;"
            "    : text_part { value = \"The user may also group a set of layers by adding a wildcard pattern to the right-hand list pane. This is\"; }"
            "    : text_part { value = \"accomplished by specifying a wildcard string in the 'Add Pattern' edit box and pressing 'Enter'.\";}"
            "    spacer;"
            "    : text_part { value = \"The order of the layers in the right-hand list pane is a representation of the proposed layer draw-order\"; }"
            "    : text_part { value =\" to be implemented by the program. This order may be altered by making a selection of layers from the list\"; }"
            "    : text_part { value = \"and pressing the relevant arrow controls on the dialog.\"; }"
            "    spacer;"
            "    : text_part { value = \"When satisfied with the order of the layers in the right-hand list pane, the user may implement the\"; }"
            "    : text_part { value = \"draw-order modification by pressing either 'Move to Top' or 'Move to Bottom', depending upon whether\"; }"
            "    : text_part { value = \"the layers are to be ordered above or below the other layers in the drawing.\";}"
            "    spacer_1;"
            "    : btext { key = \"title2\"; alignment = left; }"
            "    : row {"
            "       : arrowimg { key = \"add\"; }"
            "       : ltext { value = \"Add selected layers to list of layers whose draw-order is to be altered.\"; }"
            "    }"
            "    : row {"
            "       : arrowimg { key = \"del\"; }"
            "       : ltext { value = \"Remove selected layers from list of layers whose draw-order is to be altered.\"; }"
            "    }"
            "    : row {"
            "       : arrowimg { key = \"top\"; }"
            "       : ltext { value = \"Move selected layers to the top of the draw-order list.\"; }"
            "    }"
            "    : row {"
            "       : arrowimg { key = \"up\"; }"
            "       : ltext { value = \"Move selected layers up a place in the draw-order list.\"; }"
            "    }"
            "    : row {"
            "       : arrowimg { key = \"down\"; }"
            "       : ltext { value = \"Move selected layers down a place in the draw-order list.\"; }"
            "    }"
            "    : row {"
            "       : arrowimg { key = \"bottom\"; }"
            "       : ltext { value = \"Move selected layers to the bottom of the draw-order list.\"; }"
            "    }"
            "  }"
            "  spacer_1; ok_only;"
            "}"
            "//------------------------------------------------------------//"
            "//                         End of File                        //"
            "//------------------------------------------------------------//"
          )
          (write-line line ofile)
        )
        (setq ofile (close ofile))
        (while (not (findfile fname))) fname
      )
    )
  )

;;------------------------------------------------------------;;

  (defun _ListUp ( i l / f )
    (defun f ( a b c d e )
      (cond
        ( (or (null b) (null c))
          (list (reverse d) (append (reverse e) c))
        )
        ( (= 0 (car b))
          (f (1+ a) (mapcar '1- (cdr b)) (cdr c) (cons a d) (cons (car c) e))
        )
        ( (= 1 (car b))
          (f (1+ a) (mapcar '1- (cdr b)) (cons (car c) (cddr c)) (cons a d) (cons (cadr c) e))
        )
        ( t
          (f (1+ a) (mapcar '1- b) (cdr c) d (cons (car c) e))
        )
      )
    )
    (f 0 i l nil nil)
  )

;;------------------------------------------------------------;;

  (defun _ListDown ( i l )
    (apply 
     '(lambda ( a b )
        (list
          (reverse (mapcar '(lambda ( x ) (- (length l) x 1)) a))
          (reverse b)
        )
      )
      (_ListUp
        (reverse (mapcar '(lambda ( x ) (- (length l) x 1)) i))
        (reverse l)
      )
    )
  )

;;------------------------------------------------------------;;

  (defun _RemoveItems ( i l / j )
    (setq j -1)
    (vl-remove-if '(lambda ( x ) (member (setq j (1+ j)) i)) l)
  )

;;------------------------------------------------------------;;

  (defun _ListTop ( i l / j )
    (setq j -1)
    (list (mapcar '(lambda ( x ) (setq j (1+ j))) i)
      (append
        (mapcar '(lambda ( x ) (nth x l)) i) (_RemoveItems i l)
      )
    )
  )

;;------------------------------------------------------------;;

  (defun _ListBottom ( i l / j )
    (setq j (- (length l) (length i) 1))
    (list (mapcar '(lambda ( x ) (setq j (1+ j))) i)
      (append
        (_RemoveItems i l) (mapcar '(lambda ( x ) (nth x l)) i)
      )
    )
  )

;;------------------------------------------------------------;;

  (defun _MakeList ( key lst )
    (start_list key) (mapcar 'add_list lst) (end_list)
  )

;;------------------------------------------------------------;;

  (defun _value->list ( val ) (read (strcat "(" val ")")))

  (defun _list->value ( lst ) (vl-string-trim "()" (vl-princ-to-string lst)))

;;------------------------------------------------------------;;

  (defun _help ( id title )
    (cond
      ( (not (new_dialog "help" id))

        (alert "Unable to Load Help Dialog")
      )
      ( t
        (_arrowtiles)
        (set_tile "dcltitle" title)
        (set_tile "title1" "Layer Draw Order")
        (start_image "title1")
        (vector_image 0 (1- (dimy_tile "title1")) (dimx_tile "title1") (1- (dimy_tile "title1")) 0)
        (end_image)
        (set_tile "title2" "Program Controls")
        (start_image "title2")
        (vector_image 0 (1- (dimy_tile "title2")) (dimx_tile "title2") (1- (dimy_tile "title2")) 0)
        (end_image)
        (start_dialog)
      )
    )
  )

;;------------------------------------------------------------;;

  (defun _arrowtiles nil
    (foreach v
     '(
        ("top"
          (001 001 002 002 003 003 004 004 005 005 006 006 007 007 008 008 009 009 010 010 011 011 012 012 013 013 014 014 015 015
           016 016 017 017 018 018 019 019 020 020 021 021 022 022 023 023 024 024)
          (016 003 016 003 016 003 016 003 016 003 016 003 025 003 025 003 025 003 025 003 025 003 025 003 025 003 025 003 025 003
           025 003 025 003 025 003 016 003 016 003 016 003 016 003 016 003 016 003)
          (015 000 014 000 013 000 012 000 011 000 010 000 009 000 008 000 007 000 006 000 005 000 004 000 004 000 005 000 006 000
           007 000 008 000 009 000 010 000 011 000 012 000 013 000 014 000 015 000)
          (174 178 174 178 174 178 174 178 174 178 174 178 174 178 174 178 174 178 174 178 174 178 174 178 178 178 178 178 178 178
           178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178)
        )
        ("up"
          (001 002 003 004 005 006 007 008 009 010 011 012 013 014 015 016 017 018 019 020 021 022 023 024)
          (016 016 016 016 016 016 025 025 025 025 025 025 025 025 025 025 025 025 016 016 016 016 016 016)
          (015 014 013 012 011 010 009 008 007 006 005 004 004 005 006 007 008 009 010 011 012 013 014 015)
          (174 174 174 174 174 174 174 174 174 174 174 174 178 178 178 178 178 178 178 178 178 178 178 178)
        )
        ("down"
          (001 002 003 004 005 006 007 008 009 010 011 012 013 014 015 016 017 018 019 020 021 022 023 024)
          (009 009 009 009 009 009 000 000 000 000 000 000 000 000 000 000 000 000 009 009 009 009 009 009)
          (010 011 012 013 014 015 016 017 018 019 020 021 021 020 019 018 017 016 015 014 013 012 011 010)
          (174 174 174 174 174 174 174 174 174 174 174 174 178 178 178 178 178 178 178 178 178 178 178 178)
        )
        ("bottom"
          (001 001 002 002 003 003 004 004 005 005 006 006 007 007 008 008 009 009 010 010 011 011 012 012 013 013 014 014 015 015
           016 016 017 017 018 018 019 019 020 020 021 021 022 022 023 023 024 024)
          (009 022 009 022 009 022 009 022 009 022 009 022 000 022 000 022 000 022 000 022 000 022 000 022 000 022 000 022 000 022
           000 022 000 022 000 022 009 022 009 022 009 022 009 022 009 022 009 022)
          (010 025 011 025 012 025 013 025 014 025 015 025 016 025 017 025 018 025 019 025 020 025 021 025 021 025 020 025 019 025
           018 025 017 025 016 025 015 025 014 025 013 025 012 025 011 025 010 025)
          (174 178 174 178 174 178 174 178 174 178 174 178 174 178 174 178 174 178 174 178 174 178 174 178 178 178 178 178 178 178
           178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178)
        )
      )
      (start_image (car v))
      (apply 'mapcar (cons '(lambda ( x y z c ) (vector_image x y x z c)) (cdr v)))
      (end_image)
    )
    (foreach v
     '(
        ("add"
          (011 011 011 011 011 011 002 002 002 002 002 002 002 002 002 002 002 002 011 011 011 011 011 011)
          (001 002 003 004 005 006 007 008 009 010 011 012 013 014 015 016 017 018 019 020 021 022 023 024)
          (012 013 014 015 016 017 018 019 020 021 022 023 023 022 021 020 019 018 017 016 015 014 013 012)
          (174 174 174 174 174 174 174 174 174 174 174 174 178 178 178 178 178 178 178 178 178 178 178 178)
        )
        ("del"
          (014 014 014 014 014 014 023 023 023 023 023 023 023 023 023 023 023 023 014 014 014 014 014 014)
          (001 002 003 004 005 006 007 008 009 010 011 012 013 014 015 016 017 018 019 020 021 022 023 024)
          (013 012 011 010 009 008 007 006 005 004 003 002 002 003 004 005 006 007 008 009 010 011 012 013)
          (174 174 174 174 174 174 174 174 174 174 174 174 178 178 178 178 178 178 178 178 178 178 178 178)
        )
      )
      (start_image (car v))
      (apply 'mapcar (cons '(lambda ( x y z c ) (vector_image x y z y c)) (cdr v)))
      (end_image)
    )
  )

  ;;------------------------------------------------------------;;

  (setq acdoc (vla-get-activedocument (vlax-get-acad-object)))

  (if (not (vl-file-directory-p (setq SavePath (_GetSavePath))))
    (progn
      (princ "\n** Save Path not Valid **") (exit)
    )
  )
  (setq dclfname (strcat SavePath "\\LMAC_LDOrder_V" (vl-string-translate "." "-"  ldorderversion#) ".dcl")
        dcltitle (strcat "Layer Draw Order V" ldorderversion#)
        hlptitle (strcat "Layer Draw Order V" ldorderversion# " - Help")
  ) 

  (cond
    ( (not (_WriteDCL dclfname))

      (princ "\n--> DCL File could not be Written.")
    )
    ( (<= (setq dclid (load_dialog dclfname)) 0)

      (princ "\n--> DCL File not Found.")
    )
    ( (not (new_dialog "ldorder" dclid))

      (princ "\n--> Dialog could not be Loaded.")
    )
    (t
      (_arrowtiles)
      (set_tile "dcltitle" dcltitle)

      (
        (lambda ( / x )
          (while (setq x (tblnext "LAYER" (null x)))
            (if (not (wcmatch (cdr (assoc 2 x)) "*|*"))
              (setq layers (cons (cdr (assoc 2 x)) layers))
            )
          )
          (setq layers (acad_strlsort layers))
        )
      )

      (_MakeList   "layer1"  (setq layer1 layers))
      (set_tile    "layer1"  (setq value1 (itoa (vl-position (getvar 'CLAYER) layer1))))
      (action_tile "layer1" "(setq value1 $value)")
      (action_tile "layer2" "(setq value2 $value)")
      (set_tile    "filter"  "*")

      (action_tile "help" "(_help dclid hlptitle)")

      (action_tile "add"
        (vl-prin1-to-string
          (quote
            (
              (lambda ( / index items )
                (cond
                  ( value1
                    (setq index (_value->list value1)
                          items (mapcar '(lambda ( n ) (nth n layer1)) index)
                    )
                    (_MakeList "layer2" (setq layer2 (append layer2 items)))
                    (set_tile  "layer2" (setq value2 (cond ( value2 ) ( "0" ))))
                    
                    (_MakeList "layer1" (setq layer1 (_RemoveItems index layer1)))
                    (if layer1
                      (set_tile "layer1" (setq value1 "0"))
                      (setq value1 nil)
                    )
                  )
                )
              )
            )
          )
        )
      )

      (action_tile "del"
        (vl-prin1-to-string
          (quote
            (
              (lambda ( / index items )
                (cond
                  ( value2
                    (setq index (_value->list value2)
                          items (mapcar '(lambda ( n ) (nth n layer2)) index)
                    )
                    (_MakeList "layer1" (setq layer1 (acad_strlsort (append layer1 items))))
                    (set_tile  "layer1" (setq value1 (cond ( value1 ) ( "0" ))))
                   
                    (_MakeList "layer2" (setq layer2 (_RemoveItems index layer2)))
                    (if layer2
                      (set_tile "layer2" (setq value2 "0"))
                      (setq value2 nil)
                    )
                  )
                )
              )
            )
          )
        )
      )

      (action_tile "filter"
        (vl-prin1-to-string
          (quote
            (if (= 1 $reason)
              (cond
                ( (eq "" (setq pattn (strcase $value)))
                  (set_tile "filter" "*")
                 
                  (_MakeList "layer1"
                    (setq layer1
                      (vl-remove-if
                       '(lambda ( x )
                          (or (vl-position x layer2)
                            (vl-some '(lambda ( p ) (wcmatch (strcase x) p)) layer2)
                          )
                        )
                        layers
                      )
                    )
                  )
                  (if layer1
                    (set_tile "layer1" (setq value1 "0"))
                    (setq value1 nil)
                  )
                )
                ( t
                  (_MakeList "layer1" (setq layer1 (vl-remove-if-not '(lambda ( x ) (wcmatch (strcase x) pattn)) layer1)))
                  (if layer1
                    (set_tile  "layer1" (setq value1 "0"))
                    (setq value1 nil)
                  )
                )
              )
            )
          )
        )
      )

      (action_tile "pattern"
        (vl-prin1-to-string
          (quote
            (if (= 1 $reason)
              (cond
                ( (eq "" (setq pattn (strcase $value)))
                )
                ( (=
                    (length (append layer1 layer2))
                    (+
                      (length (setq filt1 (vl-remove-if '(lambda ( x ) (wcmatch (strcase x) pattn)) layer1)))
                      (length (setq filt2 (vl-remove-if '(lambda ( x ) (wcmatch (strcase x) pattn)) layer2)))
                    )
                  )
                  (alert "No Matches.")
                  (mode_tile "pattern" 2)
                )
                ( t
                  (_MakeList "layer2"  (setq layer2 (append filt2 (list pattn))))
                  (set_tile  "layer2"  (setq value2 (itoa (1- (length layer2)))))

                  (_MakeList "layer1"  (setq layer1 filt1))
                  (set_tile  "layer1"  (setq value1 "0"))
                  (set_tile  "pattern" "")
                )
              )
            )
          )
        )
      )
      
      (mapcar
        (function
          (lambda ( key fun )
            (action_tile key
              (strcat
                "("
                "  (lambda ( x )"
                "    (_MakeList \"layer2\" (setq layer2 (cadr x)))"
                "    (set_tile  \"layer2\" (setq value2 (_list->value (car x))))"
                "  )"
                "  (" fun "(_value->list value2) layer2)"
                ")"
              )
            )
          )
        )
       '("top"      "up"      "down"      "bottom"     )
       '("_ListTop" "_ListUp" "_ListDown" "_ListBottom")
      )

      (action_tile "totop"    "(done_dialog 1)")
      (action_tile "tobottom" "(done_dialog 2)")
     
      (if (member (setq dflag (start_dialog)) '(1 2))
        (progn
          (LDOrder   acdoc layer2 (= 1 dflag))
          (vla-regen acdoc acactiveviewport)
          (princ "\n--> Layer Draw Order Adjusted.")
        )
        (princ "\n*Cancel*")
      )
    )
  )
  (if dclid (unload_dialog dclid))
  (princ)
)

;;------------=={ Layer Draw Order Subfunction }==------------;;
;;                                                            ;;
;;  Adjusts the draw order of all entities on each layer      ;;
;;  (or set of layers matching a wildcard pattern) in a       ;;
;;  supplied list to reflect the order of the list.           ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  acdoc  - VLA Document Object                              ;;
;;  layers - List of layer names or wildcard patterns         ;;
;;  toTop  - T=order layers above others, nil=below others    ;;
;;------------------------------------------------------------;;

(defun LDOrder ( acdoc layers toTop / _SortentsTable _SafearrayVariant )

;;------------------------------------------------------------;;

  (defun _SortentsTable ( space / dict result ) 
    (cond
      (
        (not
          (vl-catch-all-error-p
            (setq result
              (vl-catch-all-apply 'vla-item
                (list (setq dict (vla-GetExtensionDictionary space)) "ACAD_SORTENTS")
              )
            )
          )
        )
        result
      )
      ( (vla-AddObject dict "ACAD_SORTENTS" "AcDbSortentsTable") )
    )
  )

;;------------------------------------------------------------;;

  (defun _SafearrayVariant ( datatype data )
    (vlax-make-variant
      (vlax-safearray-fill
        (vlax-make-safearray datatype (cons 0 (1- (length data)))) data
      )    
    )
  )

;;------------------------------------------------------------;;

  (
    (lambda ( sortents func / ss )
      (foreach x (if toTop (reverse layers) layers)
        (if (setq ss (ssget "_X" (list (cons 8 x) (cons 410 (getvar 'CTAB)))))
          (
            (lambda ( / l i )
              (repeat (setq i (sslength ss))
                (setq l (cons (vlax-ename->vla-object (ssname ss (setq i (1- i)))) l))
              )
              (func sortents (_SafearrayVariant vlax-vbobject l))
            )
          )
        )
      )
    )
    (_SortentsTable (vlax-get-property acdoc (if (= 1 (getvar 'CVPORT)) 'Paperspace 'Modelspace)))
    (if toTop vla-movetotop vla-movetobottom)
  )
  (princ)
)

;;------------------------------------------------------------;;

(vl-load-com)
(princ)
(princ
  (strcat
    "\n:: LDOrder.lsp | Version " ldorderversion# " | © Lee Mac 2011 www.lee-mac.com ::"
    "\n:: Type \"LDOrder\" to Invoke ::"
  )
)
(princ)

;;------------------------------------------------------------;;
;;                         End of File                        ;;
;;------------------------------------------------------------;;