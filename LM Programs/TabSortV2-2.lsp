;;--------------------=={ TabSort.lsp }==---------------------;;
;;                                                            ;;
;;  A program designed with the intention to aid in the       ;;
;;  organisation of layout tabs in a drawing.                 ;;
;;                                                            ;;
;;  The program enables the user to organise each layout tab  ;;
;;  using buttons to move the tabs up, down, to the top, and  ;;
;;  to the bottom of a list.                                  ;;
;;                                                            ;;
;;  The user may rename any layout by double clicking on it,  ;;
;;  and can also add a Prefix and/or Suffix to selected/every ;;
;;  layout tab.                                               ;;
;;                                                            ;;
;;  The program offers the facility to add and delete a       ;;
;;  layout tab, and also sort the tabs alphabetically,        ;;
;;  numerically, architecturally; or reverse the order in     ;;
;;  which they appear.                                        ;;
;;                                                            ;;
;;  A Find and Replace function is incorporated to allow the  ;;
;;  user to quickly replace a text string in multiple tabs.   ;;
;;                                                            ;;
;;  The user can also Copy a layout tab, and set the Selected ;;
;;  layout tab as the current tab.                            ;;
;;                                                            ;;
;;  The Help Dialog can be access by pressing "H" at the Main ;;
;;  Dialog.                                                   ;;
;;                                                            ;;
;;------------------------------------------------------------;;
;;                                                            ;;
;;  FUNCTION SYNTAX:  TABSORT                                 ;;
;;                                                            ;;
;;  Notes:-                                                   ;;
;;  The Reset button will reset Tab Order and Tab Names, but  ;;
;;  will not affect newly created tabs and will not recreate  ;;
;;  deleted tabs.                                             ;;
;;                                                            ;;
;;  In the case that multiple tabs are selected, the Current  ;;
;;  button will set the tab with the lowest index (highest in ;;
;;  the list) as the current tab.                             ;;
;;                                                            ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;                                                            ;;
;;  With Thanks To:                                           ;;
;;  --------------------                                      ;;
;;  Gilles Chanteau (Gile)                                    ;;
;;                                                            ;;
;;  For his fantastic contribution to the list manipulation   ;;
;;  required for multiple DCL selections, and effort towards  ;;
;;  the Architectural Sort functions.                         ;;
;;                                                            ;;
;;------------------------------------------------------------;;
;;  Charles Alan Butler (CAB)                                 ;;
;;                                                            ;;
;;  For his excellent idea to incorporate the Help button for ;;
;;  those who need it.                                        ;;
;;                                                            ;;
;;------------------------------------------------------------;;
;;  Version:                                                  ;;
;;                                                            ;;
;;  1.0    -  13th September 2009                             ;;
;;                                                            ;;
;;  First Release                                             ;;
;;------------------------------------------------------------;;
;;  1.1    -  14th September 2009                             ;;
;;                                                            ;;
;;  Added Mnemonics to Tiles                                  ;;
;;  Added text to notify of double-click availability         ;;
;;  Added option to only pref/suff selected tab               ;;
;;------------------------------------------------------------;;
;;  1.2    -  15th September 2009                             ;;
;;                                                            ;;
;;  Added Numerical Sort                                      ;;
;;  General Bug Fixes                                         ;;
;;------------------------------------------------------------;;
;;  1.3    -  15th September 2009                             ;;
;;                                                            ;;
;;  Multi-tab Selection Functionality                         ;;
;;------------------------------------------------------------;;
;;  1.4    -  16th September 2009                             ;;
;;                                                            ;;
;;  Added Button to Set Current Tab                           ;;
;;  Added Copy Tab Functionality                              ;;
;;------------------------------------------------------------;;
;;  1.5    -  17th September 2009                             ;;
;;                                                            ;;
;;  Fixed Bug to Copy Layout Plot Settings                    ;;
;;------------------------------------------------------------;;
;;  1.6    -  18th September 2009                             ;;
;;                                                            ;;
;;  Modified Action_Tile Stmts ~ Thanks VovKa                 ;;
;;  Added Architectural Sort Button                           ;;
;;------------------------------------------------------------;;
;;  1.6.1  -  19th September 2009                             ;;
;;                                                            ;;
;;  Fixed Bugs ~ Thanks for feedback CAB                      ;;
;;------------------------------------------------------------;;
;;  1.7    -  20th September 2009                             ;;
;;                                                            ;;
;;  Added Find & Replace Functionality                        ;;
;;  Organised buttons/Added Sort Dialog                       ;;
;;------------------------------------------------------------;;
;;  1.7.1  -  20th September 2009                             ;;
;;                                                            ;;
;;  Replaced StrBrk sub with SplitStr (Gile)                  ;;
;;------------------------------------------------------------;;
;;  1.8    -  21st September 2009                             ;;
;;                                                            ;;
;;  Added hidden Help Button (CAB)                            ;;
;;  Added Help Dialog Definition                              ;;
;;------------------------------------------------------------;;
;;  1.8.1  -  22nd September 2009                             ;;
;;                                                            ;;
;;  General Bug Fixes                                         ;;
;;------------------------------------------------------------;;
;;  1.8.2  -  23rd September 2009                             ;;
;;                                                            ;;
;;  Fixed Bug causing crash when Deleting all                 ;;
;;  Tabs ~ thanks VovKa                                       ;;
;;------------------------------------------------------------;;
;;  1.9    -     2nd October 2009                             ;;
;;                                                            ;;
;;  Modified Reverse function to only apply to selected Tabs. ;;
;;  Altered Help Dialog to ok_only button                     ;;
;;------------------------------------------------------------;;
;;  2.0    -       22nd July 2010                             ;;
;;                                                            ;;
;;  Fixed Bug with Find/Replace.                              ;;
;;------------------------------------------------------------;;
;;  2.1    -    9th December 2010                             ;;
;;                                                            ;;
;;  Fixed Bug with Double Reversal                            ;;
;;------------------------------------------------------------;;
;;  2.2    -        16th May 2011                             ;;
;;                                                            ;;
;;  Program completely rewritten to update formatting.        ;;
;;  Upgraded Find & Replace engine to allow for               ;;
;;  self-referencing find and replace terms and improved      ;;
;;  visual feedback.                                          ;;
;;  Updated list manipulation subfunctions.                   ;;
;;------------------------------------------------------------;;

(setq TabSortVersion# "2-2")

;;------------------------------------------------------------;;

(defun c:TabSort

  ( /

   ;; --={  Local Functions  }=--

   *error*
   _addtab
   _archsort
   _copytab
   _deletetabs
   _endundo
   _findreplacetab
   _getlayouts
   _getsavepath
   _list->value
   _listdown
   _listtobottom
   _listtotop
   _listup
   _makelist
   _numsort
   _removeitems
   _removenth
   _renametab
   _reverseitems
   _splitstr
   _startundo
   _tabprefixsuffix
   _tabsort
   _tabsorthelp
   _value->list
   _writedcl

   ;; --={  Local Variables  }=--

   acdoc
   aclay
   aclays
   dch
   dclfname
   dclst
   dcltitle
   express
   found
   fstr
   i
   idx
   l
   lst
   name
   ofile
   pref
   ptr
   reslst
   rstr
   savepath
   suff
   x

   ;; --={  Global Variables  }=--
 
   ;  *prefdef*  -  Setting to Apply a Prefix/Suffix to All Tabs
   ;  *SortTyp*  -  Sort Type Settings
   ;  *SortOrd*  -  Sort Order Settings

  )

;;------------------------------------------------------------;;
;;                       Local Functions                      ;;
;;------------------------------------------------------------;;

  (defun *error* ( msg )
    (if acdoc (_EndUndo acdoc))
    (if (and dch (< 0 dch)) (unload_dialog dch))
    (if (and ofile (eq 'FILE (type ofile))) (setq ofile (close ofile)))
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ)
  )

;;------------------------------------------------------------;;

  (defun _StartUndo ( doc ) (_EndUndo doc)
    (vla-StartUndoMark doc)
  )

  (defun _EndUndo ( doc )
    (if (= 8 (logand 8 (getvar 'UNDOCTL)))
      (vla-EndUndoMark doc)
    )
  )

;;------------------------------------------------------------;;

  (defun _value->list ( val ) (read (strcat "(" val ")")))

  (defun _list->value ( lst ) (vl-string-trim "()" (vl-princ-to-string lst)))

;;------------------------------------------------------------;;

  (defun _GetLayouts ( layouts / lst )
    (vlax-for l layouts
      (if (not (eq "MODEL" (strcase (vla-get-Name l))))
        (setq lst (cons l lst))
      )
    )    
    (vl-sort lst '(lambda ( a b ) (< (vla-get-TabOrder a) (vla-get-TabOrder b))))
  )

;;------------------------------------------------------------;;

  (defun _MakeList ( key lst )
    (start_list key) (mapcar 'add_list lst) (end_list)
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
    )
  )

;;------------------------------------------------------------;;

  (defun _WriteDCL ( fname / ofile )
    (cond
      (
        (findfile fname)
      )
      ( (setq ofile (open fname "w"))

        (foreach line
         '(
            "//---------------------=={ TabSort.dcl }==--------------------//"
            "//                                                            //"
            "//  Dialog Definition file for use in conjunction with        //"
            "//  TabSort.lsp                                               //"
            "//------------------------------------------------------------//"
            "//  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       //"
            "//------------------------------------------------------------//"
            ""
            "dcl_settings : default_dcl_settings { audit_level = 1; }"
            ""
            "//------------------------------------------------------------//"
            "//                   SubAssembly Definitions                  //"
            "//------------------------------------------------------------//"
            ""
            "button13 : button   { width = 13; alignment = centered; fixed_width = true; }"
            "button16 : button   { width = 16; alignment = centered; fixed_width = true; }"
            "    eBox : edit_box { alignment = centered; fixed_width = true; allow_accept = true; is_tab_stop = true; }"
            "    fBox : edit_box { alignment = left; fixed_width = true; is_tab_stop = true; }"
            "   title : text     { alignment = centered; is_bold = true; fixed_width = false;}"
            "     txt : text     { alignment = centered; fixed_width = false; }"
            "    ltxt : text     { alignment = left;     fixed_width = false; }"
            ""
            "//------------------------------------------------------------//"
            "//                        Main Dialog                         //"
            "//------------------------------------------------------------//"
            ""
            "tabsort : dialog { key = \"dcltitle\";"
            ""
            "  : image_button { key = \"help\"; label = \"&help\"; color = -15; height = 0.1; width = 0.1; fixed_width = true; }"
            "  spacer;"
            "  "
            "  : row {"
            "    : button13 { key = \"mtop\"; label = \"Top\"      ; mnemonic = \"T\"; }"
            "    : button13 { key = \"up\"  ; label = \"Up\"       ; mnemonic = \"U\"; }"
            "    : button13 { key = \"down\"; label = \"Down\"     ; mnemonic = \"D\"; }"
            "    : button13 { key = \"mbot\"; label = \"Bottom\"   ; mnemonic = \"B\"; }"
            "  }"
            "  "
            "  : list_box { key = \"tabs\"; width = 20; fixed_width = false; alignment = centered;  multiple_select = true; }"
            "  "
            "  : row {"
            "    : button13 { key = \"sort\"; label = \"Sort...\"  ; mnemonic = \"S\"; }"
            "    : button13 { key = \"rev\" ; label = \"Reverse\"  ; mnemonic = \"R\"; }"
            "    : button13 { key = \"p_s\" ; label = \"Pref/Suff\"; mnemonic = \"P\"; }"
            "    : button13 { key = \"cur\" ; label = \"Current\"  ; mnemonic = \"n\"; }"
            "  }"
            "  "
            "  spacer;"
            "  "
            "  : row { fixed_width = false;"
            "    : button13 { key = \"add\"   ; label = \"Add\"    ; mnemonic = \"A\"; }"
            "    : button13 { key = \"del\"   ; label = \"Delete\" ; mnemonic = \"e\"; }"
            "    : button13 { key = \"copy\"  ; label = \"Copy\"   ; mnemonic = \"C\"; }"
            "    : button13 { key = \"fnr\"   ; label = \"Find\"   ; mnemonic = \"F\"; }"
            "  }"
            "  "
            "  : text { label = \"Double-click to Rename a Tab, Press H for Help\" ; alignment = centered; }"
            "  "
            "  : row  { fixed_width = true ; alignment = centered;"
            "    : button13 { key = \"accept\"; label = \"Done\"   ; mnemonic = \"o\"; is_default = true; is_cancel = true; }"
            "    : button13 { key = \"res\"   ; label = \"Reset\"  ; mnemonic = \"s\"; }               "
            "  }"
            "}"
            ""
            "//------------------------------------------------------------//"
            "//                        Rename Dialog                       //"
            "//------------------------------------------------------------//"
            ""
            "rename : dialog { label = \"Rename Tab\"; spacer;"
            "  : eBox { key = \"name\"; edit_width = 20; edit_limit = 255; label = \"Tab Name:\"; }"
            "  : errtile { } "
            "  spacer; ok_cancel;"
            "}"
            ""
            "//------------------------------------------------------------//"
            "//                    Delete Warning Dialog                   //"
            "//------------------------------------------------------------//"
            ""
            "delwarn : dialog { label = \"Warning\"; spacer;"
            "  : text { alignment = centered; label = \"The Selected Tab(s) will be Permanently Deleted\"; }"
            "  : text { alignment = centered; label = \"Proceed?\"; }"
            "  spacer; ok_cancel;"
            "}"
            ""
            "//------------------------------------------------------------//"
            "//                    Prefix/Suffix Dialog                    //"
            "//------------------------------------------------------------//"
            ""
            "prefsuff : dialog { label = \"Add Prefix/Suffix\"; spacer;"
            ""
            "  : row {"
            "    : column {"
            "      : text { alignment = centered; label = \"Prefix\"; }"
            "      : eBox { key = \"pref\"; edit_width = 15; edit_limit = 255; }"
            "    }"
            "    : column {"
            "      : spacer { alignment = centered; width = 10; }"
            "      : text   { alignment = centered; label = \"< Tab Name >\"; }"
            "    }"
            "    : column {"
            "      : text { alignment = centered; label = \"Suffix\"; }"
            "      : eBox { key = \"suff\"; edit_width = 15; edit_limit = 255; }"
            "    }"
            "  }  "
            "  spacer;"
            "  "
            "  : row { spacer;"
            "    : toggle { key = \"all\"; label = \"Apply to all Tabs\"; mnemonic = \"A\"; alignment = right; is_tab_stop = true; }"
            "  }"
            "  : errtile { } ok_cancel;"
            "}"
            ""
            "//------------------------------------------------------------//"
            "//                        Find Dialog                         //"
            "//------------------------------------------------------------//"
            ""
            "find : dialog { label = \"Find and Replace\"; spacer;"
            ""
            "  : row {"
            "    : column { fixed_height = true;"
            "      : text { key = \"fw\"  ; label = \"Find What:\"           ; }"
            "      : fBox { key = \"fstr\"; edit_width = 30; mnemonic = \"W\"; }"
            "      : text { key = \"rw\"  ; label = \"Replace With:\"        ; }"
            "      : fBox { key = \"rstr\"; edit_width = 30; mnemonic = \"R\"; }"
            "    }"
            "    : spacer { width = 4; }"
            "    : column { fixed_height = true;"
            "      : spacer   { height = 0.2   ; }"
            "      : button16 { key = \"fnd\"  ; label = \"Find\"       ; mnemonic = \"F\"; }"
            "      : button16 { key = \"rep\"  ; label = \"Replace\"    ; mnemonic = \"p\"; }"
            "      : button16 { key = \"repa\" ; label = \"Replace All\"; mnemonic = \"A\"; }"
            "    }"
            "  }"
            "  "
            "  spacer;"
            "  : text { key = \"ftxt\"; alignment = left; label = \"String not Found!\"; value = \"\"; }"
            "  spacer;"
            "  : button13 { key = \"accept\";    label = \"Done\"    ; mnemonic = \"o\"; is_default = true; is_cancel = true  ; }"
            "}"
            ""
            "//------------------------------------------------------------//"
            "//                        Sort Dialog                         //"
            "//------------------------------------------------------------//"
            ""
            "sort : dialog { label = \"Sort\"; spacer;"
            ""
            "  : boxed_column { label = \"Sort Type\";"
            "  "
            "    : popup_list { key = \"typ\"; alignment = centered; }"
            "    "
            "    spacer;"
            "    "
            "    : radio_row { children_alignment = centered;"
            "      : radio_button { key = \"asc\"; label = \"Ascending\" ; }"
            "      : radio_button { key = \"des\"; label = \"Descending\"; }"
            "    }"
            "    spacer;"
            "  }"
            "  "
            "  spacer;"
            "  "
            "  : row { fixed_width = false; alignment = centered;"
            "    : button13 { key = \"accept\"; label = \"Sort\"  ; mnemonic = \"S\"; is_default = true; }"
            "    : button13 { key = \"cancel\"; label = \"Cancel\"; mnemonic = \"C\"; is_cancel  = true; }"
            "  }"
            "}"
            ""
            "//------------------------------------------------------------//"
            "//                        Help Dialog                         //"
            "//------------------------------------------------------------//"
            ""
            "help : dialog { key = \"htitle\";"
            "  spacer; : title { label = \"  ---------------------=={ TabSort.lsp }==---------------------  \" ; }"
            "  spacer; : title { label = \"Designed and Created by Lee Mac 2011\" ; }"
            "  spacer; : ltxt  { label = \"   Program Controls:\" ; }"
            "  spacer;"
            "  : row { fixed_width = true; alignment = centered;"
            "    : column {"
            "       : txt { label = \"[\"     ; }"
            "       : txt { label = \"[\"     ; }"
            "       : txt { label = \"[\"     ; }"
            "       : txt { label = \"[\"     ; }"
            "       : txt { label = \"[\"     ; }"
            "       : txt { label = \"-->\"   ; }"
            "       : txt { label = \"-->\"   ; }"
            "       : txt { label = \"-->\"   ; }"
            "       : txt { label = \"[\"     ; }"
            "       : txt { label = \"[\"     ; }"
            "       : txt { label = \"[\"     ; }"
            "       : txt { label = \"[\"     ; }"
            "       : txt { label = \"[\"     ; }"
            "       : txt { label = \"[\"     ; }"
            "       : txt { label = \"[\"     ; }"
            "       : txt { label = \"[\"     ; }"
            "       : txt { label = \"[\"     ; }"
            "    }"
            "    : column {"
            "       : txt { label = \"Top\"          ; }"
            "       : txt { label = \"Up\"           ; }"
            "       : txt { label = \"Down\"         ; }"
            "       : txt { label = \"Bottom\"       ; }"
            "       : txt { label = \"Sort\"         ; }"
            "       : txt { label = \"Alphabetical\" ; }"
            "       : txt { label = \"Numerical\"    ; }"
            "       : txt { label = \"Architectural\"; }"
            "       : txt { label = \"Reverse\"      ; }"
            "       : txt { label = \"Pref/Suff\"    ; }"
            "       : txt { label = \"Add\"          ; }"
            "       : txt { label = \"Delete\"       ; }"
            "       : txt { label = \"Copy\"         ; }"
            "       : txt { label = \"Current\"      ; }"
            "       : txt { label = \"Find\"         ; }"
            "       : txt { label = \"Done\"         ; }"
            "       : txt { label = \"Reset\"        ; }"
            "    }"
            "    : column {"
            "       : txt { label = \"]\"     ; }"
            "       : txt { label = \"]\"     ; }"
            "       : txt { label = \"]\"     ; }"
            "       : txt { label = \"]\"     ; }"
            "       : txt { label = \"]\"     ; }"
            "       : txt { label = \"<--\"   ; }"
            "       : txt { label = \"<--\"   ; }"
            "       : txt { label = \"<--\"   ; }"
            "       : txt { label = \"]\"     ; }"
            "       : txt { label = \"]\"     ; }"
            "       : txt { label = \"]\"     ; }"
            "       : txt { label = \"]\"     ; }"
            "       : txt { label = \"]\"     ; }"
            "       : txt { label = \"]\"     ; }"
            "       : txt { label = \"]\"     ; }"
            "       : txt { label = \"]\"     ; }"
            "       : txt { label = \"]\"     ; }"
            "    }"
            "    spacer;"
            "    : column {"
            "      : ltxt { label = \"Move selected Tabs to the top of the list.\"             ; }"
            "      : ltxt { label = \"Move selected Tabs up one notch in the list.\"           ; }"
            "      : ltxt { label = \"Move selected Tabs down one notch in the list.\"         ; }"
            "      : ltxt { label = \"Move selected Tabs to the bottom of the list.\"          ; }"
            "      : ltxt { label = \"Opens the Sort Dialog.\"                                 ; }"
            "      : ltxt { label = \"Sort the Tabs Alphabetically.\"                          ; }"
            "      : ltxt { label = \"Sort the Tabs Numerically.\"                             ; }"
            "      : ltxt { label = \"Sort the Tabs using an Architectural sorting method.\"   ; }"
            "      : ltxt { label = \"Reverse the Tab Order.\"                                 ; }"
            "      : ltxt { label = \"Opens the Prefix/Suffix Dialog.\"                        ; }"
            "      : ltxt { label = \"Adds a new layout Tab using the next available name.\"   ; }"
            "      : ltxt { label = \"Deletes the selected Tabs.\"                             ; }"
            "      : ltxt { label = \"Copies the selected Tabs.\"                              ; }"
            "      : ltxt { label = \"Makes the selected Tab the Current Tab.\"                ; }"
            "      : ltxt { label = \"Opens the Find and Replace Dialog.\"                     ; }"
            "      : ltxt { label = \"Finished sorting Tabs, will implement sorting.\"         ; }"
            "      : ltxt { label = \"Will reset Tab names and order - not deleted/added Tabs\"; }"
            "    }"
            "  }"
            "  spacer_1;"
            "  : title { label = \"-------------------------------------------------------------\" ; }"
            "  spacer_1; ok_only;"
            "}"
            ""
            "//------------------------------------------------------------//"
            "//                        End of File                         //"
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

  (defun _RemoveNth ( n l )
    (if (and l (< 0 n))
      (cons (car l) (_RemoveNth (1- n) (cdr l)))
      (cdr l)
    )
  )

;;------------------------------------------------------------;;

  (defun _RenameTab ( dchand n lst / name )
    (cond
      (
        (not (new_dialog "rename" dchand))

        (cond
          (Express
            (acet-ui-message "Error Loading Rename Dialog" "Warning" 16)
          )
          (t
            (princ "\n** Error Loading Rename Dialog **")
          )
        )
       
        lst
      )
      (t
        (set_tile  "name" (setq name (nth n lst)))
        (mode_tile "name" 2)

        (action_tile "name" "(setq name $value)")

        (action_tile "accept"
          (vl-prin1-to-string
            (quote
              (progn (set_tile "error" "")
                (cond
                  (
                    (eq "" name)

                    (set_tile "error" "Please Enter a Tab Name")
                  )
                  (
                    (wcmatch (strcase name) "*[<>\\/\\\":;`?`*|`,=]*")

                    (set_tile "error" "Invalid Symbol in Tab Name")
                    (mode_tile "name" 2)
                  )
                  (
                    (vl-position (strcase name) (mapcar 'strcase (_RemoveNth n lst)))

                    (set_tile "error" (strcat name " already exists!"))
                    (mode_tile "name" 2)
                  )
                  ( (done_dialog 1) )
                )
              )
            )
          )
        )

        (if (= 1 (start_dialog))
          (cond
            (
              (eq name (nth n lst))

              lst
            )
            (
              (vl-catch-all-error-p
                (vl-catch-all-apply 'vla-put-Name (list (vla-item aclay (nth n lst)) name))
              )
              (cond
                (Express
                  (acet-ui-message "Error Renaming Tab" "Warning" 16)
                )
                (t
                  (alert "\n** Error Renaming Tab **")
                )
              )
              lst
            )
            ( (subst name (nth n lst) lst) )
          )
          lst
        )
      )
    )
  )

;;------------------------------------------------------------;;

  (defun _ListUp ( ind lst ) ; (Gile)
    (cond
      ( (or (null ind) (null lst))

        lst
      )
      ( (= 0  (car ind))

        (cons (car  lst) (_ListUp (cdr (mapcar '1- ind)) (cdr lst)))
      )
      ( (= 1  (car ind))

        (cons (cadr lst) (_ListUp (cdr (mapcar '1- ind)) (cons (car lst) (cddr lst))))
      )
      ( t

        (cons (car  lst) (_ListUp (mapcar '1- ind) (cdr lst)))
      )
    )
  )

;;------------------------------------------------------------;;

  (defun _ListDown ( idx lst )
    (reverse
      (_ListUp
        (reverse
          (mapcar '(lambda ( x ) (- (1- (length lst)) x)) idx)
        )
        (reverse lst)
      )
    )
  )

;;------------------------------------------------------------;;

  (defun _RemoveItems ( idx lst / i )
    (setq i -1)
    (vl-remove-if '(lambda ( x ) (member (setq i (1+ i)) idx)) lst)
  )

;;------------------------------------------------------------;;

  (defun _ListToTop ( idx lst )
    (append (mapcar '(lambda ( i ) (nth i lst)) idx) (_RemoveItems idx lst))
  )

;;------------------------------------------------------------;;

  (defun _ListToBottom ( idx lst ) 
    (append (_RemoveItems idx lst) (mapcar '(lambda ( i ) (nth i lst)) idx))
  )

;;------------------------------------------------------------;;

  (defun _ReverseItems ( idx lst )
    (
      (lambda ( i l )
        (mapcar
          (function
            (lambda ( x )
              (if (member (setq i (1+ i)) idx)
                (setq x (nth (car l) lst)
                      l (cdr l)
                )
              )
              x
            )
          )
          lst
        )
      )
      -1 (reverse (vl-sort idx '<))
    )
  )

;;------------------------------------------------------------;;

  (defun _TabSort ( dchand lst )
    
    (or *SortTyp* (setq *SortTyp*   "0"))
    (or *SortOrd* (setq *SortOrd* "asc"))

    (cond
      (
        (not (new_dialog "sort" dchand))

        (cond
          (Express
            (acet-ui-message "Error Loading Sort Dialog" "Warning" 16)
          )
          (t
            (princ "\n** Error Loading Sort Dialog **")
          )
        )
        lst
      )
      (t

        (_MakeList "typ" '("Alphabetical" "Numerical" "Architectural"))
       
        (set_tile "typ" *SortTyp*)
        (set_tile *SortOrd*   "1")

        (action_tile "typ" "(setq *SortTyp* $value)")
        (action_tile "asc" "(setq *SortOrd*   $key)")
        (action_tile "des" "(setq *SortOrd*   $key)")

        (if (zerop (start_dialog)) lst
          (progn
            (cond
              ( (eq "0" *SortTyp*)

                (setq lst (acad_strlsort lst))
              )
              ( (eq "1" *SortTyp*)

                (setq lst (_NumSort lst))
              )
              ( (eq "2" *SortTyp*)

                (setq lst (_ArchSort lst))
              )
            )
            (if (eq "asc" *SortOrd*)
              lst
              (reverse lst)
            )
          )
        )
      )
    )
  )

;;------------------------------------------------------------;;

  (defun _SplitStr ( str / lst test rslt num tmp ) ; (Gile)

    (setq lst  (vl-string->list str)
          test (chr (car lst))
    )    
    (if (< 47 (car lst) 58)
      (setq num T)
    )
    (while (setq lst (cdr lst))
      (if num
        (cond
          ( (= 46 (car lst))

            (if
              (and
                (cadr lst)
                (setq tmp (strcat "0." (chr (cadr lst))))
                (numberp (read tmp))
              )
              (setq rslt (cons (read test) rslt)
                    test tmp
                    lst  (cdr lst)
              )
              (setq rslt (cons (read test) rslt)
                    test "."
                    num nil
              )
            )
          )
          ( (< 47 (car lst) 58)

            (setq test (strcat test (chr (car lst))))
          )
          ( t

            (setq rslt (cons (read test) rslt)
                  test (chr (car lst))
                  num  nil
            )
          )
        )
        (if (< 47 (car lst) 58)
          (setq rslt (cons test rslt)
                test (chr (car lst))
                num  T
          )
          (setq test (strcat test (chr (car lst))))
        )
      )
    )
    
    (if num
      (setq rslt (cons (read test) rslt))
      (setq rslt (cons test rslt))
    )    
    (reverse rslt)
  )

;;------------------------------------------------------------;;

  (defun _ArchSort ( lst / comparable ) ; (Gile)
    
    (defun comparable ( e1 e2 )
      (or
        (and (numberp e1) (numberp e2))
        (= 'STR (type e1) (type e2))
        (not e1)
        (not e2)
      )
    )
    
    (mapcar '(lambda ( x ) (nth x lst))      
      (vl-sort-i (mapcar '_SplitStr lst)
        (function
          (lambda ( x1 x2 / n1 n2 comp )
            (while
              (and
                (setq comp
                  (comparable (setq n1 (car x1)) (setq n2 (car x2)))
                )
                (= n1 n2)
              )
              (setq x1 (cdr x1) x2 (cdr x2))
            )            
            (if comp (< n1 n2) (numberp n1))
          )
        )
      )
    )
  )

;;------------------------------------------------------------;;

  (defun _NumSort ( lst )

    (mapcar '(lambda ( x ) (nth x lst))    
      (vl-sort-i (mapcar '(lambda ( x ) (vl-remove-if-not 'numberp (_SplitStr x))) lst)
        (function
          (lambda ( a b )
            (while (and a b (= (car a) (car b)))
              (setq a (cdr a)
                    b (cdr b)
              )
            )
            (if (or a b) (< (car a) (car b)) t)
          )
        )
      )
    )
  )

;;------------------------------------------------------------;;

  (defun _DeleteTabs ( dchand idx lst / x )
    (cond
      (Express
        (setq x
          (acet-ui-message "The Selected Tab(s) Will be Permanently Deleted.\n\nProceed?" "Delete Tab(s)?" 52)
        )
      )
      ( (not (new_dialog "delwarn" dchand))

        (princ "\n** Error Loading Delete Dialog **")
      )
      (t
        (action_tile "accept" "(setq x 6) (done_dialog)")
        (start_dialog)
      )
    )

    (if (= 6 x)
      (if
        (vl-catch-all-error-p
          (vl-catch-all-apply
           '(lambda nil
              (foreach i idx (vla-delete (vla-item aclay (nth i lst))))
            )
          )
        )
        (cond
          (Express
            (acet-ui-message "Error Deleting Tab(s)" "Warning" 16)
          )
          (t
            (alert "\n** Error Deleting Tab(s) **")
          )
        )
        (setq lst (_RemoveItems idx lst))
      )
    )
    lst
  )

;;------------------------------------------------------------;;

  (defun _TabPrefixSuffix ( dchand idx lst / pref suff item tmp i )
    
    (or *prefdef* (setq *prefdef* 0))
    
    (cond
      ( (not (new_dialog "prefsuff" dchand))

        (cond
          (Express
            (acet-ui-message "Error Loading Prefix/Suffix Dialog" "Warning" 16)
          )
          (t
            (princ "\n** Error Loading Prefix/Suffix Dialog **")
          )
        )
        lst
      )
      (t

        (set_tile "all" (itoa *prefdef*))

        (action_tile "accept"
          (vl-prin1-to-string
            (quote
              (progn
                (cond
                  (
                    (and
                      (not (eq "" (setq pref (get_tile "pref"))))
                      (wcmatch (strcase pref) "*[<>\\/\\\":;`?`*|`,=]*")
                    )

                    (set_tile "error" "Invalid Symbol in Prefix")
                    (mode_tile "pref" 2)
                  )
                  (
                    (and
                      (not (eq "" (setq suff (get_tile "suff"))))
                      (wcmatch (strcase suff) "*[<>\\/\\\":;`?`*|`,=]*")
                    )

                    (set_tile "error" "Invalid Symbol in Suffix")
                    (mode_tile "suff" 2)
                  )
                  (
                    (and (zerop *prefdef*) (setq tmp (mapcar 'strcase (_RemoveItems idx lst)))
                      (vl-some
                        (function
                          (lambda ( x )
                            (vl-position (strcase (strcat pref x suff)) tmp)
                          )
                        )
                        (mapcar '(lambda ( x ) (nth x lst)) idx) 
                      )
                    )

                    (set_tile "error" "Amendment would create Duplicate tab")
                    (mode_tile "pref" 2)
                  )
                  ( (done_dialog 1) )
                )
              )
            )
          )
        )

        (action_tile "all" "(setq *prefdef* (atoi $value))")

        (if (= 1 (start_dialog))
          (cond
            (
              (zerop *prefdef*)

              (setq i -1)

              (setq lst
                (mapcar
                  (function
                    (lambda ( x )
                      (if (member (setq i (1+ i)) idx)
                        (progn
                          (vla-put-name (vla-item aclay x) (strcat pref x suff))
                          (strcat pref x suff)
                        )
                        x
                      )
                    )
                  )
                  lst
                )
              )
            )
            (
              t

              (setq lst
                (mapcar
                  (function
                    (lambda ( x )
                      (vla-put-name (vla-item aclay x) (strcat pref x suff))
                      (strcat pref x suff)
                    )
                  )
                  lst
                )
              )
            )
          )
        )
      )
    )
    lst
  )
  
;;------------------------------------------------------------;;
  
  (defun _AddTab ( lst / tmp i upp )

    (setq tmp "Layout1" i 1 upp (mapcar 'strcase lst))
    
    (while (member (strcase tmp) upp)
      (setq tmp (strcat "Layout" (itoa (setq i (1+ i)))))
    )
    (vla-add aclay tmp)
    (append lst (list tmp))
  )

;;------------------------------------------------------------;;

  (defun _CopyTab ( idx lst / upp oldname oldlay newname i objlst newlay newblk )
    
    (setq upp (mapcar 'strcase lst))

    (foreach x idx
      (setq oldname (nth x lst)
            oldlay  (vla-item aclay oldname)
            newname (strcat oldname " (2)")
            i       2
            objlst  nil
      )
      (while (member (strcase newname) upp)
        (setq newname (strcat oldname " (" (itoa (setq i (1+ i))) ")"))
      )
      (setq newlay (vla-add aclay newname)
            newblk (vla-get-block newlay)
            lst    (append lst (list newname))
      )
      (vla-copyfrom newlay oldlay)

      (if
        (vlax-for o (vla-get-block oldlay)
          (setq objlst (cons o objlst))
        )
        (vla-copyobjects acdoc
          (vlax-make-variant
            (vlax-safearray-fill
              (vlax-make-safearray vlax-vbobject (cons 0 (1- (length objlst)))) (reverse objlst)
            )
          )
          newblk
        )
      )
    )

    lst
  )
  
;;------------------------------------------------------------;;

  (defun _FindReplaceTab ( dchand lst / fstr rstr found tab idx flen i nstr rlen m n )

    (cond
      (
        (not (new_dialog "find" dchand))

        (cond
          (Express
            (acet-ui-message "Error Loading Find and Replace Dialog" "Warning" 16)
          )
          (t
            (princ "\n** Error Loading Find & Replace Dialog **")
          )
        )
        lst
      )
      (t

        (set_tile "ftxt" "")
        (set_tile "fstr" (setq fstr ""))
        (set_tile "rstr" (setq rstr ""))

        (action_tile "fstr" "(set_tile \"ftxt\" \"\") (setq found nil fstr $value)")
        (action_tile "rstr" "(setq rstr $value)")

        (action_tile "fnd"
          (vl-prin1-to-string
            (quote
              (progn
                (cond
                  (
                    found
                   
                    (set_tile "ftxt" (caar found))
                    (setq tab   (cadar  found)
                          idx   (caddar found)
                          found (cdr    found)
                    )
                  )
                  (
                    t
                    (cond
                      (
                        (eq fstr "")

                        (set_tile "ftxt" "Please Enter a String to Find.")
                        (setq found nil tab nil idx nil)
                      )
                      (t
                        (setq flen (strlen fstr) i 0)
                       
                        (foreach x lst
                          (while (setq i (vl-string-search (strcase fstr) (strcase x) i))
                            (setq found
                              (cons
                                (list
                                  (strcat (substr x 1 i) "[" (substr x (1+ i) flen) "]" (substr x (+ i flen 1)))
                                  x
                                  i
                                )
                                found
                              )
                              i (+ i flen)
                            )
                          )
                        )
                        (setq found (reverse found))

                        (cond
                          (
                            found
                           
                            (set_tile "ftxt" (caar found))
                            (setq tab   (cadar  found)
                                  idx   (caddar found)
                                  found (cdr    found)
                            )
                          )
                          (
                            t

                            (set_tile "ftxt" "String not Found.")
                            (setq tab nil idx nil)
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )

        (action_tile "rep"
          (vl-prin1-to-string
            (quote
              (progn
                (cond
                  (
                    (not tab)

                    (set_tile "ftxt" "String not Found.")
                  )
                  (
                    (wcmatch rstr "*[<>\\/\\\":;`?`*|`,=]*")

                    (set_tile "ftxt" "Invalid Symbol in Replace String.")
                  )
                  (
                    (member
                      (vl-string-trim " "
                        (strcase
                          (setq nstr
                            (strcat (substr tab 1 idx) rstr (substr tab (+ 1 idx flen)))
                          )
                        )
                      )
                      (mapcar 'strcase lst)
                    )

                    (set_tile "ftxt" "Replacement would create Duplicate Tab.")
                  )
                  (
                    t
                    (vla-put-Name (vla-item aclay tab) nstr)
                   
                    (set_tile "ftxt" (strcat (get_tile "ftxt") "  ->  " nstr))
                    (setq lst (subst nstr tab lst))

                    (setq found
                      (mapcar
                        (function
                          (lambda ( x )
                            (if (eq tab (cadr x))
                              (progn
                                (if (< idx (setq i (caddr x)))
                                  (setq i (+ (caddr x) (- (strlen rstr) flen)))
                                )
                                (setq i (vl-string-search (strcase fstr) (strcase nstr) i))
                                (list
                                  (strcat (substr nstr 1 i) "[" (substr nstr (1+ i) flen) "]" (substr nstr (+ i flen 1)))
                                  nstr
                                  i
                                )
                              )
                              x
                            )
                          )
                        )
                        found
                      )
                    )
                  )
                )
              )
            )
          )
        )

        (action_tile "repa"
          (vl-prin1-to-string
            (quote
              (progn
                (set_tile "ftxt" "")

                (cond
                  (
                    (eq fstr "")

                    (set_tile "ftxt" "Please Enter a String to Find.")
                    (setq found nil)
                  )
                  (
                    (wcmatch rstr "*[<>\\/\\\":;`?`*|`,=]*")

                    (set_tile "ftxt" "Invalid Symbol in Replace String.")
                  )
                  (
                    t
                    (setq n 0
                          flen (strlen fstr)
                          rlen (strlen rstr)
                    )

                    (foreach tab lst
                      (setq m n
                            i 0
                            nstr tab
                      )                      
                      (while (setq i (vl-string-search (strcase fstr) (strcase nstr) i))
                        (setq nstr (strcat (substr nstr 1 i) rstr (substr nstr (+ 1 i flen)))
                                 i (+ i rlen)
                                 m (1+ m)
                        )
                      )
                      (if (not (member (strcase nstr) (mapcar 'strcase lst)))
                        (progn
                          (vla-put-name (vla-item aclay tab) nstr)
                          (setq n   m
                                lst (subst nstr tab lst)
                          )
                        )
                      )
                    )

                    (set_tile "ftxt"
                      (if (< 0 n)
                        (strcat (itoa n) " Replacements Made.")
                        "String not Found."
                      )
                    )
                  )
                )
              )
            )
          )
        )

        (start_dialog)
      )
    )
    lst
  )

;;------------------------------------------------------------;;

  (defun _TabSortHelp ( dchand title )
    (cond
      (
        (not (new_dialog "help" dchand))

        (cond
          (Express
            (acet-ui-message "Error Loading Help Dialog" "Warning" 16)
          )
          (t
            (princ "\n** Error Loading Help Dialog **")
          )
        )
      )
      (t
        (set_tile "htitle" title)
        (start_dialog)
      )
    )
  )

;;------------------------------------------------------------;;

  (defun _ListDupes ( l )
    (if l
      (if (vl-position (car l) (cdr l))
        (cons (car l) (_ListDupes (vl-remove (car l) (cdr l))))
        (_ListDupes (vl-remove (car l) (cdr l)))
      )
    )
  )

;;------------------------------------------------------------;;

  (setq acdoc (vla-get-activedocument (vlax-get-acad-object))
        aclay (vla-get-layouts acdoc)
  )

  (if (not (vl-file-directory-p (setq SavePath (_GetSavePath))))
    (progn
      (princ "\n** Save Path not Valid **") (exit)
    )
  )

  (setq dclfname (strcat SavePath "\\LMAC_TabSort_V" TabSortVersion# ".dcl")
        dcltitle (strcat "TabSort V" (vl-string-translate "-" "." TabSortVersion#))
  )
  
  (setq Express
    (and (vl-position "acetutil.arx" (arx))
      (not
        (vl-catch-all-error-p
          (vl-catch-all-apply
            (function (lambda nil (acet-sys-shift-down)))
          )
        )
      )
    )
  )

;;------------------------------------------------------------;;
  
  (cond
    (
      (not (_WriteDCL dclfname))

      (cond
        (Express
          (acet-ui-message "DCL File Could not be Written" "Warning" 16)
        )
        (t
          (princ "\n** DCL File Could not be Written **")
        )
      )
    )
    (
      (< (setq dch (load_dialog dclfname)) 0)

      (cond
        (Express
          (acet-ui-message "DCL File not Found" "Warning" 16)
        )
        (t
          (princ "\n** DCL File not Found **")
        )
      )
    )
    (
      (not (new_dialog "tabsort" dch))

      (cond
        (Express
          (acet-ui-message "Error Loading TabSort Dialog" "Warning" 16)
        )
        (t
          (princ "\n** Error Loading TabSort Dialog **")
        )
      )
    )
    (t

      (_StartUndo acdoc)

      (setq dclst  (mapcar 'vla-get-Name (_GetLayouts aclay))
            resLst dclst
      )
      (set_tile "dcltitle" dcltitle)

      (_MakeList "tabs" dclst)
     
      (set_tile "tabs"
        (setq ptr
          (if (zerop (getvar 'TILEMODE))
            (itoa (vl-position (getvar 'CTAB) dclst))
            "0"
          )
        )
      )

      (action_tile "tabs"
        (vl-prin1-to-string
          (quote
            (progn (setq ptr $value)
              (if (= 4 $reason)
                (progn
                  (_MakeList "tabs" (setq dclst (_RenameTab dch (atoi ptr) dclst)))
                  (set_tile "tabs" ptr)
                )
              )
            )
          )
        )
      )

      (action_tile "up"
        (vl-prin1-to-string
          (quote
            (progn
              (setq
                idx (_value->list ptr)
                old (mapcar '(lambda ( x ) (nth x dclst)) idx)
              )
              (_MakeList "tabs" (setq dclst (_ListUp idx dclst)))
              (set_tile  "tabs"
                (setq ptr
                  (_list->value (mapcar '(lambda ( x ) (vl-position x dclst)) old))
                )
              )
            )
          )
        )
      )
         
      (action_tile "down"
        (vl-prin1-to-string
          (quote
            (progn
              (setq
                idx (_value->list ptr)
                old (mapcar '(lambda ( x ) (nth x dclst)) idx)
              )
              (_MakeList "tabs" (setq dclst (_ListDown idx dclst)))
              (set_tile  "tabs"
                (setq ptr
                  (_list->value (mapcar '(lambda ( x ) (vl-position x dclst)) old))
                )
              )
            )
          )
        )
      )

      (action_tile "mtop"
        (vl-prin1-to-string
          (quote
            (progn
              (_MakeList "tabs" (setq dclst (_ListToTop (setq idx (_value->list ptr)) dclst)))

              (setq i -1)
              (set_tile "tabs"
                (setq ptr
                  (_list->value (mapcar '(lambda ( x ) (setq i (1+ i))) idx))
                )
              )
            )
          )
        )
      )

      (action_tile "mbot"
        (vl-prin1-to-string
          (quote
            (progn
              (_MakeList "tabs" (setq dclst (_ListToBottom (setq idx (_value->list ptr)) dclst)))
              
              (setq i (length dclst))
              (set_tile "tabs"
                (setq ptr
                  (_list->value (reverse (mapcar '(lambda ( x ) (setq i (1- i))) idx)))
                )
              )
            )
          )
        )
      )

      (action_tile "sort"
        (vl-prin1-to-string
          (quote
            (progn
              (setq
                idx (_value->list ptr)
                old (mapcar '(lambda ( x ) (nth x dclst)) idx)
              )
              (_MakeList "tabs" (setq dclst (_TabSort dch dclst)))
              (set_tile  "tabs"
                (setq ptr
                  (_list->value (mapcar '(lambda ( x ) (vl-position x dclst)) old))
                )
              )
            )
          )
        )
      )
     
      (action_tile "rev"
        (vl-prin1-to-string
          (quote
            (progn
              (_MakeList "tabs" (setq dclst (_ReverseItems (_value->list ptr) dclst)))
              (set_tile  "tabs" ptr)
            )
          )
        )
      )
         
      (action_tile "del"
        (vl-prin1-to-string
          (quote
            (progn
              (setq
                idx (_value->list ptr)
                old (mapcar '(lambda ( x ) (nth x dclst)) idx)
                tmp dclst
              )
              (setq newres
                (_RemoveItems
                  (mapcar '(lambda ( x ) (1- (vla-get-TabOrder (vla-item aclay x)))) old)
                  resLst
                )
              )
              (_MakeList "tabs"
                (setq dclst
                  (cond
                    ( (_DeleteTabs dch idx dclst) )
                    ( (mapcar 'vla-get-Name (_GetLayouts aclay)) )
                  )
                )
              )
              (if (not (equal tmp dclst)) (setq resLst (cond ( newres ) ( dclst ))))

              (set_tile "tabs"
                (setq ptr
                  (if (zerop (getvar 'TILEMODE))
                    (itoa (vl-position (getvar 'CTAB) dclst))
                    "0"
                  )
                )
              )
            )
          )
        )
      )
         
      (action_tile "p_s"
        (vl-prin1-to-string
          (quote
            (progn
              (_MakeList "tabs" (setq dclst (_TabPrefixSuffix dch (_value->list ptr) dclst)))
              (set_tile  "tabs" ptr)
            )
          )
        )
      )

      (action_tile "res"
        (vl-prin1-to-string
          (quote
            (progn
              (cond
                (
                  (_ListDupes (mapcar 'strcase resLst))

                  (cond
                    (Express
                      (acet-ui-message "Resetting would create Duplicate Tabs" "Warning" 48)
                    )
                    (t
                      (alert "Resetting would create Duplicate Tabs")
                    )
                  )
                )
                (
                  t
                 
                  (_MakeList "tabs" (setq dclst resLst))
                  (mapcar 'vla-put-Name (_GetLayouts aclay) resLst)
                  
                  (set_tile "tabs"
                    (setq ptr
                      (if (zerop (getvar 'TILEMODE))
                        (itoa (vl-position (getvar 'CTAB) dclst))
                        "0"
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )

      (action_tile "add"
        (vl-prin1-to-string
          (quote
            (progn
              (_MakeList "tabs" (setq dclst (_AddTab dclst)))
              (setq resLst (append resLst (list (last dclst))))
              (set_tile "tabs" (setq ptr (itoa (1- (length dclst)))))
            )
          )
        )
      )

      (action_tile "cur"
        (vl-prin1-to-string
          (quote
            (progn
              (setvar "CTAB" (nth (setq ptr (car (_value->list ptr))) dclst))
              (set_tile "tabs" (setq ptr (itoa ptr)))
            )
          )
        )
      )

      (action_tile "copy"
        (vl-prin1-to-string
          (quote
            (progn
              (setq
                idx (_value->list ptr)
                tmp dclst
                i   (1- (length dclst))
                j   i                      
              )
              (_MakeList "tabs" (setq dclst (_CopyTab idx dclst)))
              (setq resLst
                (append resLst
                  (mapcar '(lambda ( x ) (nth (setq i (1+ i)) dclst)) idx)
                )
              )              
              (set_tile "tabs"
                (setq ptr
                  (_list->value (mapcar '(lambda ( x ) (setq j (1+ j))) idx))
                )
              )
            )
          )
        )
      )

      (action_tile "fnr"
        (vl-prin1-to-string
          (quote
            (progn
              (_MakeList "tabs" (setq dclst (_FindReplaceTab dch dclst)))
              (set_tile "tabs" ptr)
            )
          )
        )
      )

      (action_tile "help" "(_TabSortHelp dch (strcat dcltitle \" - Help\"))")

      (start_dialog)
      (setq dch (unload_dialog dch))

      (cond
        (
          (equal (setq dclst (mapcar 'strcase dclst))
            (mapcar 'strcase
              (mapcar 'vla-get-Name
                (setq aclays (_GetLayouts aclay))
              )
            )
          )
        )
        (t
          (foreach lay aclays
            (vla-put-TabOrder lay
              (1+ (vl-position (strcase (vla-get-Name lay)) dclst))
            )
          )
        )
      )
      (_EndUndo acdoc)
    )
  )

  (princ)
)

;;------------------------------------------------------------;;

(vl-load-com) (princ)

(princ
  (strcat
    "\n:: TabSort.lsp | Version " (vl-string-translate "-" "." TabSortVersion#) " | © Lee Mac 2011 www.lee-mac.com ::"
    "\n:: Type \"TabSort\" to Invoke ::"
  )
)
(princ)

;;------------------------------------------------------------;;
;;                         End of File                        ;;
;;------------------------------------------------------------;;