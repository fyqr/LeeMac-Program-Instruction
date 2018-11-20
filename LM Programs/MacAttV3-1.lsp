;;----------------=={  Global Attribute Editor & Extractor  }==------------------;;
;;                                                                               ;;
;;  Extractor (MacAttExt):-                                                      ;;
;;  -----------------------------                                                ;;
;;                                                                               ;;
;;  The function will retrieve specific attributes from blocks that appear in a  ;;
;;  user compiled list; from all drawings in a specified directory               ;;
;;  (and SubDirectories).                                                        ;;
;;                                                                               ;;
;;  The program will then proceed to write the attribute tags/values to an Excel ;;
;;  file. The format in which attribute tags/values are written to the Excel     ;;
;;  worksheet is dependent on the mode selected in the 'Options' dialog.         ;;
;;                                                                               ;;
;;  If the user chooses to group under Filename, then all blocks will be written ;;
;;  in alphabetical order under the filename of the drawing from which they were ;;
;;  extracted (this is the default mode). If the user chooses to group under     ;;
;;  Block, then all blocks are grouped together and are written in alphabetical  ;;
;;  order.                                                                       ;;
;;                                                                               ;;
;;  Finally, if the user selects Drawing List Mode, then only tags are           ;;
;;  displayed, and all values are written in a list, with the drawing filename   ;;
;;  in the last column. This mode is more suited for a single block extraction   ;;
;;  and may produce undesirable results if extracting multiple blocks with       ;;
;;  varying tags.                                                                ;;
;;                                                                               ;;
;;  The user may also choose to extract block coordinates - with this toggle set ;;
;;  the coordinates of the insertion point of the block from which each          ;;
;;  attribute is extracted is displayed in a column adjacent to the tag listing. ;;
;;                                                                               ;;
;;  The order in which tags are written is controlled by the order in which they ;;
;;  appear in the dialog list (or, if 'ALL TAGS' is displayed, the order in      ;;
;;  which they appear in the block definition). The user may alter this order    ;;
;;  using the 'Up', 'Down', and 'ABC' controls on the dialog interface.          ;;
;;                                                                               ;;
;;  Editor (MacAttEdit):-                                                        ;;
;;  -----------------------------                                                ;;
;;                                                                               ;;
;;  The function will update the value of all tags of a block as specified in a  ;;
;;  user compiled list; from all drawings in a specified directory               ;;
;;  (and SubDirectories).                                                        ;;
;;                                                                               ;;
;;  TO CREATE A DRAWING LIST                                                     ;;
;;  -------------------------------------                                        ;;
;;                                                                               ;;
;;  The Extractor mode of this program may be used to create a drawing list,     ;;
;;  using attributes extracted from titleblocks in the drawings.                 ;;
;;                                                                               ;;
;;  To do this, specify the name(s) of, or select the titleblock(s) you wish to  ;;
;;  extract information from; and specify which attribute information you wish   ;;
;;  to extract.                                                                  ;;
;;                                                                               ;;
;;  Make sure that the program is set to 'Drawing List Mode' within the          ;;
;;  'Options' dialog. In this mode, all tags are grouped together into one list, ;;
;;  and the file from which each titleblock has been extracted is written at the ;;
;;  end of each row.                                                             ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  FUNCTION SYNTAX:  MacAttExt  /  MacAttEdit  / MacAtt                         ;;
;;                                                                               ;;
;;  Notes:-                                                                      ;;
;;  -----------                                                                  ;;
;;  As usual the known bug with ObjectDBX arises when it comes to attribute      ;;
;;  alignment after modification. A sub-function has been included to try to     ;;
;;  allow for the shift in position, however it is not 100% foolproof, and a     ;;
;;  slight shift in position may be noticed. The attribute is realigned when the ;;
;;  block is moved manually. This only affects the Global Attribute Editor.      ;;
;;                           ---------------------------------------------       ;;
;;                                                                               ;;
;;  As expected, when using the Global Attribute Editor, drawing thumbnails are  ;;
;;  lost upon saving with ObjectDBX. These return when the drawing is saved      ;;
;;  manually.                                                                    ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  Author: Lee Mac, Copyright © 2009 - www.lee-mac.com                          ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  Version:                                                                     ;;
;;                                                                               ;;
;;  1.0:  11/06/2009  -  First Release                                           ;;
;;-------------------------------------------------------------------------------;;
;;  1.1:  12/06/2009  -  Added Suggestions.                                      ;;
;;                    -  General Bug Fixes.                                      ;;
;;-------------------------------------------------------------------------------;;
;;  1.2:  24/11/2009  -  Code updated.                                           ;;
;;-------------------------------------------------------------------------------;;
;;  1.3:  17/01/2010  -  Updated to include ability to process SubDirectories.   ;;
;;-------------------------------------------------------------------------------;;
;;  1.4:  18/01/2010  -  Updated Excel format to group attribute tags.           ;;
;;-------------------------------------------------------------------------------;;
;;  1.5:  20/01/2010  -  Added Dialog Interface.                                 ;;
;;-------------------------------------------------------------------------------;;
;;  1.6:  21/01/2010  -  Updated code to account for open drawings within folder ;;
;;-------------------------------------------------------------------------------;;
;;  1.7:  24/01/2010  -  Added ability to select Tags to extract.                ;;
;;                    -  Modified code to correct order of extracted attributes. ;;
;;-------------------------------------------------------------------------------;;
;;  1.8:  29/01/2010  -  Updated code for ObjectDBX Document object aquirement.  ;;
;;-------------------------------------------------------------------------------;;
;;  1.9:  02/02/2010  -  Added option to process current drawing only.           ;;
;;                    -  Updated code to process dynamic blocks.                 ;;
;;                    -  Added Progress Bar.                                     ;;
;;-------------------------------------------------------------------------------;;
;;  2.0:  05/02/2010  -  Added config file to save dialog setup.                 ;;
;;-------------------------------------------------------------------------------;;
;;  2.1:  08/02/2010  -  Added Global Attribute Editor.                          ;;
;;-------------------------------------------------------------------------------;;
;;  2.2:  08/02/2010  -  Added compatibility for older versions.                 ;;
;;-------------------------------------------------------------------------------;;
;;  2.3:  11/02/2010  -  Updated code for Get_Unique function with a 40% speed   ;;
;;                       increase.                                               ;;
;;                    -  In Extractor: Block Tags will be displayed when blocks  ;;
;;                       are selected from drawing.                              ;;
;;                    -  In Editor: Tags and current values will be added to     ;;
;;                       list when block is selected.                            ;;
;;                    -  In Extractor: Block name can be changed when            ;;
;;                       double-clicking on entry.                               ;;
;;-------------------------------------------------------------------------------;;
;;  2.4:  14/02/2010  -  Updated program selection prompt.                       ;;
;;-------------------------------------------------------------------------------;;
;;  2.5:  11/03/2010  -  Added option to allow user to choose whether to write   ;;
;;                       drawing filenames. [Ideal for creating a DWG List].     ;;
;;-------------------------------------------------------------------------------;;
;;  2.6:  17/03/2010  -  Replaced toggle V2.5 addition with Options dialog to    ;;
;;                       choose Excel format.                                    ;;
;;                    -  Sorted data by Block Name in all modes when writing to  ;;
;;                       Excel.                                                  ;;
;;-------------------------------------------------------------------------------;;
;;  2.7:  22/03/2010  -  Added code to allow user to control the order in which  ;;
;;                       attributes are written to Excel.                        ;;
;;-------------------------------------------------------------------------------;;
;;  2.8:  06/04/2010  -  Added ability to extract Block Coordinates.             ;;
;;-------------------------------------------------------------------------------;;
;;  2.9:  21/06/2010  -  Fixed Attribute Tag bug.                                ;;
;;-------------------------------------------------------------------------------;;
;;  3.0:  08/07/2010  -  Fixed bug occurring when file contained no relevant     ;;
;;                       attribute data.                                         ;;
;;-------------------------------------------------------------------------------;;
;;  3.1:  25/10/2010  -  Added fix for Save Path                                 ;;
;;-------------------------------------------------------------------------------;;

(defun c:MacAttExt  nil (MacAtt  t ) (princ)) ;; Extractor

(defun c:MacAttEdit nil (MacAtt nil) (princ)) ;; Editor

;;-------------------------------------------------------------------------------;;

(defun c:MacAtt ( / *error* )

  (defun *error* ( msg )
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ)
  )
    
  (initget "Editor eXtractor")
  
  (if (eq "Editor" (getkword "\n [Editor/eXtractor] <eXtractor> : "))
    (MacAtt nil)
    (MacAtt  t )
  )
)

;;-------------------------------------------------------------------------------;;
;;               --=={  Global Attribute Extractor & Editor }==--                ;;
;;-------------------------------------------------------------------------------;;


(defun MacAtt


  ;;-------------------------------------------------------------------------------;;



  (   ;;         --=={ Input Arguments }==--          ;

      MODE  ; ~  Extractor/Editor Mode [t/nil]

      /

      ;;         --=={ Local Functions }==--          ;

      *ERROR*
      LM:GETSAVEPATH
      LM:WRITECONFIG
      LM:READCONFIG
      PAD
      LM:RELEASEOBJECT
      LM:DIRECTORYDIALOG
      LM:GETALLFILES
      LM:OBJECTDBXDOCUMENT
      UNIQUEASSOC
      LM:POPUP
      LM:WRITEDCL
      LM:LOGO
      MAKE_LIST
      MAKE_BLOCK_LIST
      MAKE_TAG_LIST
      SORTBYFIRST
      DIR_TEXT
      DIR_MODE
      REMOVE_ITEMS
      UNIQUE
      STR-MAKE
      PTR->L
      L->PTR
      LIST_UP
      LIST_DOWN
      TAG_CHOOSER
      TAG_CHOOSER_B
      TAG_EDITOR
      FORMAT_OPTIONS
      CALCINSPT
      
   
      ;;         --=={ Local Variables }==--          ;

      *ACAD
      *ADOC
      *MACATT_CRD*
      *MACATT_CUR*
      *MACATT_DEF*
      *MACATT_DWG*
      *MACATT_LST*
      *MACATT_PAT*
      *MACEDI_BLK*
      *MACEDI_CUR*
      *MACEDI_DEF*
      *MACEDI_LST*
      *MACEDI_PAT*
      ATT$LST
      ATTLST
      BASSOC
      BLK
      BLKASSOC
      BLKLST
      BLK_STR
      BNME
      BSTR
      CFGFNAME
      COL
      CTMP
      DBX
      DCFLAG
      DCFNAME
      DCTAG
      DCTAGTITLE
      DCTITLE
      DOCLST
      DOUBLECLICKTIME
      DWG$LST
      DWLST
      ENT
      EXPRESS
      EXTRA
      FILEPATH
      FILETYPE
      FLAG
      FMODE
      FOLDER
      I
      ITEMS
      ITM
      LST
      MAX_ROW
      NEW_TAG
      NEW_VAL
      OBJ
      OBJNME
      ODOC
      OFILE
      OLD_ROW
      OV
      PROGBAR
      PTR
      ROW
      SAVEPATH
      SHELL
      SI
      SS
      STR
      SYM
      SYMLIST
      TAG
      TAGASSOCLIST
      TAGLST
      TAG_LST
      TAG_STR
      TMP
      UATTRIBS
      UTAGS
      VAL
      VALLIST
      VERSIONNUMBER
      VL
      WSHSHELL
      XLAPP
      XLCELLS
      _F
   
      ;;         --=={ Global Variables }==--         ;

      ; --None--

   
   )
  
  (vl-load-com)
  
  (setq VersionNumber "3.1")
  

  ;;-------------------------------------------------------------------------------;;
  ;;                           --=={  Local Functions  }==--                       ;;
  ;;-------------------------------------------------------------------------------;;


  ;;  --=={  Error Handler  }==--
  
  (defun *error* ( msg )

    (and ofile  (close ofile))
    (and ov     (mapcar (function setvar) vl ov))
    (and dcTag  (unload_dialog dcTag))
    (and Express ProgBar (acet-ui-progress))
    
    (mapcar 'LM:ReleaseObject (list *acad *adoc Shell WSHShell dbx oDoc xlApp xlCells))

    (mapcar
      (function (lambda ( symbol ) (set symbol nil)))

      '(*acad *adoc *MacAtt_def* *MacAtt_Lst* *MacAtt_Pat* *MacAtt_Cur* *MacAtt_dwg* *MacAtt_crd*
                    *MacEdi_Blk* *MacEdi_cur* *MacEdi_def* *MacEdi_Lst* *MacEdi_Pat*)
    )
    
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    
    (princ)
  )

  ;;-------------------------------------------------------------------------------;;

  (defun LM:GetSavePath ( / tmp )
    (cond      
      ( (setq tmp (getvar 'ROAMABLEROOTPREFIX))

        (or (eq "\\" (substr tmp (strlen tmp)))
            (setq tmp (strcat tmp "\\"))
        )
        (strcat tmp "Support")
      )
      ( (setq tmp (findfile "ACAD.pat"))

        (setq tmp (vl-filename-directory tmp))

        (and (eq "\\" (substr tmp (strlen tmp)))
             (setq tmp (substr tmp (1- (strlen tmp))))
        )
        tmp
      )
    )
  )

  ;;-------------------------------------------------------------------------------;;

  (defun LM:WriteConfig ( filename lst / ofile )
    
    (if (setq ofile (open filename "w")) 
      (progn
        (foreach x lst (write-line (vl-prin1-to-string x) ofile))
        
        (setq ofile (close ofile))
        T
      )
    )
  )

  ;;-------------------------------------------------------------------------------;;

  (defun LM:ReadConfig ( filename lst / ofile )

    (if (and (setq filename (findfile filename))
             (setq ofile (open filename "r")))
      (progn
        (foreach x lst (set x (read (read-line ofile))))
        
        (setq ofile (close ofile))
        T
      )
    )
  ) 

  ;;-------------------------------------------------------------------------------;;
  
  (defun Pad ( Str Chc Len )
    (while (< (strlen Str) Len)
      (setq Str (strcat Str (chr Chc)))
    )    
    Str
  )

  ;;-------------------------------------------------------------------------------;;
            
  (defun LM:ReleaseObject ( obj )

    (and obj (eq 'VLA-OBJECT (type obj)) (not (vlax-object-released-p obj))
      (not
        (vl-catch-all-error-p
          (vl-catch-all-apply
            (function vlax-release-object) (list obj)
          )
        )
      )
    )
  )

  ;;-------------------------------------------------------------------------------;;

  (defun LM:DirectoryDialog ( msg dir flag / Shell Fold Path ac HWND )

    (setq Shell (vla-getInterfaceObject (setq ac (vlax-get-acad-object)) "Shell.Application")
          HWND  (vl-catch-all-apply 'vla-get-HWND (list ac))
          Fold  (vlax-invoke-method Shell 'BrowseForFolder (if (vl-catch-all-error-p HWND) 0 HWND) msg flag dir)
    )
    (vlax-release-object Shell)
    
    (if Fold
      (progn
        (setq Path (vlax-get-property (vlax-get-property Fold 'Self) 'Path))
        (vlax-release-object Fold)
        
        (and (= "\\" (substr Path (strlen Path)))
             (setq Path (substr Path 1 (1- (strlen Path)))))
      )
    )
    Path
  )

  ;;-------------------------------------------------------------------------------;;

  (defun LM:GetAllFiles ( Dir Subs Filetype / GetSubFolders )
    
    (defun GetSubFolders ( folder / _f )
      (mapcar
        (function
          (lambda ( f ) (setq _f (strcat folder "\\" f))
            (cons _f (apply (function append) (GetSubFolders _f)))
          )
        )
        (cddr (vl-directory-files folder nil -1))
      )
    )

    (apply (function append)
      (vl-remove (quote nil)
        (mapcar
          (function
            (lambda ( Filepath )
              (mapcar
                (function
                  (lambda ( Filename )
                    (strcat Filepath "\\" Filename)
                  )
                )
                (vl-directory-files Filepath Filetype 1)
              )
            )
          )
          (append (list Dir) (apply (function append) (if subs (GetSubFolders Dir))))
        )
      )
    )
  )
  
  ;;-------------------------------------------------------------------------------;;

  (defun LM:ObjectDBXDocument ( / acVer )

    (vla-GetInterfaceObject (vlax-get-acad-object)
      (if (< (setq acVer (atoi (getvar "ACADVER"))) 16)
        "ObjectDBX.AxDbDocument"
        (strcat "ObjectDBX.AxDbDocument." (itoa acVer))
      )
    )
  )

  ;;-------------------------------------------------------------------------------;;

  (defun UniqueAssoc ( lst / result ass )
    (setq result (list (car lst)))

    (while (setq lst (cdr lst))

      (setq result
        (if (setq ass (assoc (caar lst) result))
          (subst (cons (car ass) (append (cdr ass) (cdar lst))) ass result)
          (cons (car lst) result)
        )
      )
    )

    result
  )

  ;;-------------------------------------------------------------------------------;;
  
  (defun LM:Popup ( title flags msg / WSHShell result )
    
    (setq WSHShell (vlax-create-object "WScript.Shell"))
    (setq result   (vlax-invoke WSHShell 'Popup msg 0 title flags))
    (vlax-release-object WSHShell)

    result
  )

  ;;-------------------------------------------------------------------------------;;

  (defun LM:WriteDCL ( fname / ofile )

    (if (not (findfile fname))

      (if (setq ofile (open fname "w"))
        (progn          

          (foreach str

            '(
              "//----------------------=={  Global Attribute Extractor  }==---------------------//"
              "//                                                                               //"
              "//  MacAtt.dcl to be used in conjunction with MacAtt.lsp.                        //"
              "//                                                                               //"
              "//-------------------------------------------------------------------------------//"
              "//  Author: Lee Mac, Copyright © 2012 - www.lee-mac.com                          //"
              "//-------------------------------------------------------------------------------//"
              ""
              ""
              "//  --=={ Sub-Assembly Definitions }==--"
              ""
              "boxcol : boxed_column {      width =  60; fixed_width  = true; alignment = centered; }"
              "subcol : boxed_column {      width =  40; fixed_width  = true; alignment = centered; }"
              "edit34 :     edit_box { edit_width =  34; fixed_width  = true; alignment = centered; }"
              "edit34r:     edit_box { edit_width =  34; fixed_width  = true; alignment =    right; }"
              "butt3  :       button {      width =   3; fixed_width  = true; alignment = centered; }"
              "butt10 :       button {      width =  10; fixed_width  = true; alignment = centered; }"
              "butt12 :       button {      width =  12; fixed_width  = true; alignment = centered; }"
              "butt20 :       button {      width =  20; fixed_width  = true; alignment = centered; }"
              "space1 :       spacer {     height = 0.1; fixed_height = true; }"
              ""
              ""
              "//-------------------------------------------------------------------------------//"
              "                        //  --=={ Main Dialog Definition }==--  //"
              "//-------------------------------------------------------------------------------//"
              ""
              ""
              "macatt : dialog { key = \"dcl_title\";"
              "  spacer;"
              "  : text { label = \"Copyright (c) 2010 Lee Mac\"; alignment = right; }"
              ""
              "  : boxcol { label = \"Block Entry\";"
              ""
              "    : row {"
              ""
              "      : edit34 { key = \"block_name\"; label = \"Block Name:\"; }"
              "      : butt3  { key = \"block_pick\"; label = \">>\"; }"
              ""
              "    }"
              ""
              "    : row {"
              ""
              "      spacer;"
              ""
              "      : column {"
              ""
              "        space1;"
              "        : toggle { label = \"All Tags\"   ; key = \"tags\"; fixed_width = true; alignment = centered; }"
              "        space1;"
              ""
              "      }"
              ""
              "      : butt20 { label = \"Choose Tags...\"; key = \"tag_button\"; }"
              ""
              "      spacer;"
              "      "
              "    }"
              ""
              "    spacer;"
              ""
              "  }"
              "  "
              "  spacer;"
              ""
              "  : row {"
              ""
              "    spacer;"
              "    : butt20 { key = \"add\"; label = \"Add Block\"; }"
              "    : butt10 { key = \"clr\"; label = \"Clear\"; }"
              "    : butt20 { key = \"rem\"; label = \"Remove Block\"; }"
              "    spacer;"
              ""
              "  }"
              "  "
              "  spacer;"
              ""
              "  : list_box { key = \"block_list\" ; multiple_select = true; fixed_width = false;"
              "               alignment = centered; tabs = \"30\"; tab_truncate = true; }"
              ""
              "  : text   { label = \"Double-Click to Edit Entry   \"; alignment = right; }"
              ""
              "  : boxcol { label = \"Drawing Directory\";"
              ""
              "    : row {"
              ""
              "      : column {"
              ""
              "        space1;"
              "        : text   { key = \"dir_text\"; alignment = left; }"
              "        space1;"
              ""
              "      }"
              ""
              "      : butt10 { label = \"Directory...\"; key = \"dir\"; }"
              ""
              "    }"
              ""
              "    : row {"
              ""
              "      : toggle { key = \"sub_dir\"; label = \"Include Sub-Directories\"; }"
              "      : toggle { key = \"cur_dwg\"; label = \"Current Drawing Only\"   ; }"
              ""
              "    }"
              ""
              "    spacer;"
              "  }"
              ""
              "  spacer;"
              ""
              "  : row {"
              ""
              "    : butt12 { key = \"option\"; label = \"Options\"; }"
              ""
              "    : butt12 { key = \"accept\"; label = \"OK\"; is_default = true; }"
              ""
              "    : butt12 { key = \"cancel\"; label = \"Cancel\"; is_cancel = true; }"
              "    "
              "    : image  { key = \"logo\"; alignment = centered;"
              "               width = 16.06 ; fixed_width  = true;"
              "               height = 2.06 ; fixed_height = true; color = -15; }"
              "  }"
              ""
              "}"
              ""
              ""
              "//-------------------------------------------------------------------------------//"
              "                   //  --=={ Format Options Definition }==--  //"
              "//-------------------------------------------------------------------------------//"
              ""
              ""
              "macatt_opt : dialog { key = \"dcl_opt_title\";"
              "  spacer;"
              ""
              "  : boxed_radio_column { label = \"Format Options\";"
              "    spacer;"
              ""
              "    : radio_button { key = \"grp_file\"  ; label = \" Group by Filename\"; }"
              "    : radio_button { key = \"grp_block\" ; label = \" Group by Block\";    }"
              "    : radio_button { key = \"grp_dwglst\"; label = \" Drawing List Mode\"; }"
              ""
              "    spacer;"
              "  }"
              "  spacer;"
              ""
              "  : row { spacer;"
              ""
              "    : toggle { key = \"coord\"; label = \" Extract Block Coords\"; }"
              ""
              "  }"
              "  spacer;"
              ""
              "  ok_cancel;"
              "}"
              ""
              ""
              "//-------------------------------------------------------------------------------//"
              "                //  --=={ Attribute Tag Dialog Definition }==--  //"
              "//-------------------------------------------------------------------------------//"
              ""
              ""
              "macatt_tags : dialog { key = \"dcl_sub_title\";"
              ""
              "  spacer_1;"
              "  : edit34 { label =   \"Tag:\";  key = \"tag\"; }"
              ""
              "  spacer_1;"
              "  : row {"
              ""
              "    : butt20 { label = \"Add Tag\";    key = \"tag_add\"; }"
              "    : butt20 { label = \"Remove Tag\"; key = \"tag_rem\"; }"
              "    "
              "  }"
              "  spacer;"
              ""
              "  : list_box { label = \"Tags to Extract:\"; multiple_select = true; key = \"tag_list\";"
              "               fixed_width = false; alignment = centered;   }"
              ""
              "  : boxed_column { label = \"Tag Order\";"
              ""
              "    : row {"
              ""
              "      spacer;"
              ""
              "      : butt10 { label = \"Up\"    ; key = \"up\"   ; }"
              "      : butt10 { label = \"Down\"  ; key = \"down\" ; }"
              "      : butt10 { label = \"ABC\"   ; key = \"abc\"  ; }"
              ""
              "      spacer;"
              ""
              "    }"
              ""
              "    spacer;"
              "  }"
              ""
              "  spacer_1;"
              ""
              "  ok_cancel;"
              "}"
              ""
              ""
              "//-------------------------------------------------------------------------------//"
              "       //  --=={ Attribute Tag Dialog Definition [With Block Edit] }==--  //"
              "//-------------------------------------------------------------------------------//"
              ""
              ""
              "macatt_tagsb : dialog { key = \"dcl_sub_title\";"
              ""
              "  spacer_1;"
              "  : edit34r { label =   \"Block:\";  key = \"blk\"; }"
              ""
              "  spacer_1;"
              "  : edit34r { label =     \"Tag:\";  key = \"tag\"; }"
              ""
              "  spacer_1;"
              "  : row {"
              ""
              "    : butt20 { label = \"Add Tag\";    key = \"tag_add\"; }"
              "    : butt20 { label = \"Remove Tag\"; key = \"tag_rem\"; }"
              "  }"
              "  spacer;"
              ""
              "  : list_box { label = \"Tags to Extract:\"; key = \"tag_list\";"
              "               fixed_width = false; multiple_select = true; alignment = centered; }"
              ""
              "  : boxed_column { label = \"Tag Order\";"
              ""
              "    : row {"
              ""
              "      spacer;"
              ""
              "      : butt10 { label = \"Up\"    ; key = \"up\"   ; }"
              "      : butt10 { label = \"Down\"  ; key = \"down\" ; }"
              "      : butt10 { label = \"ABC\"   ; key = \"abc\"  ; }"
              ""
              "      spacer;"
              ""
              "    }"
              ""
              "    spacer;"
              "  }"
              ""
              "  spacer_1;"
              ""
              "  ok_cancel;"
              "}"
              ""
              ""
              "//-------------------------------------------------------------------------------//"
              "                //  --=={ Attribute Editor Dialog Definition }==--  //"
              "//-------------------------------------------------------------------------------//"
              ""
              ""
              "macedit : dialog { key = \"dcl_title\";"
              "  spacer;"
              "  : text { label = \"Copyright (c) 2010 Lee Mac\"; alignment = right; }"
              ""
              "  : boxcol { label = \"Block Entry\";"
              ""
              "    : row {"
              ""
              "      : edit34 { key = \"block_name\"; label = \"Block Name:\"; }"
              "      : butt3  { key = \"block_pick\"; label = \">>\"; }"
              ""
              "    }"
              ""
              "    spacer;"
              ""
              "  }"
              ""
              "  : boxcol { label = \"Tag Entry\";"
              ""
              "    : row {"
              ""
              "      : edit34 { key = \"tag_name\"; label = \"Tag Name:\"; }"
              "      : butt3  { key = \"tag_pick\"; label = \">>\"; }"
              ""
              "    }"
              ""
              "    : row {"
              ""
              "      : edit34  { key = \"new_value\"; label = \"New Value:\"; }"
              "      : spacer  { width = 3; fixed_width = true; }"
              "      "
              "    }"
              ""
              "  spacer;"
              ""
              "  }"
              "  "
              "  spacer;"
              ""
              "  : row {"
              ""
              "    spacer;"
              "    : butt20 { key = \"add\"; label = \"Add Tag\"; }"
              "    : butt10 { key = \"clr\"; label = \"Clear\"; }"
              "    : butt20 { key = \"rem\"; label = \"Remove Tag\"; }"
              "    spacer;"
              ""
              "  }"
              "  "
              "  spacer;"
              ""
              "  : list_box { key = \"tag_list\" ; multiple_select = true;"
              "               fixed_width = false; alignment = centered; tabs = \"30\"; } "
              ""
              "  : text { label = \"Double-Click to Edit Entry   \"; alignment = right; }"
              "    "
              "  : boxcol { label = \"Drawing Directory\";"
              ""
              "    : row {"
              ""
              "      : column {"
              ""
              "        space1;"
              "        : text   { key = \"dir_text\"; alignment = left; }"
              "        space1;"
              ""
              "      }"
              ""
              "      : butt10 { label = \"Directory...\"; key = \"dir\"; }"
              ""
              "    }"
              ""
              "    : row {"
              ""
              "      : toggle { key = \"sub_dir\"; label = \"Include Sub-Directories\"; }"
              "      : toggle { key = \"cur_dwg\"; label = \"Current Drawing Only\"   ; }"
              "      "
              "    }"
              ""
              "    spacer;"
              "  }"
              ""
              "  spacer;"
              ""
              "  : row {"
              ""
              "    : spacer { width = 16.06; fixed_width = true; }"
              ""
              "    ok_cancel;"
              "    "
              "    : image { key = \"logo\"; alignment = centered;"
              "              width = 16.06 ; fixed_width  = true;"
              "              height = 2.06 ; fixed_height = true; color = -15; }"
              "  }"
              "  "
              "}"
              ""
              ""
              "//-------------------------------------------------------------------------------//"
              "                //  --=={ Attribute Tag Dialog Definition }==--  //"
              "//-------------------------------------------------------------------------------//"
              ""
              ""
              "macedit_tags : dialog { key = \"dcl_sub_title_edit\";"
              ""
              "  : subcol { label = \"Tag Entry\";"
              "  "
              "    : edit34r { label = \"Tag Name:\" ; key = \"tag_sub\"; }"
              "    : edit34r { label = \"New Value:\"; key = \"new_value_sub\"; }"
              ""
              "    spacer;"
              "    "
              "  }"
              ""
              "  spacer;"
              ""
              "  ok_cancel;"
              "}"
              ""
              "//-------------------------------------------------------------------------------//"
              "//                           End of Dialog Definition                            //"
              "//-------------------------------------------------------------------------------//"

              )

            (write-line str ofile)
          )

          (setq ofile (close ofile))

          (while (not (findfile fname)))
          
        t)  ; File written successfully
        
    nil) ; Filepath not Found
      
  t)) ; DCL file already exists

  ;;-------------------------------------------------------------------------------;;

  (defun LM:Logo ( key )
    
    (start_image key)
    (mapcar 'vector_image
            '(22 21 1 0 0 0 0 7 0 0 0 0 1 6 6 6 6 7 43 36 27 36 30 21 21 21 22 22 22
              22 21 21 21 28 28 28 27 27 30 29 29 30 52 43 43 43 44 44 46 46 45 45 45
              45 52 52 52 51 51 51 51 51 52 62 65 66 68 68 68 68 67 67 75 75 75 74 74
              73 66 58 58 59 59 59 59 52 57 57 56 56 56 56 57 58 65 65 65 65 66 95 94
              94 92 91 91 91 90 89 89 88 87 86 85 74 74 75 75 76 77 78 79 80 81 82 83
              84 85 86 87 88 88 89 90 91 92 93 94 95 74 73 73 72 72 71 71 71 71 71 71
              71 72 72 72 73 84 83 82 81 80 79 79 78 77 77 76 76 76 76 76 77 77 78 79
              79 80 81 82 83 94 94 95 83 83 82 81 80 79 78 77 76 75 74 84 85 86 87 88
              89 89 90 91 91 91 91 92 95 94 93 92 91 90 89 89 88 87 86 85 84)

            '(20 20 23 23 23 24 24 0 0 0 0 1 1 20 1 1 1 0 2 24 7 15 0 0 0 0 1 1 23 23
              23 24 24 24 24 24 23 23 2 1 1 0 0 0 0 0 1 1 7 23 23 23 24 24 24 24 24 23
              23 1 1 1 0 10 16 19 21 22 23 24 24 24 24 24 24 23 23 22 4 4 5 5 6 6 7 24
              24 24 24 23 23 22 19 16 7 7 6 5 5 22 22 22 17 17 18 18 19 20 20 20 21 21
              21 21 22 23 23 23 24 24 24 25 25 25 25 25 25 25 25 24 24 24 23 23 22 22
              22 22 7 8 8 9 10 11 12 13 14 15 16 17 18 19 19 20 21 21 21 21 20 20 19 19
              18 17 16 15 14 13 12 12 11 10 9 9 8 8 8 7 7 7 7 4 4 4 4 4 4 4 5 5 6 6 7 7
              8 8 8 9 9 9 10 11 11 11 11 7 7 7 6 6 5 5 4 4 4 4 4 4)

            '(21 6 0 0 0 0 21 0 0 0 0 1 1 6 6 6 7 7 36 46 36 30 21 21 21 22 22 22 22 21
              21 21 28 28 28 27 27 27 29 29 30 30 43 43 43 44 44 43 46 45 45 45 45 52 52
              52 51 51 51 51 51 52 52 65 58 68 68 68 68 67 67 75 75 75 74 74 73 65 58 58
              59 59 59 59 51 57 57 56 56 56 56 57 66 62 65 65 65 66 66 94 94 95 91 91 91
              90 89 89 88 87 86 85 84 74 75 75 76 77 78 79 80 81 82 83 84 85 86 87 88 88
              89 90 91 92 93 94 95 92 73 73 72 72 71 71 71 71 71 71 71 72 72 72 73 74 83
              82 81 80 79 79 78 77 77 76 76 76 76 76 77 77 78 79 79 80 81 82 83 84 94 95
              94 83 82 81 80 79 78 77 76 75 74 74 85 86 87 88 89 89 90 91 91 91 91 92 95
              94 93 92 91 90 89 89 88 87 86 85 84 83)

            '(20 20 23 23 24 24 24 0 0 0 1 1 23 1 1 1 0 0 15 7 24 2 0 0 0 1 1 23 23 23 24
              24 24 24 24 23 23 7 1 1 0 0 0 0 0 1 1 2 23 23 23 24 24 24 24 24 23 23 1 1 1
              0 0 16 16 21 22 23 24 24 24 24 24 24 23 23 22 7 4 5 5 6 6 7 22 24 24 24 23
              23 22 19 19 10 7 6 5 5 4 22 22 22 17 18 18 19 20 20 20 21 21 21 21 22 23 23
              23 24 24 24 25 25 25 25 25 25 25 25 24 24 24 23 23 22 22 22 22 17 8 8 9 10 11
              12 13 14 15 16 17 18 19 19 20 21 21 21 21 20 20 19 19 18 17 16 15 14 13 12 12
              11 10 9 9 8 8 8 7 7 7 7 7 4 4 4 4 4 4 5 5 6 6 7 7 8 8 8 9 9 9 10 11 11 11 11
              7 7 7 6 6 5 5 4 4 4 4 4 4 4)

            '(178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178)
    )
    (end_image)
  )

  ;;-------------------------------------------------------------------------------;;

  (defun Make_List ( key lst )
    (start_list key)
    (mapcar (function add_list) lst)
    (end_list)
  )

  ;;-------------------------------------------------------------------------------;;

  (defun Make_Block_List ( key lst )
    (start_list key)
    
    (mapcar (function add_list)
      (mapcar
        (function
          (lambda ( x )
            (strcat
              (if (< 29 (strlen (car x)))
                (strcat (substr (car x) 1 26) "...") (car x)
              )
              "\t"
              (if (cdr x)
                (Str-Make (cdr x) ",") "ALL TAGS"
              )
            )
          )
        )
        lst
      )
    )
    (end_list)
  )

  ;;-------------------------------------------------------------------------------;;

  (defun Make_Tag_List ( key lst )
    (start_list key)
    
    (mapcar (function add_list)
      (mapcar
        (function
          (lambda ( x )
            (strcat
              (if (< 29 (strlen (car x)))
                (strcat (substr (car x) 1 26) "...") (car x)
              )
              "\t"
              (cond ((cdr x)) ("- NO VALUE -"))
            )
          )
        )
        lst
      )
    )
    (end_list)
  )

  ;;-------------------------------------------------------------------------------;;

  (defun SortByFirst ( lst )
    (if lst
      (vl-sort lst
        (function
          (lambda ( a b ) (< (car a) (car b)))
        )
      )
    )
  )

  ;;-------------------------------------------------------------------------------;;

  (defun Dir_Text ( key str )
    (set_tile key
      (if str
        (if (< 45 (strlen str))
          (strcat (substr str 1 42) "...") str
        )
        ""
      )
    )
  )

  ;;-------------------------------------------------------------------------------;;

  (defun Dir_Mode ( val )
    (mapcar
      (function
        (lambda ( x )
          (mode_tile x (atoi val))
        )
      )
      '("sub_dir" "dir" "dir_text")
    )
  )
  
  ;;-------------------------------------------------------------------------------;;

  (defun Remove_Items ( items lst / i ) (setq i -1)
    (vl-remove-if
      (function
        (lambda ( x )
          (vl-position (setq i (1+ i)) items)
        )
      )
      lst
    )
  )

  ;;-------------------------------------------------------------------------------;;

  (defun unique ( lst / result )
    (reverse
      (while (setq itm (car lst))
        (setq lst (vl-remove itm lst) result (cons itm result))
      )
    )
  )

  ;;-------------------------------------------------------------------------------;;

  (defun Str-Make ( lst del / str x ) (setq str (car lst))
    (foreach x (cdr lst)
      (setq str (strcat str del x))
    )
    str
  )

  ;;-------------------------------------------------------------------------------;;

  (defun Ptr->L ( ptr )
    (read (strcat "(" ptr ")"))
  )

  ;;-------------------------------------------------------------------------------;;

  (defun L->Ptr ( lst )
    (vl-string-trim "()" (vl-princ-to-string lst))
  )

  ;;-------------------------------------------------------------------------------;;

  (defun List_Up ( ind lst ) ; Gile
    (cond (  (or (null ind) (null lst)) lst)
          (  (= 0  (car ind))
             (cons (car lst)  (list_up (cdr (mapcar '1- ind)) (cdr lst))))
          (  (= 1  (car ind))
             (cons (cadr lst) (list_up (cdr (mapcar '1- ind)) (cons (car lst) (cddr lst)))))
          (t (cons (car lst)  (list_up (mapcar '1- ind) (cdr lst))))))

  ;;-------------------------------------------------------------------------------;;

  (defun List_Down ( ind lst )
    (reverse
      (list_up
        (reverse
          (mapcar
            (function
              (lambda ( x )
                (- (1- (length lst)) x)
              )
            )
            ind
          )
        )
        (reverse lst)
      )
    )
  )

  ;;-------------------------------------------------------------------------------;;

  (defun Tag_Chooser ( tag lst / str tmp ptr items oldptr )

    (cond (  (not (new_dialog "macatt_tags" tag))

             (LM:Popup "Warning" 16 "Attribute Tag Dialog could not be Loaded"))

          (t

             (Make_List "tag_list" (setq tmp lst))
             (set_tile "dcl_sub_title" dcTagTitle)
             (mode_tile "tag" 2)
           
             (action_tile "tag"      "(setq str $value)")

             (action_tile "tag_list" "(setq ptr $value)")

             (action_tile "abc"
               (vl-prin1-to-string
                 (quote
                   (if tmp
                     (Make_List "tag_list" (setq tmp (acad_strlsort tmp)))
                   )
                 )
               )
             )
                   
             (action_tile "up"
               (vl-prin1-to-string
                 (quote
                   (progn
                     (if ptr
                       (progn
                         (setq oldptr
                           (mapcar
                             (function
                               (lambda ( x ) (nth x tmp))
                             )
                             (Ptr->L ptr)
                           )
                         )                         
                         (Make_List "tag_list" (setq tmp (List_Up (Ptr->L ptr) tmp)))
                         
                         (set_tile "tag_list"
                           (setq ptr
                             (L->Ptr
                               (mapcar
                                 (function
                                   (lambda ( x )
                                     (vl-position x tmp)
                                   )
                                 )
                                 oldptr
                               )
                             )
                           )
                         )
                       )
                       (LM:Popup "Information" 48 "Please Select Tag(s) to Move Up")
                     )
                   )
                 )
               )
             )

             (action_tile "down"
               (vl-prin1-to-string
                 (quote
                   (progn
                     (if ptr
                       (progn
                         (setq oldptr
                           (mapcar
                             (function
                               (lambda ( x ) (nth x tmp))
                             )
                             (Ptr->L ptr)
                           )
                         )                         
                         (Make_List "tag_list" (setq tmp (List_Down (Ptr->L ptr) tmp)))
                         
                         (set_tile "tag_list"
                           (setq ptr
                             (L->Ptr
                               (mapcar
                                 (function
                                   (lambda ( x )
                                     (vl-position x tmp)
                                   )
                                 )
                                 oldptr
                               )
                             )
                           )
                         )
                       )                       
                       (LM:Popup "Information" 48 "Please Select Tag(s) to Move Down")
                     )
                   )
                 )
               )
             )

             (action_tile "tag_add"
               (vl-prin1-to-string
                 (quote
                   (progn
                     (cond
                       ( (or (not str) (eq "" str))

                         (LM:Popup "Information" 48 "Please Enter a Tag to Add"))

                       ( (or (not (snvalid str)) (vl-string-position 32 str))

                         (LM:Popup "Information" 64
                           (strcat "Attribute Tag not Valid"
                             (if (vl-string-position 32 str)
                               "\n\n[ Tag Cannot Contain Spaces ]" ""
                             )
                           )
                         )
                       )                       
                       ( (vl-position (strcase str) tmp)
                        
                         (LM:Popup "Information" 48 "Tag Already Appears in List")
                       )                       
                       (t
                         (set_tile "tag" "")
                        
                         (Make_List "tag_list"
                           (setq tmp
                             (append tmp (list (strcase str)))
                           )
                         )
                         (setq str nil)
                       )
                     )
                   )
                 )
               )
             )

             (action_tile "tag_rem"
               (vl-prin1-to-string
                 (quote
                   (progn
                     (if tmp
                       (if (and ptr (listp (setq items (Ptr->L ptr))))
                         (progn
                           (setq tmp (Remove_Items items tmp) ptr nil)
                           (Make_List "tag_list" tmp)
                         )
                         (LM:Popup "Information" 48 "Please Select a Tag to Remove")
                       )
                       (LM:Popup "Information" 64 "No Tags Found to Remove")
                     )
                   )
                 )
               )
             )

             (action_tile "accept" "(setq lst tmp) (done_dialog)")
           
             (action_tile "cancel" "(done_dialog)")

             (start_dialog)
          )
    )
    (if (listp lst) lst (list lst))
  )

  ;;-------------------------------------------------------------------------------;;

  (defun Tag_Chooser_b ( tag lst / ChkLst bStr str tmp ptr items oldptr )

    (setq chkLst (vl-remove-if
                   (function
                     (lambda ( x )
                       (eq (strcase (car lst)) x)
                     )
                   )
                   (mapcar
                     (function
                       (lambda ( x ) (strcase (car x)))
                     )
                     *MacAtt_Lst*
                   )
                 )
    )

    (cond (  (not (new_dialog "macatt_tagsb" tag))

             (LM:Popup "Warning" 16 "Attribute Tag Dialog could not be Loaded"))

          (t

             (Make_List "tag_list" (setq tmp (cdr lst)))
           
             (set_tile "dcl_sub_title" dcTagTitle)
             (set_tile "blk" (setq bStr (car lst)))
           
             (mode_tile "tag" 2)

             (action_tile "blk"      "(setq bStr $value)")
           
             (action_tile "tag"      "(setq str  $value)")

             (action_tile "tag_list" "(setq ptr  $value)")

             (action_tile "abc"
               (vl-prin1-to-string
                 (quote
                   (if tmp
                     (Make_List "tag_list" (setq tmp (acad_strlsort tmp)))
                   )
                 )
               )
             )
                   
             (action_tile "up"
               (vl-prin1-to-string
                 (quote
                   (progn
                     (if ptr
                       (progn
                         (setq oldptr
                           (mapcar
                             (function
                               (lambda ( x ) (nth x tmp))
                             )
                             (Ptr->L ptr)
                           )
                         )                         
                         (Make_List "tag_list" (setq tmp (List_Up (Ptr->L ptr) tmp)))
                         
                         (set_tile "tag_list"
                           (setq ptr
                             (L->Ptr
                               (mapcar
                                 (function
                                   (lambda ( x )
                                     (vl-position x tmp)
                                   )
                                 )
                                 oldptr
                               )
                             )
                           )
                         )
                       )
                       (LM:Popup "Information" 48 "Please Select Tag(s) to Move Up")
                     )
                   )
                 )
               )
             )

             (action_tile "down"
               (vl-prin1-to-string
                 (quote
                   (progn
                     (if ptr
                       (progn
                         (setq oldptr
                           (mapcar
                             (function
                               (lambda ( x ) (nth x tmp))
                             )
                             (Ptr->L ptr)
                           )
                         )                         
                         (Make_List "tag_list" (setq tmp (List_Down (Ptr->L ptr) tmp)))
                         
                         (set_tile "tag_list"
                           (setq ptr
                             (L->Ptr
                               (mapcar
                                 (function
                                   (lambda ( x )
                                     (vl-position x tmp)
                                   )
                                 )
                                 oldptr
                               )
                             )
                           )
                         )
                       )                       
                       (LM:Popup "Information" 48 "Please Select Tag(s) to Move Down")
                     )
                   )
                 )
               )
             )

             (action_tile "tag_add"
               (vl-prin1-to-string
                 (quote
                   (progn
                     (cond
                       ( (or (not str) (eq "" str))

                         (LM:Popup "Information" 48 "Please Enter a Tag to Add")
                       )                       
                       ( (or (not (snvalid str)) (vl-string-position 32 str))
                        
                         (LM:Popup "Information" 64
                           (strcat "Attribute Tag not Valid"
                             (if (vl-string-position 32 str)
                               "\n\n[ Tag Cannot Contain Spaces ]" ""
                             )
                           )
                         )
                       )                       
                       ( (vl-position (strcase str) tmp)
                        
                         (LM:Popup "Information" 48 "Tag Already Appears in List")
                       )                       
                       (t
                         (set_tile "tag" "")
                        
                         (Make_List "tag_list"
                           (setq tmp (append tmp (list (strcase str))))
                         )
                         (setq str nil)
                       )
                     )
                   )
                 )
               )
             )

             (action_tile "tag_rem"
               (vl-prin1-to-string
                 (quote
                   (progn
                     (if tmp
                       (if (and ptr (listp (setq items (Ptr->L ptr))))
                         (progn
                           (setq tmp (Remove_Items items tmp) ptr nil)
                           (Make_List "tag_list" tmp)
                         )
                         (LM:Popup "Information" 48 "Please Select a Tag to Remove")
                       )
                       (LM:Popup "Information" 64 "No Tags Found to Remove")
                     )
                   )
                 )
               )
             )

             (action_tile "accept"
               (vl-prin1-to-string
                 (quote
                   (progn
                     (cond
                       ( (or (not bStr) (eq "" bStr))

                         (LM:Popup "Information" 64 "Please Enter a Block Name")
                       )                       
                       ( (not (snvalid bStr))
                        
                         (LM:Popup "Information" 48 "Block Name Not Valid")
                       )                       
                       ( (vl-position (strcase bStr) ChkLst)
                        
                         (LM:Popup "Information" 48 "Block Already Appears in List")
                       )                       
                       (t
                         (setq lst (cons bStr tmp))
                        
                         (done_dialog)
                       )
                     )
                   )
                 )
               )
             )

             (action_tile "cancel" "(done_dialog)")

             (start_dialog)
          )
    )
    lst
  )

  ;;-------------------------------------------------------------------------------;;

  (defun Tag_Editor ( tag lst / tmp new_tag new_val )

    (cond (  (not (new_dialog "macedit_tags" tag))

             (LM:Popup "Warning" 16 "Tag Editor Dialog could not be Loaded"))

          (t
             (setq tmp
               (vl-remove-if
                 (function
                   (lambda ( x )
                     (eq (strcase (car lst)) (strcase (car x)))
                   )
                 )
                 *MacEdi_lst*
               )
             )
           
             (set_tile "dcl_sub_title" dcTagTitle)
           
             (set_tile "tag_sub"       (setq new_tag (car lst)))
           
             (set_tile "new_value_sub" (setq new_val (cond ((cdr lst)) (""))))

             (action_tile "tag_sub"       "(setq new_tag $value)")
           
             (action_tile "new_value_sub" "(setq new_val $value)")

             (action_tile "accept"
               (vl-prin1-to-string
                 (quote
                   (progn
                     (cond
                       ( (or (not new_tag) (eq "" new_tag))

                         (LM:Popup "Information" 48 "Please Enter a Tag Name")
                       )                       
                       ( (or (not (snvalid new_tag)) (vl-string-position 32 new_tag))
                        
                         (LM:Popup "Information" 64
                           (strcat "Attribute Tag not Valid"
                             (if (vl-string-position 32 new_tag)
                               "\n\n[ Tag Cannot Contain Spaces ]" ""
                             )
                           )
                         )
                       )                       
                       ( (assoc (setq new_tag (strcase new_tag)) tmp)
                        
                         (LM:Popup "Information" 48 "Tag Already Appears in List")
                       )                       
                       (t
                         (and (eq "" new_val) (setq new_val nil))
                        
                         (setq lst (cons new_tag new_val))
                        
                         (done_dialog)
                       )
                     )
                   )
                 )
               )
             )

             (action_tile "cancel" "(done_dialog)")

             (start_dialog)
          )
    )
    lst
  )

  ;;-------------------------------------------------------------------------------;;

  (defun Format_Options ( tag fmode / tmp ctmp )

    (cond (  (not (new_dialog "macatt_opt" tag))

             (LM:Popup "Warning" 16 "Option Dialog could not be Loaded"))

          (t

             (set_tile (setq tmp fmode) "1")
             (set_tile "dcl_opt_title" "Options")

             (setq ctmp (set_tile "coord" *MacAtt_crd*))
 
             (mapcar
               (function
                 (lambda ( tile )
                   (action_tile tile
                     (strcat "(setq tmp " (vl-prin1-to-string tile) " )")
                   )
                 )
               )
               '("grp_file" "grp_block" "grp_dwglst")
             )

             (action_tile "coord"  "(setq ctmp $value)")

             (action_tile "accept" "(setq fmode tmp *MacAtt_crd* ctmp) (done_dialog)")

             (action_tile "cancel" "(done_dialog)")

             (start_dialog)
          )
    )
    fmode
  )

  ;;-------------------------------------------------------------------------------;;

  (defun CalcInsPt ( obj str / eLst Alig )
    ;; Modification of VovKa's Routine

    (setq eLst (entget (vlax-vla-object->ename obj))
          Alig (cdr (assoc 72 eLst)))

    (polar
      (vlax-get obj 'InsertionPoint)
      (vla-get-Rotation obj)
      
      (*
        (apply (function +)
          (mapcar
            (function
              (lambda ( e1 e2 ) (- (car e1) (car e2)))
            )          
            (textbox eLst)
            (textbox (subst (cons 1 str) (assoc 1 eLst) eLst))
          )
        )      
        (cond
          ( (or (= Alig 1) (= Alig 4)) 0.5)
          
          ( (= Alig 2) 1.0)
          
          (t 0.0)
        )
      )
    )
  )

  ;;-------------------------------------------------------------------------------;;
  ;;                           --=={  Preliminaries  }==--                         ;;
  ;;-------------------------------------------------------------------------------;;
  
  (if (not (vl-file-directory-p (setq SavePath (LM:GetSavePath))))
    (progn
      (LM:Popup "Warning" 16 "Save Path not Valid")
      (exit)
    )
  )  

  (setq dcfname    (strcat SavePath "\\LMAC_MacAtt_V" VersionNumber ".dcl")

        cfgfname   (strcat SavePath "\\LMAC_MacAtt_V" VersionNumber ".cfg"))

  (setq DoubleClickTime 0.0000011667)  ;; Increase to allow for slower double-click
  
  (setq *acad (vlax-get-acad-object)
        *adoc (vla-get-ActiveDocument *acad))

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
  
  ;;-------------------------------------------------------------------------------;;
  ;;                           --=={  Main Function  }==--                         ;;
  ;;-------------------------------------------------------------------------------;;

  (setq SymList '(*MacAtt_def* *MacAtt_lst* *MacAtt_pat* *MacAtt_cur* *MacAtt_dwg* *MacAtt_crd*
                  *MacEdi_def* *MacEdi_lst* *MacEdi_pat* *MacEdi_cur* *MacEdi_blk*)

        ValList  (list "1" 'nil (getvar 'DWGPREFIX) "0" "grp_file" "0" "1" 'nil (getvar 'DWGPREFIX) "0" 'nil)
  )  

  (setq vl '("DIMZIN") ov (mapcar (function getvar) vl))
  (mapcar (function setvar) vl '(0))
  

  ;;                         --=={  Setup Defaults  }==--                          ;;

  (or (findfile cfgfname)
      (LM:WriteConfig cfgfname ValList))

  (LM:ReadConfig cfgfname SymList)

  (mapcar '(lambda ( sym val ) (or (boundp sym) (set sym val))) SymList ValList)

  ;;-------------------------------------------------------------------------------;;
  

  (cond (  (not (LM:WriteDCL dcfname))

           (LM:Popup "Warning" 16 "Dialog Definition File could not be Written")
           (princ "\n** DCL File Could not be Written **"))

        (  (<= (setq dcTag (load_dialog dcfname)) 0)

           (LM:Popup "Warning" 16 "Dialog Definition File could not be Found")
           (princ "\n** DCL File could not be Found **"))
        

        (Mode

           ;;-------------------------------------------------------------------------------;;

           ;;                           --=={  Extractor Mode  }==--                        ;;

           ;;-------------------------------------------------------------------------------;;
         

           (setq dcTitle    (strcat "Global Attribute Extractor V" VersionNumber)

                 dcTagTitle "Attributes to Extract")

           ;;                        --=={  Begin DCL While Loop  }==--                     ;;

           (while (not (vl-position dcFlag '(1 0)))

             (cond (  (not (new_dialog "macatt" dcTag))

                      (LM:Popup "Warning" 16 "Global Attribute Extractor Dialog could not be Loaded")
                      (princ "\n** DCL could not be Loaded **")

                      (setq dcFlag 0))

                   (t

                      (Make_Block_List "block_list" (setq *MacAtt_Lst* (SortByFirst *MacAtt_Lst*)))
             
                      (Dir_Text  "dir_text"   *MacAtt_pat*)
                      (set_tile  "sub_dir"    *MacAtt_def*)
                      (set_tile  "dcl_title"       dcTitle)
                      
                      (set_tile  "tags" "1")
                      (mode_tile "tag_button" 1)

                      (LM:Logo "logo")

                      (Dir_Mode (set_tile "cur_dwg" *MacAtt_cur*))

                      (action_tile "cur_dwg" "(Dir_Mode (setq *MacAtt_cur* $value))")

                      (action_tile "option"  "(setq *MacAtt_dwg* (Format_Options dcTag *MacAtt_dwg*))")

                      (action_tile "dir"
                        (vl-prin1-to-string
                          (quote
                            (progn
                              (if (setq tmp (LM:DirectoryDialog "Select Directory of Drawings to Process..." nil 0))
                                (Dir_Text "dir_text" (setq *MacAtt_pat* tmp))
                              )
                            )
                          )
                        )
                      )

                      (action_tile "rem"
                        (vl-prin1-to-string
                          (quote
                            (progn
                              (if *MacAtt_Lst*
                                (if (and ptr (listp (setq items (Ptr->L ptr))))
                                  (progn
                                    (setq *MacAtt_Lst* (Remove_Items items *MacAtt_Lst*) ptr nil)
                                    (Make_Block_List "block_list" (setq *MacAtt_Lst* (SortByFirst *MacAtt_Lst*)))
                                  )                                
                                  (LM:Popup "Information" 48 "Please Select a Block from the List to Remove")
                                )
                                (LM:Popup "Information" 64 "No Blocks Found to Remove")
                              )
                            )
                          )
                        )
                      )

                      (action_tile "add"
                        (vl-prin1-to-string
                          (quote
                            (progn
                              (cond
                                ( (or (not blk_str) (eq "" blk_str))

                                  (LM:Popup "Information" 64 "Please Enter a Block Name")
                                )                                
                                ( (not (snvalid blk_str))
                                 
                                  (LM:Popup "Information" 48 "Block Name Not Valid")
                                )                                
                                ( (vl-position (strcase blk_str)
                                    (mapcar
                                      (function
                                        (lambda ( x ) (strcase (car x)))
                                      )
                                      *MacAtt_Lst*
                                    )
                                  )
                                 
                                  (LM:Popup "Information" 48 "Block Already Appears in List")
                                )                                
                                (t
                                  (set_tile "block_name" "")
                                 
                                  (and (eq "1" (get_tile "tags")) (setq tagLst nil))
                                 
                                  (Make_Block_List "block_list"
                                    (setq *MacAtt_Lst*
                                      (SortByFirst (cons (cons blk_str tagLst) *MacAtt_Lst*))
                                    )
                                  )
                                 
                                  (set_tile  "tags" "1")
                                  (mode_tile "tag_button" 1)
                                 
                                  (setq blk_str nil tagLst nil)
                                )
                              )
                            )
                          )
                        )
                      )

                      (action_tile "tags"
                        (vl-prin1-to-string
                          (quote
                            (progn
                              (mode_tile "tag_button" (atoi $value))
                            )
                          )
                        )
                      )

                      (action_tile "clr"
                        (vl-prin1-to-string
                          (quote
                            (progn
                              (Make_Block_List "block_list" (setq *MacAtt_Lst* nil))
                            )
                          )
                        )
                      )
                    
                      (action_tile "tag_button" "(setq tagLst (Tag_Chooser dcTag tagLst))")

                      (action_tile "sub_dir"    "(setq *MacAtt_def* $value)")

                      (action_tile "block_name" "(setq blk_str $value)")

                      (action_tile "block_list"
                        (vl-prin1-to-string
                          (quote
                            (progn
                              (setq #st (getvar "DATE") ptr $value)
                              
                              (if (and (eq dclkptr $value)
                                       (< (abs (read (rtos (- #en #st) 2 10))) DoubleClickTime))
                                (progn

                                  (setq n (nth (atoi ptr) *MacAtt_Lst*))

                                  (Make_Block_List "block_list"
                                    (setq *MacAtt_Lst*
                                      (SortByFirst
                                        (subst
                                          (Tag_Chooser_b dcTag n) n *MacAtt_Lst*
                                        )
                                      )
                                    )
                                  )
                                  
                                  (setq dclkptr nil)
                                )
                                
                                (setq #en (getvar "DATE") dclkptr $value)
                              )
                            )
                          )
                        )
                      )

                      (action_tile "block_pick" "(done_dialog 2)")
                      
                      (action_tile "accept"
                        (vl-prin1-to-string
                          (quote
                            (progn
                              (cond
                                ( (not *MacAtt_Lst*)

                                  (LM:Popup "Information" 64 "Please Add a Block to the List")
                                )
                                (t
                                  (done_dialog 1)
                                )
                              )
                            )
                          )
                        )
                      )                              
                              
                      (action_tile "cancel"     "(done_dialog 0)")
                    
                      (setq dcflag (start_dialog))
                   )
             )

             ;;-------------------------------------------------------------------------------;;

             (if (= dcflag 2)
               
               (if (setq si -1 ss (ssget '((0 . "INSERT") (66 . 1))))
                 
                 (while (setq ent (ssname ss (setq si (1+ si))))
                   (setq obj (vlax-ename->vla-object ent))
                   
                   (if
                     (not
                       (vl-position
                         (strcase
                           (setq bNme
                             (cond
                               (  (vlax-property-available-p obj 'EffectiveName)

                                  (vla-get-EffectiveName obj)
                               )
                               (t (vla-get-Name obj))
                             )
                           )
                         )
                         (mapcar (function strcase)
                           (mapcar (function car) *MacAtt_Lst*)
                         )
                       )
                     )
                     
                     (setq *MacAtt_Lst*
                       (cons
                         (cons bNme
                           (mapcar (function vla-get-TagString)
                             (append
                               (vlax-invoke Obj 'GetAttributes)
                               (vlax-invoke Obj 'GetConstantAttributes)
                             )
                           )
                         )
                         *MacAtt_Lst*
                       )
                     )
                   )
                 )
               )
             )
           )

           ;;                         --=={  End of DCL While Loop  }==--                   ;;

           (setq dcTag (unload_dialog dcTag))

           (if (= 1 dcflag)
             (progn
               
               (setq BlkLst
                 (mapcar
                   (function
                     (lambda ( x ) (cons (strcase (car x)) (cdr x)))
                   )
                   *MacAtt_Lst*
                 )
               )               
      
               (vlax-for doc (vla-get-Documents *acad)
                 (setq DocLst
                   (cons
                     (cons (strcase (vla-get-fullname doc)) doc) DocLst
                   )
                 )
               )

               (setq dbx (LM:ObjectDBXDocument))               

               (setq dwLst
                 (cond
                   ( (eq "1" *MacAtt_cur*)
                                               
                     (list
                       (cond
                         (  (eq "" (vla-get-FullName *adoc))
                            
                            (strcat (vla-get-Path *adoc) (vla-get-Name *adoc))
                         )
                         (t (vla-get-FullName *adoc))
                       )
                     )
                   )                   
                   ( (LM:GetAllFiles *MacAtt_pat* (eq "1" *MacAtt_def*) "*.dwg") )
                 )
               )

               (if Express (setq ProgBar (acet-ui-progress "Extracting..." (length dwLst))))

               (foreach dwg dwLst

                 (if Express (acet-ui-progress -1))                 

                 (cond (  (setq flag (eq "1" *MacAtt_cur*))

                          (setq oDoc *adoc))

                       (  (setq flag (and (setq oDoc (cdr (assoc (strcase dwg) DocLst))))))

                       (t (setq flag (not (vl-catch-all-error-p
                                            (vl-catch-all-apply
                                              (function vla-open) (list dbx dwg)))))
                          (setq oDoc dbx)))

                 (if flag
                   (progn
                     
                     (vlax-for lay (vla-get-Layouts oDoc)

                       (vlax-for Obj (vla-get-Block lay)

                         (if
                           (and
                             (eq (vla-get-ObjectName Obj) "AcDbBlockReference")
                             (eq :vlax-true (vla-get-HasAttributes Obj))
                             (setq BlkAssoc
                               (assoc
                                 (strcase
                                   (setq ObjNme
                                     (cond
                                       (  (vlax-property-available-p obj 'EffectiveName)
                                        
                                          (vla-get-EffectiveName Obj))
                                       
                                       (t (vla-get-Name Obj))
                                     )
                                   )
                                 )
                                 BlkLst
                               )
                             )
                           )
                           (progn
                             (setq uAttribs (mapcar 'strcase (cdr BlkAssoc)))

                             (foreach Att (append (vlax-invoke Obj 'GetAttributes)
                                                  (vlax-invoke Obj 'GetConstantAttributes))

                               (if (or (not uAttribs)
                                       (vl-position
                                         (strcase (vla-get-TagString Att)) uAttribs))

                                 (setq AttLst
                                   (cons
                                     (cons (vla-get-TagString Att) (list (vla-get-TextString Att))) AttLst
                                   )
                                 )
                               )
                             )                             

                             (if (eq "grp_dwglst" *MacAtt_dwg*)
                               (setq AttLst (cons (cons "CAD Filename" (list dwg)) AttLst))
                             )

                             (if (eq "1" *MacAtt_crd*)
                               (setq AttLst
                                 (cons
                                   (cons "Block Coords"
                                     (list
                                       (Str-Make
                                         (mapcar (function rtos)
                                           (vlax-get Obj 'InsertionPoint)
                                         )
                                         ","
                                       )
                                     )
                                   )
                                   AttLst
                                 )
                               )
                             )

                             (setq Att$lst (cons (cons ObjNme AttLst) Att$lst) AttLst nil)
                           )
                         )
                       )
                     )

                     (if Att$Lst
                       (setq Dwg$Lst
                         (cons
                           (cons dwg
                             (SortByFirst
                               (mapcar
                                 (function
                                   (lambda ( x )
                                     (cons (car x) (UniqueAssoc (cdr x)))
                                   )
                                 )
                                 (UniqueAssoc Att$lst)
                               )
                             )
                           )
                           Dwg$lst
                         )
                         Att$lst nil
                       )
                       (princ (strcat "\n-- No Attribute Data Found in Drawing: " (vl-filename-base dwg) ".dwg --"))
                     )
                   )
                   
                   (princ (strcat "\n** Error Opening File: " (vl-filename-base dwg)  ".dwg **"))
                 )
                 
               ) ; Foreach

               (if Express (setq ProgBar (acet-ui-progress)))

  ;;-------------------------------------------------------------------------------;;

               (if (and Dwg$lst (apply 'or (apply 'append (mapcar 'cadr Dwg$Lst))))
                 (progn

                   (setq xlApp     (vlax-get-or-create-object "Excel.Application")
                         
                         xlCells   (vlax-get-property
                                     (vlax-get-property
                                       (vlax-get-property
                                         (vlax-invoke-method
                                           (vlax-get-property xlApp "Workbooks")
                                             "Add"
                                         )
                                         "Sheets"
                                       )
                                       "Item" 1
                                     )
                                     "Cells"
                                   )
                   )

  ;;-------------------------------------------------------------------------------;;

                   (cond (  (eq "grp_file" *MacAtt_dwg*)
                   
                            (setq col 1 row 1 max_row 1)
                            (foreach Dwg (reverse Dwg$Lst)

                              (vlax-put-property xlCells 'Item row col (car Dwg))
                              (setq row (1+ row))
                              
                              (foreach Blk (cdr Dwg)
                               
                                (vlax-put-property xlCells 'Item row col (car Blk))
                                (setq row (1+ row))

                                (setq bAssoc (assoc (strcase (car Blk)) BlkLst))
                                
                                (foreach Tag
                                   (if (cdr bAssoc)
                                     (mapcar
                                       (function
                                         (lambda ( x ) (assoc x (cdr Blk)))
                                       )                                       
                                       (if (eq "1" *MacAtt_crd*)
                                         (append (cdr bAssoc) '("Block Coords"))
                                         (cdr bAssoc)
                                       )
                                     )
                                     (cdr Blk)
                                   )
                                  
                                  (setq old_row row)
                                  
                                  (vlax-put-property xlCells 'Item row col (car Tag))
                                  (setq row (1+ row))

                                  (foreach Val (cdr Tag)
                                    
                                    (vlax-put-property xlCells 'Item row col Val)
                                    (setq row (1+ row))

                                    (if (< max_row row) (setq max_row row))
                                  )
                                  (setq col (1+ col) row old_row)
                                )
                                (setq col 1 row (1+ max_row))
                              )
                              (setq row (1+ row))
                            )
                         )
                         (  (eq "grp_block" *MacAtt_dwg*)

                            (setq Dwg$Lst
                              (SortByFirst
                                (mapcar
                                  (function
                                    (lambda ( x )
                                      (cons (car x)
                                        (reverse (UniqueAssoc (cdr x)))
                                      )
                                    )
                                  )
                                  (UniqueAssoc
                                    (apply (function append)
                                      (mapcar (function cdr) Dwg$Lst)
                                    )
                                  )
                                )
                              )
                            )

                            (setq col 1 row 1 max_row 1)
                            (foreach Blk Dwg$Lst

                              (vlax-put-property xlCells 'Item row col (car Blk))
                              (setq row (1+ row))

                              (setq bAssoc (assoc (strcase (car Blk)) BlkLst))

                              (foreach Tag
                                 (if (cdr bAssoc)
                                   (mapcar
                                     (function
                                       (lambda ( x ) (assoc x (cdr Blk)))
                                     )                                     
                                     (if (eq "1" *MacAtt_crd*)
                                       (append (cdr bAssoc) '("Block Coords"))
                                       (cdr bAssoc)
                                     )
                                   )
                                   (cdr Blk)
                                 )
                                
                                (setq old_row row)

                                (vlax-put-property xlCells 'Item row col (car Tag))
                                (setq row (1+ row))

                                (foreach Val (cdr Tag)

                                  (vlax-put-property xlCells 'Item row col Val)
                                  (setq row (1+ row))

                                  (if (< max_row row) (setq max_row row))
                                )
                                (setq col (1+ col) row old_row)
                              )
                              (setq col 1 row (1+ max_row))
                            )
                         )
                         (t

                            (setq Dwg$Lst
                              (reverse
                                (UniqueAssoc                                  
                                  (apply (function append)
                                    (mapcar (function cdr)
                                      (apply (function append)
                                        (mapcar (function cdr) Dwg$Lst)
                                      )
                                    )
                                  )                                  
                                )
                              )
                            )

                            (setq TagAssocList
                              (vl-remove 'nil
                                (append 
                                  (setq UTags
                                    (Unique
                                      (apply (function append)
                                        (mapcar (function cdr) BlkLst)
                                      )
                                    )
                                  )
                                  (  (lambda ( data / extra )
                                       (mapcar
                                         (function
                                           (lambda ( x )
                                             (if (not (or (vl-position x UTags)
                                                          (vl-position x '("CAD Filename" "Block Coords"))))
                                               (setq extra (cons x extra))
                                             )
                                           )
                                         )                                         
                                         (mapcar (function car) data)
                                       )                                       
                                       (reverse extra)
                                     )
                                    Dwg$Lst
                                  )
                                  '("CAD Filename" "Block Coords")
                                )
                              )
                            )
                          
                            (setq col 1 row 1)
                          
                            (foreach Tag
                              (mapcar
                                (function
                                  (lambda ( x ) (assoc x Dwg$Lst))
                                )
                                TagAssocList
                              )

                              (vlax-put-property xlCells 'Item row col (car Tag))
                              (setq row (1+ row))

                              (foreach Val (cdr Tag)

                                (vlax-put-property xlCells 'Item row col val)
                                (setq row (1+ row))
                              )
                              (setq col (1+ col) row 1)
                            )
                          )
                   )
                   
                   ;;-------------------------------------------------------------------------------;;
                   
                   (vla-put-visible xlApp :vlax-true)

                   (princ (strcat "\n<< " (itoa (length dwLst)) " Drawings Processed >>"))
                 )
                 
                 (princ "\n** No Data Found to Write **")
               )
               
               (LM:WriteConfig cfgfname (mapcar 'eval SymList))
             )             
             (princ "*Cancel*")
           )

           (mapcar 'LM:ReleaseObject (list dbx oDoc xlApp xlCells)) (gc) (gc)
        )        

;;-------------------------------------------------------------------------------;;

        (t

           ;;-------------------------------------------------------------------------------;;

           ;;                             --=={  Editor Mode  }==--                         ;;

           ;;-------------------------------------------------------------------------------;;
         

           (setq dcTitle    (strcat "Global Attribute Editor V" VersionNumber)

                 dcTagTitle "New Attribute Value")

           ;;                           --=={  Begin DCL While Loop  }==--                  ;;

           (while (not (vl-position dcflag '(1 0)))

             (cond (  (not (new_dialog "macedit" dcTag))

                      (LM:Popup "Warning" 16 "Global Attribute Editor Dialog could not be Loaded")
                      (princ "\n** DCL could not be Loaded **")

                      (setq dcFlag 0))

                   (t

                      (Make_Tag_List "tag_list" (setq *MacEdi_Lst* (SortByFirst *MacEdi_Lst*)))

                      (Dir_Text  "dir_text"   *MacEdi_pat*)
                      (set_tile  "sub_dir"    *MacEdi_def*)
                      (set_tile  "dcl_title"     dcTitle  )

                      (set_tile "block_name" (cond ( *MacEdi_blk* ) ("")))
                      (set_tile "tag_name"   (cond (    tag_str   ) ("")))
                      (set_tile "new_value"  (cond (    new_tag   ) ("")))
                    
                      (LM:Logo "logo")

                      (Dir_Mode (set_tile "cur_dwg" *MacEdi_cur*))

                      (action_tile "cur_dwg" "(Dir_Mode (setq *MacEdi_cur* $value))")

                      (action_tile "sub_dir" "(setq *MacEdi_def* $value)")
 
                      (action_tile "dir"
                        (vl-prin1-to-string
                          (quote
                            (progn
                              (if (setq tmp (LM:DirectoryDialog "Select Directory of Drawings to Process..." nil 0))
                                (Dir_Text "dir_text" (setq *MacEdi_pat* tmp))
                              )
                            )
                          )
                        )
                      )

                      (action_tile "rem"
                        (vl-prin1-to-string
                          (quote
                            (progn
                              (if *MacEdi_Lst*
                                (if (and ptr (listp (setq items (read (strcat "(" ptr ")")))))
                                  (progn
                                    (setq *MacEdi_Lst* (Remove_Items items *MacEdi_Lst*) ptr nil)
                                    (Make_Tag_List "tag_list" (setq *MacEdi_Lst* (SortByFirst *MacEdi_Lst*)))
                                  )                                
                                  (LM:Popup "Information" 48 "Please Select a Tag from the List to Remove")
                                )
                                (LM:Popup "Information" 64 "No Tags Found to Remove")
                              )
                            )
                          )
                        )
                      )

                      (action_tile "add"
                        (vl-prin1-to-string
                          (quote
                            (progn
                              (cond
                                (  (or (not *MacEdi_blk*) (eq "" *MacEdi_blk*))

                                   (LM:Popup "Information" 64 "Please Enter a Block Name")
                                )
                                (  (not (snvalid *MacEdi_blk*))
                                 
                                   (LM:Popup "Information" 48 "Block Name Not Valid")
                                )                                
                                (  (or (not tag_str) (eq "" tag_str))
                                 
                                   (LM:Popup "Information" 64 "Please Enter a Tag Name")
                                )                                
                                (  (or (not (snvalid tag_str)) (vl-string-position 32 tag_str))
                                 
                                   (LM:Popup "Information" 64
                                     (strcat "Attribute Tag not Valid"
                                       (if (vl-string-position 32 tag_str)
                                         "\n\n[ Tag Cannot Contain Spaces ]" ""
                                       )
                                     )
                                   )
                                )                                
                                (  (vl-position (strcase tag_str)
                                     (mapcar
                                       (function
                                         (lambda ( x ) (strcase (car x)))
                                       )
                                       *MacEdi_Lst*
                                     )
                                   )
                                 
                                   (LM:Popup "Information" 48 "Tag Already Appears in List")
                                )                                
                                (t (mapcar (function set_tile) '("tag_name" "new_value") '("" ""))
                                 
                                   (and (eq "" new_tag) (setq new_tag nil))
                                 
                                   (Make_Tag_List "tag_list"
                                     (setq *MacEdi_Lst*
                                       (SortByFirst
                                         (cons (cons (strcase tag_str) new_tag) *MacEdi_Lst*)
                                       )
                                     )
                                   )
                                 
                                   (setq tag_str nil new_tag nil)
                                )
                              )
                            )
                          )
                        )
                      )
                    
                      (action_tile "clr"
                        (vl-prin1-to-string
                          (quote
                            (progn
                              (Make_Tag_List "tag_list" (setq *MacEdi_Lst* nil))
                            )
                          )
                        )
                      )

                      (action_tile "block_name" "(setq *MacEdi_blk* $value)")
                    
                      (action_tile "tag_name"   "(setq tag_str $value)")

                      (action_tile "new_value"  "(setq new_tag $value)")

                      (action_tile "block_pick" "(done_dialog 2)")

                      (action_tile "tag_pick"
                        (vl-prin1-to-string
                          (quote
                            (progn
                              (cond
                                (  (or (not *MacEdi_blk*) (eq "" *MacEdi_blk*))

                                   (LM:Popup "Information" 64 "Please Enter a Block Name")
                                )                                
                                (  (not (snvalid *MacEdi_blk*))
                                 
                                   (LM:Popup "Information" 48 "Block Name Not Valid")
                                )                                
                                (  (not (tblsearch "BLOCK" *MacEdi_blk*))
                                 
                                   (LM:Popup "Information" 64
                                     (strcat "Block Not Found in Drawing"
                                       "\nBlock must appear in Drawing for Tags to be Selectable"
                                     )
                                   )
                                )                                
                                (t (done_dialog 3) )
                              )
                            )
                          )
                        )
                      )

                      (action_tile "tag_list"
                        (vl-prin1-to-string
                          (quote
                            (progn
                              (setq #st (getvar "DATE") ptr $value)
                              
                              (if (and (eq dclkptr $value)
                                       (< (abs (read (rtos (- #en #st) 2 10))) DoubleClickTime))
                                (progn

                                  (setq n (nth (atoi ptr) *MacEdi_Lst*))

                                  (Make_Tag_List "tag_list"
                                    (setq *MacEdi_Lst*
                                      (SortByFirst
                                        (subst
                                          (Tag_Editor dcTag n) n *MacEdi_Lst*
                                        )
                                      )
                                    )
                                  )

                                  (setq dclkptr nil)
                                )                                
                                (setq #en (getvar "DATE") dclkptr $value)
                              )
                            )
                          )
                        )
                      )

                      (action_tile "accept"
                        (vl-prin1-to-string
                          (quote
                            (progn
                              (cond
                                (  (or (not *MacEdi_blk*) (eq "" *MacEdi_blk*))

                                   (LM:Popup "Information" 64 "Please Enter a Block Name")
                                )                                
                                (  (not (snvalid *MacEdi_blk*))
                                 
                                   (LM:Popup "Information" 48 "Block Name Not Valid")
                                )                                
                                (  (not *MacEdi_Lst*)
                                 
                                   (LM:Popup "Information" 64 "Please Add a Tag to the List")
                                )                                
                                (t (done_dialog 1) )
                              )
                            )
                          )
                        )
                      )
                    
                      (action_tile "cancel" "(done_dialog 0)")

                      (setq dcflag (start_dialog))
                   )
             )

             ;;-------------------------------------------------------------------------------;;

             (cond (  (= 2 dcflag)

                      (while
                        (progn
                          (setq ent (car (entsel "\nSelect Block: ")))

                          (cond (  (eq 'ENAME (type ent))

                                   (if (and (eq "INSERT" (cdr (assoc 0 (entget ent))))
                                            (= 1 (cdr (assoc 66 (entget ent)))))

                                     (not
                                       (setq *MacEdi_blk* (cond (  (vlax-property-available-p
                                                                      (setq obj (vlax-ename->vla-object ent)) 'EffectiveName)
                                                                    
                                                                    (vla-get-EffectiveName obj))

                                                                 (t (vla-get-Name obj)))
                                                
                                              *MacEdi_Lst* (mapcar
                                                             (function
                                                               (lambda ( x ) (cons (vla-get-TagString x)
                                                                                   (vla-get-TextString x)))
                                                             )
                                                             (vlax-invoke obj 'GetAttributes)
                                                           )
                                       )
                                     )
                                     
                                     (princ "\n** Object must be an Attributed Block **")
                                   )
                                )
                          )
                        )
                      )
                   )
                   (  (= 3 dcflag)

                      (while
                        (progn
                          (setq ent (car (nentsel "\nSelect Attribute: ")))

                          (cond (  (eq 'ENAME (type ent))

                                   (if (eq "ATTRIB" (cdr (assoc 0 (entget ent))))

                                     (if
                                       (progn
                                         (setq obj
                                           (vla-objectidtoobject *adoc
                                             (vla-get-ownerid
                                               (vlax-ename->vla-object ent)
                                             )
                                           )
                                         )
                                         (eq (strcase *MacEdi_blk*)
                                           (strcase
                                             (cond
                                               (  (vlax-property-available-p obj 'EffectiveName)
                                                    
                                                   (vla-get-EffectiveName obj)
                                               )
                                               (t (vla-get-Name obj) )
                                             )
                                           )
                                         )
                                       )

                                       (not (setq tag_str (cdr (assoc 2 (entget ent)))
                                                  new_tag (cdr (assoc 1 (entget ent)))))

                                       (princ (strcat "\n** Tag must belong to block: " *MacEdi_blk* " **"))
                                     )

                                     (princ "\n** Object must be an Attribute **")
                                   )
                                )
                          )
                        )
                      )
                   )
             )
           )

           ;;                        --=={  End of DCL While Loop  }==--                    ;;

           (setq dcTag (unload_dialog dcTag))

           (if (= 1 dcflag)
             (progn

               (setq Tag_Lst
                 (mapcar
                   (function
                     (lambda ( x ) (cons (strcase (car x)) (cdr x)))
                   )
                   *MacEdi_Lst*
                 )
               )

               (vlax-for doc (vla-get-Documents *acad)
                 (setq DocLst
                   (cons
                     (cons (strcase (vla-get-fullname doc)) doc) DocLst
                   )
                 )
               )

               (setq dbx (LM:ObjectDBXDocument))

               (setq dwLst
                 (cond
                   ( (eq "1" *MacEdi_cur*)
                    
                     (list
                       (cond
                         (  (eq "" (vla-get-FullName *adoc))
                               
                            (strcat (vla-get-Path *adoc) (vla-get-Name *adoc)))
                        
                         (t (vla-get-FullName *adoc) )
                       )
                     )
                   )                   
                   ( (LM:GetAllFiles *MacEdi_pat* (eq "1" *MacEdi_def*) "*.dwg") )
                 )
               )

               (if Express (setq ProgBar (acet-ui-progress "Updating Attributes..." (length dwLst))))

               (foreach dwg dwLst

                 (if Express (acet-ui-progress -1))                 

                 (cond (  (setq flag (eq "1" *MacEdi_cur*))

                          (setq oDoc *adoc))

                       (  (setq flag (and (setq oDoc (cdr (assoc (strcase dwg) DocLst))))))

                       (t (setq flag (not (vl-catch-all-error-p
                                            (vl-catch-all-apply
                                              (function vla-open) (list dbx dwg)))))
                          (setq oDoc dbx)))

                 (if flag
                   (progn

                     (vlax-for lay (vla-get-layouts oDoc)

                       (vlax-for obj (vla-get-Block lay)

                         (if
                           (and
                             (eq "AcDbBlockReference" (vla-get-ObjectName obj))
                             (eq :vlax-true (vla-get-HasAttributes obj))
                             (eq (strcase *MacEdi_blk*)
                               (strcase
                                 (cond
                                   (  (vlax-property-available-p obj 'EffectiveName)
                                        
                                      (vla-get-EffectiveName obj))
                                   
                                   (t (vla-get-Name obj) )
                                 )
                               )
                             )
                           )
                           (progn

                             (foreach att (vlax-invoke obj 'GetAttributes)

                               (if (setq tag (assoc (strcase (vla-get-TagString att)) *MacEdi_Lst*))
                                 (progn
                                   
                                   (vla-put-InsertionPoint att
                                     (vlax-3D-point
                                       (CalcInsPt att (cond ( (cdr tag) ) ("")))
                                     )
                                   )

                                   (vla-put-TextString att (cond ( (cdr tag) ) ("")))
                                 )
                               )
                             )
                           )
                         )
                       )
                     )
                     (vla-saveas oDoc dwg)
                   )
                   (princ (strcat "\n** Error Opening File: " (vl-filename-base dwg)  ".dwg **"))
                 )
                 
               ) ; Foreach

               (if Express (setq ProgBar (acet-ui-progress)))

               ;;-------------------------------------------------------------------------------;;

               (princ (strcat "\n<< " (itoa (length dwLst)) " Drawings Processed >>"))

               (LM:WriteConfig cfgfname (mapcar 'eval SymList))
             )
             
             (princ "\n*Cancel*")
           )

           (mapcar 'LM:ReleaseObject (list dbx oDoc)) (gc) (gc)
        )
    
  ) ;; COND

;;-------------------------------------------------------------------------------;;
  
  (mapcar (function setvar) vl ov)
  (princ)
)

;;-------------------------------------------------------------------------------;;

(vl-load-com)
(princ "\n:: MacAtt.lsp | Version 3.1 | © Lee Mac 2012 www.lee-mac.com ::")
(princ "\n:: Extractor: \"MacAttExt\"  -==-  Editor: \"MacAttEdit\" ::")
(princ)

;;;¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,;;;
;;                                                                               ;;
;;                             End of Program Code                               ;;
;;                                                                               ;;
;;;ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,¤º°`°º¤;;;