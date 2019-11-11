;;----------------------------=={  Point Manager  }==----------------------------;;
;;                                                                               ;;
;;  Program will allow the user to manipulate points in the drawing or a file in ;;
;;  a variety of ways, including:                                                ;;
;;                                                                               ;;
;;  From File:-                                                                  ;;
;;  --------------                                                               ;;
;;                                                                               ;;
;;  - Insert a Block at all Points from a File (txt/csv)                         ;;
;;  - Insert a Point at all Points from a File (txt/csv)                         ;;
;;  - Create an LW/3D Polyline from all Points in a File (txt/csv)               ;;
;;                                                                               ;;
;;  From ACAD Points:-                                                           ;;
;;  ---------------------                                                        ;;
;;                                                                               ;;
;;  - Insert a Block at all/selected Points in a drawing                         ;;
;;  - Export all/selected Points in a drawing to a File (txt/csv)                ;;
;;  - Create an LW/3D Polyline from all/Selected Points in a drawing             ;;
;;                                                                               ;;
;;  From LW/3D Polylines:-                                                       ;;
;;  --------------------------                                                   ;;
;;                                                                               ;;
;;  - Insert a Block at all vertices of an LW/3D Polyline                        ;;
;;  - Export all Vertices of an LW/3D Polyline to a File (txt/csv)               ;;
;;  - Insert a Point at all Vertices of an LW/3D Polyline                        ;;
;;  - Create an LW/3D Polyline from all/selected LW/3D Polylines                 ;;
;;                                                                               ;;
;;  From Blocks:-                                                                ;;
;;  ----------------                                                             ;;
;;                                                                               ;;
;;  - Export all insertions of a Block to a File (txt/csv)                       ;;
;;  - Insert a Point at all instances of a Block                                 ;;
;;  - Create an LW/3D Polyline at all insertions of a Block                      ;;
;;                                                                               ;;
;;                                                                               ;;
;;  When reading from/writing to Files, a data delimiter may be selected from    ;;
;;  the drop-down, with the exception of using a CSV file, in which a comma must ;;
;;  be used.                                                                     ;;
;;                                                                               ;;
;;  The user can also choose to sort the points by X/Y/Z Coordinate, in          ;;
;;  Ascending or Descending order.                                               ;;
;;                                                                               ;;
;;  When the input is of Block type and output of File type (or vice-versa),     ;;
;;  Block Attributes may be written (or entered).                                ;;
;;                                                                               ;;
;;                                                                               ;;
;;                 --<<  Click Help for more information  >>--                   ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  FUNCTION SYNTAX:  PtManager  /  PtM  /  PTM_clear (Resets Defaults)          ;;
;;                                                                               ;;
;;  Notes:-                                                                      ;;
;;  -----------                                                                  ;;
;;  If no Z-Coord is found, zero elevation is assumed.                           ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  Author: Lee Mac, Copyright ?December 2009 - www.lee-mac.com                 ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  Version:                                                                     ;;
;;                                                                               ;;
;;  1.0:  05/12/2009  -  First Release                                           ;;
;;-------------------------------------------------------------------------------;;
;;  1.1:  05/12/2009  -  Added Dialog interface.                                 ;;
;;-------------------------------------------------------------------------------;;
;;  1.2:  11/12/2009  -  Updated Dialog Interface to allow for various           ;;
;;                       input/output.                                           ;;
;;-------------------------------------------------------------------------------;;
;;  1.3:  12/12/2009  -  Added ability to Create LW and 3D Polylines.            ;;
;;-------------------------------------------------------------------------------;;
;;  1.4:  14/12/2009  -  Added ability to select data delimiter when reading     ;;
;;                       from/writing to files.                                  ;;
;;-------------------------------------------------------------------------------;;
;;  1.5:  17/12/2009  -  Fixed Bug when selecting 3D poly as output.             ;;
;;-------------------------------------------------------------------------------;;
;;  1.6:  26/12/2009  -  Added Ability to Extract Block information.             ;;
;;-------------------------------------------------------------------------------;;
;;  1.7:  29/12/2009  -  Added ability to sort points.                           ;;
;;                    -  Added option to write/enter block attributes.           ;;
;;                    -  Improved Default Handling.                              ;;
;;                    -  Added option to use 'other' delimiter.                  ;;
;;                    -  Added ability to change point format.                   ;;
;;-------------------------------------------------------------------------------;;
;;  1.8:  02/01/2010  -  Created Help File & added Help button.                  ;;
;;                    -  Fixed bug when selecting a block input.                 ;;
;;-------------------------------------------------------------------------------;;
;;  1.9:  04/01/2010  -  Modified Delimiter functions to accept delimiters of    ;;
;;                       length > 1.                                             ;;
;;-------------------------------------------------------------------------------;;
;;  2.0:  05/01/2010  -  Modified Delimiter Mode_Tile functions.                 ;;
;;                    -  Fixed bug when selecting output block from drawing.     ;;
;;-------------------------------------------------------------------------------;;
;;  2.1:  07/01/2010  -  Corrected Help File Typo.                               ;;
;;                    -  Updated Delimiter Check Function                        ;;
;;-------------------------------------------------------------------------------;;
;;  2.2:  07/01/2010  -  Added Object Options Button and Dialog.                 ;;
;;-------------------------------------------------------------------------------;;
;;  2.3:  08/03/2010  -  Fixed bug to include elevation.                         ;;
;;                    -  Added code to disable/enable Object Options button.     ;;
;;-------------------------------------------------------------------------------;;
;;  2.4:  19/04/2010  -  Fixed bug when reading from files.                      ;;
;;-------------------------------------------------------------------------------;;

(defun c:PtM nil (c:PtManager))

(defun c:PtManager (/  ;; --=={  Local Functions  }==--

                       *error*
                       Dcl_Write Delim_Modes
                       Get_Delim Get_Format Get_Order Get_SS_Text Get_Tiles 
                       LoadHelpFile Logo
                       Make_List
                       Obj_Options
                       Set_Format Str-Break Str-Make
                       Tile_Modes
                       Unique
                       vlax-list->2D-point vlax-list->3D-point

                       ;; --=={  Local Variables  }==--

                       *INPUT$BLOCK* *INPUT$FILE* *OUTPUT$BLOCK* *OUTPUT$FILE*
                       *PTM_ATT *PTM_DEL *PTM_FORMAT *PTM_IN *PTM_OBJOPT *PTM_OUT *PTM_SRT
                       ATTRIB_LST ATTRIB_SUB
                       BLK BLKROT BLKSCL
                       CATT
                       DCTAG DC_FNAME DC_TITLE DEL DEST DLST DOC
                       ENT
                       FLAG
                       HF_FNAME
                       I IBLK IMOD IN_LIST IO ITM
                       J
                       LEN LST
                       NEW_ORDER NL
                       OBJ OBJLAY OFILE OMOD OUT_LIST
                       PT_ORDER
                       SHELL SPC SS
                       TMP
                       UFLAG
                       X
                       Y
                       Z

                       ;; --=={  Global Variables  }==--

                       ; *PTM|DefaultSettings*  ~  Default Settings

                    )

  (setq dc_fname "LMAC_PtMan_V2.4.dcl"
        hf_fname "LMAC_PtMan_V2.2.pdf"
        dc_title "Point Manager V2.4")

  (or *PTM|DefaultSettings*

      (setq *PTM|DefaultSettings* (list nil                ;; Input File  *input$file*
                                        (getvar "INSNAME") ;; Block Input
                                        nil                ;; Output File *output$file*
                                        (getvar "INSNAME") ;; Block Ouput
                                        "File"             ;; *PTM_in
                                        "Block"            ;; *PTM_out
                                        '("0" "0" "")      ;; *PTM_del
                                        '("0" "0" "0")     ;; *PTM_srt
                                        "1"                ;; *PTM_att
                                        '("0" "1" "2")     ;; *PTM_format
                  (list (getvar "CLAYER") "0.0" "1.0")     ;; Object Options
  )))            
  

  (mapcar (function set) '(*input$file* *input$block* *output$file* *output$block*
                           *PTM_in *PTM_out *PTM_del *PTM_srt *PTM_att *PTM_format *PTM_ObjOpt)
          
          *PTM|DefaultSettings*)
                                      

  ;;  --=={  Error Handler  }==--
  

  (defun *error* (msg)
    
    (and dcTag (unload_dialog dcTag))
    (and uFlag (vla-EndUndoMark doc))
    (and ofile (close ofile))
    
    (and Shell (not (vlax-object-released-p Shell))
         (vlax-release-object Shell))
    
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ))
  


  ;;-------------------------------------------------------------------------------;;
                           ;;  --=={ Sub-Functions }==-- ;;
  ;;-------------------------------------------------------------------------------;;
  

  (defun unique (lst / result)
    (reverse
      (while (setq itm (car lst))
        (setq lst (vl-remove itm lst)
              result (cons itm result)))))

  
  (defun vlax-list->3D-point (lst)
    (if lst
      (cons (list (car lst) (cadr lst) (caddr lst))
            (vlax-list->3D-point (cdddr lst)))))
  

  (defun vlax-list->2D-point (lst elev)
    (if lst
      (cons (list (car lst) (cadr lst) elev)
            (vlax-list->2D-point (cddr lst) elev))))  
  

  (defun Make_list (key lst)
    (start_list key)
    (mapcar 'add_list lst)
    (end_list))  

    
  (defun Str-Break (str del / pos lst)
    (while (setq pos  (vl-string-search del str))
      (setq lst (cons (substr str 1 pos) lst)
            str (substr str (+ pos 1 (strlen del)))))
    (reverse (cons str lst)))
  

  (defun Str-Make (lst del / str x)
    (setq str  (car lst))
    (foreach x (cdr lst) (setq str (strcat str del x)))

    str)
  

  (defun dcl_write (fname / wPath)

    (if (not (findfile fname))

      (if (setq wPath (findfile "ACAD.PAT"))
        (progn
          (setq wPath (vl-filename-directory wPath))
          
          (or (eq "\\" (substr wPath (strlen wPath)))
              (setq wPath (strcat wPath "\\")))

          (setq ofile (open (strcat wPath fname) "w"))

          (foreach str

            '(
                "//---------------------------=={  PtManager.dcl  }==-----------------------------//"
                "//                                                                               //"
                "//  PtManager.dcl to be used in conjunction with PtManager.lsp                   //"
                "//                                                                               //"
                "//-------------------------------------------------------------------------------//"
                "//                                                                               //"
                "//  Author: Lee Mac, Copyright ?December 2009 - www.lee-mac.com                 //"
                "//                                                                               //"
                "//-------------------------------------------------------------------------------//"
                ""
                "// --=={ Sub-Assembly Definitions }==--"
                ""
                "edit30  : edit_box     { edit_width = 30; alignment = centered; }"
                "edit3   : edit_box     { edit_width = 3;  alignment = centered; }"
                "edit5   : edit_box     { edit_width = 5;  alignment = centered; }"
                "butt10  : button       { width =  10; fixed_width = true ; alignment = centered; }"
                "pop     : popup_list   { width =  20; fixed_width = true ; alignment = centered; }"
                "pop2    : popup_list   { width =  35; fixed_width = true ; alignment = centered; }"
                "boxcol1 : boxed_column { height = 5.2; fixed_height = true; children_alignment = centered; }"
                "boxcol2 : boxed_column { height = 5.5; fixed_height = true; children_alignment = centered; }"
                "boxcol3 : boxed_column { height = 2.0; fixed_height = true; children_alignment = centered; fixed_width = true; }"
                "bar     : image        { width = 50 ; height = 0.3; color = -15;  alignment = centered;"
                "                         fixed_width = true; fixed_height = true; }"
                ""
                "// --=={ Main Dialog }==--"
                ""
                "ptman_dcl : dialog { key = \"dcl_title\";"
                "  : text { value = \"Copyright (c) 2009 Lee Mac\"; alignment = right; }"
                "  spacer;"
                ""
                "  : row {"
                ""
                "    : boxcol1 { label = \"Input Type\"; children_alignment = centered;"
                "      : pop { key = \"input_type\"; }"
                "      spacer;"
                "    }"
                ""
                "    : boxcol1 { label = \"Input File\"; children_alignment = left;"
                ""
                "      : row {"
                "        : edit30 { label = \"&File: \";    key = \"input_file\"; mnemonic = \"F\"; }"
                "        : butt10 { label = \"B&rowse...\"; key = \"input_browse\"; mnemonic = \"r\"; }"
                "        : button { width = 3; fixed_width = true; label = \">>\"; key = \"input_pick\"; }"
                "      }"
                "         "
                "      : row {"
                "        : text   { key = \"block_info\";"
                "                   label = \"Separate Multiple Block Names with a Comma\"; alignment = left; }"
                "        : text   { key = \"in_sel_txt\"; label = \"Selected\"; alignment = right; }"
                "      }"
                ""
                "      : spacer { height = 0.1; fixed_height = true; }"
                "        "
                "    } // Input"
                ""
                "  } // TOP ROW"
                ""
                "  : row {"
                ""
                "    : boxcol1 { label = \"Output Type\"; children_alignment = centered;"
                "      : pop { key = \"output_type\"; }"
                "      spacer;"
                "    }"
                ""
                "    : boxcol1 { label = \"Output File\"; children_alignment = left;"
                "      "
                "      : row {"
                "        : edit30 { label = \"File: \"; key = \"output_file\"; }"
                "        : butt10 { label = \"Browse...\"; key = \"output_browse\"; }"
                "        : button { width = 3; fixed_width = true; label = \">>\"; key = \"output_pick\"; }"
                "      }"
                "        "
                "      spacer;"
                ""
                "      : row {"
                "        : toggle { label = \" Write/Enter Block Attributes\"; key = \"attrib\"; alignment = left; }"
                "        : butt10 { label = \"Object Options\"; key = \"obj_opt\"; }"
                "      }"
                ""
                "      spacer;"
                "        "
                "    } // Output"
                ""
                "  } // MIDDLE ROW"
                ""
                "  : row {"
                ""
                "    : boxcol2 { label = \"Data Delimiter\"; fixed_width = true;"
                "      : pop { key = \"del\"; }"
                ""
                "      : row {"
                "        spacer;"
                "        : column {"
                "          : spacer { height = 0.1; fixed_height = true ; }"
                "          : toggle { label = \" Other:\"; key = \"del_other_tog\"; alignment = right; }"
                "        }"
                "        : edit3  { key = \"del_other\"; }"
                "        spacer;"
                "      }"
                "      spacer;"
                "    }"
                ""
                "    : boxcol2 { label = \"Point Options\"; children_alignment = centered;"
                ""
                "      : row {"
                "        "
                "        : column {"
                "          : row {"
                "            : spacer { width = 0.1; fixed_width = true;  }"
                "            : text   { label = \" Sort\"; }       "
                "            : toggle { key = \"sort\";   }"
                "            : spacer { width = 0.1; fixed_width = true;  }"
                "          }"
                "        }"
                ""
                "        : column {"
                "          : spacer { width = 0.1; fixed_width = true;  }"
                "          : text   { label = \" By:\"; alignment = right; }"
                "          : spacer { width = 0.1; fixed_width = true;  }"
                "        }"
                "          "
                "        : column {"
                "          : row {"
                "            : pop    { key = \"sort_by\";  }"
                "            : pop    { key = \"sort_ord\"; }"
                "          }"
                "          : spacer { width = 0.1; fixed_width = true;  }"
                "        }"
                "      }"
                ""
                "      : bar { key = \"bar\"; }"
                ""
                "      : row {"
                "        : spacer { width = 1.6; fixed_width = true; }"
                "        : column {"
                "          : spacer { width = 0.1; fixed_width = true;  }"
                "          : text   { key = \"format\"; alignment = left; label = \"Current Point Format:     X, Y, Z\"; }"
                "          : spacer { width = 0.1; fixed_width = true;  }"
                "        }"
                "        : butt10 { label = \"Change...\"; key = \"format_change\"; }"
                "        : spacer { width = 1.6; fixed_width = true; }"
                "      }"
                "        "
                "    } // Sort"
                ""
                "  } // BOTTOM ROW"
                "  "
                "  : errtile { }"
                ""
                "  : row { children_alignment = centered;"
                ""
                "    : butt10 { key = \"help\"; label = \"Help\"; }"
                ""
                "    spacer;"
                ""
                "    ok_cancel;"
                ""
                "    spacer;"
                ""
                "    : image { key = \"logo\"; alignment = centered;"
                "              width = 16.06 ; fixed_width  = true;"
                "              height = 2.06 ; fixed_height = true; color = -15; }"
                ""
                "  }"
                ""
                "}"
                ""
                "// --=={ Format Dialog }==--"
                "              "
                "format : dialog { label = \"Point Format\";"
                "  spacer;"
                ""
                "  : row { children_alignment = centered; alignment = centered;"
                ""
                "    : boxcol3 { label = \"Column 1\";"
                "      : pop { key = \"col1\"; }"
                "      spacer;"
                "    }"
                ""
                "    : boxcol3 { label = \"Column 2\";"
                "      : pop { key = \"col2\"; }"
                "      spacer;"
                "    }"
                ""
                "    : boxcol3 { label = \"Column 3\";"
                "      : pop { key = \"col3\"; }"
                "      spacer;"
                "    }"
                ""
                "  }"
                "  spacer;"
                "  ok_cancel;"
                "}"
                ""
                "// --=={ Object Options Dialog }==--"
                ""
                "objoptions : dialog { label = \"Object Options\";"
                "  spacer;"
                ""
                "  : row {"
                "    spacer;"
                "    : column {"
                "      : spacer { width = 0.1; fixed_width = true;  }"
                "      : text   { label = \"Layer: \"; }"
                "      : spacer { width = 0.1; fixed_width = true;  }"
                "    }"
                "    : pop2 { key = \"objlay\"; }"
                "    spacer;"
                "  }"
                ""
                "  spacer;"
                ""
                "  : bar { key = \"bar2\"; }"
                ""
                "  spacer;"
                ""
                "  : row {"
                "    spacer;"
                "    : edit5 { key = \"blkscl\"; label = \"Block Scale:\"   ; }"
                "    spacer;"
                "    : edit5 { key = \"blkrot\"; label = \"Block Rotation:\"; }"
                "    spacer;"
                "  }"
                "  spacer_1;"
                "  ok_cancel;"
                "}"
                ""
                "//-------------------------------------------------------------------------------//"
                "//                                 End of File                                   //"
                "//-------------------------------------------------------------------------------//"
           )

           (write-line str ofile))
          
        (setq ofile (close ofile))
          
        t)  ; File written successfully
        
    nil) ; Filepath not Found
      
  t)) ; DCL file already exists
  
  

  (defun Get_Tiles (/ bNmes in out iFlag oFlag dFlag e)

    ;;  --=={ Input }==--

    (cond (  (eq "Block" *PTM_in)

             (setq bNmes (get_tile "input_file"))

             (cond (  (not (or (setq in ss)
                               (setq in (ssget "_X" (append (list (cons 0 "INSERT"))

                                                            (if (and bNmes (/= "" bNmes))
                                                              (list (cons 2 bNmes))))))))

                      (set_tile "error" "** Block not Found in Drawing **"))

                   (  (setq iFlag t))))

          (  (eq "File" *PTM_in)

             (cond (  (eq "" (setq in (get_tile "input_file")))

                      (set_tile "error" "** No Input File Entered **"))

                   (  (not (vl-position (strcase (vl-filename-extension in)) '(".TXT" ".CSV")))

                      (set_tile "error" "** Invalid Input Filetype **"))

                   (  (not (setq in (findfile in)))

                      (set_tile "error" "** Input file not Found **"))

                   (  (setq iFlag t))))

          (  (eq "Point" *PTM_in)

             (cond (  (not (or (setq in ss)
                               (setq in (ssget "_X" '((0 . "POINT"))))))

                      (set_tile "error" "** No Points Found in Drawing **"))

                   (  (setq iFlag t))))

          (  (eq "LW Polyline" *PTM_in)

             (cond (  (not (or (setq in ss)
                               (setq in (ssget "_X" '((0 . "LWPOLYLINE"))))))

                      (set_tile "error" "** No LW Polylines Found in Drawing **"))

                   (  (setq iFlag t))))

          (  (eq "3D Polyline" *PTM_in)

             (cond (  (not (or (setq in ss)
                               (setq in (ssget "_X" '((0 . "POLYLINE"))))))

                      (set_tile "error" "** No 3D Polylines Found in Drawing **"))

                   (  (setq iFlag t)))))                          


    ;;  --=={ Output }==--

    (cond (  (eq "Block" *PTM_out)

             (cond (  (eq "" (setq out (get_tile "output_file")))

                      (set_tile "error" "** No Block Entered **"))

                   (  (and (setq e (vl-filename-extension out))
                           (not (eq ".DWG" (strcase e))))

                      (set_tile "error" "** Invalid Block Filetype **"))

                   (  (not (or (tblsearch "BLOCK" out)
                               (findfile out)
                               (findfile (strcat out ".dwg"))))

                      (set_tile "error" "** Block not Found **"))

                   (t (cond (  (tblsearch "BLOCK" out))

                            (  (if (not (vl-filename-extension out))
                                 (setq out (findfile (strcat out ".dwg")))
                                 (setq out (findfile out)))))

                      (setq oFlag t))))

          (  (eq "File" *PTM_out)

             (cond (  (eq "" (setq out (get_tile "output_file")))

                      (set_tile "error" "** No Output File Entered **"))

                   (  (not (vl-position (strcase (vl-filename-extension out)) '(".TXT" ".CSV")))

                      (set_tile "error" "** Invalid Output Filetype **"))

                   (  (setq oFlag t))))

          (t (setq oFlag t)))
    

    ;;   --=={ Delimiter Check }==--

    (setq *PTM_del (list (get_tile "del") (get_tile "del_other_tog") (get_tile "del_other")))

    (cond (  (eq "1" (cadr *PTM_del))

             (if (eq "" (caddr *PTM_del))
               
               (set_tile "error" "** No Delimiter Entered **")

               (setq dFlag t)))

          (t (setq dFlag t)))
    

    (and iFlag oFlag dFlag (done_dialog 1))

    (list in out))
  

  (defun logo (key)
    
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
              178 178 178 178 178 178 178 178 178))
    
  (end_image))
  

  (defun Tile_Modes nil
    
    (mapcar (function mode_tile)
            '("input_file" "input_browse" "input_pick" "in_sel_txt" "output_file" "output_browse" "output_pick" "obj_opt")

            (append

              (cond (  (eq "File"   *PTM_in)  '(0 0 1 1))

                    (  (eq "Block"  *PTM_in)  '(0 1 0 0))
                    
                    (  (vl-position *PTM_in
                         '("Point" "LW Polyline" "3D Polyline"))  '(1 1 0 0))
                     
                    (t '(1 1 1 1)))

              (cond (  (eq "Block" *PTM_out) '(0 0 0 0))

                    (  (eq "File"  *PTM_out) '(0 0 1 1))
               
                    (t '(1 1 1 0)))))

    (if (not (vl-position *PTM_in '("Block" "Point" "LW Polyline" "3D Polyline")))
      (set_tile "in_sel_txt" ""))

    (if (and (vl-position *PTM_in  '("Block" "File"))
             (vl-position *PTM_out '("Block" "File")))
      (mode_tile "attrib" 0)
      (mode_tile "attrib" 1))

    (mapcar (function mode_tile)  '("sort_ord" "sort_by")
            (if (eq "1" (car *PTM_srt)) '(0 0) '(1 1))))
  

  (defun Delim_Modes nil
    
    (cond (  (or (and (eq "File" *PTM_in)  *input$file*  (/= "" *input$file*)
                      (eq ".CSV" (strcase (vl-filename-extension *input$file*))))
                 (and (eq "File" *PTM_out) *output$file* (/= "" *output$file*)
                      (eq ".CSV" (strcase (vl-filename-extension  *output$file*)))))

             (set_tile  "del" "1")
             (setq *PTM_del (list (car *PTM_del) (set_tile "del_other_tog" "0") (caddr *PTM_del)))
             (mapcar (function mode_tile) '("del_other_tog" "del_other" "del") '(1 1 1)))

          (  (vl-position "File" (list *PTM_in *PTM_out))

             (mapcar (function set_tile)  '("del" "del_other_tog" "del_other") *PTM_del)
             (mapcar (function mode_tile) '("del_other_tog" "del_other" "del") (list 0 (- 1 (atoi (cadr *PTM_del)))
                                                                                            (atoi (cadr *PTM_del)))))

          (t (mapcar (function set_tile) ' ("del" "del_other_tog" "del_other") *PTM_del)
             (mapcar (function mode_tile) '("del_other_tog" "del_other" "del") '(1 1 1)))))
  

  (defun Get_Delim (var)
    
    (cond (  (zerop (atoi (cadr var)))

             (nth (atoi (car var)) '((46) (44) (59) (9) (32))))

          (  (vl-string->list (caddr var)))))
  

  (defun Get_SS_Text (/ tmp_ss)
    
    (or (and ss (set_tile "in_sel_txt" (strcat (itoa (sslength ss)) " Selected")))
        
        (and (vl-position *PTM_in '("Block" "Point" "LW Polyline" "3D Polyline"))
             (setq tmp_ss (ssget "_X" (append (list (cons 0 (cond (  (eq "Block" *PTM_in)       "INSERT")
                                                                  (  (eq "Point" *PTM_in)       "POINT")
                                                                  (  (eq "LW Polyline" *PTM_in) "LWPOLYLINE")
                                                                  (  (eq "3D Polyline" *PTM_in) "POLYLINE"))))
                                              
                                              (if (and *input$block* (/= "" *input$block*) (eq "Block" *PTM_in))
                                                (list (cons 2 *input$block*))))))
             
             (set_tile "in_sel_txt" (strcat (itoa (sslength tmp_ss)) " Found")))
        
        (set_tile "in_sel_txt" "0 Found"))

    (set_tile "block_info" (if (eq "Block" *PTM_in) "Separate Multiple Block Names with a Comma" "")))
  

  (defun Get_Order (lst)

    (vl-sort-i lst
      (function
        (lambda (a b)
          
          (apply (if (eq "0" (caddr *PTM_srt)) '< '>)
                 
            (cond (  (eq "0" (cadr *PTM_srt))
                     (list (car a) (car b)))

                  (  (eq "1" (cadr *PTM_srt))
                     (list (cadr a) (cadr b)))

                  (t (list (caddr a) (caddr b)))))))))
  

  (defun Get_Format (tag / doups tmp)

    (defun doups  (lst / result)
      (while (setq x (car lst))
        (if (vl-position x (setq lst (cdr lst)))
          (setq result (cons x result)
                lst    (vl-remove x lst))))
      result)

    (cond (  (not (new_dialog "format" tag))

             (princ "\n** Format Dialog Could not be Loaded **"))

          (t
             (mapcar (function Make_List) '("col1" "col2" "col3")
                     '(("X - Coordinate" "Y - Coordinate" "Z - Coordinate")
                       ("X - Coordinate" "Y - Coordinate" "Z - Coordinate")
                       ("X - Coordinate" "Y - Coordinate" "Z - Coordinate")))

             (mapcar (function set_tile)  '("col1" "col2" "col3") *PTM_format)

             (action_tile "accept"
               (vl-prin1-to-string
                 (quote
                   (progn

                     (cond (  (doups
                                (setq tmp
                                  (mapcar (function get_tile) '("col1" "col2" "col3"))))

                              (alert "Columns Must Contain Distinct Values"))

                           (t (done_dialog)))))))
                                               
             (action_tile "cancel" "(setq tmp *PTM_format) (done_dialog)")

             (start_dialog)))
               
    tmp)
  

  (defun Set_Format nil

    (set_tile "format" (strcat "Current Point Format:     "

                               (apply 'strcat
                                 (mapcar 'strcat
                                   (mapcar
                                     (function
                                       (lambda (x)
                                         (nth (atoi x) '("X" "Y" "Z")))) *PTM_format)
                                         '(", " ", " ""))))))


  (defun Obj_Options (tag / GetLayers lays tmp)

    (setq *acad (cond (*acad) ((vlax-get-acad-object))))
    (setq doc   (cond (doc)   ((vla-get-ActiveDocument *acad))))

    (defun GetLayers (/ lst)
      (vlax-map-collection (vla-get-layers doc)
        (function
          (lambda (x) (setq lst (cons (vla-get-name x) lst)))))

      (acad_strlsort lst))
      

    (cond (  (not (new_dialog "objoptions" tag))

             (princ "\n** Object Options Dialog Could not be Loaded **"))

          (t
             (setq lays (GetLayers))
             
             (start_image "bar2")
             (mapcar (function vector_image) '(0 0) '(2 1) '(300 300) '(2 1) '(9 7))
             (end_image)

             (Make_list "objlay" Lays)
             (set_tile  "objlay" (itoa (vl-position (car *PTM_ObjOpt) Lays)))

             (mapcar (function mode_tile) '("blkrot" "blkscl") (if (eq "Block" *PTM_out) '(0 0) '(1 1)))

             (mapcar (function set_tile) '("blkrot" "blkscl") (cdr *PTM_ObjOpt))

             (action_tile "accept"
               (vl-prin1-to-string
                 (quote
                   (progn

                     (cond (  (equal 0.0 (distof (get_tile "blkscl")) 0.00001)
                            
                              (alert "** Invalid Block Scale Entered **"))

                           (  (setq tmp (list (nth (atoi (get_tile "objlay")) lays)
                                                         (get_tile "blkrot")
                                                         (get_tile "blkscl")))

                              (done_dialog)))))))

             (action_tile "cancel" "(setq tmp *PTM_ObjOpt) (done_dialog)")

             (start_dialog)))
    tmp)
  

  ;;-------------------------------------------------------------------------------;;
                           ;;  --=={ Help Functions }==-- ;;
  ;;-------------------------------------------------------------------------------;;


  (defun LoadHelpFile (file / GetHelpFile HFile)

    (defun GetHelpFile (url file / util wPath NewDest)

      (setq *acad (cond (*acad) ((vlax-get-acad-object))))

      (setq util (vla-get-Utility
                   (cond (doc) (setq doc (vla-get-ActiveDocument *acad)))))      

      (if (setq wPath (findfile "ACAD.PAT"))
        (progn
          (setq wPath (vl-filename-directory wPath))

          (or (eq "\\" (substr wPath (strlen wPath)))
              (setq wPath (strcat wPath "\\")))

          (setq NewDest (strcat wPath file))

          (if (not
                (vl-catch-all-error-p
                  (vl-catch-all-apply (function vla-getRemoteFile)
                    (list util url 'Dest :vlax-false))))

            (if (eq 'INT (type (vl-file-copy Dest NewDest))) NewDest)))))

    (cond (  (or (setq HFile (findfile file))
                 (setq HFile (GetHelpFile "http://www.theswamp.org/lilly_pond/leemac/LMAC_PtManager_Help_V2-1.pdf" file)))

             (setq Shell (vla-getInterfaceObject *acad "Shell.Application"))

             (if (vl-catch-all-error-p
                   (vl-catch-all-apply (function vlax-invoke) (list Shell 'Open HFile)))
               
               (alert "** Error Opening Help File **"))

             (vlax-release-object Shell)
             (setq Shell (not (vlax-object-released-p Shell))))                       
             
          (t (alert "** Cannot Load Help File **"))))


  ;;-------------------------------------------------------------------------------;;
                           ;;  --=={ Main Function }==--  ;;
  ;;-------------------------------------------------------------------------------;;
  

  (setq doc (vla-get-ActiveDocument
              (setq *acad (vlax-get-Acad-Object)))
        
        spc (if (zerop (vla-get-activespace doc))
              (if (= (vla-get-mspace doc) :vlax-true)
                (vla-get-modelspace doc)
                (vla-get-paperspace doc))
              (vla-get-modelspace doc)))
  

  ;;  --=={  Available Modes  }==--

  (setq iMod '("Block" "File" "Point" "LW Polyline" "3D Polyline")    ;; input

        oMod '("Block" "File" "Point" "LW Polyline" "3D Polyline"))   ;; output

  ;;-------------------------------------------------------------------------------;;

  (setq dLst '("Point [ . ]" "Comma [ , ]" "Semi-Colon [ ; ]" "Tab [       ]" "Space [   ]"))  ;; Delimiters

  
  (cond (  (not (dcl_write dc_fname))

           (princ "\n** Dialog File could not be Written **"))

        (  (<= (setq dcTag (load_dialog dc_fname)) 0)

           (princ "\n** Dialog File could not be Found **"))

        (t

           (while (not (vl-position flag '(1 0)))
             
            (if (not (new_dialog "ptman_dcl" dcTag))
              (progn
                (princ "\n** Dialog could not be Loaded **") (exit)))

             (set_tile "dcl_title" dc_title)             
                       
             (logo "logo")

             (start_image "bar")
             (mapcar (function vector_image) '(0 0) '(2 1) '(300 300) '(2 1) '(9 7))
             (end_image)

             (setq out_list (vl-remove *PTM_in  oMod)
                   in_list  (vl-remove *PTM_out iMod))

             (mapcar 'Make_List '("input_type" "output_type" "del" "sort_by" "sort_ord")
                     (list in_list out_list DLst '("X - Coordinate" "Y - Coordinate" "Z - Coordinate")
                                                 '("Ascending" "Descending")))
             
             (mapcar 'set_tile  '("input_type" "output_type")
                     (mapcar 'itoa
                             (mapcar 'vl-position
                                     (list *PTM_in *PTM_out) (list in_list out_list))))

             (mapcar (function set_tile) '("attrib" "sort" "sort_by" "sort_ord")
                     (append (list *PTM_att) *PTM_srt))

             ;; Input Block Default

             (and *input$block*
                  (vl-every (function (lambda (x) (tblsearch "BLOCK" x))) (Str-Break *input$block* ","))
                  (eq "Block" *PTM_in)
                  (set_tile "input_file" *input$block*))

             ;; Output Block Default

             (and *output$block*
                  (or (tblsearch "BLOCK" *output$block*)
                      (setq *output$block* (findfile (if (not (vl-filename-extension  *output$block*))
                                                       (strcat *output$block* ".dwg") *output$block*))))
                  (eq "Block" *PTM_out)
                  (set_tile "output_file" *output$block*))

             ;; Input File Default

             (and *input$file*
                  (setq *input$file* (findfile *input$file*))
                  (eq "File" *PTM_in)
                  (set_tile "input_file" *input$file*))

             ;; Output File Default

             (and *output$file*
                  (setq *output$file* (findfile *output$file*))
                  (eq "File" *PTM_out)
                  (set_tile "output_file" *output$file*))

             
             (Delim_Modes)
             (Get_SS_Text)
             (Tile_Modes)
             (Set_Format)
             

             ;; --=={  Action Tiles  }==--

             
             (action_tile "input_browse"
               (vl-prin1-to-string
                 (quote
                   (progn
                     (if (setq tmp (getfiled "Select Input File" (cond (*input$file*) ("")) "txt;csv" 16))
                       (set_tile "input_file" (setq *input$file* tmp)))

                     (Delim_Modes)))))
             

             (action_tile "output_browse"
               (vl-prin1-to-string
                 (quote
                   (progn
                     (cond (  (eq "Block" *PTM_out)

                              (if (setq tmp (getfiled "Select Block" (cond (*output$block*) ("")) "dwg" 16))
                                (set_tile "output_file" (setq *output$block* (findfile tmp)))))
                           
                           (  (eq "File"  *PTM_out)
                            
                              (if (setq tmp (getfiled "Select File"  (cond (*output$file*) ("")) "txt;csv" 9))
                                (set_tile "output_file" (setq *output$file* (cond ((findfile tmp)) (tmp)))))))

                     (Delim_Modes)))))
             

             (action_tile "input_type"
               (vl-prin1-to-string
                 (quote
                   (progn
                     (setq *PTM_in (nth (atoi $value) in_list) ss nil)
                     
                     (Get_SS_Text)
                     (Tile_Modes)
                     
                     (Make_List "output_type" (setq out_list (vl-remove *PTM_in oMod)))
                     (set_tile  "output_type" (itoa (cond ((vl-position *PTM_out out_list)) (0))))

                     (cond (  (eq "Block" *PTM_in)

                              (and *input$block*
                                   (vl-every (function (lambda (x) (tblsearch "BLOCK" x))) (Str-Break *input$block* ","))
                                   (set_tile "input_file" *input$block*)))

                           (  (eq "File" *PTM_in)

                              (and *input$file*
                                   (setq *input$file* (findfile *input$file*))
                                   (set_tile "input_file" *input$file*))))

                     (Delim_Modes)))))
             
             
             (action_tile "output_type"
               (vl-prin1-to-string
                 (quote
                   (progn
                     (setq *PTM_out (nth (atoi $value) out_list))
                     
                     (Tile_Modes)
                     
                     (Make_List "input_type" (setq in_list (vl-remove *PTM_out iMod)))
                     (set_tile  "input_type" (itoa (cond ((vl-position *PTM_in in_list)) (0))))

                     (cond (  (eq "Block" *PTM_out)

                              (and *output$block*
                                   (or (tblsearch "BLOCK" *output$block*)
                                       (setq *output$block* (findfile (if (not (vl-filename-extension *output$block*))
                                                                        (strcat *output$block* ".dwg") *output$block*))))
                                   (set_tile "output_file" *output$block*)))

                           (  (eq "File" *PTM_out)

                              (and *output$file*
                                   (setq *output$file* (findfile *output$file*))
                                   (set_tile "output_file" *output$file*))))
                     
                     (Delim_Modes)))))
             

             (action_tile "del"          "(setq *PTM_del (cons $value (cdr *PTM_del)))")
             (action_tile "del_other"    "(setq *PTM_del (list (car *PTM_del) (cadr *PTM_del) $value))")
             (action_tile "del_other_tog"
               (vl-prin1-to-string
                 (quote
                   (progn
                     (setq *PTM_del (list (car *PTM_del) $value (caddr *PTM_del)))
                     (mode_tile "del_other" (- 1 (atoi $value)))
                     (mode_tile "del"       (atoi $value))))))
             
             (action_tile "output_file"  "(set (if (eq *PTM_out \"File\") '*output$file* '*output$block*) $value) (Delim_Modes)")
             (action_tile "input_file"   "(set (if (eq *PTM_in  \"File\") '*input$file*  '*input$block* ) $value) (Get_SS_Text) (Delim_Modes)")
             (action_tile "attrib"       "(setq *PTM_att     $value)")

             (action_tile "sort"         "(setq *PTM_srt (cons $value (cdr *PTM_srt))) (Tile_Modes)")
             (action_tile "sort_by"      "(setq *PTM_srt (list (car *PTM_srt) $value (caddr *PTM_srt)))")
             (action_tile "sort_ord"     "(setq *PTM_srt (list (car *PTM_srt) (cadr *PTM_srt) $value))")

             (action_tile "output_pick"  "(done_dialog 2)")
             (action_tile "input_pick"   "(done_dialog 3)")

             (action_tile "format_change""(setq *PTM_format (Get_Format dcTag)) (Set_Format)")

             (action_tile "obj_opt"      "(setq *PTM_ObjOpt (Obj_Options dcTag))")

             (action_tile "help"         "(LoadHelpFile hf_fname)")
             
             (action_tile "accept"       "(setq IO (Get_Tiles))")
             (action_tile "cancel"       "(done_dialog 0)")


             ;;-------------------------------------------------------------------------------;;
             

             (setq flag (start_dialog))

             (cond (  (eq flag 2)
                    
                      (while
                        (progn
                          (setq ent (car (entsel "\nSelect Block: ")))

                          (cond (  (eq 'ENAME (type ent))

                                   (if (eq "INSERT" (cdr (assoc 0 (entget ent))))
                                     (not (setq *output$block* (cdr (assoc 2 (entget ent)))))
                                     (princ "\n** Object must be a Block **")))))))

                   (  (eq flag 3)
                    
                      (setq ss (ssget (append (list (cons 0 (cond (  (eq "Block" *PTM_in)       "INSERT")
                                                                  (  (eq "Point" *PTM_in)       "POINT")
                                                                  (  (eq "LW Polyline" *PTM_in) "LWPOLYLINE")
                                                                  (  (eq "3D Polyline" *PTM_in) "POLYLINE"))))
                                              
                                              (if (and *input$block* (/= "" *input$block*) (eq "Block" *PTM_in))
                                                (list (cons 2 *input$block*))))))

                      (if (and ss (eq "Block" *PTM_in))
                        (set_tile "input_file" (setq *input$block*
                                                 (Str-Make
                                                   (Unique
                                                     (mapcar
                                                       (function
                                                         (lambda (x) (cdr (assoc 2 (entget x)))))
                                                       
                                                       (vl-remove-if (function listp)
                                                         (mapcar (function cadr) (ssnamex ss))))) ","))))))

             )  ; DCL While Loop
         
           (setq dcTag (unload_dialog dcTag))

         

         ;;-------------------------------------------------------------------------------;;
         ;;                           --=={  Main Routine  }==--                          ;;
         ;;-------------------------------------------------------------------------------;;



         (if (eq 1 flag)
           (progn
             (setq i 0 uFlag (not (vla-StartUndoMark doc)))

             (setq Pt_Order  (mapcar (function atoi) *PTM_format))

             (setq ObjLay (car *PTM_ObjOpt)
                   BlkRot (angtof (cadr *PTM_ObjOpt))
                   BlkScl (distof (caddr *PTM_ObjOpt)))

             ;; --=={ Input }==--

             (cond (  (eq "Block" *PTM_in)

                      (setq j -1)
                    
                      (while (setq ent (ssname (car IO) (setq j (1+ j))))
                        (setq lst (cons (cdr (assoc 10 (entget ent))) lst))

                        (if (eq "1" *PTM_att)
                          
                          (if (eq :vlax-true (vla-get-Hasattributes
                                               (setq obj (vlax-ename->vla-object ent))))
                            (progn
                              
                              (foreach att (append
                                             (vlax-safearray->list
                                               (vlax-variant-value
                                                 (vla-getAttributes obj)))

                                             (cond (  (vl-catch-all-error-p
                                                        (setq cAtt
                                                          (vl-catch-all-apply
                                                            (function vlax-safearray->list)
                                                              (list
                                                                (vlax-variant-value
                                                                  (vla-getConstantAttributes obj)))))) nil)

                                                   (cAtt)))
                                
                                (setq Attrib_Sub (cons (vla-get-TextString att) Attrib_Sub)))

                              (setq Attrib_Lst (cons (reverse Attrib_Sub) Attrib_Lst) Attrib_Sub nil))

                            (setq Attrib_Lst (cons 'nil Attrib_Lst)))))                         

                      (setq lst (reverse lst) Attrib_Lst (reverse Attrib_Lst)))
                   
                   ;;-------------------------------------------------------------------------------;;
                   
                   (  (eq "File" *PTM_in)

                      (setq *input$file* (car IO) ofile (open (car IO) "r") del (vl-list->string (Get_Delim *PTM_del)))

                      (while (setq nl (read-line ofile))
                        (setq lst (cons (Str-Break nl del) lst)))
                      (setq ofile (close ofile))

                      (setq lst (mapcar
                                  (function
                                    (lambda (x / z)
                                      (if (not (vl-position 'nil (setq z (list (distof (car x))
                                                                               (distof (cadr x))
                                                                               (cond   ((null (caddr x)) 0.0) ((distof (caddr x))) (0.0))))))
                                        (progn
                                          (setq Attrib_Lst (cons (cdddr x) Attrib_Lst))

                                          (mapcar
                                            (function
                                              (lambda (y)
                                                (nth y z))) Pt_Order)))))
                                      
                            (reverse lst)))

                      (setq Attrib_Lst (reverse Attrib_Lst)))

                   ;;-------------------------------------------------------------------------------;;
                   
                   (  (eq "Point" *PTM_in)

                      (setq j -1)
                            
                      (while (setq ent (ssname (car IO) (setq j (1+ j))))
                        (setq lst (cons (cdr (assoc 10 (entget ent))) lst)))

                      (setq lst (reverse lst)))

                   ;;-------------------------------------------------------------------------------;;

                   (  (eq "LW Polyline" *PTM_in)

                      (setq j -1)

                      (while (setq ent (ssname (car IO) (setq j (1+ j))))
                        (setq lst (append lst (vlax-list->2D-point
                                                (vlax-get
                                                  (setq LWPObj
                                                    (vlax-ename->vla-object ent)) 'Coordinates)

                                                (vla-get-elevation LWPObj))))))

                   ;;-------------------------------------------------------------------------------;;

                   (  (eq "3D Polyline" *PTM_in)

                      (setq j -1)

                      (while (setq ent (ssname (car IO) (setq j (1+ j))))
                        (setq lst (append lst (vlax-list->3D-point
                                                (vlax-get
                                                  (vlax-ename->vla-object ent) 'Coordinates)))))))

             ;;-------------------------------------------------------------------------------;;

             (setq lst (vl-remove 'nil lst))

             (if (eq "1" (car *PTM_srt))
               (progn
                 (setq new_order (Get_Order lst))

                 (setq lst (mapcar
                             (function
                               (lambda (x) (nth x lst))) new_order))

                 (if Attrib_Lst (setq Attrib_Lst (mapcar
                                                   (function
                                                     (lambda (x) (nth x Attrib_Lst))) new_order)))))


             ;;  --=={ Ouput }==--

             (cond (  (eq "Block" *PTM_out)

                      (setq blk (cadr IO))

                      (foreach pt lst                        

                        (if (vl-catch-all-error-p
                              (setq iBlk
                                (vl-catch-all-apply (function vla-InsertBlock)
                                  (list spc (vlax-3D-point pt) blk blkscl blkscl blkscl blkrot))))

                          (progn
                            (princ (strcat "\n** Error: " (vl-catch-all-error-message iBlk) " **"))
                            (setq i (1+ i)))
                          
                          (progn
                            (vla-put-layer iBlk ObjLay)

                            (if (and (eq "1" *PTM_att)
                                     (setq x (car Attrib_Lst))
                                     (eq :vlax-true (vla-get-HasAttributes iBlk))
                                     (setq bAttribs (vlax-safearray->list
                                                      (vlax-variant-value
                                                        (vla-getAttributes iBlk)))))

                              (while (and (setq s (car x))
                                          (setq a (car bAttribs)))
                                (vla-put-TextString a s)
                                (setq x (cdr x) bAttribs (cdr bAttribs))))))
                        
                        (setq Attrib_Lst (cdr Attrib_Lst)))

                      (or (zerop i) (princ (strcat "\n** " (itoa i) " Blocks Failed to be Inserted **"))))

                   ;;-------------------------------------------------------------------------------;;

                   (  (eq "File"  *PTM_out)

                      (setq *output$file* (cadr IO) ofile (open (cadr IO) "a") del (vl-list->string (Get_Delim *PTM_del))

                            len (length lst))

                      (while (setq x (car lst))
                        
                        (setq x (mapcar (function rtos)
                                        (mapcar
                                          (function (lambda (i) (nth i x))) Pt_Order)))
                        
                        (if (and (eq "1" *PTM_att) (setq y (car Attrib_Lst)))
                          (setq x (append x y)))
                        
                        (write-line (Str-Make x del) ofile)
                        
                        (setq lst (cdr lst) Attrib_Lst (cdr Attrib_Lst)))
                    
                      (setq ofile (close ofile))

                      (princ (strcat "\n<< " (itoa len) " Points Written to File >>")))
                   
                   ;;-------------------------------------------------------------------------------;;

                   (  (eq "Point" *PTM_out)

                      (foreach pt lst (vla-put-layer (vla-AddPoint spc (vlax-3D-point pt)) ObjLay)))

                   ;;-------------------------------------------------------------------------------;;

                   (  (eq "LW Polyline" *PTM_out)

                      (setq lst
                        (apply 'append
                          (mapcar
                            (function
                              (lambda (x) (list (car x) (cadr x)))) lst)))

                      (vla-put-layer
                        (vla-AddLightWeightPolyline spc
                          (vlax-make-variant
                            (vlax-safearray-fill
                              (vlax-make-safearray
                                 vlax-vbDouble (cons 0 (1- (length lst)))) lst))) ObjLay))

                   ;;-------------------------------------------------------------------------------;;

                   (  (eq "3D Polyline" *PTM_out)

                      (setq lst (apply 'append lst))

                      (vla-put-layer
                        (vla-Add3DPoly spc
                          (vlax-make-variant
                            (vlax-safearray-fill
                              (vlax-make-safearray
                                 vlax-vbDouble (cons 0 (1- (length lst)))) lst))) ObjLay)))

             ;;-------------------------------------------------------------------------------;;
                    
             (setq uFlag (vla-EndUndoMark doc)))

           (princ "\n*Cancel*"))))


  (setq *PTM|DefaultSettings* (list *input$file* *input$block* *output$file* *output$block*
                                    *PTM_in *PTM_out *PTM_del *PTM_srt *PTM_att *PTM_format *PTM_ObjOpt))
  
  (princ))

(defun c:PTM_clear nil (setq *PTM|DefaultSettings* nil))

(vl-load-com)
(princ "\n:: Point Manager | Version 2.4 | ?Lee Mac 2009 www.lee-mac.com ::")
(princ "\n:: Type \"PtM\" to Invoke ::")
(princ)

;;-------------------------------------------------------------------------------;;
;;                                 End of File                                   ;;
;;-------------------------------------------------------------------------------;;
