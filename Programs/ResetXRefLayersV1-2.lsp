;;----------------------=={ Reset XRef Layers }==-----------------------;;
;;                                                                      ;;
;;  This program enables the user to reset all or specific layer        ;;
;;  properties of xref dependent layers to match the properties         ;;
;;  present in the xref source drawing file.                            ;;
;;                                                                      ;;
;;  Upon starting the program, the user is prompted to select an xref   ;;
;;  whose layers are to be reset. Following a valid selection, the      ;;
;;  properties of all layers dependent on the selected xref are reset   ;;
;;  to match the values found in the source drawing for the selected    ;;
;;  xref.                                                               ;;
;;                                                                      ;;
;;  From the selection prompt, the user may also choose 'Multiple',     ;;
;;  'All', or 'Settings'.                                               ;;
;;                                                                      ;;
;;  If 'Multiple' is selected, the user may select several xrefs using  ;;
;;  the standard selection interface (e.g. via window selection).       ;;
;;                                                                      ;;
;;  If 'All' is selected, the layer properties of every xref found in   ;;
;;  the active drawing is reset to match the original values found in   ;;
;;  the respective source drawings.                                     ;;
;;                                                                      ;;
;;  Finally, if the 'Settings' option is selected, a dialog interface   ;;
;;  is displayed allowing the user to choose which layer properties     ;;
;;  are to be reset.                                                    ;;
;;                                                                      ;;
;;  The user may select multiple properties to be reset from: Colour,   ;;
;;  Linetype, Lineweight, Plot, Plot Style, Frozen in Viewports, On,    ;;
;;  Locked, Frozen, & Description.                                      ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2014  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2011-11-19                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2011-11-27                                      ;;
;;                                                                      ;;
;;  - Added code to search for XRef Source File in working directory &  ;;
;;    support directories if not found at XRef Path.                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.2    -    2014-04-13                                      ;;
;;                                                                      ;;
;;  - Program entirely rewritten.                                       ;;
;;  - Modified program to account for layers whose colour property      ;;
;;    uses a True Colour or Colour Book colour.                         ;;
;;----------------------------------------------------------------------;;

(defun c:rxl ( / *error* bit dbx dch dcl dcs def idx lst sel tmp xrf )

    (defun *error* ( msg )
        (if (< 0 dch)
            (unload_dialog dch)
        )
        (if (and (= 'str (type dcl)) (setq dcl (findfile dcl)))
            (vl-file-delete dcl)
        )
        (if (and (= 'vla-object (type dbx)) (not (vlax-object-released-p dbx)))
            (vlax-release-object dbx)
        )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (if (null (setq bit (getenv "LMac\\RXLProps")))
        (setq bit (+ 1 2 4 8 512))
        (setq bit (atoi bit))
    )
    (while (setq def (tblnext "block" (null def)))
        (if (= 4 (logand 4 (cdr (assoc 70 def))))
            (setq lst (cons "," (cons (cdr (assoc 2 def)) lst)))
        )
    )
    (cond
        (   (null lst)
            (princ "\nNo xrefs found in the active drawing.")
        )
        (   (progn
                (setq dbx
                    (vl-catch-all-apply 'vla-getinterfaceobject
                        (list (LM:acapp)
                            (if (< (setq tmp (atoi (getvar 'acadver))) 16)
                                "objectdbx.axdbdocument"
                                (strcat "objectdbx.axdbdocument." (itoa tmp))
                            )
                        )
                    )
                )
                (or (null dbx) (vl-catch-all-error-p dbx))
            )
            (prompt "\nUnable to interface with ObjectDBX.")
        )
        (   t
            (LM:startundo (LM:acdoc))
            (vlax-for doc (vla-get-documents (LM:acapp))
                (setq dcs (cons (cons (strcase (vla-get-fullname doc)) doc) dcs))
            )
            (while
                (progn
                    (setvar 'errno 0)
                    (initget "Multiple All Settings Exit")
                    (setq sel (entsel "\nSelect xref to reset [Multiple/All/Settings] <Exit>: "))
                    (cond
                        (   (= 7 (getvar 'errno))
                            (princ "\nMissed, try again.")
                        )
                        (   (or (= "Exit" sel) (null sel))
                            nil
                        )
                        (   (= "Multiple" sel)
                            (if
                                (setq sel
                                    (LM:rxl:ssget "\nSelect xrefs to reset <Exit>: "
                                        (list (list '(0 . "INSERT") (cons 2 (apply 'strcat (cdr lst)))))
                                    )
                                )
                                (repeat (setq idx (sslength sel))
                                    (LM:rxl:resetxreflayers
                                        (setq xrf (cdr (assoc 2 (entget (ssname sel (setq idx (1- idx)))))))
                                        (LM:rxl:getdocumentobject dbx dcs xrf)
                                        (LM:acdoc)
                                        bit
                                    )
                                )
                            )
                            nil
                        )
                        (   (= "All" sel)
                            (foreach xrf (cdr lst)
                                (if (/= "," xrf)
                                    (LM:rxl:resetxreflayers xrf
                                        (LM:rxl:getdocumentobject dbx dcs xrf)
                                        (LM:acdoc)
                                        bit
                                    )
                                )
                            )
                            nil
                        )
                        (   (= "Settings" sel)
                            (setq bit (LM:rxl:settings bit))
                        )
                        (   (vl-consp sel)
                            (if (= "INSERT" (cdr (assoc 0 (setq sel (entget (car sel))))))
                                (if (= 4 (logand 4 (cdr (assoc 70 (tblsearch "block" (cdr (assoc 2 sel)))))))
                                    (LM:rxl:resetxreflayers
                                        (setq xrf (cdr (assoc 2 sel)))
                                        (LM:rxl:getdocumentobject dbx dcs xrf)
                                        (LM:acdoc)
                                        bit
                                    )
                                    (princ "\nSelected block is not an xref.")
                                )
                                (princ "\nInvalid object selected.")
                            )
                        )
                    )
                )
            )
            (vla-regen (LM:acdoc) acallviewports)
            (vlax-release-object dbx)
            (LM:endundo (LM:acdoc))
        )
    )
    (setenv "LMac\\RXLProps" (itoa bit))
    (princ)
)

;;----------------------------------------------------------------------;;

(defun LM:rxl:ssget ( msg arg / sel )
    (princ msg)
    (setvar 'nomutt 1)
    (setq sel (vl-catch-all-apply 'ssget arg))
    (setvar 'nomutt 0)
    (if (not (vl-catch-all-error-p sel)) sel)
)

;;----------------------------------------------------------------------;;

(defun LM:rxl:settings ( code / bit dch dcl tmp )
    (cond
        (   (not
                (and
                    (setq dcl (vl-filename-mktemp nil nil ".dcl"))
                    (setq tmp (open dcl "w"))
                    (progn
                        (foreach line
                           '(
                                "rxl : dialog { label = \"Settings\"; spacer;"
                                "    : boxed_column { label = \"Properties to Reset\"; width = 65.0; fixed_width = true; alignment = centered; spacer;"
                                "        : row { alignment = centered; spacer; "
                                "            : column {"
                                "                : toggle { key = \"colour\";     label = \"Colour\"; }"
                                "                : toggle { key = \"linetype\";   label = \"Linetype\"; }"
                                "                : toggle { key = \"lineweight\"; label = \"Lineweight\"; }"
                                "            }"
                                "            : column {"
                                "                : toggle { key = \"plot\";      label = \"Plot\"; }"
                                "                : toggle { key = \"plotstyle\"; label = \"Plot Style\"; }"
                                "                : toggle { key = \"frozenvp\";  label = \"Frozen in VP\"; }"
                                "            }"
                                "            : column {"
                                "                : toggle { key = \"on\";     label = \"On\"; }"
                                "                : toggle { key = \"locked\"; label = \"Locked\"; }"
                                "                : toggle { key = \"frozen\"; label = \"Frozen\"; }"
                                "            }"
                                "            : column {"
                                "                : toggle { key = \"description\"; label = \"Description\"; }"
                                "                spacer;"
                                "                : toggle { key = \"selectall\";   label = \"Select All\"; }"
                                "            }"
                                "        }"
                                "        spacer;"
                                "    }"
                                "    spacer; ok_cancel;"
                                "}"
                            )
                            (write-line line tmp)
                        )
                        (setq tmp (close tmp))
                        (while (null (findfile dcl)))
                        (< 0 (setq dch (load_dialog dcl)))
                    )
                    (new_dialog "rxl" dch)
                )
            )
            (princ "\nError loading dialog.")
        )
        (   t
            (setq bit 1
                  tmp code
            )
            (if (= 1023 tmp)
                (set_tile "selectall" "1")
            )
            (foreach tile
                (setq tiles
                   '(
                        "colour"
                        "linetype"
                        "lineweight"
                        "plot"
                        "plotstyle"
                        "frozenvp"
                        "on"
                        "locked"
                        "frozen"
                        "description"
                    )
                )
                (if (= bit (logand tmp bit))
                    (set_tile tile "1")
                    (set_tile tile "0")
                )
                (action_tile tile
                    (strcat
                        "(setq tmp (boole 6 tmp " (itoa bit) "))"
                        "(set_tile \"selectall\" (if (= 1023 tmp) \"1\" \"0\")))"
                    )
                )
                (setq bit (lsh bit 1))
            )
            (action_tile "selectall"
                (strcat
                    "(foreach tile tiles (set_tile tile $value))"
                    "(if (eq \"1\" $value)"
                    "    (setq tmp 1023)"
                    "    (setq tmp 0)"
                    ")"
                )
            )                        
            (if (= 1 (start_dialog))
                (setq code tmp)
            )
        )
    )
    (if (< 0 dch)
        (setq dch (unload_dialog dch))
    )
    (if (and (= 'str (type dcl)) (setq dcl (findfile dcl)))
        (vl-file-delete dcl)
    )
    code
)

;;----------------------------------------------------------------------;;

(defun LM:rxl:getdocumentobject ( dbx dcs xrf / err pat xrp )
    (setq xrp (cdr (assoc 1 (entget (tblobjname "block" xrf)))))
    (cond
        (   (not
                (or (setq pat (findfile xrp))
                    (setq pat (findfile (strcat (vl-filename-base xrp) ".dwg")))
                )
            )
            (prompt (strcat "\nSource drawing for " xrf " not found."))
        )
        (   (cdr (assoc (strcase pat) dcs)))
        (   (not (vl-catch-all-error-p (setq err (vl-catch-all-apply 'vla-open (list dbx pat)))))
            dbx
        )
        (   (prompt
                (strcat "\nUnable to open " xrf " source drawing: "
                    (vl-catch-all-error-message err)
                )
            )
        )
    )
)

;;----------------------------------------------------------------------;;

(defun LM:rxl:getlayerproperties ( doc flg / bit dat lst )
    (vlax-for lay (vla-get-layers doc)
        (setq bit 1
              lst nil
        )
        (foreach prp
           '(
                truecolor
                linetype
                lineweight
                plottable
                plotstylename
                viewportdefault
                layeron
                lock
                freeze
                description
            )
            (if (and (vlax-property-available-p lay prp)
                     (= bit (logand bit flg))
                )
                (setq lst (cons (cons bit (vlax-get-property lay prp)) lst))
            )
            (setq bit (lsh bit 1))
        )
        (setq dat (cons (cons (strcase (vla-get-name lay)) (reverse lst)) dat))
    )
    dat
)

;;----------------------------------------------------------------------;;

(defun LM:rxl:resetxreflayers ( xrf xrd doc flg / ass bit dat def lyn pos val )
    (cond
        (   (null xrd) nil)
        (   (vl-catch-all-error-p (setq def (vl-catch-all-apply 'vla-item (list (vla-get-blocks doc) xrf))))
            (prompt "\nXref not present in drawing.")
        )
        (   (setq dat (LM:rxl:getlayerproperties xrd flg))
            (vla-reload def)
            (vlax-for lay (vla-get-layers doc)
                (setq lyn (strcase (vla-get-name lay))
                      bit 1
                )
                (if
                    (and
                        (setq pos (vl-string-position 124 lyn))
                        (= (strcase xrf) (substr lyn 1 pos))
                        (setq ass (cdr (assoc (substr lyn (+ 2 pos)) dat)))
                    )
                    (foreach prp
                       '(
                            truecolor
                            linetype
                            lineweight
                            plottable
                            plotstylename
                            viewportdefault
                            layeron
                            lock
                            freeze
                            description
                        )
                        (if
                            (and
                                (vlax-property-available-p lay prp t)
                                (= bit (logand bit flg))
                                (setq val (cdr (assoc bit ass)))
                            )
                            (if (and (= 2 bit) (/= "continuous" (strcase val t)))
                                (vl-catch-all-apply 'vlax-put-property (list lay prp (strcat xrf "|" val)))
                                (vl-catch-all-apply 'vlax-put-property (list lay prp val))
                            )
                        )
                        (setq bit (lsh bit 1))
                    )
                )
            )
            t
        )
    )
)

;;----------------------------------------------------------------------;;

(defun LM:startundo ( doc )
    (LM:endundo doc)
    (vla-startundomark doc)
)

;;----------------------------------------------------------------------;;

(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;;----------------------------------------------------------------------;;

(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)

;;----------------------------------------------------------------------;;

(defun LM:acapp nil
    (eval (list 'defun 'LM:acapp 'nil (vlax-get-acad-object)))
    (LM:acapp)
)

;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: ResetXRefLayers.lsp | Version 1.2 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"RXL\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;