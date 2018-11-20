;;--------------------------=={ Block Count }==-------------------------;;
;;                                                                      ;;
;;  This program will generate a report detailing the individual        ;;
;;  quantities of primary and nested blocks, dynamic blocks & xrefs in  ;;
;;  a selection or an entire drawing.                                   ;;
;;                                                                      ;;
;;  The program will correctly count the number of nested blocks,       ;;
;;  nested dynamic blocks and nested xrefs; nested to any level; and    ;;
;;  with any number of instances of the same block at each level.       ;;
;;                                                                      ;;
;;  Of course, the program may be used as a standard block counter,     ;;
;;  since the nested block count data is displayed separately from the  ;;
;;  primary block count data, and the nested report will not be shown   ;;
;;  if there are no nested blocks in the drawing.                       ;;
;;                                                                      ;;
;;  The report is printed to the command-line and separately details    ;;
;;  the number of primary and nested blocks with a break-down of the    ;;
;;  quantities for each individual block name, together with the total  ;;
;;  number of primary and nested blocks located adjacent to the report  ;;
;;  headings. The printed report may subsequently be extracted to       ;;
;;  either a Text or CSV file.                                          ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2014  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.5    -    2014-02-02                                      ;;
;;----------------------------------------------------------------------;;

(defun c:BlockCount

    (
        /
        _GetBlockCount
        _OutputResults
        _Main
    )

    (defun _GetBlockCount

        (
            selection
            /
            _Assoc++
            _BlockHierarchy
            _GetBlockHierarchy
            _EffectiveName
            _UpdateNestedBlockCount
            _IterateSelection
        )

        (defun _Assoc++ ( key value lst / pair )
            (if (setq pair (assoc key lst))
                (subst (cons key (+ value (cdr pair))) pair lst)
                (cons  (cons key value) lst)
            )
        )

        (defun _BlockHierarchy ( blk / alist enx )
            (while (setq blk (entnext blk))
                (if
                    (and
                        (= "INSERT" (cdr (assoc 0 (setq enx (entget blk)))))
                        (/= 1 (cdr (assoc 60 enx)))
                    )
                    (setq alist (_Assoc++ (cdr (assoc 2 enx)) 1 alist))
                )
            )
            alist
        )

        (defun _GetBlockHierarchy ( / block name tree )
            (while (setq block (tblnext "block" (null block)))
                (setq tree
                    (cons
                        (cons
                            (setq name (cdr (assoc 2 block)))
                            (_BlockHierarchy (tblobjname "block" name))
                        )
                        tree
                    )
                )
            )
            tree
        )

        (defun _EffectiveName ( ent / blk rep )
            (if (wcmatch (setq blk (cdr (assoc 2 (entget ent)))) "`**")
                (if
                    (and
                        (setq rep
                            (cdadr
                                (assoc -3
                                    (entget
                                        (cdr
                                            (assoc 330
                                                (entget
                                                    (tblobjname "block" blk)
                                                )
                                            )
                                        )
                                       '("AcDbBlockRepBTag")
                                    )
                                )
                            )
                        )
                        (setq rep (handent (cdr (assoc 1005 rep))))
                    )
                    (setq blk (cdr (assoc 2 (entget rep))))
                )
            )
            blk
        )

        (defun _UpdateNestedBlockCount ( name count tree alist / nests )
            (if (setq nests (cdr (assoc name tree)))
                (foreach nest nests
                    (setq alist
                        (_UpdateNestedBlockCount (car nest) (* count (cdr nest)) tree
                            (_Assoc++
                                (_EffectiveName (tblobjname "block" (car nest)))
                                (* count (cdr nest))
                                alist
                            )
                        )
                    )
                )
                alist
            )
        )

        (defun _IterateSelection ( selection blocktree / block idx nested primary )
            (if selection
                (repeat (setq idx (sslength selection))
                    (setq block   (ssname selection (setq idx (1- idx)))
                          primary (_Assoc++ (_EffectiveName block) 1 primary)
                          nested  (_UpdateNestedBlockCount (cdr (assoc 2 (entget block))) 1 blocktree nested)
                    )
                )
            )
            (list primary nested)
        )

        (_IterateSelection selection (_GetBlockHierarchy))
    )

    (defun _OutputResults

        (
            data
            /
            _PrintReport
            _PrintFile
            _PrintOutput
        )

        (defun _PrintReport

            (
                data
                /
                _PadBetween
                _PrintIt
            )

            (defun _PadBetween ( s1 s2 ch ln )
                (
                    (lambda ( a b c )
                        (repeat (- ln (length b) (length c)) (setq c (cons a c)))
                        (vl-list->string (append b c))
                    )
                    (ascii ch)
                    (vl-string->list s1)
                    (vl-string->list s2)
                )
            )

            (defun _PrintIt ( lst wid )
                (princ (_PadBetween "\n" "" "=" wid))
                (princ "\n Block Count")
                (princ (_PadBetween "\n" "" "=" wid))
                (princ
                    (_PadBetween
                        (strcat
                            "\n Primary Blocks ("
                            (itoa (apply '+ (mapcar 'cdr (car lst))))
                            ")"
                        )
                        "Count" " " wid
                    )
                )
                (princ (_PadBetween "\n" "" "-" wid))
                (foreach item
                    (vl-sort
                        (car lst)
                        (function (lambda ( a b ) (< (car a) (car b))))
                    )
                    (princ (_PadBetween (strcat "\n " (car item)) (itoa (cdr item)) "." wid))
                )
                (if (cadr lst)
                    (progn
                        (princ (_PadBetween "\n" "" "=" wid))
                        (princ
                            (_PadBetween
                                (strcat
                                    "\n Nested Blocks ("
                                    (itoa (apply '+ (mapcar 'cdr (cadr lst))))
                                    ")"
                                )
                                "Count" " " wid
                            )
                        )
                        (princ (_PadBetween "\n" "" "-" wid))
                        (foreach item
                            (vl-sort
                                (cadr lst)
                                (function (lambda ( a b ) (< (car a) (car b))))
                            )
                            (princ (_PadBetween (strcat "\n " (car item)) (itoa (cdr item)) "." wid))
                        )
                    )
                )
                (princ (_PadBetween "\n" "" "=" wid))
            )

            (_PrintIt data 70)
        )

        (defun _PrintFile
             
            (
                data
                file
                /
                _PrintIt
                _WriteFile
            )

            (defun _PrintIt ( lst del des )
                (princ "Block Count" des)
                (princ
                    (strcat
                        "\nPrimary Blocks ("
                        (itoa (apply '+ (mapcar 'cdr (car lst))))
                        ")"
                        del
                        "Count"
                    )
                    des
                )
                (foreach item
                    (vl-sort
                        (car lst)
                        (function (lambda ( a b ) (< (car a) (car b))))
                    )
                    (princ (strcat "\n" (car item) del (itoa (cdr item))) des)
                )
                (if (cadr lst)
                    (progn
                        (princ
                            (strcat
                                "\n\nNested Blocks ("
                                (itoa (apply '+ (mapcar 'cdr (cadr lst))))
                                ")"
                                del
                                "Count"
                            )
                            des
                        )
                        (foreach item
                            (vl-sort
                                (cadr lst)
                                (function (lambda ( a b ) (< (car a) (car b))))
                            )
                            (princ (strcat "\n" (car item) del (itoa (cdr item))) des)
                        )
                    )
                )
            )

            (defun _WriteFile ( data file / desc )
                (if file
                    (if (setq desc (open file "w"))
                        (progn
                            (_PrintIt
                                data
                                (if (= ".txt" (strcase (vl-filename-extension file) t)) "\t" ",")
                                desc
                            )
                            (close desc)
                            (startapp "explorer" file)
                        )
                        (princ "\nUnable to write to the selected file.")
                    )
                    (princ "\n*Cancel*")
                )
            )

            (_WriteFile data file)
        )

        (defun _PrintOutput ( data / out )
            (_PrintReport data)
            (textpage)
            (initget "TXT CSV")
            (cond
                (   (setq out (getkword "\nOutput Results to [TXT/CSV] <Exit>: "))
                    (_PrintFile data (getfiled "Create Output File" "" (strcase out t) 1))
                    (graphscr)
                )
                (   (graphscr)   )
            )
        )

        (_PrintOutput data)
    )

    (defun _Main

        (
            /
            *error*
            _CountBlocks
        )
        
        (defun *error* ( msg )
            (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
                (princ (strcat "\nError: " msg))
            )
            (princ)
        )
        
        (defun _CountBlocks ( / allblocks sel )
            (cond
                (   (null (setq allblocks (ssget "_X" '((0 . "INSERT")))))
                    (princ "\nNo blocks found in current drawing.")
                )
                (   (progn
                        (setvar 'nomutt 1)
                        (princ "\nSelect blocks to count <all>: ")
                        (setq sel
                            (cond
                                (   (null (setq sel (vl-catch-all-apply 'ssget '(((0 . "INSERT"))))))
                                    allblocks
                                )
                                (   (null (vl-catch-all-error-p sel))
                                    sel
                                )
                            )
                        )
                        (setvar 'nomutt 0)
                        sel
                    )
                    (_OutputResults (_GetBlockCount sel))
                )
            )
            (princ)
        )

        (_CountBlocks)
    )
    
    (_Main)
)

;;----------------------------------------------------------------------;;

(princ
    (strcat
        "\n:: BlockCount.lsp | Version 1.5 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"BlockCount\" to Invoke ::"
    )
)

(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;