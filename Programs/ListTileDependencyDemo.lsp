;; Two List Tile Dependency Example  -  Lee Mac
;; Requires ListTileDependency.lsp to be loaded.

(defun c:test1 ( / *error* dch dcl des lst rtn )

    (defun *error* ( msg )
        (if (= 'file (type des))
            (close des)
        )
        (if (< 0 dch)
            (unload_dialog dch)
        )
        (if (and (= 'str (type dcl)) (findfile dcl))
            (vl-file-delete dcl)
        )
        (if (and msg (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (setq lst
       '(
            (
                "Item 1"
                (
                    "Item 1-a"
                    "Item 1-b"
                    "Item 1-c"
                )
            )
            (
                "Item 2"
                (
                    "Item 2-a"
                    "Item 2-b"
                    "Item 2-c"
                    "Item 2-d"
                    "Item 2-e"
                )
            )
            (
                "Item 3"
                (
                    "Item 3-a"
                    "Item 3-b"
                )
            )
        )
    )

    (if
        (and
            (setq dcl (vl-filename-mktemp "tmp.dcl"))
            (setq des (open dcl "w"))
            (foreach str
               '(
                    "lbx : list_box"
                    "{"
                    "    alignment = centered;"
                    "    fixed_width = true;"
                    "    fixed_height = true;"
                    "    width = 20;"
                    "    height = 15;"
                    "}"
                    "test : dialog"
                    "{"
                    "    label = \"List Box Dependency Example\";"
                    "    spacer;"
                    "    : row"
                    "    {"
                    "        : lbx { key = \"lb0\"; label = \"List Box 1\"; }"
                    "        : lbx { key = \"lb1\"; label = \"List Box 2\"; }"
                    "    }"
                    "    spacer;"
                    "    ok_cancel;"
                    "}"
                )
                (write-line str des)
            )
            (not (setq des (close des)))
            (< 0 (setq dch (load_dialog dcl)))
            (new_dialog "test" dch)
        )
        (progn           
            (setq rtn '(0 0))
            (LM:dcld:action '("lb0" "lb1") 'lst 'rtn)
            (if (= 1 (start_dialog))
                (princ
                    (strcat "\nThe user selected:"
                        (substr
                            (apply 'strcat
                                (mapcar '(lambda ( x ) (strcat ", " x))
                                    (LM:dcld:getitems rtn lst)
                                )
                            )
                            2
                        )
                    )
                )
                (princ "\n*Cancel*")
            )
        )
    )
    (*error* nil)
    (princ)
)


;; Five List Tile Dependency Example  -  Lee Mac
;; Requires ListTileDependency.lsp to be loaded.

(defun c:test2 ( / *error* dch dcl des lst rtn )

    (defun *error* ( msg )
        (if (= 'file (type des))
            (close des)
        )
        (if (< 0 dch)
            (unload_dialog dch)
        )
        (if (and (= 'str (type dcl)) (findfile dcl))
            (vl-file-delete dcl)
        )
        (if (and msg (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (setq lst
       '(
            (
                "Item 1"
                (
                    "Item 1-a"
                    (
                        "Item 1-a-i"
                        (
                            "Item 1-a-i-1"
                            (
                                "Item 1-a-i-1-a"
                                "Item 1-a-i-1-b"
                            )
                        )
                        (
                            "Item 1-a-i-2"
                            (
                                "Item 1-a-i-2-a"
                                "Item 1-a-i-2-b"
                                "Item 1-a-i-2-c"
                                "Item 1-a-i-2-d"
                            )
                        )
                        (
                            "Item 1-a-i-3"
                            (
                                "Item 1-a-i-3-a"
                                "Item 1-a-i-3-b"
                                "Item 1-a-i-3-c"
                                "Item 1-a-i-3-d"
                                "Item 1-a-i-3-e"
                            )
                        )
                    )
                    (
                        "Item 1-a-ii"
                        (
                            "Item 1-a-ii-1"
                            (
                                "Item 1-a-ii-1-a"
                                "Item 1-a-ii-1-b"
                                "Item 1-a-ii-1-c"
                                "Item 1-a-ii-1-d"
                            )
                        )
                        (
                            "Item 1-a-ii-2"
                            (
                                "Item 1-a-ii-2-a"
                                "Item 1-a-ii-2-b"
                                "Item 1-a-ii-2-c"
                                "Item 1-a-ii-2-d"
                            )
                        )
                    )
                )
                (
                    "Item 1-b"
                    (
                        "Item 1-b-i"
                        (
                            "Item 1-b-i-1"
                            (
                                "Item 1-b-i-1-a"
                                "Item 1-b-i-1-b"
                                "Item 1-b-i-1-c"
                                "Item 1-b-i-1-d"
                            )
                        )
                        (
                            "Item 1-b-i-2"
                            (
                                "Item 1-b-i-2-a"
                                "Item 1-b-i-2-b"
                                "Item 1-b-i-2-c"
                                "Item 1-b-i-2-d"
                            )
                        )
                        (
                            "Item 1-b-i-3"
                            (
                                "Item 1-b-i-3-a"
                                "Item 1-b-i-3-b"
                                "Item 1-b-i-3-c"
                                "Item 1-b-i-3-d"
                            )
                        )
                    )
                )
                (
                    "Item 1-c"
                    (
                        "Item 1-c-i"
                        (
                            "Item 1-c-i-1"
                            (
                                "Item 1-c-i-1-a"
                                "Item 1-c-i-1-b"
                                "Item 1-c-i-1-c"
                                "Item 1-c-i-1-d"
                            )
                        )
                        (
                            "Item 1-c-i-2"
                            (
                                "Item 1-c-i-2-a"
                                "Item 1-c-i-2-b"
                                "Item 1-c-i-2-c"
                                "Item 1-c-i-2-d"
                            )
                        )
                        (
                            "Item 1-c-i-3"
                            (
                                "Item 1-c-i-3-a"
                                "Item 1-c-i-3-b"
                                "Item 1-c-i-3-c"
                                "Item 1-c-i-3-d"
                            )
                        )
                    )
                    (
                        "Item 1-c-ii"
                        (
                            "Item 1-c-ii-1"
                            (
                                "Item 1-c-ii-1-a"
                                "Item 1-c-ii-1-b"
                                "Item 1-c-ii-1-c"
                                "Item 1-c-ii-1-d"
                            )
                        )
                        (
                            "Item 1-c-ii-2"
                            (
                                "Item 1-c-ii-2-a"
                                "Item 1-c-ii-2-b"
                                "Item 1-c-ii-2-c"
                                "Item 1-c-ii-2-d"
                            )
                        )
                        (
                            "Item 1-c-ii-3"
                            (
                                "Item 1-c-ii-3-a"
                                "Item 1-c-ii-3-b"
                                "Item 1-c-ii-3-c"
                                "Item 1-c-ii-3-d"
                            )
                        )
                    )
                    (
                        "Item 1-c-iii"
                        (
                            "Item 1-c-iii-1"
                            (
                                "Item 1-c-iii-1-a"
                                "Item 1-c-iii-1-b"
                                "Item 1-c-iii-1-c"
                                "Item 1-c-iii-1-d"
                            )
                        )
                        (
                            "Item 1-c-iii-2"
                            (
                                "Item 1-c-iii-2-a"
                                "Item 1-c-iii-2-b"
                                "Item 1-c-iii-2-c"
                                "Item 1-c-iii-2-d"
                            )
                        )
                        (
                            "Item 1-c-iii-3"
                            (
                                "Item 1-c-iii-3-a"
                                "Item 1-c-iii-3-b"
                                "Item 1-c-iii-3-c"
                                "Item 1-c-iii-3-d"
                            )
                        )
                    )
                )
            )
            (
                "Item 2"
                (
                    "Item 2-a"
                    (
                        "Item 2-a-i"
                        (
                            "Item 2-a-i-1"
                            (
                                "Item 2-a-i-1-a"
                                "Item 2-a-i-1-b"
                                "Item 2-a-i-1-c"
                                "Item 2-a-i-1-d"
                            )
                        )
                        (
                            "Item 2-a-i-2"
                            (
                                "Item 2-a-i-2-a"
                                "Item 2-a-i-2-b"
                                "Item 2-a-i-2-c"
                                "Item 2-a-i-2-d"
                            )
                        )
                        (
                            "Item 2-a-i-3"
                            (
                                "Item 2-a-i-3-a"
                                "Item 2-a-i-3-b"
                                "Item 2-a-i-3-c"
                                "Item 2-a-i-3-d"
                            )
                        )
                    )
                    (
                        "Item 2-a-ii"
                        (
                            "Item 2-a-ii-1"
                            (
                                "Item 2-a-ii-1-a"
                                "Item 2-a-ii-1-b"
                                "Item 2-a-ii-1-c"
                                "Item 2-a-ii-1-d"
                            )
                        )
                        (
                            "Item 2-a-ii-2"
                            (
                                "Item 2-a-ii-2-a"
                                "Item 2-a-ii-2-b"
                                "Item 2-a-ii-2-c"
                                "Item 2-a-ii-2-d"
                            )
                        )
                        (
                            "Item 2-a-ii-3"
                            (
                                "Item 2-a-ii-3-a"
                                "Item 2-a-ii-3-b"
                                "Item 2-a-ii-3-c"
                                "Item 2-a-ii-3-d"
                            )
                        )
                    )
                )
                (
                    "Item 2-b"
                    (
                        "Item 2-b-i"
                        (
                            "Item 2-b-i-1"
                            (
                                "Item 2-b-i-1-a"
                                "Item 2-b-i-1-b"
                                "Item 2-b-i-1-c"
                                "Item 2-b-i-1-d"
                            )
                        )
                        (
                            "Item 2-b-i-2"
                            (
                                "Item 2-b-i-2-a"
                                "Item 2-b-i-2-b"
                                "Item 2-b-i-2-c"
                                "Item 2-b-i-2-d"
                            )
                        )
                        (
                            "Item 2-b-i-3"
                            (
                                "Item 2-b-i-3-a"
                                "Item 2-b-i-3-b"
                                "Item 2-b-i-3-c"
                                "Item 2-b-i-3-d"
                            )
                        )
                    )
                    (
                        "Item 2-b-ii"
                        (
                            "Item 2-b-ii-1"
                            (
                                "Item 2-b-ii-1-a"
                                "Item 2-b-ii-1-b"
                                "Item 2-b-ii-1-c"
                                "Item 2-b-ii-1-d"
                            )
                        )
                        (
                            "Item 2-b-ii-2"
                            (
                                "Item 2-b-ii-2-a"
                                "Item 2-b-ii-2-b"
                                "Item 2-b-ii-2-c"
                                "Item 2-b-ii-2-d"
                            )
                        )
                        (
                            "Item 2-b-ii-3"
                            (
                                "Item 2-b-ii-3-a"
                                "Item 2-b-ii-3-b"
                                "Item 2-b-ii-3-c"
                                "Item 2-b-ii-3-d"
                            )
                        )
                    )
                )
                (
                    "Item 2-c"
                    (
                        "Item 2-c-i"
                        (
                            "Item 2-c-i-1"
                            (
                                "Item 2-c-i-1-a"
                                "Item 2-c-i-1-b"
                                "Item 2-c-i-1-c"
                                "Item 2-c-i-1-d"
                            )
                        )
                        (
                            "Item 2-c-i-2"
                            (
                                "Item 2-c-i-2-a"
                                "Item 2-c-i-2-b"
                                "Item 2-c-i-2-c"
                                "Item 2-c-i-2-d"
                            )
                        )
                        (
                            "Item 2-c-i-3"
                            (
                                "Item 2-c-i-3-a"
                                "Item 2-c-i-3-b"
                                "Item 2-c-i-3-c"
                                "Item 2-c-i-3-d"
                            )
                        )
                    )
                    (
                        "Item 2-c-ii"
                        (
                            "Item 2-c-ii-1"
                            (
                                "Item 2-c-ii-1-a"
                                "Item 2-c-ii-1-b"
                                "Item 2-c-ii-1-c"
                                "Item 2-c-ii-1-d"
                            )
                        )
                        (
                            "Item 2-c-ii-2"
                            (
                                "Item 2-c-ii-2-a"
                                "Item 2-c-ii-2-b"
                                "Item 2-c-ii-2-c"
                                "Item 2-c-ii-2-d"
                            )
                        )
                        (
                            "Item 2-c-ii-3"
                            (
                                "Item 2-c-ii-3-a"
                                "Item 2-c-ii-3-b"
                                "Item 2-c-ii-3-c"
                                "Item 2-c-ii-3-d"
                            )
                        )
                    )
                )
            )
            (
                "Item 3"
                (
                    "Item 3-a"
                    (
                        "Item 3-a-i"
                        (
                            "Item 3-a-i-1"
                            (
                                "Item 3-a-i-1-a"
                                "Item 3-a-i-1-b"
                                "Item 3-a-i-1-c"
                                "Item 3-a-i-1-d"
                            )
                        )
                        (
                            "Item 3-a-i-2"
                            (
                                "Item 3-a-i-2-a"
                                "Item 3-a-i-2-b"
                                "Item 3-a-i-2-c"
                                "Item 3-a-i-2-d"
                            )
                        )
                        (
                            "Item 3-a-i-3"
                            (
                                "Item 3-a-i-3-a"
                                "Item 3-a-i-3-b"
                                "Item 3-a-i-3-c"
                                "Item 3-a-i-3-d"
                            )
                        )
                    )
                    (
                        "Item 3-a-ii"
                        (
                            "Item 3-a-ii-1"
                            (
                                "Item 3-a-ii-1-a"
                                "Item 3-a-ii-1-b"
                                "Item 3-a-ii-1-c"
                                "Item 3-a-ii-1-d"
                            )
                        )
                        (
                            "Item 3-a-ii-2"
                            (
                                "Item 3-a-ii-2-a"
                                "Item 3-a-ii-2-b"
                                "Item 3-a-ii-2-c"
                                "Item 3-a-ii-2-d"
                            )
                        )
                        (
                            "Item 3-a-ii-3"
                            (
                                "Item 3-a-ii-3-a"
                                "Item 3-a-ii-3-b"
                                "Item 3-a-ii-3-c"
                                "Item 3-a-ii-3-d"
                            )
                        )
                    )
                )
                (
                    "Item 3-b"
                    (
                        "Item 3-b-i"
                        (
                            "Item 3-b-i-1"
                            (
                                "Item 3-b-i-1-a"
                                "Item 3-b-i-1-b"
                                "Item 3-b-i-1-c"
                                "Item 3-b-i-1-d"
                            )
                        )
                        (
                            "Item 3-b-i-2"
                            (
                                "Item 3-b-i-2-a"
                                "Item 3-b-i-2-b"
                                "Item 3-b-i-2-c"
                                "Item 3-b-i-2-d"
                            )
                        )
                        (
                            "Item 3-b-i-3"
                            (
                                "Item 3-b-i-3-a"
                                "Item 3-b-i-3-b"
                                "Item 3-b-i-3-c"
                                "Item 3-b-i-3-d"
                            )
                        )
                    )
                    (
                        "Item 3-b-ii"
                        (
                            "Item 3-b-ii-1"
                            (
                                "Item 3-b-ii-1-a"
                                "Item 3-b-ii-1-b"
                                "Item 3-b-ii-1-c"
                                "Item 3-b-ii-1-d"
                            )
                        )
                        (
                            "Item 3-b-ii-2"
                            (
                                "Item 3-b-ii-2-a"
                                "Item 3-b-ii-2-b"
                                "Item 3-b-ii-2-c"
                                "Item 3-b-ii-2-d"
                            )
                        )
                        (
                            "Item 3-b-ii-3"
                            (
                                "Item 3-b-ii-3-a"
                                "Item 3-b-ii-3-b"
                                "Item 3-b-ii-3-c"
                                "Item 3-b-ii-3-d"
                            )
                        )
                    )
                )
                (
                    "Item 3-c"
                    (
                        "Item 3-c-i"
                        (
                            "Item 3-c-i-1"
                            (
                                "Item 3-c-i-1-a"
                                "Item 3-c-i-1-b"
                                "Item 3-c-i-1-c"
                                "Item 3-c-i-1-d"
                            )
                        )
                        (
                            "Item 3-c-i-2"
                            (
                                "Item 3-c-i-2-a"
                            )
                        )
                        (
                            "Item 3-c-i-3"
                            (
                                "Item 3-c-i-3-a"
                                "Item 3-c-i-3-b"
                                "Item 3-c-i-3-c"
                                "Item 3-c-i-3-d"
                            )
                        )
                    )
                    (
                        "Item 3-c-ii"
                        (
                            "Item 3-c-ii-1"
                            (
                                "Item 3-c-ii-1-a"
                                "Item 3-c-ii-1-b"
                                "Item 3-c-ii-1-c"
                                "Item 3-c-ii-1-d"
                            )
                        )
                        (
                            "Item 3-c-ii-2"
                            (
                                "Item 3-c-ii-2-a"
                                "Item 3-c-ii-2-b"
                                "Item 3-c-ii-2-c"
                                "Item 3-c-ii-2-d"
                            )
                        )
                        (
                            "Item 3-c-ii-3"
                            (
                                "Item 3-c-ii-3-a"
                                "Item 3-c-ii-3-b"
                            )
                        )
                    )
                )
            )
        )
    )

    (if
        (and
            (setq dcl (vl-filename-mktemp "tmp.dcl"))
            (setq des (open dcl "w"))
            (foreach str
               '(
                    "lbx : list_box"
                    "{"
                    "    alignment = centered;"
                    "    fixed_width = true;"
                    "    fixed_height = true;"
                    "    width = 20;"
                    "    height = 15;"
                    "}"
                    "test : dialog"
                    "{"
                    "    label = \"List Box Dependency Example\";"
                    "    spacer;"
                    "    : row"
                    "    {"
                    "        : lbx { key = \"lb0\"; label = \"List Box 1\"; }"
                    "        : lbx { key = \"lb1\"; label = \"List Box 2\"; }"
                    "        : lbx { key = \"lb2\"; label = \"List Box 3\"; }"
                    "        : lbx { key = \"lb3\"; label = \"List Box 4\"; }"
                    "        : lbx { key = \"lb4\"; label = \"List Box 5\"; }"
                    "    }"
                    "    spacer;"
                    "    ok_cancel;"
                    "}"
                )
                (write-line str des)
            )
            (not (setq des (close des)))
            (< 0 (setq dch (load_dialog dcl)))
            (new_dialog "test" dch)
        )
        (progn           
            (setq rtn '(0 0 0 0 0))
            (LM:dcld:action '("lb0" "lb1" "lb2" "lb3" "lb4") 'lst 'rtn)
            (if (= 1 (start_dialog))
                (princ
                    (strcat "\nThe user selected:"
                        (substr
                            (apply 'strcat
                                (mapcar '(lambda ( x ) (strcat ", " x))
                                    (LM:dcld:getitems rtn lst)
                                )
                            )
                            2
                        )
                    )
                )
                (princ "\n*Cancel*")
            )
        )
    )
    (*error* nil)
    (princ)
)