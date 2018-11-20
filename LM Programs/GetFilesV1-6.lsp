;;------------------------=={ Get Files Dialog }==----------------------;;
;;                                                                      ;;
;;  An analog of the 'getfiled' function for multiple file selection.   ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2012  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Arguments:                                                          ;;
;;  msg - [str/nil] Dialog box label; 'Select Files' if nil or "".      ;;
;;  def - [str/nil] Default directory; dwgprefix if nil or "".          ;;
;;  ext - [str/nil] File extension filter (e.g. "dwg;lsp"); "*" if nil  ;;
;;----------------------------------------------------------------------;;
;;  Returns:  List of selected files, else nil                          ;;
;;----------------------------------------------------------------------;;
;;  Version 1.6    -    2016-03-21                                      ;;
;;----------------------------------------------------------------------;;

(defun LM:getfiles ( msg def ext / *error* dch dcl des dir dirdata lst rtn )

    (defun *error* ( msg )
        (if (= 'file (type des))
            (close des)
        )
        (if (and (= 'int (type dch)) (< 0 dch))
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
    
    (if
        (and
            (setq dcl (vl-filename-mktemp nil nil ".dcl"))
            (setq des (open dcl "w"))
            (progn
                (foreach x
                   '(
                        "lst : list_box"
                        "{"
                        "    width = 40.0;"
                        "    height = 20.0;"
                        "    fixed_width = true;"
                        "    fixed_height = true;"
                        "    alignment = centered;"
                        "    multiple_select = true;"
                        "}"
                        "but : button"
                        "{"
                        "    width = 20.0;"
                        "    height = 1.8;"
                        "    fixed_width = true;"
                        "    fixed_height = true;"
                        "    alignment = centered;"
                        "}"
                        "getfiles : dialog"
                        "{"
                        "    key = \"title\"; spacer;"
                        "    : row"
                        "    {"
                        "        alignment = centered;"
                        "        : edit_box { key = \"dir\"; label = \"Folder:\"; }"
                        "        : button"
                        "        {"
                        "            key = \"brw\";"
                        "            label = \"Browse\";"
                        "            fixed_width = true;"
                        "        }"
                        "    }"
                        "    spacer;"
                        "    : row"
                        "    {"
                        "        : column"
                        "        {"
                        "            : lst { key = \"box1\"; }"
                        "            : but { key = \"add\" ; label = \"Add Files\"; }"
                        "        }"
                        "        : column {"
                        "            : lst { key = \"box2\"; }"
                        "            : but { key = \"del\" ; label = \"Remove Files\"; }"
                        "        }"
                        "    }"
                        "    spacer; ok_cancel;"
                        "}"
                    )
                    (write-line x des)
                )
                (setq des (close des))
                (< 0 (setq dch (load_dialog dcl)))
            )
            (new_dialog "getfiles" dch)
        )
        (progn
            (setq ext (if (= 'str (type ext)) (LM:getfiles:str->lst (strcase ext) ";") '("*")))
            (set_tile "title" (if (member msg '(nil "")) "Select Files" msg))
            (set_tile "dir"
                (setq dir
                    (LM:getfiles:fixdir
                        (if (or (member def '(nil "")) (not (vl-file-directory-p (LM:getfiles:fixdir def))))
                            (getvar 'dwgprefix)
                            def
                        )
                    )
                )
            )
            (setq lst (LM:getfiles:updatefilelist dir ext nil))
            (mode_tile "add" 1)
            (mode_tile "del" 1)

            (action_tile "brw"
                (vl-prin1-to-string
                   '(if (setq tmp (LM:getfiles:browseforfolder "" nil 512))
                        (setq lst (LM:getfiles:updatefilelist (set_tile "dir" (setq dir tmp)) ext rtn)
                              rtn (LM:getfiles:updateselected dir rtn)
                        )                              
                    )
                )
            )

            (action_tile "dir"
                (vl-prin1-to-string
                   '(if (= 1 $reason)
                        (setq lst (LM:getfiles:updatefilelist (set_tile "dir" (setq dir (LM:getfiles:fixdir $value))) ext rtn)
                              rtn (LM:getfiles:updateselected dir rtn)
                        )
                    )
                )
            )

            (action_tile "box1"
                (vl-prin1-to-string
                   '(
                        (lambda ( / itm tmp )
                            (if (setq itm (mapcar '(lambda ( n ) (nth n lst)) (read (strcat "(" $value ")"))))
                                (if (= 4 $reason)
                                    (cond
                                        (   (equal '("..") itm)
                                            (setq lst (LM:getfiles:updatefilelist (set_tile "dir" (setq dir (LM:getfiles:updir dir))) ext rtn)
                                                  rtn (LM:getfiles:updateselected dir rtn)
                                            )
                                        )
                                        (   (vl-file-directory-p (setq tmp (LM:getfiles:checkredirect (strcat dir "\\" (car itm)))))
                                            (setq lst (LM:getfiles:updatefilelist (set_tile "dir" (setq dir tmp)) ext rtn)
                                                  rtn (LM:getfiles:updateselected dir rtn)
                                            )
                                        )
                                        (   (setq rtn (LM:getfiles:sort (append rtn (mapcar '(lambda ( x ) (strcat dir "\\" x)) itm)))
                                                  rtn (LM:getfiles:updateselected dir rtn)
                                                  lst (LM:getfiles:updatefilelist dir ext rtn)
                                            )
                                        )
                                    )
                                    (if (vl-every '(lambda ( x ) (vl-file-directory-p (strcat dir "\\" x))) itm)
                                        (mode_tile "add" 1)
                                        (mode_tile "add" 0)
                                    )
                                )
                            )
                        )
                    )
                )
            )

            (action_tile "box2"
                (vl-prin1-to-string
                   '(
                        (lambda ( / itm )
                            (if (setq itm (mapcar '(lambda ( n ) (nth n rtn)) (read (strcat "(" $value ")"))))
                                (if (= 4 $reason)
                                    (setq rtn (LM:getfiles:updateselected dir (vl-remove (car itm) rtn))
                                          lst (LM:getfiles:updatefilelist dir ext rtn)
                                    )
                                    (mode_tile "del" 0)
                                )
                            )
                        )
                    )
                )
            )

            (action_tile "add"
                (vl-prin1-to-string
                   '(
                        (lambda ( / itm )
                            (if
                                (setq itm
                                    (vl-remove-if 'vl-file-directory-p
                                        (mapcar '(lambda ( n ) (nth n lst)) (read (strcat "(" (get_tile "box1") ")")))
                                    )
                                )
                                (setq rtn (LM:getfiles:sort (append rtn (mapcar '(lambda ( x ) (strcat dir "\\" x)) itm)))
                                      rtn (LM:getfiles:updateselected dir rtn)
                                      lst (LM:getfiles:updatefilelist dir ext rtn)
                                )
                            )
                            (mode_tile "add" 1)
                            (mode_tile "del" 1)
                        )
                    )
                )
            )

            (action_tile "del"
                (vl-prin1-to-string
                   '(
                        (lambda ( / itm )
                            (if (setq itm (read (strcat "(" (get_tile "box2") ")")))
                                (setq rtn (LM:getfiles:updateselected dir (LM:getfiles:removeitems itm rtn))
                                      lst (LM:getfiles:updatefilelist dir ext rtn)
                                )
                            )
                            (mode_tile "add" 1)
                            (mode_tile "del" 1)
                        )
                    )
                )
            )
         
            (if (zerop (start_dialog))
                (setq rtn nil)
            )
        )
    )
    (*error* nil)
    rtn
)

(defun LM:getfiles:listbox ( key lst )
    (start_list key)
    (foreach x lst (add_list x))
    (end_list)
    lst
)

(defun LM:getfiles:listfiles ( dir ext lst )
    (vl-remove-if '(lambda ( x ) (member (strcat dir "\\" x) lst))
        (cond
            (   (cdr (assoc dir dirdata)))
            (   (cdar
                    (setq dirdata
                        (cons
                            (cons dir
                                (append
                                    (LM:getfiles:sortlist (vl-remove "." (vl-directory-files dir nil -1)))
                                    (LM:getfiles:sort
                                        (if (member ext '(("") ("*")))
                                            (vl-directory-files dir nil 1)
                                            (vl-remove-if-not
                                                (function
                                                    (lambda ( x / e )
                                                        (and
                                                            (setq e (vl-filename-extension x))
                                                            (setq e (strcase (substr e 2)))
                                                            (vl-some '(lambda ( w ) (wcmatch e w)) ext)
                                                        )
                                                    )
                                                )
                                                (vl-directory-files dir nil 1)
                                            )
                                        )
                                    )
                                )
                            )
                            dirdata
                        )
                    )
                )
            )
        )
    )
)

(defun LM:getfiles:checkredirect ( dir / itm pos )
    (cond
        (   (vl-directory-files dir) dir)
        (   (and
                (=  (strcase (getenv "UserProfile"))
                    (strcase (substr dir 1 (setq pos (vl-string-position 92 dir nil t))))
                )
                (setq itm
                    (cdr
                        (assoc (substr (strcase dir t) (+ pos 2))
                           '(
                                ("my documents" . "Documents")
                                ("my pictures"  . "Pictures")
                                ("my videos"    . "Videos")
                                ("my music"     . "Music")
                            )
                        )
                    )
                )
                (vl-file-directory-p (setq itm (strcat (substr dir 1 pos) "\\" itm)))
            )
            itm
        )
        (   dir   )
    )
)

(defun LM:getfiles:sort ( lst )
    (apply 'append
        (mapcar 'LM:getfiles:sortlist
            (vl-sort
                (LM:getfiles:groupbyfunction lst
                    (lambda ( a b / x y )
                        (and
                            (setq x (vl-filename-extension a))
                            (setq y (vl-filename-extension b))
                            (= (strcase x) (strcase y))
                        )
                    )
                )
                (function
                    (lambda ( a b / x y )
                        (and
                            (setq x (vl-filename-extension (car a)))
                            (setq y (vl-filename-extension (car b)))
                            (< (strcase x) (strcase y))
                        )
                    )
                )
            )
        )
    )
)

(defun LM:getfiles:sortlist ( lst )
    (mapcar (function (lambda ( n ) (nth n lst)))
        (vl-sort-i (mapcar 'LM:getfiles:splitstring lst)
            (function
                (lambda ( a b / x y )
                    (while
                        (and
                            (setq x (car a))
                            (setq y (car b))
                            (= x y)
                        )
                        (setq a (cdr a)
                              b (cdr b)
                        )
                    )
                    (cond
                        (   (null x) b)
                        (   (null y) nil)
                        (   (and (numberp x) (numberp y)) (< x y))
                        (   (numberp x))
                        (   (numberp y) nil)
                        (   (< x y))
                    )
                )
            )
        )
    )
)

(defun LM:getfiles:groupbyfunction ( lst fun / tmp1 tmp2 x1 )
    (if (setq x1 (car lst))
        (progn
            (foreach x2 (cdr lst)
                (if (fun x1 x2)
                    (setq tmp1 (cons x2 tmp1))
                    (setq tmp2 (cons x2 tmp2))
                )
            )
            (cons (cons x1 (reverse tmp1)) (LM:getfiles:groupbyfunction (reverse tmp2) fun))
        )
    )
)

(defun LM:getfiles:splitstring ( str )
    (
        (lambda ( l )
            (read
                (strcat "("
                    (vl-list->string
                        (apply 'append
                            (mapcar
                                (function
                                    (lambda ( a b c )
                                        (cond
                                            (   (member b '(45 46 92))
                                                (list 32)
                                            )
                                            (   (< 47 b 58)
                                                (list b)
                                            )
                                            (   (list 32 34 b 34 32))
                                        )
                                    )
                                )
                                (cons nil l) l (append (cdr l) '(( )))
                            )
                        )
                    )
                    ")"
                )
            )
        )
        (vl-string->list (strcase str))
    )
)

(defun LM:getfiles:browseforfolder ( msg dir flg / err fld pth shl slf )
    (setq err
        (vl-catch-all-apply
            (function
                (lambda ( / app hwd )
                    (if (setq app (vlax-get-acad-object)
                              shl (vla-getinterfaceobject app "shell.application")
                              hwd (vl-catch-all-apply 'vla-get-hwnd (list app))
                              fld (vlax-invoke-method shl 'browseforfolder (if (vl-catch-all-error-p hwd) 0 hwd) msg flg dir)
                        )
                        (setq slf (vlax-get-property fld 'self)
                              pth (LM:getfiles:fixdir (vlax-get-property slf 'path))
                        )
                    )
                )
            )
        )
    )
    (if slf (vlax-release-object slf))
    (if fld (vlax-release-object fld))
    (if shl (vlax-release-object shl))
    (if (vl-catch-all-error-p err)
        (prompt (vl-catch-all-error-message err))
        pth
    )
)

(defun LM:getfiles:full->relative ( dir path / p q )
    (setq dir (vl-string-right-trim "\\" dir))
    (cond
        (   (and
                (setq p (vl-string-position 58  dir))
                (setq q (vl-string-position 58 path))
                (/= (strcase (substr dir 1 p)) (strcase (substr path 1 q)))
            )
            path
        )
        (   (and
                (setq p (vl-string-position 92  dir))
                (setq q (vl-string-position 92 path))
                (= (strcase (substr dir 1 p)) (strcase (substr path 1 q)))
            )
            (LM:getfiles:full->relative (substr dir (+ 2 p)) (substr path (+ 2 q)))
        )
        (   (and
                (setq q (vl-string-position 92 path))
                (= (strcase dir) (strcase (substr path 1 q)))
            )
            (strcat ".\\" (substr path (+ 2 q)))
        )
        (   (= "" dir)
            path
        )
        (   (setq p (vl-string-position 92 dir))
            (LM:getfiles:full->relative (substr dir (+ 2 p)) (strcat "..\\" path))
        )
        (   (LM:getfiles:full->relative "" (strcat "..\\" path)))
    )
)

(defun LM:getfiles:str->lst ( str del / pos )
    (if (setq pos (vl-string-search del str))
        (cons (substr str 1 pos) (LM:getfiles:str->lst (substr str (+ pos 1 (strlen del))) del))
        (list str)
    )
)

(defun LM:getfiles:updatefilelist ( dir ext lst )
    (LM:getfiles:listbox "box1" (LM:getfiles:listfiles dir ext lst))
)

(defun LM:getfiles:updateselected ( dir lst )
    (LM:getfiles:listbox "box2" (mapcar '(lambda ( x ) (LM:getfiles:full->relative dir x)) lst))
    lst
)

(defun LM:getfiles:updir ( dir )
    (substr dir 1 (vl-string-position 92 dir nil t))
)

(defun LM:getfiles:fixdir ( dir )
    (vl-string-right-trim "\\" (vl-string-translate "/" "\\" dir))
)

(defun LM:getfiles:removeitems ( itm lst / idx )
    (setq idx -1)
    (vl-remove-if '(lambda ( x ) (member (setq idx (1+ idx)) itm)) lst)
)

(vl-load-com) (princ)