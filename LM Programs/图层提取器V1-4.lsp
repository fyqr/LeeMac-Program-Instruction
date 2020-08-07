;;-------------------=={ Layer Extractor }==------------------;;
;;                                                            ;;
;;  Produces a report detailing the layer structure in every  ;;
;;  drawing in a selected directory (and subdirectories).     ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright ?2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Version:  1.4  28-04-2011                                 ;;
;;------------------------------------------------------------;;

(defun c:LX nil (c:LayerExtract))

(defun c:LayerExtract

   ( /

    ;; --=={ Local Functions }==--

    *error*
    _dclist
    _directorymode
    _directorytext
    _fixdir
    _getlayerproperties
    _getsavepath
    _logo
    _padright
    _popup
    _readconfig
    _replaceentrefs 
    _writeconfig
    _writedcl
    _xmloptions

    ;; --=={ Local Variables }==--

    _lxbit
    _lxcur
    _lxdir
    _lxl
    _lxpad
    _lxref
    _lxsl
    _lxsub
    acapp
    acdoc
    acdocs
    cfgfname
    ch
    data
    dc
    dclfname
    dcltitle
    df
    doc
    ext
    i
    l
    len
    ll
    ln
    lst
    odbx
    ofile
    out
    p
    padc
    savepath
    sfile
    shell
    str
    sym
    symlist
    tc
    tiles
    title
    titles
    tl
    val
    vallist
    versionnumber
    x
    xtitles

    ;; --=={ Global Variables }==--

    ;; -None-
    
   )
  
  (vl-load-com)
  ;; ?Lee Mac 2010

  (setq VersionNumber "1-4")
  
  (setq titles '("图层" "颜色" "线型" "线宽" "打印" "打印样式" "开" "锁定" "冻结" "冻结新视口" "说明")) ;; Headings

  ;;----------------------------------------------------------;;
  ;;                     Local Functions                      ;;
  ;;----------------------------------------------------------;;

  (defun *error* ( msg )

    (if dc (unload_dialog dc))
    (mapcar
      (function
        (lambda ( file ) (if (and file (eq 'FILE (type file))) (close file)))
      )
      (list ofile sfile)
    )
    
    (mapcar 'LM:ReleaseObject (list Shell odbx))
    
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** 错误信息: " msg " **")))
    (princ)
  )

  ;............................................................;

  (defun _GetLayerProperties ( doc mode xref / _TF funcs l )

    (defun _tf ( v ) (if (eq :vlax-true v) "是" "否"))

    (setq funcs
     '(
        (lambda ( x ) (if  (vlax-property-available-p x 'Description) (vla-get-Description x) ""))
        (lambda ( x ) (_tf (vla-get-ViewportDefault x)))
        (lambda ( x ) (_tf (vla-get-Freeze    x)))
        (lambda ( x ) (_tf (vla-get-Lock      x)))
        (lambda ( x ) (_tf (vla-get-Layeron   x)))
        (lambda ( x ) (vla-get-PlotStyleName  x))
        (lambda ( x ) (_tf (vla-get-Plottable x)))
        (lambda ( x / w )
          (if (minusp (setq w (vla-get-lineweight x)))
            "默认值"
            (rtos (/ w 100.) 2 2)
          )
        )
        (lambda ( x ) (vla-get-Linetype x))
        (lambda ( x / tc )
          (if (eq acColorMethodByACI (vla-get-ColorMethod (setq tc (vla-get-TrueColor x))))
            (itoa (vla-get-Color x))
            (vl-prin1-to-string
              (LM:lst->str
                (mapcar '(lambda ( p ) (itoa (vlax-get-property tc p))) '(Red Green Blue)) ","
              )
            )
          )
        )
        (lambda ( x ) (vla-get-name x))
      )
    )        
    
    (vlax-for layer (vla-get-Layers doc)
      (if (or xref (not (wcmatch (vla-get-name layer) "*|*")))
        (setq l
          (cons
            (
              (lambda ( i / result )
                (foreach x funcs
                  (if (= (setq i (lsh i -1)) (logand i mode))
                    (setq result (cons ((eval x) layer) result))
                  )
                )
                result
              )
              2048
            )
            l
          )
        )
      )
    )
    (vl-sort l '(lambda ( a b ) (< (car a) (car b))))
  )

  ;............................................................;
  
  (defun _PadRight ( st ch ln )
    (
      (lambda ( l )
        (while (< (length l) ln) (setq l (cons ch l)))
        (vl-list->string (reverse l))
      )
      (reverse (vl-string->list st))
    )
  )

  ;............................................................;

  (defun _GetSavePath ( / tmp1 )
    (cond      
      ( (setq tmp1 (getvar 'ROAMABLEROOTPREFIX))

        (or (eq "\\" (substr tmp1 (strlen tmp1)))
            (setq tmp1 (strcat tmp1 "\\"))
        )
        (strcat tmp1 "Support")
      )
      ( (setq tmp1 (findfile "ACAD.pat"))

        (setq tmp1 (vl-filename-directory tmp1))

        (and (eq "\\" (substr tmp1 (strlen tmp1)))
             (setq tmp1 (substr tmp1 (1- (strlen tmp1))))
        )
        tmp1
      )
    )
  )

  ;............................................................;

  (defun _WriteConfig ( filename lst / ofile )
    
    (if (setq ofile (open filename "w")) 
      (progn
        (foreach x lst (write-line (vl-prin1-to-string x) ofile))
        
        (setq ofile (close ofile))
        T
      )
    )
  )

  ;............................................................;

  (defun _ReadConfig ( filename lst / ofile )

    (if (and (setq filename (findfile filename))
             (setq ofile (open filename "r")))
      (progn
        (foreach x lst (set x (read (read-line ofile))))
        
        (setq ofile (close ofile))
        T
      )
    )
  )
  
  ;............................................................;

  (defun _WriteDCL ( fname / ofile )
    
    (if (not (findfile fname))

      (if (setq ofile (open fname "w"))
        (progn
          (foreach str

            '(
             "//--------------------=={ 图层提取器 }==-------------------//"
             "//                                                            //"
             "//  此文件是图层提取器的对话框文件，图层提取器LISP程序        //"
             "//  运行时将调用此文件。             			    //"
             "//------------------------------------------------------------//"
             "//  作者: Lee Mac, Copyright ?2011 - www.lee-mac.com       //"
             "//------------------------------------------------------------//"
             ""
             "boxcol : boxed_column {  width =  65.0; fixed_width  = true; alignment = centered; }"
             "butt12 :       button {  width =  12.0; fixed_width  = true; alignment = centered; }"
             "space1 :       spacer { height =   0.1; fixed_height = true;                       }"
             "pop    : popup_list   {  width =  13.6; fixed_width = true ; alignment = centered; }"
             ""
             "layerextract : dialog { key = \"dctitle\"; spacer;"
             "  : text { label = \"Copyright (c) 2011 Lee Mac\"; alignment = right; }"
             ""
             "  : boxcol { label = \"要提取的图层要素\";"
             "    spacer;"
             ""
             "    : row { alignment = centered; spacer;"
             ""
             "      : column {"
             ""
             "        : toggle { key = \"layer\"; label = \"名称\"; value = \"1\"; is_enabled = false; }"
             ""
             "        : toggle { key = \"colour\"; label = \"颜色\"; }"
             ""
             "        : toggle { key = \"linetype\"; label = \"线型\"; }"
             ""
             "      }"
             ""
             "      : column {"
             "      "
             "        : toggle { key = \"lineweight\"; label = \"线宽\"; }"
             ""
             "        : toggle { key = \"plot\"; label = \"打印\"; }"
             ""
             "        : toggle { key = \"plotstyle\"; label = \"打印样式\"; }"
             ""
             "      }"
             ""
             "      : column {"
             ""
             "        : toggle { key = \"on\"; label = \"开\"; }"
             ""
             "        : toggle { key = \"locked\"; label = \"锁定\"; }"
             ""
             "        : toggle { key = \"frozen\"; label = \"冻结\"; }"
             "        "
             "      }"
             ""
             "      : column {"
             ""
             "        : toggle { key = \"frozenvp\"; label = \"冻结新视口\"; }"
             ""
             "        : toggle { key = \"description\"; label = \"说明\"; }"
             ""
             "        : spacer { height = 1.5; fixed_height = true; }"
             "        "
             "      }"
             "      "
             "    }"
             ""
             "    spacer;"
             "  }"
             "  "
             "  spacer;"
             ""
             "  : boxcol { label = \"图纸目录\";"
             ""
             "    : row {"
             ""
             "      : column {"
             ""
             "        space1;"
             "        : text { key = \"dir_text\"; alignment = left; }"
             "        space1;"
             ""
             "      }"
             ""
             "      : butt12 { label = \"浏览...\"; key = \"dir\"; }"
             ""
             "    }"
             ""
             "    : row {"
             ""
             "      : toggle { key = \"sub_dir\"; label = \"包括子目录\"; }"
             "      : toggle { key = \"cur_dwg\"; label = \"仅当前图纸文件\"   ; }"
             "      "
             "    }"
             ""
             "    spacer;"
             "  }"
             ""
             "  spacer;"
             ""
             "  : boxcol { label = \"输出选项\";"
             ""
             "    : row {"
             ""
             "      : column {"
             ""
             "        space1;"
             "        : text { key = \"out_text\"; alignment = left; }"
             "        space1;"
             ""
             "      }"
             ""
             "      : butt12 { label = \"浏览...\"; key = \"browse\"; }"
             ""
             "    }"
             ""
             "    : row {"
             ""
             "      : column {"
             ""
             "        space1;"
             "        : toggle { key = \"xref\"; label = \"包括外部参照图层\"; alignment = left; }"
             "        space1;"
             ""
             "      }"
             ""
             "      : column {"
             ""
             "        space1;"
             "        : text { key = \"pad_text\"; label = \"填充字符:\"; alignment = right; }"
             "        space1;"
             ""
             "      }"
             ""
             "      : column {"
             ""
             "        : pop { key = \"pad\"; }"
             "        space1;"
             ""
             "      }"
             ""
             "    }"
             "  "
             "  }"
             ""
             "  spacer;"
             ""
             "  : row { spacer;"
             ""
             "    : butt12 { key = \"xml\"; label = \"可扩展标记语言\"; }"
             ""
             "    : spacer { width = 3.06; fixed_width  = true;"
             "               height = 2.06; fixed_height = true; }"
             ""
             "    ok_cancel;"
             "    "
             "    : image { key = \"logo\"; alignment = centered;"
             "              width = 16.06 ; fixed_width  = true;"
             "              height = 2.06 ; fixed_height = true; color = -15; }"
             "  }"
             "}"
             ""
             "xml : dialog { label = \"语言选项\"; spacer;"
             ""
             "  : row { spacer_1;"
             ""
             "    : column {"
             ""
             "      : toggle { key = \"xl\"; label = \"Excel兼容模式\"; }"
             ""
             "      : toggle { key = \"xsl\"; label = \"创建XSL样式表\"; }"
             ""
             "    }"
             ""
             "  }"
             ""
             "  spacer; ok_cancel;"
             "}"
             )

            (write-line str ofile)
          )
          (setq ofile (close ofile))

          (while (not (findfile fname)))
          t
        )
      )
      t
    )
  )

  ;............................................................;

  (defun _Logo ( key )
  
    (start_image key)
    (mapcar 'vector_image
     '(022 021 001 000 000 000 000 007 000 000 000 000 001 006 006 006 006 007 043 036 027 036 030 021 021 021 022 022 022 022
       021 021 021 028 028 028 027 027 030 029 029 030 052 043 043 043 044 044 046 046 045 045 045 045 052 052 052 051 051 051
       051 051 052 062 065 066 068 068 068 068 067 067 075 075 075 074 074 073 066 058 058 059 059 059 059 052 057 057 056 056
       056 056 057 058 065 065 065 065 066 095 094 094 092 091 091 091 090 089 089 088 087 086 085 074 074 075 075 076 077 078
       079 080 081 082 083 084 085 086 087 088 088 089 090 091 092 093 094 095 074 073 073 072 072 071 071 071 071 071 071 071
       072 072 072 073 084 083 082 081 080 079 079 078 077 077 076 076 076 076 076 077 077 078 079 079 080 081 082 083 094 094
       095 083 083 082 081 080 079 078 077 076 075 074 084 085 086 087 088 089 089 090 091 091 091 091 092 095 094 093 092 091
       090 089 089 088 087 086 085 084)
     '(020 020 023 023 023 024 024 000 000 000 000 001 001 020 001 001 001 000 002 024 007 015 000 000 000 000 001 001 023 023
       023 024 024 024 024 024 023 023 002 001 001 000 000 000 000 000 001 001 007 023 023 023 024 024 024 024 024 023 023 001
       001 001 000 010 016 019 021 022 023 024 024 024 024 024 024 023 023 022 004 004 005 005 006 006 007 024 024 024 024 023
       023 022 019 016 007 007 006 005 005 022 022 022 017 017 018 018 019 020 020 020 021 021 021 021 022 023 023 023 024 024
       024 025 025 025 025 025 025 025 025 024 024 024 023 023 022 022 022 022 007 008 008 009 010 011 012 013 014 015 016 017
       018 019 019 020 021 021 021 021 020 020 019 019 018 017 016 015 014 013 012 012 011 010 009 009 008 008 008 007 007 007
       007 004 004 004 004 004 004 004 005 005 006 006 007 007 008 008 008 009 009 009 010 011 011 011 011 007 007 007 006 006
       005 005 004 004 004 004 004 004)
     '(021 006 000 000 000 000 021 000 000 000 000 001 001 006 006 006 007 007 036 046 036 030 021 021 021 022 022 022 022 021
       021 021 028 028 028 027 027 027 029 029 030 030 043 043 043 044 044 043 046 045 045 045 045 052 052 052 051 051 051 051
       051 052 052 065 058 068 068 068 068 067 067 075 075 075 074 074 073 065 058 058 059 059 059 059 051 057 057 056 056 056
       056 057 066 062 065 065 065 066 066 094 094 095 091 091 091 090 089 089 088 087 086 085 084 074 075 075 076 077 078 079
       080 081 082 083 084 085 086 087 088 088 089 090 091 092 093 094 095 092 073 073 072 072 071 071 071 071 071 071 071 072
       072 072 073 074 083 082 081 080 079 079 078 077 077 076 076 076 076 076 077 077 078 079 079 080 081 082 083 084 094 095
       094 083 082 081 080 079 078 077 076 075 074 074 085 086 087 088 089 089 090 091 091 091 091 092 095 094 093 092 091 090
       089 089 088 087 086 085 084 083)
     '(020 020 023 023 024 024 024 000 000 000 001 001 023 001 001 001 000 000 015 007 024 002 000 000 000 001 001 023 023 023
       024 024 024 024 024 023 023 007 001 001 000 000 000 000 000 001 001 002 023 023 023 024 024 024 024 024 023 023 001 001
       001 000 000 016 016 021 022 023 024 024 024 024 024 024 023 023 022 007 004 005 005 006 006 007 022 024 024 024 023 023
       022 019 019 010 007 006 005 005 004 022 022 022 017 018 018 019 020 020 020 021 021 021 021 022 023 023 023 024 024 024
       025 025 025 025 025 025 025 025 024 024 024 023 023 022 022 022 022 017 008 008 009 010 011 012 013 014 015 016 017 018
       019 019 020 021 021 021 021 020 020 019 019 018 017 016 015 014 013 012 012 011 010 009 009 008 008 008 007 007 007 007
       007 004 004 004 004 004 004 005 005 006 006 007 007 008 008 008 009 009 009 010 011 011 011 011 007 007 007 006 006 005
       005 004 004 004 004 004 004 004)
     '(178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
       178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
       178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
       178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
       178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
       178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
       178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
       178 178 178 178 178 178 178 178)
    )    
    (end_image)
  )

  ;............................................................;

  (defun _FixDir ( s )
    (if (eq "\\" (substr s (strlen s)))
      (substr s 1 (1- (strlen s)))
      s
    )
  )

  ;............................................................;

  (defun _DirectoryMode ( val )
    (foreach x '("sub_dir" "dir" "dir_text") (mode_tile x (atoi val)))
  )

  ;............................................................;

  (defun _DirectoryText ( key str )
    (set_tile key
      (if str
        (if (< 50 (strlen str))
          (strcat (substr str 1 47) "...") str
        )
        ""
      )
    )
  )

  ;............................................................;

  (defun _dcList ( key lst )
    (start_list key)
    (mapcar 'add_list lst)
    (end_list)
  )

  ;............................................................;

  (defun _Popup ( title flags msg / WSHShell result )
    
    (setq WSHShell (vlax-create-object "WScript.Shell"))
    (setq result   (vlax-invoke WSHShell 'Popup msg 0 title flags))
    (vlax-release-object WSHShell)

    result
  )

  ;............................................................;

  (defun _ReplaceEntRefs ( string / _stringsubst )

    (defun _stringsubst ( new old string / i nl )
      (setq i 0 nl (strlen new))

      (while (and (< i (strlen string)) (setq i (vl-string-search old string i)))
        (setq string (vl-string-subst new old string i) i (+ i nl))
      )
      string
    )
    
    (mapcar
      (function
        (lambda ( pair ) (setq string (_stringsubst (car pair) (cdr pair) string)))
      )
     '(
        ("&lt;"   .  "<")
        ("&gt;"   .  ">")
        ("&amp;"  .  "&")
        ("&apos;" .  "'")
        ("&quot;" . "\"")
      )
    )
    string
  )

  ;............................................................;

  (defun _XMLOptions ( handle / xl xsl )    
    (cond
      (
        (not (new_dialog "xml" handle))
        (princ "\n** 加载语言选项对话框时发生错误 **")
      )
      (t
        (set_tile "xl"  (setq xl  _LXL ))
        (set_tile "xsl" (setq xsl _LXSL))

        (action_tile "xl"  "(setq xl  $value xsl (set_tile \"xsl\" \"0\"))")
        (action_tile "xsl" "(setq xsl $value xl  (set_tile \"xl\"  \"0\"))")

        (if (= 1 (start_dialog)) (setq _LXL xl _LXSL xsl))
      )
    )
  )

  ;;----------------------------------------------------------;;
  ;;                       Main Routine                       ;;
  ;;----------------------------------------------------------;;

  (setq acapp  (vlax-get-acad-object)
        acdoc  (vla-get-ActiveDocument acapp)
        acdocs (vlax-for doc (vla-get-Documents acapp)
                 (setq acdocs (cons (cons (strcase (vla-get-FullName doc)) doc) acdocs))
               )
  )

  (if (not (vl-file-directory-p (setq SavePath (_GetSavePath))))
    (progn
      (princ "\n** 保存路径无效 **") (exit)
    )
  )

  (setq dclfname (strcat SavePath "\\LMAC_LayerExtract_V" VersionNumber ".dcl")
        cfgfname (strcat SavePath "\\LMAC_LayerExtract_V" VersionNumber ".cfg")
        dcltitle (strcat "图层提取器 V" (vl-string-translate "-" "." VersionNumber))
  )

  (setq SymList '(_LXDir _LXCur _LXSub _LXBit _LXRef _LXPad _LXL _LXSL)
        ValList  (list (_FixDir (getvar 'DWGPREFIX)) "0" "0" 2047 "0" "0" "0" "1")
  )

  (or (findfile cfgfname)
      (_WriteConfig cfgfname ValList)
  )

  (_ReadConfig cfgfname SymList)

  (mapcar '(lambda ( sym val ) (or (boundp sym) (set sym val))) SymList ValList)
  
  ;............................................................;

  (cond
    (
      (not (_WriteDCL dclfname))

      (princ "\n** 生成对话框时发生错误 **")
    )
    (
      (<= (setq dc (load_dialog dclfname)) 0)

      (princ "\n** 加载DCL对话框文件时发生错误 **")
    )
    (
      (not (new_dialog "layerextract" dc))

      (setq dc (unload_dialog dc))
      (princ "\n** 加载对话框界面时发生错误 **")
    )
    (t


      (setq tiles '("colour" "linetype" "lineweight" "plot" "plotstyle" "on" "locked" "frozen" "frozenvp" "description"))

      (set_tile "dctitle" dcltitle)

      (_DirectoryText "dir_text" _LXDir)
      (set_tile       "sub_dir"  _LXSub)
      (_DirectoryMode (set_tile "cur_dwg" _LXCur))

      (set_tile "out_text" "请选择输出文件路径")
      (set_tile "xref" _LXref)

      (_dcList  "pad" '("空格 [ ]" "制表符 [	 ]" "点 [.]" "连字符 [-]" "逗号 [,]" "分号 [;]"))
      (set_tile "pad" _LXPad)

      (foreach x '("pad" "pad_text") (mode_tile x 1))
      (mode_tile "xml" 1)

      (_Logo "logo")

      (
        (lambda ( i )
          (mapcar
            (function
              (lambda ( tile )
                (set_tile tile
                  (if (= (setq i (lsh i 1)) (logand _LXBit i)) "1" "0")
                )
              )
            )
            tiles
          )
        )
        1
      )

      (action_tile "dir"
        (vl-prin1-to-string
          (quote
            (progn
              (if (setq tmp1 (LM:DirectoryDialog "选择要处理的图纸目录..." nil 320))
                (_DirectoryText "dir_text" (setq _LXDir tmp1))
              )
            )
          )
        )
      )

      (action_tile "sub_dir" "(setq _LXSub $value)")

      (action_tile "cur_dwg" "(_DirectoryMode (setq _LXCur $value))")

      (action_tile "browse"
        (vl-prin1-to-string
          (quote
            (progn
              (if (setq tmp1 (getfiled "创建输出文件" "" "txt;csv;xml" 1))
                (_DirectoryText "out_text" (setq out tmp1))
                (if out
                  (_DirectoryText "out_text" out)
                  (set_tile "out_text" "请选择输出文件的路径")
                )
              )
              (if out
                (cond
                  ( (eq ".TXT" (setq ext (strcase (vl-filename-extension out))))

                    (foreach x '("pad" "pad_text") (mode_tile x 0))
                    (mode_tile "xml" 1)
                  )
                  ( (eq ".XML" ext)

                    (foreach x '("pad" "pad_text") (mode_tile x 1))
                    (mode_tile "xml" 0)
                  )
                  (t
                   
                    (foreach x '("pad" "pad_text") (mode_tile x 1))
                    (mode_tile "xml" 1)
                  )
                )
              )
            )
          )
        )
      )

      (action_tile "xref" "(setq _LXref $value)")

      (action_tile "pad"  "(setq _LXPad $value)")

      (action_tile "xml"  "(_XMLOptions dc)")

      (
        (lambda ( i )
          (mapcar
            (function
              (lambda ( tile )
                (action_tile tile
                  (strcat "(setq _LXBit ((if (eq \"1\" $value) + -) _LXBit " (itoa (setq i (lsh i 1))) " ))")
                )
              )
            )
            tiles
          )
        )
        1
      )

      (action_tile "accept"
        (vl-prin1-to-string
          (quote
            (progn
              (cond
                (
                  (not out)

                  (_Popup "Information" 64 "Please Specify an Output File")
                )
                (
                  (not
                    (setq lst
                      (cond
                        ( (eq "1" _LXCur)
                         
                          (list
                            (cond
                              ( (eq "" (vla-get-FullName acdoc))
                               
                                (strcat (_FixDir (vla-get-Path acdoc)) "\\" (vla-get-name acdoc))
                              )
                              ( (vla-get-FullName acdoc) )
                            )
                          )
                        )
                        ( (LM:GetAllFiles _LXDir (eq "1" _LXSub) "*.dwg") )
                      )
                    )
                  )

                  (_Popup "Information" 48 "No Drawing Files Found in the Selected Directory")
                )
                ( (done_dialog 1) )
              )
            )
          )
        )
      )
     
      (setq df (start_dialog) dc (unload_dialog dc))

  ;............................................................;

      (if (= 1 df)
        (progn
          (setq odbx (LM:ObjectDBXDocument))
         
          (foreach dwg lst
            (if
              (setq doc
                (cond
                  ( (eq "1" _LXCur) acdoc )
                  ( (cdr (assoc (strcase dwg) acdocs)) )
                  ( (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-open (list odbx dwg)))) odbx )
                )
              )
              (progn
                (setq data (cons (cons dwg (_GetLayerProperties doc _LXBit (eq "1" _LXref))) data))
                (princ (strcat "\n--> 提取文件: " (vl-filename-base dwg) ".dwg"))
              )          
              (princ (strcat "\n**  打开: " (vl-filename-base dwg) ".dwg时发生错误"))
            )
          )
          (LM:ReleaseObject odbx)

          (if (setq data (vl-sort data '(lambda ( a b ) (< (car a) (car b)))) l data)

            (if (setq ofile (open out "w"))
              (progn
                (
                  (lambda ( i )
                    (setq titles
                      (vl-remove-if-not
                        (function
                          (lambda ( title ) 
                            (= (setq i (if (zerop i) 1 (lsh i 1))) (logand i _LXBit))
                          )
                        )
                        titles
                      )
                    )
                  )
                  0
                )

                (cond
                  (
                    (eq ".TXT" (setq ext (strcase (vl-filename-extension out))))
                   
                    (write-line
                      (strcat "图层提取时间: "
                        (menucmd "m=$(edtime,$(getvar,DATE),DDDD DD MONTH YYYY HH:MM:SS)")
                      )
                      ofile
                    )

                    (setq tl titles l (apply 'append (mapcar 'cdr l)) padC (nth (atoi _LXPad) '(32 9 46 45 44 59)))

                    (while (car (setq x (mapcar 'car l)))
                      (setq ll (cons (+ 5 (apply 'max (mapcar 'strlen (cons (car tl) x)))) ll) tl (cdr tl) l (mapcar 'cdr l))
                    )
                    (setq ll (reverse ll))

                    (foreach x data
                      (write-line (strcat "\n图纸文件:  " (car x)) ofile)

                      (foreach item (cons titles (cdr x))
                        (write-line
                          (vl-string-right-trim (chr padC)
                            (apply 'strcat
                              (mapcar '(lambda ( str len ) (_PadRight str padC len)) item ll)
                            )
                          )
                          ofile
                        )
                      )
                    )
                  )
                  (
                    (eq ".CSV" ext)
                   
                    (write-line
                      (strcat "图层提取时间:,"
                        (menucmd "m=$(edtime,$(getvar,DATE),DDDD DD MONTH YYYY HH:MM:SS)")
                      )
                      ofile
                    )

                    (foreach x data
                      (write-line (strcat "\n图纸文件:," (car x)) ofile)

                      (foreach item (cons titles (cdr x))
                        (write-line (LM:lst->str item ",") ofile)
                      )
                    )
                  )
                  (
                    (eq ".XML" ext)

                    (cond
                      (
                        (eq "0" _LXL)
                                          
                        (setq xtitles (mapcar '(lambda ( title ) (vl-string-translate " " "_" (strcase title t))) titles))
                       
                        (write-line "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>" ofile)
                        (if (eq "1" _LXSL)
                          (write-line (strcat "<?xml-stylesheet type=\"text/xsl\" href=\"" (vl-filename-base out) ".xsl\"?>") ofile)
                        )                       
                        (write-line "<extraction>" ofile)

                        (write-line
                          (strcat "\t<title>Layer Extraction: "
                            (menucmd "m=$(edtime,$(getvar,DATE),DDDD DD MONTH YYYY HH:MM:SS)")
                            "</title>"
                          )
                          ofile
                        )

                        (foreach x data
                          (write-line "\t<file>" ofile)
                          (write-line (strcat "\t\t<filename>" (_ReplaceEntRefs (car x)) "</filename>") ofile)

                          (foreach item (cdr x) (write-line "\t\t<layer_item>" ofile)                            
                            (mapcar
                              (function
                                (lambda ( tag value )
                                  (write-line (strcat "\t\t\t<" tag ">" (_ReplaceEntRefs value) "</" tag ">") ofile)
                                )
                              )
                              xtitles item
                            )
                            (write-line "\t\t</layer_item>" ofile)
                          )
                          (write-line "\t</file>" ofile)
                        )
                        (write-line "</extraction>" ofile)

                        (if (eq "1" _LXSL)
                          (if (setq sfile (open (strcat (vl-filename-directory out) "\\" (vl-filename-base out) ".xsl") "w"))
                            (progn
                              (mapcar
                                (function
                                  (lambda ( x ) (write-line x sfile))
                                )
                                (append
                                  (list
                                    "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>"
                                    "<xsl:stylesheet version=\"1.0\" xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\">"
                                    "<xsl:template match=\"/\">"
                                    "<html>"
                                    "\t<head><title>Layer Extraction</title></head>"
                                    "\t<body style=\"font-family:Verdana,Arial;font-size:10pt;background-color:#cccccc;\">"
                                    "\t\t<h3><xsl:value-of select=\"extraction/title\"/></h3>"
                                    "\t\t<table border=\"1\" cellpadding=\"5\" style=\"border-collapse:collapse;border:1px solid navy;\">"
                                    "\t\t\t<xsl:for-each select=\"extraction/file\">"
                                    "\t\t\t\t<tr bgcolor=\"#2e2e2e\">"
                                    "\t\t\t\t<th colspan=\"11\" style=\"text-align:left;color:silver;\"><xsl:value-of select=\"filename\"/></th>"
                                    "\t\t\t\t</tr>"
                                    "\t\t\t\t<tr bgcolor=\"#a8c3f3\">"
                                  )
                                  (mapcar
                                    (function
                                      (lambda ( title ) (strcat "\t\t\t\t<th>" title "</th>"))
                                    )
                                    titles
                                  )
                                  (list
                                    "\t\t\t\t</tr>"
                                    "\t\t\t\t<xsl:for-each select=\"layer_item\">"
                                    "\t\t\t\t\t<tr>"                         
                                    "\t\t\t\t\t<xsl:choose>"
                                    "\t\t\t\t\t\t<xsl:when test=\"position() mod 2 = 0\">"
                                    "\t\t\t\t\t\t\t<xsl:attribute name=\"bgcolor\">#a8c3f3</xsl:attribute>"
                                    "\t\t\t\t\t\t</xsl:when>"
                                    "\t\t\t\t\t\t<xsl:otherwise>"
                                    "\t\t\t\t\t\t\t<xsl:attribute name=\"bgcolor\">#5c97ff</xsl:attribute>"
                                    "\t\t\t\t\t\t</xsl:otherwise>"
                                    "\t\t\t\t\t</xsl:choose>"
                                    (strcat "\t\t\t\t\t<td style=\"font-weight:bold;\"><xsl:value-of select=\"" (car xtitles) "\"/></td>")
                                  )
                                  (mapcar
                                    (function
                                      (lambda ( title ) (strcat "\t\t\t\t\t<td><xsl:value-of select=\"" title "\"/></td>"))
                                    )
                                    (cdr xtitles)
                                  )
                                  (list
                                    "\t\t\t\t\t</tr>"
                                    "\t\t\t\t</xsl:for-each>"
                                    "\t\t\t</xsl:for-each>"
                                    "\t\t</table>"
                                    "\t</body>"
                                    "</html>"
                                    "</xsl:template>"
                                    "</xsl:stylesheet>"
                                  )
                                )
                              )
                              (setq sfile (close sfile))
                            )
                            ;;(princ "\n** Unable to Create XML StyleSheet (XSL) File **")
                            (princ "\n** 无法创建样式表(XSL)文件 **")
                          )
                        )
                      )
                      (
                        (eq "1" _LXL)

                        (foreach x
                         '(
                           "<?xml version=\"1.0\"?>"
                           "<?mso-application progid=\"Excel.Sheet\"?>"
                           "<Workbook"
                           "xmlns=\"urn:schemas-microsoft-com:office:spreadsheet\""
                           "xmlns:o=\"urn:schemas-microsoft-com:office:office\""
                           "xmlns:x=\"urn:schemas-microsoft-com:office:excel\""
                           "xmlns:ss=\"urn:schemas-microsoft-com:office:spreadsheet\""
                           "xmlns:html=\"http://www.w3.org/TR/REC-html40\">"
                          )
                          (write-line x ofile)
                        )

                        (foreach x data
                          (write-line (strcat "\t<Worksheet ss:Name=\"" (_ReplaceEntRefs (vl-filename-base (car x))) "\">") ofile)

                          (write-line
                            (strcat
                              "\t\t<Table ss:ExpandedColumnCount=\"" (itoa (length titles)) "\""
                              " ss:ExpandedRowCount=\"" (itoa (1+ (length (cdr x))))
                              "\" x:FullColumns=\"1\" x:FullRows=\"1\" ss:DefaultRowHeight=\"15\">"
                            )
                            ofile
                          )
                          (write-line "\t\t\t<Row>" ofile)
                          
                          (foreach title titles
                            (write-line (strcat "\t\t\t\t<Cell><Data ss:Type=\"String\">" (_ReplaceEntRefs title) "</Data></Cell>") ofile)
                          )
                          (write-line "\t\t\t</Row>" ofile)
                          
                          (foreach item (cdr x) (write-line "\t\t\t<Row>"  ofile)                            
                            (foreach prop item
                              (write-line
                                (strcat "\t\t\t\t<Cell><Data ss:Type=\""
                                  (if (numberp (read prop)) "Number" "String") "\">" (_ReplaceEntRefs prop) "</Data></Cell>"
                                )
                                ofile
                              )
                            )
                            (write-line "\t\t\t</Row>" ofile)
                          )                          
                          (write-line "\t\t</Table>" ofile)
                          (write-line "\t</Worksheet>" ofile)
                        )
                        (write-line "</Workbook>" ofile)
                      )
                    )
                  )
                )

                (setq ofile (close ofile))
                (princ "\n-->>> 提取完成.")
                
                (_WriteConfig cfgfname (mapcar 'eval SymList))
              )
              (princ "\n** 创建输出文件时发生出错 **")
            )
            (princ "\n** 没有数据写入输出文件 **")
          )
        )
        (princ "\n*取消*")
      )
    )
  )

  (princ)
)

;;-------------------=={ Directory Dialog }==-----------------;;
;;                                                            ;;
;;  Displays a dialog prompting the user to select a folder   ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright ?2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  msg  - message to display at top of dialog                ;;
;;  dir  - root directory (or nil)                            ;;
;;  flag - bit coded flag specifying dialog display settings  ;;
;;------------------------------------------------------------;;
;;  Returns:  Selected folder filepath, else nil              ;;
;;------------------------------------------------------------;;

(defun LM:DirectoryDialog ( msg dir flag / Shell HWND Fold Self Path ac )

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

;;--------------------=={ Get All Files }==-------------------;;
;;                                                            ;;
;;  Retrieves all files or those of a specified filetype that ;;
;;  reside in a directory (and, optionally, subdirectories)   ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright ?2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  Dir      - Directory to search                            ;;
;;  Subs     - Boolean, if T, subdirectories are included     ;;
;;  Filetype - (optional) Filter for filetype (DOS pattern)   ;;
;;------------------------------------------------------------;;
;;  Returns:  List of filenames, else nil if none are found   ;;
;;------------------------------------------------------------;;

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

;;-----------------=={ ObjectDBX Document }==-----------------;;
;;                                                            ;;
;;  Retrieves a version specific ObjectDBX Document object    ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright ?2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments: - None -                                       ;;
;;------------------------------------------------------------;;
;;  Returns:  VLA ObjectDBX Document object, else nil         ;;
;;------------------------------------------------------------;;

(defun LM:ObjectDBXDocument ( / acVer )
  (vla-GetInterfaceObject (vlax-get-acad-object)
    (if (< (setq acVer (atoi (getvar "ACADVER"))) 16)
      "ObjectDBX.AxDbDocument"
      (strcat "ObjectDBX.AxDbDocument." (itoa acVer))
    )
  )
)

;;------------------=={ Release Object }==--------------------;;
;;                                                            ;;
;;  Releases a VLA Object from memory via plentiful error     ;;
;;  trapping                                                  ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright ?2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  obj - VLA Object to be released from memory               ;;
;;------------------------------------------------------------;;
;;  Returns:  T if Object Released, else nil                  ;;
;;------------------------------------------------------------;;

(defun LM:ReleaseObject ( obj ) (vl-load-com)
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

;;-------------------=={ List to String }==-------------------;;
;;                                                            ;;
;;  Constructs a string from a list of strings separating     ;;
;;  each element by a specified delimiter                     ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright ?2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  lst - a list of strings to process                        ;;
;;  del - delimiter by which to separate each list element    ;;
;;------------------------------------------------------------;;
;;  Returns:  String containing each string in the list       ;;
;;------------------------------------------------------------;;

(defun LM:lst->str ( lst del )
  (if (cdr lst)
    (strcat (car lst) del (LM:lst->str (cdr lst) del))
    (car lst)
  )
)

;;------------------------------------------------------------;;

(princ)
(princ "\n:: 图层提取器 | Version 1.4 | ?Lee Mac 2011 www.lee-mac.com ::")
(princ "\n:: 调用命令 \"LayerExtract\" 或 \"LX\"  ::")
(princ)

;;------------------------------------------------------------;;
;;                         End of File                        ;;
;;------------------------------------------------------------;;