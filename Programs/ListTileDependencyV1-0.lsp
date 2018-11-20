;; DCL List-Tile Dependency  -  Lee Mac
;; Configures action_tile statements for the list of keys to enabled dependency between the DCL tiles.
;; key     - [lst] List of DCL tile keys in order of dependency
;; lst-sym - [sym] Quoted variable containing list data
;; rtn-sym - [sym] Quoted variable containing initial selection indexes

(defun LM:dcld:action ( key lst-sym rtn-sym )
    
    (defun LM:dcld:addlist ( key lst )
        (start_list key)
        (foreach itm lst (add_list itm))
        (end_list)
    )
    (defun LM:dcld:getlists ( idx lst )
        (if (cdr idx)
            (cons (mapcar 'car lst) (LM:dcld:getlists (cdr idx) (cdr (nth (car idx) lst))))
            lst
        )
    )
    (defun LM:substnth ( itm idx lst )
        (if lst
            (if (zerop idx)
                (cons itm (cdr lst))
                (cons (car lst) (LM:substnth itm (1- idx) (cdr lst)))
            )
        )
    )
    (defun LM:dcld:actions ( key lst-sym rtn-sym lvl / fun )
        (setq fun
            (if (cdr key)
                (list 'lambda '( val lst / tmp )
                    (list 'setq  rtn-sym  (list 'LM:substnth '(atoi val) lvl rtn-sym)
                                'tmp      (list 'LM:dcld:getlists rtn-sym 'lst)
                    )
                    (list 'LM:dcld:addlist (cadr key) (list 'nth (1+ lvl) 'tmp))
                    (list 'if (list '<= (list 'length (list 'nth (1+ lvl) 'tmp)) (list 'nth (1+ lvl) rtn-sym))
                        (list 'setq rtn-sym (list 'LM:substnth 0 (1+ lvl) rtn-sym))
                    )
                    (list
                        (LM:dcld:actions (cdr key) lst-sym rtn-sym (1+ lvl))
                        (list 'set_tile (cadr key) (list 'itoa (list 'nth (1+ lvl) rtn-sym))) 'lst
                    )
                )
                (list 'lambda '( val lst )
                    (list 'setq rtn-sym (list 'LM:substnth '(atoi val) lvl rtn-sym))
                )
            )
        )
        (action_tile (car key) (vl-prin1-to-string (list fun '$value lst-sym)))
        fun
    )
    (mapcar 'LM:dcld:addlist key (LM:dcld:getlists (eval rtn-sym) (eval lst-sym)))
    (   (eval (LM:dcld:actions key lst-sym rtn-sym 0))
        (set_tile (car key) (itoa (car (eval rtn-sym))))
        (eval lst-sym)
    )
    (princ)
)

;; DCL List-Tile Dependency  -  Get Items  -  Lee Mac
;; Returns a list of the items selected from each dependent list tile.
;; idx - [lst] List of selection indexes
;; lst - [lst] List data

(defun LM:dcld:getitems ( idx lst / tmp )
    (if (cdr idx)
        (cons (car (setq tmp (nth (car idx) lst))) (LM:dcld:getitems (cdr idx) (cdr tmp)))
        (list (nth (car idx) (car lst)))
    )
)