;; MD5 Cryptographic Hash Function  -  V1.1 2016-03-27  -  Lee Mac
;; AutoLISP implementation of the MD5 algorithm by Ronald Rivest
;; lst - [lst] List of bytes for which to generate hash
;; Returns 128-bit (16-byte) hash string.

(defun LM:MD5 ( lst / a b c d f g h i k l r w x y )
    
    ;; k[n] = floor(abs(sin(n+1)))*2^32 ; n:1-64
    (setq k
        (mapcar '(lambda ( x ) (md5:int->bits x 32))
           '(
                3614090360 3905402710 0606105819 3250441966 4118548399 1200080426 2821735955 4249261313
                1770035416 2336552879 4294925233 2304563134 1804603682 4254626195 2792965006 1236535329
                4129170786 3225465664 0643717713 3921069994 3593408605 0038016083 3634488961 3889429448
                0568446438 3275163606 4107603335 1163531501 2850285829 4243563512 1735328473 2368359562
                4294588738 2272392833 1839030562 4259657740 2763975236 1272893353 4139469664 3200236656
                0681279174 3936430074 3572445317 0076029189 3654602809 3873151461 0530742520 3299628645
                4096336452 1126891415 2878612391 4237533241 1700485571 2399980690 4293915773 2240044497
                1873313359 4264355552 2734768916 1309151649 4149444226 3174756917 0718787259 3951481745
            )
        )
    )
    
    ;; bit-shift values
    (setq r
       '(
            07 12 17 22  07 12 17 22  07 12 17 22  07 12 17 22
            05 09 14 20  05 09 14 20  05 09 14 20  05 09 14 20
            04 11 16 23  04 11 16 23  04 11 16 23  04 11 16 23
            06 10 15 21  06 10 15 21  06 10 15 21  06 10 15 21
        )
    )
    
    ;; Initial hash values: forward/backward count in little-endian hex
    (setq h
        (mapcar '(lambda ( x ) (md5:int->bits x 32))
           '(
                1732584193 ; 0x67452301 = 01234567
                4023233417 ; 0xefcdab89 = 89abcdef
                2562383102 ; 0x98badcfe = fedcda98
                0271733878 ; 0x10325476 = 76543210
            )
        )
    )
    
    ;; Pre-processing:
    ;; Append 0x80 to list of byte values
    ;; Append 0x00 until list has length of 448 bits (mod 512)
    ;; Append length of string in bytes (little-endian) mod(2^64)
    (setq l (cons 128 (reverse lst)))
    (repeat (rem (+ 64 (- 56 (rem (length l) 64))) 64) (setq l (cons 0 l)))
    (setq l (append (reverse l) (md5:bits->bytes (md5:int->bits (* 8 (length lst)) 64))))
    
    ;; Process list in 512-bit (64-byte) chunks
    (repeat (/ (length l) 64)
        
        ;; Construct list of 16 32-bit (4-byte) words
        (repeat 16
            (setq w (cons (md5:bytes->bits (mapcar '+ l '(0 0 0 0))) w)
                  l (cddddr l)
            )
        )
        (setq w (reverse w))
        
        ;; Initialise variables to hash values
        ;; Lists of bit values are used as AutoLISP does not support 32-bit unsigned integers
        (mapcar 'set '(a b c d) h)

        ;; Main MD5 algorithm:
        (setq i 0)
        (repeat 64
            (cond
                (   (< i 16)
                    (setq f (mapcar 'logior (mapcar 'logand b c) (mapcar 'logand (mapcar '(lambda ( a ) (+ 2 (~ a))) b) d))
                          g i
                    )
                )
                (   (< i 32)
                    (setq f (mapcar 'logior (mapcar 'logand d b) (mapcar 'logand (mapcar '(lambda ( a ) (+ 2 (~ a))) d) c))
                          g (rem (1+ (* 5 i)) 16)
                    )
                )
                (   (< i 48)
                    (setq f (mapcar '(lambda ( a b c ) (boole 6 a b c)) b c d)
                          g (rem (+ 5 (* 3 i)) 16)
                    )
                )
                (   (setq f (mapcar '(lambda ( a b ) (boole 6 a b)) c (mapcar 'logior b (mapcar '(lambda ( a ) (+ 2 (~ a))) d)))
                          g (rem (* 7 i) 16)
                    )
                )
            )
            (mapcar 'set '(d c a b i)
                (list c b d
                    (md5:uint32_+ b
                        (md5:leftrotate
                            (md5:uint32_+
                                (md5:uint32_+
                                    (md5:uint32_+ a f)
                                    (nth i k)
                                )
                                (nth g w)
                            )
                            (nth i r)
                        )
                    )
                    (1+ i)
                )
            )
        )

        ;; Update hash values for this chunk
        (setq h (mapcar 'md5:uint32_+ h (list a b c d))
              w nil
        )
    )

    ;; Convert the 4 32-bit integer values to 128-bit hash string of 32 hex digits
    (apply 'strcat
        (mapcar 'md5:byte->hex
            (apply 'append (mapcar 'md5:bits->bytes h))
        )
    )
)
(defun md5:int->bits ( n b / l x )
    (repeat b (setq l (cons 0 l)))
    (foreach x (vl-string->list (rtos n 2 0))
        (setq x (- x 48)
              l (mapcar '(lambda ( a ) (setq a (+ (* a 10) x) x (/ a 2)) (rem a 2)) l)
        )
    )
    (reverse l) ;; output is big-endian
)
(defun md5:bits->int ( l ) ;; input is big-endian
    (   (lambda ( f ) (f (reverse l)))
        (lambda ( l ) (if l (+ (* 2.0 (f (cdr l))) (car l)) 0))
    )
)
(defun md5:bits->bytes ( l / b r ) ;; input is big-endian
    (repeat (/ (length l) 8)
        (repeat 8
            (setq b (cons (car l) b)
                  l (cdr l)
            )
        )
        (setq r (cons (fix (+ 1e-8 (md5:bits->int (reverse b)))) r)
              b nil
        )
    )
    r ;; output is little-endian
)
(defun md5:bytes->bits ( l ) ;; input is little-endian
    (apply 'append (mapcar '(lambda ( b ) (md5:int->bits b 8)) (reverse l))) ;; output is big-endian
)
(defun md5:int->char ( n )
    (chr (+ n (if (< n 10) 48 87)))
)
(defun md5:byte->hex ( x )
    (strcat (md5:int->char (/ x 16)) (md5:int->char (rem x 16)))
)
(defun md5:leftrotate ( l x )
    (repeat x (setq l (append (cdr l) (list (car l)))))
)
(defun md5:uint32_+ ( bl1 bl2 / r ) ;; input is big-endian
    (setq r 0)
    (reverse
        (mapcar
           '(lambda ( a b c / x )
                (setq x (boole 6 (boole 6 a b) r)
                      r (boole 7 (boole 1 a b) (boole 1 a r) (boole 1 b r))
                )
                x
            )
            (append (reverse bl1) (md5:uint32_0))
            (append (reverse bl2) (md5:uint32_0))
            (md5:uint32_0)
        )
    ) ;; output is big-endian
)
(defun md5:uint32_0 ( / l )
    (repeat 32 (setq l (cons 0 l)))
    (eval (list 'defun 'md5:uint32_0 nil (list 'quote l)))
    (md5:uint32_0)
)
(princ)