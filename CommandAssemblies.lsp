;本程序旨在加载LeeMac程序DTcurve
;以下两种方法均可行



(defun C:wr()
	(if (not (type C:DTcurve))
		(Load "C:/Users/Administrator/Documents/GitHub/LeeMac-Program-Instruction/LM Programs/DTCurveV2-9.lsp")
	)
	(c:DTcurve)	
)


;(defun C:wr()
;	(if (null C:DTcurve)
;		(Load "C:/Users/Administrator/Documents/GitHub/LeeMac-Program-Instruction/LM Programs/DTCurveV2-9.lsp")
;	)
;	(c:DTcurve)	
;)
;