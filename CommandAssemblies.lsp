;������ּ�ڼ���LeeMac����DTcurve
;�������ַ���������



(defun C:wr()
	(if (not (type C:DTcurve))
		(Load "D:/GitHub/LeeMac-Program-Instruction/LM Programs/DTCurveV2-9.lsp")
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