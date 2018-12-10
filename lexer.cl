(setq a (make-array 1000 :fill-pointer 0))


;->Matematiksel operatörler (+,-,/,*,**) sonrasında en az bir tane space karakteri 
;olmak zorunda.


;* Hatalı koşullar sağlandığında işlem hata verilerek sonlandırılıyor. 

(defvar ResultList '())	; Butun islemin sonucu en son bu liste oluyor.
(defvar TempList '())	; \
(defvar countter )		;  \
(defvar countter2 )		;  	>> Bunlar hesaplamaları yaparken kullandigim degiskenler.
(defvar illegalCounter );  /
(defvar tempRes '())	; /

;Operatorleri ve keywordleri tespit etmek icin kullanilan degiskenler.
(defvar Key_deffun '(#\d #\e #\f #\f #\u #\n)) 
(defvar Key_and '(#\a #\n #\d))
(defvar Key_or '(#\o #\r))
(defvar Key_not '(#\n #\o #\t)) 
(defvar Key_equal '(#\e #\q #\u #\a #\l))
(defvar Key_append '(#\a #\p #\p #\e #\n #\d))
(defvar Key_concat '(#\c #\o #\n #\c #\a #\t))
(defvar Key_set '(#\s #\e #\t))
(defvar Key_while '(#\w #\h #\i #\l #\e))
(defvar Key_for '(#\f #\o #\r))
(defvar Key_if '(#\i #\f))
(defvar Key_exit '(#\e #\x #\i #\t))

(defvar Key_plus '(#\+))
(defvar Key_minus '(#\-))
(defvar Key_divide '(#\/))
(defvar Key_mult '(#\*))
(defvar Key_multunary '(#\* #\*))

(defvar Key_True '(#\t #\r #\u #\e))
(defvar Key_false '(#\f #\a #\l #\s #\e))



(defun lexer (filename) ; dişarıdan cagrilan method sadece ilklendirmeleri yapipi asil islemlerin yapıldıgı
						; foo methodunu cagiriyor.
	(setq countter 0)
	(setq countter2 0)
	(setq illegalCounter 0)
	(setq TempList nil)
	(setq ResultList nil)
	(foo filename)
	(write ResultList)
	(format t "~%işlem sonu.")
)

(defun foo (filename) ; Dosyanın acilip icersindeki verinin karakter karakter okunması saglayip gerekli analizleri yapan method.

	(with-open-file (stream filename)
	    (do ((char (read-char stream nil)
            (read-char stream nil)))
        	((null char))
        	
		    	(if (or (char= char #\SPACE) (char= char #\newline ))	; operator ve keywordlerden sonra bosluk olmasi gerekiyor
		    	
		    		(if (char/= char #\TAB)		    				
		    			(checktype)		    			     	
		    		)
		    		(if (char/= char #\TAB)
		    			(setq TempList (append TempList (list char)))   
		    		)
		    	)		    		
            
			(if (and (char/= char #\NEWLINE) (char/= char #\TAB))
            	(checkOperator char)		 
			)
		)
	)
)


(defun checkOperator (char) ; bir elemanın tanımlı operatörlerden biri olup olmadığını kontrol ediyor, 	
							; eğer operatörse bu operatörü result listesine ekliyor.
	(setq tempRes nil)
	(setq countter2 0) ; herhangi bir operator tespiti yapilip yapilmadigini kontrol etmek icin kullanıyorum.

	(if (char= char #\( )
		(setq tempRes(append tempRes (list " [ OPERATOR : (  ] ")))
		(setq countter2 (+ countter2 1))     		
	)
	(if (char= char #\) )
		(setq tempRes(append tempRes (list " [ OPERATOR : )  ] ")))      		
		(setq countter2 (+ countter2 1))
	)	

	(if(/= countter2 2);  
		(if (> (length TempList ) 1) 
			(checkfunc TempList) 
		)
	)	
	(if(/= countter2 2) ;
		(setq ResultList(append ResultList tempRes))
	)
	(if(/= countter2 2) ;
		(setq TempList nil)     
	)
)

(defun checktype () ; keyword ve arkasindan bosluk gelmesi gereken degerleri yakalamaya calisir.
	(setq countter 0)
	;keyword kontrolleri :
	(if(checkEquality TempList Key_deffun)
		(setq ResultList(append ResultList (list " [ KEYWORD : DEFFUN ] ")))
		(setq countter (+ countter 1))
	)
	(if(checkEquality TempList Key_and)
		(setq ResultList(append ResultList (list " [ KEYWORD : AND ] ")))
		(setq countter (+ countter 1))
	)
	(if(checkEquality TempList Key_or)
		(setq ResultList(append ResultList (list " [ KEYWORD : OR ] ")))
		(setq countter (+ countter 1))
	)
	(if(checkEquality TempList Key_not)
		(setq ResultList(append ResultList (list " [ KEYWORD : NOT ] ")))
	    (setq countter (+ countter 1))
	)
	(if(checkEquality TempList Key_equal)
		(setq ResultList(append ResultList (list " [ KEYWORD : EQUAL ] ")))
		(setq countter (+ countter 1))
	)
	(if(checkEquality TempList Key_append)
		(setq ResultList(append ResultList (list " [ KEYWORD : APPEND ] ")))
		(setq countter (+ countter 1))
	)
	(if(checkEquality TempList Key_concat)
		(setq ResultList(append ResultList (list " [ KEYWORD : CONCAT ] ")))
		(setq countter (+ countter 1))
	)
	(if(checkEquality TempList Key_while)
		(setq ResultList(append ResultList (list " [ KEYWORD : WHİLE ] ")))
		(setq countter (+ countter 1))
	)
	(if(checkEquality TempList Key_for)
		(setq ResultList(append ResultList (list " [ KEYWORD : FOR ] ")))
		(setq countter (+ countter 1))
	)
	(if(checkEquality TempList Key_if)
		(setq ResultList(append ResultList (list " [ KEYWORD : İF ] ")))
		(setq countter (+ countter 1))
	)
	(if(checkEquality TempList Key_exit)
		(setq ResultList(append ResultList (list " [ KEYWORD : EXİT ] ")))
		(setq countter (+ countter 1))
	)
	(if(checkEquality TempList Key_True)
		(setq ResultList(append ResultList (list " [ BinaryValue : true ] ")))
		(setq countter (+ countter 1))
	)
	(if(checkEquality TempList Key_false)
		(setq ResultList(append ResultList (list " [ BinaryValue : false ] ")))
		(setq countter (+ countter 1))
	)
	;-------------------------------------------------------------------------
	;+ ,-,/,* ve ** operatorleri kontrollleri
	(if (= (length TempList) 1) 
		(if(char= (car TempList) #\+) 
			(setq ResultList(append ResultList (list " [ OPERATOR : +  ] ")))      		       		
			(setq countter (+ countter 1))
		)
		(setq countter (+ countter 1))
	)
	(if (= (length TempList) 1) 
		(if(char= (car TempList) #\-) 
			(setq ResultList(append ResultList (list " [ OPERATOR : -  ] ")))      		       		
			(setq countter (+ countter 1))
		)
		(setq countter (+ countter 1))
	)
	(if (= (length TempList) 1) 
		(if(char= (car TempList) #\/) 
			(setq ResultList(append ResultList (list " [ OPERATOR : /  ] ")))      		       		
			(setq countter (+ countter 1))
		)
		(setq countter (+ countter 1))
	)
	(if (= (length TempList) 1) 
		(if(char= (car TempList) #\*) 
			(setq ResultList(append ResultList (list " [ OPERATOR : *  ] ")))      		       		
			(setq countter (+ countter 1))
		)
		(setq countter (+ countter 1))
	)
	(if (= (length TempList) 2) 
		(if(char= (car TempList) #\*) 
			(if(char= (car(cdr TempList)) #\*)
				(setq ResultList(append ResultList (list " [ OPERATOR : ** ] ")))
				(setq countter (+ countter 1))
			)	      		       		
			(setq countter (+ countter 1))
		)
		(setq countter (+ countter 1))
	)		
	
	;----------------------------------------------------------
	; herhangi bir keyword ve operator degilse identifier mı ınteger mi yoksa hatalı bir deger olup olmadigin kontrolu.
	(setq illegalCounter 0)
	(if(= countter 18)
		(if (/= (length TempList) 0)
			(if(> (length TempList) 1)
				(if (checkInteger TempList)
					; bir rakamlar dizisi olma durumu 
					(setq ResultList(append ResultList (list (concatenate 'string " [ Integer : "  (format nil "~{~A~}" TempList) " ] " ))))
					; ya abc yani variable name olabilir   yada 123abc  yani geçersiz karakter dizisi olabilir. 
					(if (checkIllegaltyRecursive TempList)
						(if(and (char= #\- (car TempList)) (checkInteger (cdr TempList)))
							(setq ResultList(append ResultList (list (concatenate 'string " [ Integer : "  (format nil "~{~A~}" TempList) " ] " ))))
							(error "~S is a unvalid variable name" TempList) 
						)
						 ; geçersiz bir variable name girilme durumu
						(setq ResultList(append ResultList (list (concatenate 'string " [ identifier : "  (format nil "~{~A~}" TempList) " ] " ))))
						; geçerli variable name durumu 
					)
				)
				(if (checknumber (car TempList))   ; 
					(setq ResultList(append ResultList (list (concatenate 'string " [ Integer : "  (format nil "~{~A~}" TempList) " ] " ))))
					(setq ResultList(append ResultList (list (concatenate 'string " [ identifier : "  (format nil "~{~A~}" TempList) " ] " ))))						
				)				
			)			
		)
	)
	(setq TempList nil) ; temp list in içeriği işlendiği için sıfırlanıyor.
)

(defun checkEquality (list1 list2 ); templist içindeki verinin herhangi bir keyword olup olmadığını kontrol etmek için
								   ; iki listenin eşitliğini kontrol eden bir fonksiyon yazdım.
	(if (= (length list1) (length list2))
		(if(/= (length list1) 0)
			(if(char= (car list1) (car list2))
				(checkEquality (cdr list1) (cdr list2))
			    nil
	 	    )
		   	T
	    )
	    nil
	)
) 
(defun checkInteger (Temp) ; bir listenin rakamlar dizisi olup olmadığını kontrol ediyor.

	(if(>= (length Temp) 1)
		(if(checknumber (car Temp))
			(checkInteger (cdr Temp))
			nil
		)
		T
	)	
)

(defun checkIllegaltyRecursive (liste ) ; parametre olarak gelen listenin gecersiz bir değişken adı olup olmadığını kontrol ediyor.
	(if (> (length liste ) 0 )
		(if (or (checknumber (car liste )) (checkSymbol (car liste )))
			(setq illegalCounter (+ illegalCounter 1))
			(checkIllegaltyRecursive (cdr liste))
		)
		(if(> illegalCounter 0)
			T
			nil
		)
	)
)
(defun checkSymbol (var) ; bu method degisken isimlerinde bulunması kurallarda belirttigim gibi yasak olan karakterlerin
						 ; olup olmadigini inceliyor. Bu listeye farklı sembollerde eklenebilir , ornek amacli bu kadarını
						 ; kontrol ediyorum.
	(if(char= var #\+ )
   	 T
	 (if(char= var #\- )
      T
	  (if(char= var #\/ )
	   T
	   (if(char= var #\* )
		T
		(if(char= var #\. )
		 T
	     (if(char= var #\, )
		  T
	      (if(char= var #\; )
		   T 
	       (if(char= var #\! )
		    T
		    (if(char= var #\? )
		    T
		     (if(char= var #\^ )
		     T	
		    nil
			))))))))))
)
(defun checknumber (var) ; var parametresi olarak bir elemanın rakam olup olmadığını kontrol ederek boolean değer olarak bildiriyor. 
	(if(char= var #\0 )
   	 T
	 (if(char= var #\1 )
      T
	  (if(char= var #\2 )
	   T
	   (if(char= var #\3 )
		T
		(if(char= var #\4 )
		 T
	     (if(char= var #\5 )
		  T
	      (if(char= var #\6 )
		   T 
	       (if(char= var #\7 )
		    T
	        (if(char= var #\8 )
	     	 T
             (if(char= var #\9 )
		      T
		      nil
			 ))))))))))
)

(defun writeList (liste) ; verilen bir listenin elemanları satir satir ekrana basiyor, işlem sonrasi sonuc listesini 
						 ; gormek icin kullanilabilir.
	(terpri)
	(write (car liste))
	(if (> (length liste ) 1)
		(writeList (cdr liste))
	)
	
)
(defun checkfunc (liste) ; sonunda bosluk bırakılmadan yazilan ifadeleri yakalamak icin kullanılan method.
	(setq TempList (reverse (cdr (reverse TempList ))))
	(checktype)
)
