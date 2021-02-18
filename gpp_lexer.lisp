(defun gppinterpreter(filename)
	(with-open-file (stream filename)
	(setf previous_char 3)	;this is used for the "*" and "" case check
	(setf number 0)		;for number control
	(setf letter 0)		;for letter control
	(setf minusControl 0) ;for checking is minus for negatif integer or is an operator
	(setf myList '())	;it is from file list
	(setf real_list '())	;it is my last list
	(setf temp '())	;for comparing identifier or key
	(setf temp_result '())	;it is for my temproary lists
		(do ((char (read-char stream nil)	;main loop for read file get char
			(read-char stream nil)))
			((null char))
			(push char myList)	;i get the char and push to list
			(if (is_it_letter(car myList))	;if it is a letter set letter 1
				(setf letter 1)
			)
				  ;if it start a num it is not gonna be identifier nor key
			(cond ;push all the letters and numbers until i get a diffrent character
				((and (= letter 1) (is_it_letter(car myList))) (push (string (car myList)) temp))	
				((and (= letter 1) (not (is_it_letter(car myList))))
					(setf letter 0)	;this means word finish
					(push (concatenating (reversing temp)) temp_result)	;i reverse list and concatenate the strings then i push word to temp_result
					(if (is_it_keyword (concatenating (reversing temp)));to check if the word is keyword or identifier
						(progn
							(cond 
								((equal (concatenating (reversing temp)) "and")		(push "KW_AND" real_list))
								((equal (concatenating (reversing temp)) "or")		(push "KW_OR" real_list))
								((equal (concatenating (reversing temp)) "not")		(push "KW_ NOT" real_list))
								((equal (concatenating (reversing temp)) "equal")	(push "KW_EQUAL" real_list))
								((equal (concatenating (reversing temp)) "less")	(push "KW_LESS" real_list))
								((equal (concatenating (reversing temp)) "nil")		(push "KW_NIL" real_list))
								((equal (concatenating (reversing temp)) "list") 	(push " KW_LIST" real_list))
								((equal (concatenating (reversing temp)) "append") 	(push "KW_APPEND" real_list))
								((equal (concatenating (reversing temp)) "concat") 	(push "KW_CONCAT" real_list))
								((equal (concatenating (reversing temp)) "set")		(push "KW_SET" real_list))
								((equal (concatenating (reversing temp)) "deffun") 	(push "KW_DEFFUN" real_list))
								((equal (concatenating (reversing temp)) "for")		(push "KW_FOR" real_list))
								((equal (concatenating (reversing temp)) "if")		(push "KW_IF" real_list))
								((equal (concatenating (reversing temp)) "exit") 	(push "KW_EXIT" real_list))
								((equal (concatenating (reversing temp)) "load") 	(push "KW_LOAD" real_list))
								((equal (concatenating (reversing temp)) "disp") 	(push "KW_DISP" real_list))
								((equal (concatenating (reversing temp)) "true") 	(push "KW_TRUE" real_list))
								((equal (concatenating (reversing temp)) "false") 	(push "KW_FALSE" real_list))
							)
						)
						(push "IDENTIFIER" real_list)	;push "identifier" to my real_list
					)
					(setf temp_result '())	;after using the temp_result i set it empty again to use it next time
					(setf temp '())	;i also set temp empty to use it again
				)
			)
			
			(if (and (is_it_num(car myList)) (= letter 0))	;it is number and not any letter from before
				(setf number 1)
			)

			(if (and (= minusControl 1) (not (is_it_num (car myList))))	;there is not any integer after "-" so i set it "2" to write as operator
				(setf minusControl 2)
			)
			(cond ;push all the numbers to diffrent character from number
				((and (eq temp nil) (= number 1) (equal (string (car myList)) "0"))
					(setf number 0)	;number finnish
					(push "VALUE" real_list)	;push VALUE my real list
					(setf temp '())
				)
				((and (= number 1) (is_it_num (car myList))) 
					(cond 
						((and (eq temp nil) (= minusControl 1)) 
							(push "VALUE" temp)
							(setf minusControl 0)
						)
						(t
							(push (string (car myList)) temp)	;if there is not a "-" then just push integer
						)
					)
				)
				((and (= number 1) (not (is_it_num (car myList)))) 
					(setf number 0)	
					(push (concatenating (reversing temp)) temp_result)
					(push "VALUE" real_list) 
					(setf temp_result '())	
					(setf temp '())	
				)
			)

			(if (= (char-code (car myList)) 45)
				(setf minusControl 1)
			)

			(when (= previous_char 2)	;checking is is one or double star(*)
				(if (is_it_star(car myList))	
					(push "OP_DBLMULT" real_list)	
					(push "OP_MULT" real_list)
				)
				(setf temp_result '())	;after using the temp_result i set it empty again to use it next time
			)
	
			(if (and (is_it_star(car myList)) (not (= previous_char 2)))	;if char is "*" and previous char not star then set previos_char 1 for next loop
					(setf previous_char 1)
			)
			(cond
				((and (is_it_op(car myList)) (= minusControl 0))
					(cond
						((= (char-code (car myList)) 40) (push "OP_OP" real_list))	;( op
						((= (char-code (car myList)) 41) (push "OP_CP" real_list))	;) op
						((= (char-code (car myList)) 43) (push "OP_PLUS" real_list))	;+ op
						((= (char-code (car myList)) 47) (push "OP_DIV" real_list))	;/ op
					)
					(setf temp_result '())	;after using the temp_result i set it empty again to use it next time
				)
				((= minusControl 2)	;this means previous character was "-" but there is not any integer after it so it is an operator
					(push "OP_MINUS" real_list)	;push "operator" to my real list
					(setf temp_result '())	;after using the temp_result i set it empty again to use it next time
					(setf minusControl 0)	;set minusControl 0 again
				)
			)
			(setf previous_char (+ previous_char 1))	;this is used for "" and "*" case
		)	
	)
	(reverse real_list)	;i push all the small lists to my main list so i reverse it for the true form
)

(defun is_it_keyword(isKey)	;checkin is it keyword
	(cond 
		((equal isKey "and") t)
		((equal isKey "or") t)
		((equal isKey "not") t)
		((equal isKey "equal") t)
		((equal isKey "less") t)
		((equal isKey "nil") t)
		((equal isKey "list") t)
		((equal isKey "append") t)
		((equal isKey "concat") t)
		((equal isKey "set") t)
		((equal isKey "deffun") t)
		((equal isKey "for") t)
		((equal isKey "if")	t)
		((equal isKey "exit") t)
		((equal isKey "load") t)
		((equal isKey "disp") t)
		((equal isKey "true") t)
		((equal isKey "false") t)
		(nil)
	)
)


(defun reversing (reverese_it)	;reverse
	(cond
		((null reverese_it) '())
			(T (append (reversing (cdr reverese_it)) (list (car reverese_it))))
	)
)

(defun concatenating (concat_list)		;it takes a list and concatenate all the strings in that list
	(let ((result ""))
		(dolist (element concat_list)
			(if (stringp element)
				(setf result (concatenate 'string result element))
			)
		)
		result
	)
)

(defun is_it_star(op)	;checking is it one * op according to ascii table
	(if (= (char-code op) 42)	;* op
		t
		nil
	)
)

(defun is_it_op(op)	;checkin is it an operator according to ascii table
	(cond
		((= (char-code op) 43) 	t)	;+ op
		((= (char-code op) 45) 	t)	;- op
		((= (char-code op) 47) 	t)	;/ op
		((= (char-code op) 40) 	t)	;( op
		((= (char-code op) 41) 	t)	;) op
		(nil))
)

(defun is_it_letter(ch)	;checking is it a letter according to ascii table
	(if (or
			(and (<= (char-code ch) 122) (>= (char-code ch) 97));a-z
			(and (<= (char-code ch) 90) (>= (char-code ch) 65))	;A-Z
		)
	t
	nil
	)	
)

(defun is_it_num(num)	;checkink is it a number according to ascii table
	(if (and
			(>= (char-code num) 48)	
			(<= (char-code num) 57)	
		)
		t
		nil
	)
)