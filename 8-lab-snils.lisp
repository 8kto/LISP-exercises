; Напишите функцию format+, которая поддерживает все возможности функции format, но вдобавок позволяет обрабатывать форматную директиву ~z, предназначенную для форматирования страхового номера индивидуального лицевого счета (СНИЛС) гражданина РФ.
;
; СНИЛС должен форматироваться следующим образом: ABC-DEF-GHI-CC, где ABC, DEF и GHI – группы из трех цифр, а CC (контрольная сумма) – группа из двух цифр. Контрольная сумма рассчитывается следующим образом:
; 1. Находится сумма произведений цифр номера на номер их позиции, то есть A*9+B*8+C*7+D*6+E*5+F*4+G*3+H*2+I.
; 2. Находится остаток от деления суммы на 101 и из него берутся две младших цифры, они и являются контрольной суммой.
;
; Аргументом директивы ~z может являться либо целое положительное число, либо строка. Если в числе не хватает значащих цифр, то слева оно дополняется нулями до 11 разрядов. Строка может содержать не более 11 десятичных цифр. В случае недопустимых аргументов и других ошибок считать СНИЛС неправильно сформированным и отображать как [bad SNILS].
;
; Примеры форматирования с помощью директивы ~z:
; (format+ t "Vasya's ~a = ~z" "SNILS" "00202203341") ==> Vasya's SNILS = 002-022-033-41
; (format+ nil "~d: ~z, ~d: ~z" 1 120344511 2 "111abc22233344") ==> "1: [bad SNILS], 2: [bad SNILS]"

; Заменить подстроку
(defun str-replace(search replace subject &aux slen newstr)
    (setf slen (length search))
    (loop for i from 0 to (- (length subject) slen) do
        (when (string= search (subseq subject i (+ i slen)))
            (setf newstr (concatenate 'string (subseq subject 0 i) replace (subseq subject (+ i slen))))
            (return-from str-replace newstr))))

; Содержит ли аргумент только цифры
(defun onlydigitsp(num &aux (valid t))
    (when (null num) (return-from onlydigitsp nil))
    (if (numberp num)
        (setf num (write-to-string num)))
    (dotimes (i (length num) valid)
        (when (not (ignore-errors (parse-integer (string (subseq num i (1+ i))))))
            (return-from onlydigitsp nil))))

;; Предикат для проверки валидности SNILS
(defun snilsp(x &aux csum (counter 0) accsum)
    (when (numberp x)
        ; Только положительные числа
        (if (>= 0 x) (return-from snilsp nil))
        (setf x (write-to-string x)))
    (if (string= "0" x) (return-from snilsp nil))

    (setf x (reverse x))

    ; Только цифры
    (unless (onlydigitsp x)
        (return-from snilsp nil))

    ; Последние две цифры - контрольная сумма
    (setf csum (parse-integer (reverse (subseq x 0 2))))

    ; Сложить первые 9 цифр по алгоритму
    (setf accsum (reduce
        #'(lambda (acc el)
            (incf counter)
            (+ acc (* counter (parse-integer (string el)))))
    (subseq x 2) :initial-value 0))

    ; Совпадает ли контрольная сумма
    (= csum (rem accsum 101)))

; Расширенный формат
(defun format+ (destination control-string &rest args)
    (loop while (search "~z" control-string) do
        (let ((val) (_str_) (_anum_) (_res_) (_csum_))
            ; Вырезать строку до вхождения ~z
            (setf _str_ (subseq control-string 0 (search "~z" control-string)))

            ; Количество вхождений ~ в этой строке даёт номер аргумента для ~z
            (setf _anum_ (nth (count (char "~" 0) _str_) args))

            ; WTF section
            (setf val (if (symbolp _anum_)
                (symbol-value _anum_) _anum_))

            (if (numberp val)
                (setf val (write-to-string val)))
            (if (snilsp val)
                (progn
                    (setf _csum_ (parse-integer (subseq val (- (length val) 2))))
                    (setf _res_ (format nil "~d" (parse-integer (subseq val 0 (- (length val) 2)))))

                    ; Для укороченных строк дополнить нулями слева
                    (dotimes (i (- 9 (length _res_)))
                        (setf _res_ (format nil "0~a" _res_)))

                    ; Собрать строку
                    (let ((1st (subseq _res_ 0 3)) (2nd (subseq _res_ 3 6)) (3rd (subseq _res_ 6 9)))
                        (setf _res_ (format nil "~a-~a-~a-~2,'0d" 1st 2nd 3rd _csum_))))

                ; Невалидный номер
                (setf _res_ "[bad SNILS]"))

            ; Заменить все вхождения ~z
            (setf control-string (str-replace "~z" _res_ control-string))
            ; Удалить аргумент из списка
            (setf args (delete _anum_ args :count 1 :start (count (char "~" 0) _str_)))))

        ; Оригинальный формат
        (apply #'format destination control-string args))

;; Tests
(when t
    ;; Сравнить аргументы
    (defun assert-eq(actual expected)
        (when (not (string= actual expected) )
            (format t ">> FAIL:       ~a     IS NOT       ~a~%" actual expected)
            (return-from assert-eq nil))
        ; (format t ">> OK: ~a~%" expected)
        t)

    (if (and
        ; str-replace
        (assert-eq (str-replace "preved" "hello" "test jowwe preved medved preved!") "test jowwe hello medved preved!")
        (assert-eq (str-replace "xx" "" (str-replace "~z" ":x" "xx~z")) ":x")
        (assert-eq (str-replace "~z" ":D" "~z ~a test") ":D ~a test")

        ; onlydigitsp
        (assert-eq (onlydigitsp 300) t)
        (assert-eq(onlydigitsp 0121212300) t)
        (assert-eq(onlydigitsp "300") t)
        (assert-eq(onlydigitsp "0") t)
        (assert-eq(onlydigitsp "0121212300") t)
        (assert-eq(onlydigitsp "300a") nil)
        (assert-eq(onlydigitsp "b300") nil)
        (assert-eq(onlydigitsp "asas") nil)
        (assert-eq(onlydigitsp nil) nil)

        ; format+
        (assert-eq (format+ nil "test ~a ~a ~a ~a" 300 200 500 11) "test 300 200 500 11")
        (assert-eq (format+ nil "~a" 1050.12) "1050.12")
        (assert-eq (format+ nil "~f" 1050.12) "1050.12")
        (assert-eq (format+ nil "~x~:*~x" 300) "12C12C")
        (assert-eq (format+ nil "~,3f" 300.40123) "300.401")
        (assert-eq (format+ nil "Vasya's ~a = ~z" "SNILS" "00202203341") "Vasya's SNILS = 002-022-033-41")
        (assert-eq (format+ nil "~d: ~z, ~d: ~z" 1 120344511 2 "111abx22233344") "1: [bad SNILS], 2: [bad SNILS]")
        (assert-eq (format+ nil "~d: ~z, ~d: ~z" 1 "120344511ab" 2 "00202203341") "1: [bad SNILS], 2: 002-022-033-41")
        (assert-eq (format+ nil "~z" "2d0344511ab") "[bad SNILS]")
        (assert-eq (format+ nil "~a is ~z" "SNILS" "00011122227") "SNILS is 000-111-222-27")
        (assert-eq (format+ nil "~a is ~z" "SNILS" "11122200054") "SNILS is 111-222-000-54")
        (assert-eq (format+ nil "~a is ~z" "SNILS" "00000011106") "SNILS is 000-000-111-06")
        (assert-eq (format+ nil "~a is ~z" "SNILS" "00000000101") "SNILS is 000-000-001-01")
        (assert-eq (format+ nil "~a is ~z" "SNILS" "10000000009") "SNILS is 100-000-000-09")
        (assert-eq (format+ nil "~a is ~z" "SNILS" "101") "SNILS is 000-000-001-01")
        (assert-eq (format+ nil "~z" "10124599719") "101-245-997-19")
        (assert-eq (format+ nil "~z" 10124599719) "101-245-997-19")
        (assert-eq (format+ nil "~d: ~z, ~d: ~z" 1 10124599719 2 "20330566713") "1: 101-245-997-19, 2: 203-305-667-13")
        (assert-eq (format+ nil "~d: ~z, ~d: ~z" 1 10124599719 2 nil) "1: 101-245-997-19, 2: [bad SNILS]")
        (assert-eq (format+ nil "~z" 99999999901) "999-999-999-01")
        (assert-eq (format+ nil "~z" 88888888857) "888-888-888-57")
        (assert-eq (format+ nil "~z ~z" 99999999901 "00000000101") "999-999-999-01 000-000-001-01")
        (assert-eq (format+ nil "~d: ~z ~d: ~z" 1 99999999901 2 "00000000101") "1: 999-999-999-01 2: 000-000-001-01")
        (assert-eq (format+ nil "~z" -88888888857) "[bad SNILS]")
        (assert-eq (format+ nil "~z" 88888888857.82) "[bad SNILS]")
        (assert-eq (format+ nil "~z" 00000000101.00) "[bad SNILS]")
        (assert-eq (format+ nil "~z" 00000000101.100) "[bad SNILS]")
        (assert-eq (format+ nil "~z ~z" nil nil) "[bad SNILS] [bad SNILS]")
        (assert-eq (format+ nil "~z ~z ~z" nil nil 99999999901) "[bad SNILS] [bad SNILS] 999-999-999-01")
        (assert-eq (format+ nil "~z" 0 ) "[bad SNILS]")
        (assert-eq (format+ nil "~z" "~z" ) "[bad SNILS]")
        (assert-eq (format+ nil "~z ~z ~z ~z" 0 -1 300 1/2) "[bad SNILS] [bad SNILS] [bad SNILS] [bad SNILS]")
        (assert-eq (format+ nil "~z ~z ~z ~z" "0" "-1" "300" "1/2") "[bad SNILS] [bad SNILS] [bad SNILS] [bad SNILS]")
        (assert-eq (format+ nil "~z ~z ~z ~z ~z" "0" "-1" "99999999901" "300" "1/2") "[bad SNILS] [bad SNILS] 999-999-999-01 [bad SNILS] [bad SNILS]")
    )
    (format t "Tests OK~%")
    (format t "Tests FAILED~%")))
