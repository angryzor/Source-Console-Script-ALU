#!r6rs

(import (rnrs base (6))
        (rnrs io simple (6))
        (tf2scripts gates))

(define (build-conn num pre post input)
  (define (rec n)
    (if (>= n num)
        '()
        (cons (conn (string-append pre (number->string n) post) input) (rec (+ n 1)))))
  (rec 0))

; 4-bit FULL ADDER
;=========================================================

(define (make-alu bits)
  (let ((alu "alu")
        (fa "_fa"))
    (define (make-decoder)
      (let ((not0 "alu_dec_not0")
            (not1 "alu_dec_not1")
            (and0 "alu_dec_and0")
            (and1 "alu_dec_and1")
            (and2 "alu_dec_and2")
            (and3 "alu_dec_and3"))
        (display "//*************************************************************************************")(newline)
        (display "// Operation decoder")(newline)
        (display "//*************************************************************************************")(newline)(newline)(newline)
        
        (alias "alu_OP0__0" (conc (putbit and1 0 0) (putbit and3 0 0) (putbit not0 0 0)))
        (alias "alu_OP0__1" (conc (putbit and1 0 1) (putbit and3 0 1) (putbit not0 0 1)))
        
        (alias "alu_OP1__0" (conc (putbit and2 1 0) (putbit and3 1 0) (putbit not1 0 0)))
        (alias "alu_OP1__1" (conc (putbit and2 1 1) (putbit and3 1 1) (putbit not1 0 1)))
        
        (gnot not0 '() `(,(conn and0 0) ,(conn and2 0)))
        (gnot not1 '() `(,(conn and0 1) ,(conn and1 1)))
        
        (gand and0 `(,not0 ,not1) (build-conn bits alu "_op0" 0))
        (gand and1 `(,not1) (build-conn bits alu "_op1" 0))
        (gand and2 `(,not0) (build-conn bits alu "_op2" 0))
        (gand and3 `() (build-conn bits alu "_op3" 0))))
    
    
    
    
    
    (define (make-bit-alu bit)
      (let* ((bs (number->string bit))
             ; full adder
             (xor0 (string-append alu bs fa "_xor0"))
             (xor1 (string-append alu bs fa "_xor1"))
             (and0 (string-append alu bs fa "_and0"))
             (and1 (string-append alu bs fa "_and1"))
             (orn (string-append alu bs fa "_or"))
             ; Logic unit
             (land (string-append alu bs "_land"))
             (lor (string-append alu bs "_lor"))
             (lxor (string-append alu bs "_lxor"))
             ; op selector
             (op0 (string-append alu bs "_op0"))
             (op1 (string-append alu bs "_op1"))
             (op2 (string-append alu bs "_op2"))
             (op3 (string-append alu bs "_op3"))
             (opj (string-append alu bs "_opj")))
        
        (define (make-inputs)
          (alias (string-append alu bs "_A__0") (conc (putbit xor0 0 0) (putbit and1 0 0) (putbit land 0 0) (putbit lor 0 0) (putbit lxor 0 0)))
          (alias (string-append alu bs "_A__1") (conc (putbit xor0 0 1) (putbit and1 0 1) (putbit land 0 1) (putbit lor 0 1) (putbit lxor 0 1)))
          
          (alias (string-append alu bs "_B__0") (conc (putbit xor0 1 0) (putbit and1 1 0) (putbit land 1 0) (putbit lor 1 0) (putbit lxor 1 0)))
          (alias (string-append alu bs "_B__1") (conc (putbit xor0 1 1) (putbit and1 1 1) (putbit land 1 1) (putbit lor 1 1) (putbit lxor 1 1))))
          
        
        ; Full adder
        (define (make-full-adder)
          (display "//*************************************************************************************")(newline)
          (display "// Full Adder for bit ")(display bit)(display " (")(display bits)(display " total)")(newline)
          (display "//*************************************************************************************")(newline)(newline)(newline)
          
          (if (= bit 0)
              (begin
                (alias (string-append alu bs "_Cin__0") (conc (putbit xor1 1 0) (putbit and0 1 0)))
                (alias (string-append alu bs "_Cin__1") (conc (putbit xor1 1 1) (putbit and0 1 1)))))
          
          (gxor xor0 '() `(,(conn xor1 0) ,(conn and0 0)))
          (gxor xor1 
                (if (= bit 0)
                    `(,xor0)
                    `(,xor0 ,(string-append alu (number->string (- bit 1)) fa "_or")))
                `(,(conn op0 1)))
          (gand and0  
                (if (= bit 0)
                    `(,xor0)
                    `(,xor0 ,(string-append alu (number->string (- bit 1)) fa "_or")))
                `(,(conn orn 0)))
          (gand and1 '() `(,(conn orn 1)))
          (if (= bit (- bits 1))
              (gorm orn `(,and0 ,and1) "echo Carry = 0" "echo Carry = 1")
              (gor orn `(,and0 ,and1) `(,(conn (string-append alu (number->string (+ bit 1)) fa "_xor1") 1) ,(conn (string-append alu (number->string (+ bit 1)) fa "_and0") 1)))))
        
        ; Logic unit
        (define (make-logic-unit)
          (display "//*************************************************************************************")(newline)
          (display "// Logic unit for bit ")(display bit)(display " (")(display bits)(display " total)")(newline)
          (display "//*************************************************************************************")(newline)(newline)(newline)
          
          (gand land '() `(,(conn op1 1)))
          (gor lor '() `(,(conn op2 1)))
          (gxor lxor '() `(,(conn op3 1))))
          
        
        ; Op selector
        (define (make-op-selector)
          (display "//*************************************************************************************")(newline)
          (display "// Op selector for bit ")(display bit)(display " (")(display bits)(display " total)")(newline)
          (display "//*************************************************************************************")(newline)(newline)(newline)
          
          (gand op0 `("alu_dec_and0" ,xor1) `(,(conn opj 0)))
          (gand op1 `("alu_dec_and1" ,land) `(,(conn opj 1)))
          (gand op2 `("alu_dec_and2" ,lor) `(,(conn opj 2)))
          (gand op3 `("alu_dec_and3" ,lxor) `(,(conn opj 3)))
          (gor4m opj `(,op0 ,op1 ,op2 ,op3) (string-append "echo S[" bs "] = 0") (string-append "echo S[" bs "] = 1")))
        
        (make-inputs)
        (make-full-adder)
        (make-logic-unit)
        (make-op-selector)))
        
    (define (iter bit)
      (if (< bit bits)
          (begin
            (make-bit-alu bit)
            (iter (+ bit 1)))))
    
    (make-decoder)
    (iter 0)))

(make-alu 8)
