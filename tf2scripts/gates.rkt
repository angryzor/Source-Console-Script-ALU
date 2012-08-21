#!r6rs
(library (tf2scripts gates)
 (export gnot gand gor gxor gnand gnor gor4
         gnotm gandm gorm gxorm gnandm gnorm gor4m
         conc conn putbit alias)
 (import (rnrs base (6))
         (rnrs io simple (6)))
 
 (define (conc . acts)
   (cond ((null? acts) "")
         ((null? (cdr acts)) (car acts))
         (else (string-append (car acts) "; " (apply conc (cdr acts))))))
 
 (define (conn nametgt input)
   (string-append nametgt "_in_" (number->string input)))
 
 (define (putbit nametgt input bit)
   (string-append nametgt "_in_" (number->string input) "__" (number->string bit)))
 
 (define (alias name cmd)
   (display (string-append "alias \"" name "\" \"" cmd "\""))(newline))
 
 
 (define (outs name out-action0 out-action1)
   (display (string-append "alias \"" name "_out__0\" \"" out-action0 "\""))(newline)
   (display (string-append "alias \"" name "_out__1\" \"" out-action1 "\""))(newline))
 
 (define (ticks name depends)
   (display (string-append "alias \"" name "_tick\" \"" name "_output\""))(newline)
   (display (string-append "alias \"" name "_tickrecurse\" \"" (apply conc (map (lambda (d)
                                                                                  (string-append d "_execute")) depends)) "\""))(newline)
   (display (string-append "alias \"" name "_rcl\" \"alias " name "_execute ; " name "_tickrecurse; " name "_tick; alias " name "_execute " name "_rcl\""))(newline)
   (display (string-append "alias \"" name "_execute\" \"" name "_rcl\""))(newline))
 
 (define (pre type name)
   (display "//============================================================")(newline)
   (display "// ")(display type)(display " gate \"")(display name)(display "\"")(newline)
   (display "//============================================================")(newline)(newline))

 (define (gnotm name depends out-action0 out-action1)
   (pre "NOT" name)
   (display (string-append "alias \"" name "_in_0__0\" \"alias " name "_output " name "_out__1\""))(newline)
   (display (string-append "alias \"" name "_in_0__1\" \"alias " name "_output " name "_out__0\""))(newline)(newline)
   
   (display (string-append "alias \"" name "_output\" \"\""))(newline)(newline)
   
   (outs name out-action0 out-action1)(newline)
   
   (ticks name depends)(newline)(newline))
 
 
 (define (gandm name depends out-action0 out-action1)
   (pre "AND" name)
   (display (string-append "alias \"" name "_in_0__0\" \"alias " name "_output " name "_out__0\""))(newline)
   (display (string-append "alias \"" name "_in_0__1\" \"alias " name "_output " name "_stage1\""))(newline)(newline)
   
   (display (string-append "alias \"" name "_stage1\" \"" name "_out__0\""))(newline)(newline)
   
   (display (string-append "alias \"" name "_in_1__0\" \"alias " name "_stage1 " name "_out__0\""))(newline)
   (display (string-append "alias \"" name "_in_1__1\" \"alias " name "_stage1 " name "_out__1\""))(newline)(newline)
   
   (display (string-append "alias \"" name "_output\" \"\""))(newline)(newline)
   
   (outs name out-action0 out-action1)(newline)
   
   (ticks name depends)(newline)(newline))
 
 
 (define (gorm name depends out-action0 out-action1)
   (pre "OR" name)
   (display (string-append "alias \"" name "_in_0__0\" \"alias " name "_output " name "_stage1\""))(newline)
   (display (string-append "alias \"" name "_in_0__1\" \"alias " name "_output " name "_out__1\""))(newline)(newline)
   
   (display (string-append "alias \"" name "_stage1\" \"" name "_out__0\""))(newline)(newline)
   
   (display (string-append "alias \"" name "_in_1__0\" \"alias " name "_stage1 " name "_out__0\""))(newline)
   (display (string-append "alias \"" name "_in_1__1\" \"alias " name "_stage1 " name "_out__1\""))(newline)(newline)
   
   (display (string-append "alias \"" name "_output\" \"\""))(newline)(newline)
   
   (outs name out-action0 out-action1)(newline)
   
   (ticks name depends)(newline)(newline))
 
 
 (define (gor4m name depends out-action0 out-action1)
   (pre "OR4" name)
   (display (string-append "alias \"" name "_in_0__0\" \"alias " name "_output " name "_stage1\""))(newline)
   (display (string-append "alias \"" name "_in_0__1\" \"alias " name "_output " name "_out__1\""))(newline)(newline)
   
   (display (string-append "alias \"" name "_stage1\" \"" name "_out__0\""))(newline)(newline)
   
   (display (string-append "alias \"" name "_in_1__0\" \"alias " name "_stage1 " name "_stage2\""))(newline)
   (display (string-append "alias \"" name "_in_1__1\" \"alias " name "_stage1 " name "_out__1\""))(newline)(newline)
   
   (display (string-append "alias \"" name "_stage2\" \"" name "_out__0\""))(newline)(newline)
   
   (display (string-append "alias \"" name "_in_2__0\" \"alias " name "_stage2 " name "_stage3\""))(newline)
   (display (string-append "alias \"" name "_in_2__1\" \"alias " name "_stage2 " name "_out__1\""))(newline)(newline)
   
   (display (string-append "alias \"" name "_stage3\" \"" name "_out__0\""))(newline)(newline)
   
   (display (string-append "alias \"" name "_in_3__0\" \"alias " name "_stage3 " name "_out__0\""))(newline)
   (display (string-append "alias \"" name "_in_3__1\" \"alias " name "_stage3 " name "_out__1\""))(newline)(newline)
   
   (display (string-append "alias \"" name "_output\" \"\""))(newline)(newline)
   
   (outs name out-action0 out-action1)(newline)
   
   (ticks name depends)(newline)(newline))
 
 
 (define (gxorm name depends out-action0 out-action1)
   (pre "XOR" name)
   (display (string-append "alias \"" name "_in_0__0\" \"alias " name "_output " name "_stage1\""))(newline)
   (display (string-append "alias \"" name "_in_0__1\" \"alias " name "_output " name "_stage2\""))(newline)(newline)
   
   (display (string-append "alias \"" name "_stage1\" \"" name "_out__0\""))(newline)(newline)
   (display (string-append "alias \"" name "_stage2\" \"" name "_out__1\""))(newline)(newline)
   
   (display (string-append "alias \"" name "_in_1__0\" \"alias " name "_stage1 " name "_out__0; alias " name "_stage2 " name "_out__1\""))(newline)
   (display (string-append "alias \"" name "_in_1__1\" \"alias " name "_stage1 " name "_out__1; alias " name "_stage2 " name "_out__0\""))(newline)(newline)
   
   (display (string-append "alias \"" name "_output\" \"\""))(newline)(newline)
   
   (outs name out-action0 out-action1)(newline)
   
   (ticks name depends)(newline)(newline))
 
 
 (define (gnandm name depends out-action0 out-action1)
   (let ((andname (string-append name "_auto_and"))
         (notname  (string-append name "_auto_not")))
     (gand andname depends (conn notname 0))
     (gnotm notname `(,andname) out-action0 out-action1)))
 
 (define (gnorm name depends out-action0 out-action1)
   (let ((orname (string-append name "_auto_or"))
         (notname  (string-append name "_auto_not")))
     (gor orname depends (conn notname 0))
     (gnotm notname `(,orname) out-action0 out-action1)))
 
 (define (conn-specific gate)
   (lambda (name depends connections)
     (gate name 
           depends 
           (apply conc (map (lambda (connection)
                              (string-append connection "__0")) connections))
           (apply conc (map (lambda (connection)
                              (string-append connection "__1")) connections)))))
 
 
 (define gand (conn-specific gandm))
 (define gnot (conn-specific gnotm))
 (define gor (conn-specific gorm))
 (define gxor (conn-specific gxorm))
 (define gnand (conn-specific gnandm))
 (define gnor (conn-specific gnorm))
 (define gor4 (conn-specific gor4m))
 )