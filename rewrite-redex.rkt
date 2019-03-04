#lang racket
(require redex)

; andrew blinn
; adapted from: https://docs.racket-lang.org/redex/tutorial.html


(define-language L
  (e (e e)
     (λ (x t) -> e)
     (λ (x t) (x t) -> e)
     x
     number
     (apply e e)
     (mkVarT e)
     foo
     bar
     +)
  (t (→ t t)
     (Var t)
     Int)
  (x variable-not-otherwise-mentioned))


#;(redex-match
   L
   (e_1 e_2)
   (term ((λ (x Int) -> (apply x 1))
          (+ 1 2))))

(define-extended-language L+Γ L
  [Γ · (x : t Γ)])

#; (define-metafunction L+Γ
  [(different x_1 x_1) #f]
  [(different x_1 x_2) #t])

#;(define-judgment-form
  L+Γ
  #:mode (types I I O)
  #:contract (types Γ e t)

  [(types Γ e_1 t_1)
   --------------------------
   (types Γ (mkVarT e_1) (Var t_1))]
  
  [(types Γ e_1 (Var (→ t_1 t_2)))
   (types Γ e_2 (Var t_1))
   --------------------------
   (types Γ (apply e_1 e_2) (Var t_2))]
 
  [(types Γ e_1 (→ t_2 t_3))
   (types Γ e_2 t_2)
   -------------------------
   (types Γ (e_1 e_2) t_3)]
 
  [(types (x : t_1 Γ) e t_2)
   -----------------------------------
   (types Γ (λ (x t_1) e) (→ t_1 t_2))]

  [(types (x : t_1 Γ) e t_2)
   -----------------------------------
   (types Γ (λ (x t_1) -> e) (→ t_1 t_2))]

  [(types (x_1 : t_1 (x_2 : t_2 Γ)) e t_3)
   -----------------------------------
   (types Γ (λ (x_1 t_1) (x_2 t_2) -> e) (→ t_1 (→ t_2 t_3)))]
 
  [(types Γ e (→ (→ t_1 t_2) (→ t_1 t_2)))
   ---------------------------------------
   (types Γ (fix e) (→ t_1 t_2))]
 
  [---------------------
   (types (x : t Γ) x t)]
 
  [(types Γ x_1 t_1)
   (side-condition (different x_1 x_2))
   ------------------------------------
   (types (x_2 : t_2 Γ) x_1 t_1)]
 
  [(types Γ e1 Int)
   (types Γ e2 Int)
   -----------------------
   (types Γ (+ e1 e2) Int)]
 
  [--------------------
   (types Γ number Int)])



#;(judgment-holds
 (types · 1 t)
 t)

#;(judgment-holds
 (types · (λ (x Int) 4) t)
 t)

#;(judgment-holds
 (types · (mkVarT 5) t)
 t)

#;(judgment-holds
 (types · (apply (mkVarT (λ (x Int) 4)) (mkVarT 5)) t)
 t)

#;(judgment-holds
 (types · (apply (mkVarT (λ (x Int) (y Int) -> 4)) (mkVarT 5)) t)
 t)



(define-extended-language Ev L+Γ
  (E (v E)
     (E e)
     (mkVarT E)
     (apply e E)
     (apply E e)
     hole)
  (v (λ (x t) e)
     (λ (x t) -> e)
     (λ (x t) (x t) -> e)
     foo
     bar
     +
     number))


(define red
  (reduction-relation
   Ev
   #:domain e
   
   (--> (in-hole E (apply (mkVarT (λ (x t) -> x)) e_1))
        (in-hole E e_1)
        "v1")
   (--> (in-hole E (apply (apply (mkVarT (λ (x_1 t_1) (x_2 t_2) -> x_1)) e_1) e_2))
        (in-hole E e_1)
        "v1a")
   (--> (in-hole E (apply (apply (mkVarT (λ (x_1 t_1) (x_2 t_2) -> x_2)) e_1) e_2))
        (in-hole E e_2)
        "v1b")
   
   (--> (in-hole E (apply (mkVarT (λ (x t) -> e_2)) e_1))
        (in-hole E (mkVarT e_2))
        "v2")
   (--> (in-hole E (apply (apply (mkVarT (λ (x_1 t_1) (x_2 t_2) -> e_1)) e_2) e_3))
        (in-hole E (mkVarT e_1))
        "v2a")
   
   (--> (in-hole E (apply (apply (mkVarT (λ (x_1 t_1) (x_2 t_2) -> (e_1 e_2))) e_11) e_22))
        (in-hole E (apply (apply (apply (mkVarT (λ (x_1 t_1) (x_2 t_2) -> e_1)) e_11) e_22)
                        (apply (apply (mkVarT (λ (x_1 t_1) (x_2 t_2) -> e_2)) e_11) e_22)))
        "v3")
   ))


#;(traces red
          (term (apply (mkVarT (λ (x Int) -> x)) (mkVarT 1))))

#;(traces red
          (term (apply (apply (mkVarT (λ (x Int) (y Int) -> (6 7))) 1) 2)))

#;(traces red
        (term (apply
               (apply
                (mkVarT
                 (λ (x Int)
                   (y Int)
                   ->
                   (foo 7)))
                1)
               2)))

#;(traces red
        (term (apply (mkVarT (λ (x Int) -> x)) (apply (mkVarT (λ (x Int) -> x)) 1))))

#;(traces red
        (term (apply (apply (mkVarT (λ (x Int) (y Int) -> (+ (foo 7)))) 1) 2)))

(traces red
          (term (apply (apply (mkVarT (λ (a Int) (b Int) -> ((+ (foo a)) (bar b)))) 1) 2)))

