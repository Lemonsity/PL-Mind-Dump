#lang typed/racket

#|
The Little Schemer Template

Here we implement some functions from The Little Schemer by D Friedman and M Felleisen

Note The Little Schemer's target languages are LISP and Scheme
We use Typed Racker here. Some functions may not behave as expected
|#

(: atom? (-> Any Boolean))
(define atom?
  (λ (x)
    (and (not (pair? x)) (not (null? x)))))

(: lat? (-> (Listof Any) Boolean))
(define lat?
  (λ (list)
    (cond
      [(null? list) #t]
      [(atom? (car list)) (lat? (cdr list))]
      [else #f]
    )
  )
)

(: remove-member (-> Symbol (Listof Symbol) (Listof Symbol)))
(define remove-member
  (λ (a l)
    (cond
      [(null? l) '()]
      [(equal? a (car l)) (cdr l)]
      [else (cons (car l) (remove-member a (cdr l)))]
    )
  )
)

(: firsts (-> (Listof (Listof Symbol)) (Listof Symbol)))
(define firsts
  (λ (lat)
    (cond
      [(null? lat) '()]
      [else (cons (car (car lat)) (firsts (cdr lat)))]
    )
  )
)

(: insertR (-> Symbol Symbol (Listof Symbol) (Listof Symbol)))
(define insertR
  (λ (new old lat)
    (cond
      [(null? lat) (quote ())]
      [(equal? old (car lat)) (cons (car lat) (cons new (cdr lat)))]
      [else (cons (car lat) (insertR new old (cdr lat)))]
    )
  )
)

(: o+ (-> Integer Integer Integer))
(define o+
  (λ (n m)
    (cond
      [(zero? n) m]
      [else
        (add1 (o+ (sub1 n) m))
      ]
    )
  )
)

(: o- (-> Integer Integer Integer))
(define o-
  (λ (n m)
    (cond
      [(zero? m) n]
      [else (o- (sub1 m) (sub1 n))]
    )
  )
)

(: o* (-> Integer Integer Integer))
(define o*
  (λ (n m)
    (cond
      [(zero? m) 1]
      [else (o+ n (o* n (sub1 m)))]
    )
  )
)

(: addtup (-> (Listof Integer) Integer))
(define addtup
  (λ (tup)
    (cond
      [(null? tup) 0]
      [else (o+ (car tup) (addtup (cdr tup)))]
    )
  )
)

(: rempick (-> Integer (Listof Symbol) (Listof Symbol)))
(define rempick
  (λ (n lat)
    (cond
      [(zero? (sub1 n)) (cdr lat)]
      [else
       (cons
        (car lat)
        (rempick (sub1 n) (cdr lat)))]
    )
  )
)

(: tup+ (-> (Listof Integer) (Listof Integer) (Listof Integer)))
(define tup+
  (λ (tup1 tup2)
    (cond
      [(null? tup1) tup2]
      [(null? tup2) tup1]
      [else (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))]
    )
  )
)

(: o< (-> Integer Integer Boolean))
(define o<
  (λ (a b)
    (cond
      [(zero? b) #f]
      [(zero? a) #t]
      [else (o< (sub1 a) (sub1 b))]
    )
  )
)

(: o> (-> Integer Integer Boolean))
(define o>
  (λ (a b)
    (cond
      [(zero? a) #f]
      [(zero? b) #t]
      [else (o> (sub1 a) (sub1 b))]
    )
  )
)

(: o= (-> Integer Integer Boolean))
(define o=
  (λ (a b)
    (cond
      [(o< a b) #f]
      [(o> a b) #f]
      [else #t]
    )
  )
)

; Technically this function only takes positive intergers as input
; We using else defined o- only because we are following the textbook
(: o/ (-> Integer Integer Integer))
(define o/
  (λ (n m)
    (cond
      [(o< n m) 0]
      [else (add1 (o/ (o- n m) m))]
    )
  )
)

(: o^ (-> Integer Integer Integer))
(define o^
  (λ (a b)
    (cond
      [(zero? b) 1]
      [else (o* a (o^ a (sub1 b)))]
    )
  )
)

(: my-length (All (a) (-> (Listof a) Integer)))
(define my-length
  (λ (l)
    (cond
      [(empty? l) 0]
      [else (add1 (my-length (cdr l)))]
    )
  )
)

#|
An interesting thought. What would happen when we start to incorporate context into typing judgement?
In some langauges, 
|#
(: pick (-> Integer (Listof Symbol) Symbol))
(define pick
  (λ (n lat)
    (cond
      [(zero? (sub1 n)) (car lat)]
      [else (pick (sub1 n) (cdr lat))]
    )
  )
)
