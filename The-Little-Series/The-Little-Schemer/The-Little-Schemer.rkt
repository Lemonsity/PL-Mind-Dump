#lang typed/racket

#|
The Little Schemer Template

Here we implement many functions from The Little Schemer by D Friedman and M Felleisen

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

(: o+ (-> Number Number Number))
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

(: o- (-> Number Number Number))
(define o-
  (λ (n m)
    (cond
      [(zero? m) n]
      [else (o- (sub1 m) (sub1 n))]
    )
  )
)

(: addtup (-> (Listof Number) Number))
(define addtup
  (λ (tup)
    (cond
      [(null? tup) 0]
      [else (o+ (car tup) (addtup (cdr tup)))]
    )
  )
)

(: rempick (-> Number (Listof Symbol) (Listof Symbol)))
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

(: tup+ (-> (Listof Number) (Listof Number) (Listof Number)))
(define tup+
  (λ (tup1 tup2)
    (cond
      [(null? tup1) tup2]
      [(null? tup2) tup1]
      [else (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))]
    )
  )
)

(: o< (-> Number Number Boolean))
(define o<
  (λ (a b)
    (cond
      [(zero? b) #f]
      [(zero? a) #t]
      [else (o< (sub1 a) (sub1 b))]
    )
  )
)

(: o> (-> Number Number Boolean))
(define o>
  (λ (a b)
    (cond
      [(zero? a) #f]
      [(zero? b) #t]
      [else (o> (sub1 a) (sub1 b))]
    )
  )
)

(: o= (-> Number Number Boolean))
(define o=
  (λ (a b)
    (cond
      [(o< a b) #f]
      [(o> a b) #f]
      [else #t]
    )
  )
)
