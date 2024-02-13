#lang typed/racket

(: peirce (All (a) (-> (-> (-> a Nothing) a) a)))
(define peirce
  (λ (f) ; (a -> Nothing) -> a
    (call/cc (λ ([k : (-> a Nothing)])
               (f k)))))


(: temp (Listof Number))
(define temp
  (cons 1
        (call/cc (λ ([k : (-> (Listof Number) (Listof Number))])
                   (cons 2
                         (k (cons 3 empty)))))))