#lang racket
#|
    Take a permutation of {1,...,n}, for example: {4,2,1,5,3}.
    Take the first element, here 4, and reverse the order of the first 4 elements: {5,1,2,4,3}.
    Repeat this until the first element is a 1, so flipping won't change anything more: {3,4,2,1,5}, {2,4,3,1,5}, {4,2,3,1,5}, {1,3,2,4,5}.
    Count the number of flips, here 5.
    Keep a checksum
        checksum = checksum + (if permutation_index is even then flips_count else -flips_count)
        checksum = checksum + (toggle_sign_-1_1 * flips_count)
    Do this for all n! permutations, and record the maximum number of flips needed for any permutation.
|#

(provide permutations)

(module+ test
  (require rackunit))

(define (vector-swap! v i j)
  (let ([t (vector-ref v i)])
    (vector-set! v i (vector-ref v j))
    (vector-set! v j t)))

(define (flip v)
  (let ([n (vector-ref v 0)])
    (for ([i (in-range (ceiling (/ n 2)))])
      (vector-swap! v i (- n i)))))

(define (flip-count vi)
  (let ([v (vector-copy vi)])
    (let loop ([i 0])
      (cond [(not (zero? (vector-ref v 0)))
             (flip v)
             (loop (add1 i))]
            [else i]))))

(define (rotate v n)
  (let ([t (vector-ref v 0)])
    (vector-copy! v 0 v 1 (add1 n))
    (vector-set! v n t)))

(define (permutations n)
  (define v (make-vector n))
  (for ([i (in-range n)])
    (vector-set! v i i))

  (define c (make-vector n 0))

  ;; (displayln v)
  (let loop ([i 0]
             [checksum 0]
             [toggle #f]
             [max-flip 0])
    (cond [(< i n)
           (when (not (zero? i))
             (rotate v i))
           (cond [(>= (vector-ref c i) i)
                  (vector-set! c i 0)
                  (loop (add1 i) checksum toggle max-flip)]
                 [else
                  (vector-set! c i (add1 (vector-ref c i)))
                  ;; (displayln v)
                  (define f (flip-count v))
                  (define m (max max-flip f))
                  (loop 1
                        (if toggle
                            (+ checksum f)
                            (- checksum f))
                        (not toggle)
                        m)])]
          [else
           (values checksum max-flip)])))

(module+ test

  (define v (vector 4 2 1 5 3))
  (rotate v 3)
  (check-equal? v  #(2 1 5 4 3))
  (rotate v 2)
  (check-equal? v  #(1 5 2 4 3))
  (rotate v 0)
  (vector-swap! v 0 1)
  (check-equal? v #(5 1 2 4 3))
  (vector-swap! v 0 4)
  (check-equal? v #(3 1 2 4 5))
  (define v2 (vector 3 1 0 4 2))
  (flip v2)
  (check-equal? v2 #(4 0 1 3 2))
  (flip v2)
  (check-equal? v2 #(2 3 1 0 4))
  (check-equal? (flip-count (vector 3 1 0 4 2)) 5)
  (check-equal? (flip-count (vector 1 3 0 4 2)) 6)
  (check-equal? (call-with-values (thunk (permutations 5)) list) '(11 7))
  (check-equal? (call-with-values (thunk (permutations 6)) list) '(49 10))
  (check-equal? (call-with-values (thunk (permutations 7)) list) '(228 16))
  (check-equal? (call-with-values (thunk (permutations 8)) list) '(1616 22))
  (check-equal? (call-with-values (thunk (permutations 9)) list) '(8629 30)))

(module* main #f
  (define args (current-command-line-arguments))
  (cond [(= 1 (vector-length args))
         (define n (string->number (vector-ref args 0)))
         (cond [(and (> n 4) (< n 13))
                (define-values (checksum max-flip) (time (permutations n)))
                (printf "~a\nPfannkuchen(~a) = ~a\n" checksum n max-flip)]
               [else
                (print "4 < n < 13")])]
        [else
         (print "usage: this-script number")]))
