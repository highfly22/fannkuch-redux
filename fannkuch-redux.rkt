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

(provide permutations-parallel permutations-place)

(require racket/unsafe/ops
         racket/place)

(module+ test
  (require rackunit))

(define (count-flips pi rho)
  (vector-copy! rho 0 pi)
  (let loop ([i 0])
    (if (unsafe-fx= (unsafe-vector-ref rho 0) 0)
        i
        (begin
          ;; (vector-reverse-slice! rho 0 (unsafe-fx+ 1 (unsafe-vector-ref rho 0)))
          (let reverse-loop ([i 0]
                             [j (unsafe-vector-ref rho 0)])
            (when (unsafe-fx> j i)
              (vector-swap! rho i j)
              (reverse-loop (unsafe-fx+ 1 i) (unsafe-fx- j 1))))
          (loop (unsafe-fx+ 1 i))))))

(define (vector-reverse-slice! v i j)
  (let loop ([i i]
             [j (unsafe-fx- j 1)])
    (when (unsafe-fx> j i)
      (vector-swap! v i j)
      (loop (unsafe-fx+ 1 i) (unsafe-fx- j 1)))))

(define-syntax-rule (vector-swap! v i j)
  (let ((t (unsafe-vector-ref v i)))
    (unsafe-vector-set! v i (unsafe-vector-ref v j))
    (unsafe-vector-set! v j t)))

(define-syntax-rule (rotate v n)
  (let ([t (vector-ref v 0)])
    (for ([i (in-range n)])
      (unsafe-vector-set! v i (unsafe-vector-ref v (unsafe-fx+ 1 i))))
    (unsafe-vector-set! v n t)))

(define (fact n)
  (for/product ([i (in-range 1 (add1 n))]) i))

(define (permutations-init n start)
  (define v (make-vector n))
  (for ([i (in-range n)])
    (vector-set! v i i))

  (define c (make-vector n 0))

  ;; (displayln v)
  (if (zero? start)
      (values v c 0)
      (let loop ([index 1]
                 [i 0])
        (when (not (unsafe-fx= 0 i))
          (rotate v i))
        (cond [(unsafe-fx>= (unsafe-vector-ref c i) i)
               (unsafe-vector-set! c i 0)
               (loop index (unsafe-fx+ 1 i))]
              [else
               (unsafe-vector-set! c i (unsafe-fx+ 1 (unsafe-vector-ref c i)))
               ;; (displayln v)

               (cond [(unsafe-fx>= index start)
                      (values v c 1)]
                     [else
                      (loop (unsafe-fx+ 1 index) 1)])]))))

(define (permutations-loop v c ci start end)
  (define n (vector-length v))
  (define n-1 (sub1 n))
  (define tmp (make-vector n 0))
  (define end-1 (sub1 end))

  (define flips-on-start (count-flips v tmp))
  (define checksum-on-start (if (even? start)
                                flips-on-start
                                (- flips-on-start)))
  ;; (printf "~a ~a ~a\n" v start checksum-on-start)

  ;; (displayln v)
  (let loop ([index (add1 start)]
             [i ci]
             [checksum checksum-on-start]
             [max-flip flips-on-start]
             [toggle (odd? start)])
    (when (not (unsafe-fx= 0 i))
      (rotate v i))
    (cond [(unsafe-fx>= (unsafe-vector-ref c i) i)
           (unsafe-vector-set! c i 0)

           (cond [(unsafe-fx< i n-1)
                  (loop index (unsafe-fx+ 1 i) checksum max-flip toggle)]
                 [else
                  (list checksum max-flip)])]
          [else
           (unsafe-vector-set! c i (unsafe-fx+ 1 (vector-ref c i)))
           ;; (displayln v)

           (define f (count-flips v tmp))
           (define m (max max-flip f))
           (define csum (if toggle
                            (unsafe-fx+ checksum f)
                            (unsafe-fx- checksum f)))

           ;; (printf "~a ~a ~a\n" v index f)           
           (cond [(unsafe-fx>= index end-1)
                  (list csum m)]
                 [else
                  (loop (unsafe-fx+ 1 index) 1 csum m (not toggle))])])))

(define (permutations-part n start end)
  (define-values (v c i) (permutations-init n start))
  (permutations-loop v c i start end))


(define (permutations-part-2 n start end)
  (define v (make-vector n))
  (for ([i (in-range n)])
    (vector-set! v i i))

  (define c (make-vector n 0))
  (define tmp (make-vector n 0))

  ;; (displayln v)
  (let loop ([index 1]
             [i 0]
             [checksum 0]
             [max-flip 0]
             [toggle #f])
    (when (not (unsafe-fx= 0 i))
      (rotate v i))
    (cond [(unsafe-fx>= (unsafe-vector-ref c i) i)
           (unsafe-vector-set! c i 0)
           (cond [(unsafe-fx< i (sub1 n))
                  (loop index (unsafe-fx+ 1 i) checksum max-flip toggle)]
                 [else
                  (list checksum max-flip)])]
          [else
           (unsafe-vector-set! c i (unsafe-fx+ 1 (unsafe-vector-ref c i)))

           (cond [(unsafe-fx>= index start)
                  (define f (count-flips v tmp))
                  (define m (max max-flip f))
                  (define csum (if toggle
                                   (unsafe-fx+ checksum f)
                                   (unsafe-fx- checksum f)))
                  ;; (printf "~a ~a ~a\n" v index f)
                  (if (unsafe-fx>= index (sub1 end))
                      (list csum m)
                      (loop (add1 index) 1 csum m (not toggle)))]
                 [else
                  (loop (add1 index) 1 0 0 (not toggle))])])))

(define (permutations-place pch)
  (define args (place-channel-get pch))
  (place-channel-put pch (time (apply permutations-part args)))
  (printf "~a Done!\n" args))

(define (permutations-parallel n core)
  (define part (/ (fact n) core))
  (define pls (for/list ([i (in-range core)])
                (dynamic-place "fannkuch-redux.rkt" 'permutations-place)))
  (for ([ch pls]
        [i (in-range core)])
    (place-channel-put ch (list n (* i part) (* (add1 i) part))))
  (for/fold ([checksum 0]
             (max-flips 0))
      ([ch pls])
    (define result (place-channel-get ch))
    (values (+ checksum (first result))
            (max max-flips (second result)))))

(define (permutations-single n)
  (define v (make-vector n))
  (for ([i (in-range n)])
    (vector-set! v i i))

  (define c (make-vector n 0))

  (define tmp (make-vector n 0))

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
                  (define f (count-flips v tmp))
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

  (check-equal? (count-flips (vector 3 1 0 4 2) (make-vector 5)) 5)
  (check-equal? (count-flips (vector 1 3 0 4 2) (make-vector 5)) 6)
  
  (check-equal? (call-with-values (thunk (permutations-single 5)) list) '(11 7))
  (check-equal? (call-with-values (thunk (permutations-single 6)) list) '(49 10))
  (check-equal? (call-with-values (thunk (permutations-single 7)) list) '(228 16))
  (check-equal? (call-with-values (thunk (permutations-single 8)) list) '(1616 22))
  (check-equal? (call-with-values (thunk (permutations-single 9)) list) '(8629 30))

  (check-equal? (call-with-values (thunk (permutations-parallel 5 4)) list) '(11 7))
  (check-equal? (call-with-values (thunk (permutations-parallel 6 4)) list) '(49 10))
  (check-equal? (call-with-values (thunk (permutations-parallel 7 4)) list) '(228 16))
  (check-equal? (call-with-values (thunk (permutations-parallel 8 4)) list) '(1616 22))
  (check-equal? (call-with-values (thunk (permutations-parallel 9 4)) list) '(8629 30)))

(module* main #f
  (define-values (n core)
    (command-line
     #:args (n-str [core-str "4"])
     (values (string->number n-str)
             (string->number core-str))))
  (define-values (checksum max-flip) (permutations-parallel n core))
  (printf "~a\nPfannkuchen(~a) = ~a\n" checksum n max-flip))
