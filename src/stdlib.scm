(define fib-r (lambda (j i n)
    (if (= n 0)
        j
        (fib-r (+ i j) j (- n 1)))))

(define fib-fast (lambda (n)
    (fib-r 0 1 n)))

(define fib-slow (lambda (n)
    (if (= n 0)
        0
        (if (= n 1)
            1
            (+ (fib-slow (- n 1)) (fib-slow (- n 2)))))))