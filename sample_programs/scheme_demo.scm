; A small demo program for the Scheme interpreter.

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(display "10! = ")
(display (fact 10))
(newline)

(define (range from to)
  (if (>= from to)
      '()
      (cons from (range (+ from 1) to))))

(define (fizzbuzz n)
  (cond ((= (modulo n 15) 0) "FizzBuzz")
        ((= (modulo n 3) 0) "Fizz")
        ((= (modulo n 5) 0) "Buzz")
        (else n)))

(display (map fizzbuzz (range 1 16)))
(newline)
