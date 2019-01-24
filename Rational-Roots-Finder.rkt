;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Rational-Roots-Finder) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))

(define (factors num lon)
  (filter (lambda (x) (= (modulo num x) 0)) lon))


(define (factor-main num)
  (factors num (build-list (abs num) (lambda (x) (+ 1 x)))))


(define (p-q lon1 lon2)
  (cond [(empty? lon2) empty]
        [else (cons (/ (first lon1) (first lon2)) (p-q lon1 (rest lon2)))]))

(define (p-qmain lon1 lon2)
  (cond [(empty? lon1) empty]
        [else (append (p-q lon1 lon2) (p-qmain (rest lon1) lon2))]))

(define (proper-p-q lon1 lon2)
  (local [(define list1 (append (p-qmain lon1 lon2) (map - (p-qmain lon1 lon2))))
          (define (remove-duplicates list1)
            (cond [(empty? list1) empty]
                  [(member? (first list1) (rest list1)) (remove-duplicates (rest list1))]
                  [else (cons (first list1) (remove-duplicates (rest list1)))]))]
    (quicksort (remove-duplicates list1) <)))

(define (root-finder loc)
  (local [(define a (first loc))
          (define b (first (reverse loc)))]
    (proper-p-q (factor-main b) (factor-main a))))
    

(define (polynomial coeffs)
  (lambda (x)
    (cond
      [(= (length coeffs) 0) 0]
      [(= (length coeffs ) 1) (first coeffs)]
      [else (+ (* (first coeffs) (expt x (- (length coeffs) 1))) (poly-helper (rest coeffs) x))])))

(define (poly-helper coeffs x)
  (cond
    [(= (length coeffs) 0) 0]
    [(= (length coeffs ) 1) (first coeffs)]
    [else (+ (* (first coeffs) (expt x (- (length coeffs) 1))) (poly-helper (rest coeffs) x))]))

;; Main Function
;; (find-the-roots loc) produces a list of the rational roots of a polynomial when given a
;;  a list of coefficients of the polynomial (loc)
;; find-the-roots: (listof Num) -> (listof Num)
;; Example:
(check-expect (find-the-roots '(1 3 2)) '(-2 -1))

(define (find-the-roots loc)
  (filter (lambda (x) (= ((polynomial loc) x) 0)) (root-finder loc)))
