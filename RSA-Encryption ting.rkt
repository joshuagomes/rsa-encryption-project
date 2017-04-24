;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |RSA-Encryption ting|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct rsa (n theta e d))

(define (RSA-ITEMS p1 p2)
  (local
    [(define (n p1 p2) (* p1 p2))
     (define (theta p1 p2) (* (- p1 1) (- p2 1)))
     (define (euclid-gcd n m)
       (cond [(zero? m) n]
             [else (euclid-gcd m (remainder n m))]))
     (define (find-e theta)
       (first (filter (lambda (x)
                        (= 1 (euclid-gcd x theta)))
                      (build-list (- theta 2) (lambda (x) (+ x 2))))))
     (define (find-d e theta)
       (first (filter (lambda (x)
                        (integer? (/ (- (* e x) 1) theta)))
                      (build-list (- theta 2) (lambda (x) (+ x 2))))))]
    (list (n p1 p2)
          (theta p1 p2)
          (find-e (theta p1 p2))
          (find-d (find-e (theta p1 p2)) (theta p1 p2)))))

(define main-rsa-items (RSA-ITEMS 4409 1039))

(define my-rsa (make-rsa (first main-rsa-items)
                         (second main-rsa-items)
                         (third main-rsa-items)
                         (fourth main-rsa-items)))

(define (encrypt M)
  (modulo (expt M (rsa-e my-rsa)) (rsa-n my-rsa)))

(define (decrypt C)
  (modulo (expt C (rsa-d my-rsa)) (rsa-n my-rsa)))

(define (convert-word word)
  (map char->integer (string->list word)))

(define (convert-back lon)
  (map integer->char lon))

(define (final-encrypt word)
  (map encrypt (shift-up (secret-change (convert-word word)))))

(define (final-decrypt lon)
  (list->string (convert-back (un-secret-change (shift-down (map decrypt lon))))))

(define (shift-up lon)
  (local
    [(define leng (length lon))
     (define (work lon c)
       (cond
         [(= c leng) empty]
         [else (cons (+ c (first lon))
                     (work (rest lon) (+ 1 c)))]))]
    (work lon 0)))

(define (shift-down lon)
  (local
    [(define leng (length lon))
     (define (work lon c)
       (cond
         [(= c leng) empty]
         [else (cons (- (first lon) c)
                     (work (rest lon) (+ 1 c)))]))]
    (work lon 0)))

(define (secret-change lon)
  (local
    [(define start (first lon))
     (define (work lon previous)
       (cond
         [(empty? lon) empty]
         [(= (first lon) start) (cons start (work (rest lon) start))]
         [else (cons (* previous (first lon)) (work (rest lon) (first lon)))]))]
    (work lon 0)))

(define (un-secret-change lon)
  (local
    [(define start (first lon))
     (define (work lon previous)
       (cond
         [(empty? lon) empty]
         [(= (first lon) start) (cons start (work (rest lon) start))]
         [else (cons (/ (first lon) previous) (work (rest lon) (/ (first lon) previous)))]))]
    (work lon 0)))

(define (squish lon counter) ;;requires 2 digit numbers
  (cond                      ;;counter is ((length of lon - 1) * 2)
    [(empty? lon) 0]
    [else (+ (* (first lon) (expt 10 counter))
             (squish (rest lon) (- counter 2)))]))

(define (un-squish n)
  (local
    [(define (char->num c)
       (- (char->integer c) 48))
     (define (split n)
       (map (lambda (x) (char->num x)) (string->list
                                        (number->string n))))
     (define (group lon)
       (cond
         [(empty? lon) empty]
         [else (cons (+ (* (first lon) (expt 10 1))
                        (* (second lon) (expt 10 0)))
                     (group (rest (rest lon))))]))]
    (group (split n))))


