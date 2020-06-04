;Question 1
(define findindex_recur
  (lambda (list x count)
    (if (null? list)
        list
        (if (= (car list) x)
            (cons count (findindex_recur (cdr list) x (+ count 1)))
            (findindex_recur (cdr list) x (+ count 1))
            ))))

(define findindex
  (lambda (list x)
    (define ans (findindex_recur list x 0))
    (if (null? ans) '(-1) ans)))

;Question 2
(define (inc x) (+ x 1))
(define findrangeindex_recur
  (lambda (list inc x count)
    (if (null? list)
        list
        (if (= (inc (car list)) x)
            (cons count (findrangeindex_recur (cdr list) inc x (+ count 1)))
            (findrangeindex_recur (cdr list) inc x (+ count 1))
            ))))

(define findrangeindex
  (lambda (list inc x)
    (define ans (findrangeindex_recur list inc x 0))
    (if (null? ans) '(-1) ans)))

;Question 3
(define contain
  (lambda (x list2)
    (if (null? list2)
        #f
        (if (equal? x (car list2))
            #t
            (contain x (cdr list2))))))

(define remove_dup
  (lambda (lb list1)
    (if (null? list1)
        lb
        (if (contain (car list1) (cdr list1))
            (remove_dup lb (cdr list1))
            (remove_dup (cons (car list1) lb) (cdr list1))))))

(define helper
  (lambda (undup list2 ans)
    (if (null? undup)
        ans
        (if (contain (car undup) list2)
            (helper (cdr undup) list2 (cons (car undup) ans)) 
            (helper (cdr undup) list2 ans)))))

(define (intersection list1 list2)
   (helper (remove_dup '() list1) list2 '()))

;Question 4
(define compose
  (lambda (func1 func2)
    (lambda (x)
      (func2 (func1 x)))))

;Question 5
(define map2_recur
  (lambda (f p j l)
    (if (null? l) l
        (if (p (car j))
            (cons (f (car l)) (map2_recur f p (cdr j) (cdr l)))
            (cons (car l) (map2_recur f p (cdr j) (cdr l)))
            ))))

(define equal_len
  (lambda (j l)
    (if (and (null? l) (null? j))
        #t 
        (if (or (null? l) (null? j))
            #f
            (equal_len (cdr j) (cdr l))))))

(define map2
  (lambda (j l p f)
    (if (not (equal_len j l))
        "ERROR"
        (map2_recur f p j l))))

;Question 6
(define (skip x)
  (lambda a
    (if (= x 0)
        a
        (skip (- x 1)))))