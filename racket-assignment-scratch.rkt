#lang racket

; FUNCTIONS USED IN MEAN, MEDIAN AND MODE

; findMax finds the maximum value of a list
(define (findMax lis)
  (if (= (length lis) 1)
      (car lis) 
      (if (> (car lis)(findMax (cdr lis)))
          (car lis)
          (findMax (cdr lis)))))

; findMaxC does the same as findMax but uses cond
(define (findMaxC lis)
  (cond
    [(= (length lis) 1) (car lis)]
    [(> (car lis) (findMaxC (cdr lis))) (car lis)]
    [else (findMaxC (cdr lis))]))

; sumList calculates the sum of a list
(define (sumList lis)
  (if (empty? lis) 0 (+ (car lis) (sumList (cdr lis)))))

; middleOne returns the middle value of a list with an odd length
(define (middleOne lis)
  (if (even? (length lis))
  0
  (if
   (= 1 (length lis))
   lis
   (middleOne (cdr (sort (cdr (sort lis <)) >))))))

; middleTwo returns the middle two values of list with an even length
(define (middleTwo lis)
  (if
   (even? (length lis))
   (if (= 2 (length lis))
       lis
       (middleTwo (cdr (sort (cdr (sort lis <)) >))))
   0))

; if i is 0 then return the first item
;otherwise decrement i and recurse on the tail of the list
(define (memberAt i lis)
  (if (= i 0)
      (car lis)
      (memberAt (- i 1) (cdr list))))

; middle1 and middle2 created in class
(define (middle1 lis)
  (memberAt (floor (/ (length lis) 2)) lis))

(define (middle2 lis)
  (let
      [
       (leftmid (memberAt (- (/ (length lis) 2) 1) lis))
       (rightmid (memberAt (/ (length lis) 2) lis))
       ]
    (list leftmid rightmid)))

  
; MEAN
; mean adds the values of the list and divides by the length of the list
(define (mean lis)
  (if (empty? lis)
      lis
      (/ (sumList lis) (length lis))))


; MEDIAN 
; median returns the middle value of a list with an odd length or the 
; average of the middle two values of a list with an even length
; created before we talked about the easier way
(define (median lis)
  (if (empty? lis)
      lis
      (if (even? (length lis))
      (/ (sumList (middleTwo lis)) 2)
      (middleOne lis))))

; created in class after we talked about the easier way
(define (median2 lis)
  (let
      [(sortedlis (sort lis <))]
    (if (even? (length lis))
        (/ (sumList (middle2 lis)) 2)
        (middle1 lis))))
    
; MODE

; count how
; base case - empty list ==> return 0
(define (countAppearances atm  lis)
  (cond
    [(empty? lis) 0]
    [(equal? atm (car lis)) (+ 1 (countAppearances atm (cdr lis)))]
    [else (countAppearances atm (cdr lis))]))
  
; countAll lis1 lis2
; counts how many times all the items in lis1 appear in lis2
; lis1 will be traversed recursively
; lis2 will never change
(define (countAll lis1 lis2)
  (if (= (length lis1) 1) (list (cons (car lis1) (countAppearances (car lis1) lis2)))
      (append (list (cons (car lis1) (countAppearances (car lis1) lis2))) (countAll (cdr lis1) lis2))))


(define (pairorder a b)
  (> (cdr a) (cdr b)))


(define (mode lis)
  (let
      [
       (listcounts (countAll (remove-duplicates lis) lis))
      ]
    (car (car (sort listcounts pairorder)))))
      