#lang racket
(require pdf-read)

;; convert day of month codes to two digits with leading 0
(define (d->dd x)
  (if (equal? (string-length x) 1) (string-append "0" x) x))

;; convert 3 plus character alphabetic months to 2 digit codes
(define (mmm->mm x)
  (let ([month (string-downcase x)])
    (cond [ (equal? "jan" (substring month 0 3)) "-01-" ]          [ (equal? "feb" (substring month 0 3)) "-02-" ]
          [ (equal? "mar" (substring month 0 3)) "-03-" ]          [ (equal? "apr" (substring month 0 3)) "-04-" ]
          [ (equal? "may" (substring month 0 3)) "-05-" ]          [ (equal? "jun" (substring month 0 3)) "-06-" ]
          [ (equal? "jul" (substring month 0 3)) "-07-" ]          [ (equal? "aug" (substring month 0 3)) "-08-" ]
          [ (equal? "sep" (substring month 0 3)) "-09-" ]          [ (equal? "oct" (substring month 0 3)) "-10-" ]
          [ (equal? "nov" (substring month 0 3)) "-11-" ]          [ (equal? "dec" (substring month 0 3)) "-12-" ]
          [ else "-99-" ]   )))
          
;; this is a custom version of member which looks for 2 consecutive pattern matches
(define (seek-double pattern string)
  (if (equal? 0 (length string))
      (list "FAIL" "FAIL" "FAIL")
      (if (and (equal? (first pattern) (first string)) (equal? (second pattern) (second string)))
          (cdr (cdr string))
          (seek-double pattern (cdr string))  )))

;; current sort case statement finds Walmart statements / TDW Confirmations / TDW Statements ... for now
(define (sort1 x)
  (let ([text-list (string-split (string-replace (page-text x) "," " "))]
        [filename (path->string x)]
        [filename-list (string-split (regexp-replace #rx"[\\(.]" (path->string x) "_") "_")])
          ; Walmart MCD Statements
     (cond [ (equal? "document" (first filename-list))
               (let ([date-field (seek-double (list "STATEMENT" "PERIOD") text-list)])
                 (if (string-contains? (first date-field) "FAIL") x
                     ; build filename THEN mv if does NOT exist
                     (let ([new-filename (string-append "WMT-" (third date-field) (mmm->mm (first date-field)) (d->dd (second date-field)) ".pdf")])
                       (if (file-exists? new-filename) "FILE EXISTS" (rename-file-or-directory x new-filename))))) ]
          ; TDW Confirmations
          [ (equal? "Confirmation" (first filename-list))
               (let ([date-field (seek-double (list "settlement" "on:") text-list)]
                     [account-field (seek-double (list "and" "type") text-list)])
                 (if (or (string-contains? (first date-field) "FAIL") (string-contains? (first date-field) "FAIL") ) x
                     (string-append "confirm-" (first account-field) "-" (third date-field) (mmm->mm (first date-field)) (d->dd (second date-field)) ".pdf"))) ]
          ; TDW Statements
          [ (equal? "Statement" (first filename-list))
               (let ([date-field (member "statement" text-list)])
                 (if (or (string-contains? (first date-field) "FAIL") (string-contains? (first date-field) "FAIL") ) x
                     (string-append "state-" (second filename-list) "-" (fourth date-field) (mmm->mm (second date-field)) (d->dd (third date-field)) ".pdf"))) ]
          [ else filename-list ]   )))

;;;; Set directory / create file filter / glob files
(current-directory "/home/daddio/Downloads")
(define (pdf-filter x) (filter (lambda (x) (string-contains? (path->string x) "pdf")) (x) )) 

(map sort1 (pdf-filter directory-list) )
