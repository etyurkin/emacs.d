;;; Port of common lisp (format nil "~r" 1234) and (format nil "~:r" 1234) functions:

;;; (number-to-english-cardinal 1234) ==> "one thousand two hundred thirty-four"
;;; (number-to-english-ordinal 1234) ==> "one thousand two hundred thirty-fourth"

;;; see https://github.com/sbcl/sbcl/blob/e95100470561cfda5a2f8efd70274509d340686a/src/code/target-format.lisp

(defvar *cardinal-ones*
  (vector nil "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))

(defvar *cardinal-tens*
  (vector nil nil "twenty" "thirty" "forty"
          "fifty" "sixty" "seventy" "eighty" "ninety"))

(defvar *cardinal-teens*
  (vector "ten" "eleven" "twelve" "thirteen" "fourteen"
          "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"))

(defvar *cardinal-periods*
  (vector "" " thousand" " million" " billion" " trillion" " quadrillion"
          " quintillion" " sextillion" " septillion" " octillion" " nonillion"
          " decillion" " undecillion" " duodecillion" " tredecillion"
          " quattuordecillion" " quindecillion" " sexdecillion" " septendecillion"
          " octodecillion" " novemdecillion" " vigintillion"))

(defvar *ordinal-ones*
  (vector nil "first" "second" "third" "fourth"
          "fifth" "sixth" "seventh" "eighth" "ninth"))

(defvar *ordinal-tens*
  (vector nil "tenth" "twentieth" "thirtieth" "fortieth"
          "fiftieth" "sixtieth" "seventieth" "eightieth" "ninetieth"))

(defun number-to-english-small-cardinal (n)
  (let ((result "")
        (hundreds (truncate n 100))
        (rem (% n 100)))
    (when (cl-plusp hundreds)
      (setq result (concat result
                           (elt *cardinal-ones* hundreds)
                           " hundred"))
      (when (cl-plusp rem)
        (setq result (concat result " "))))
    (when (cl-plusp rem)
      (let ((tens (truncate rem 10))
            (ones (% rem 10)))
        (cond ((< 1 tens)
               (setq result (concat result (elt *cardinal-tens* tens)))
               (when (cl-plusp ones)
                 (setq result (concat result
                                      "-"
                                      (elt *cardinal-ones* ones)))))
              ((= tens 1)
               (setq result (concat result (elt *cardinal-teens* ones))))
              ((cl-plusp ones)
               (setq result (concat result (elt *cardinal-ones* ones)))))))
    result))

;; (number-to-english-small-cardinal 100)

(defun number-to-english-cardinal-aux (n period err)
  (let ((result "")
        (beyond (truncate n 1000))
        (here (% n 1000)))
    (unless (<= period 21)
      (error "Number too large to print in English: %d" err))
    (unless (zerop beyond)
      (setq result (concat result
                           (number-to-english-cardinal-aux beyond (1+ period) err))))
    (unless (zerop here)
      (unless (zerop beyond)
        (setq result (concat result " ")))
      (setq result (concat result
                           (number-to-english-small-cardinal here)
                           (elt *cardinal-periods* period))))
    result))

;; (number-to-english-cardinal-aux 1000 0 1000)

(defun number-to-english-cardinal (n)
  "Return number N as a human readable english cardinal number."
  (interactive "nNumber? ")
  (let ((result ""))
    (cond ((cl-minusp n)
           (setq result (concat result
                                "negative "
                                (number-to-english-cardinal-aux (- n) 0 n))))
          ((zerop n)
           (setq result "zero"))
          (t
           (setq result (number-to-english-cardinal-aux n 0 n))))
    (when (called-interactively-p 'any)
      (print result))
    result))

;; (number-to-english-cardinal 10000432320000123)

(defun number-to-english-ordinal (n)
  "Return number N as a human readable english ordinal number."
  (interactive "nNumber? ")
  (let ((result ""))
    (when (cl-minusp n)
      (setq result "negative "))
    (let ((number (abs n)))
      (let ((top (truncate number 100))
            (bot (% number 100)))
        (unless (zerop top)
          (setq result (concat result (number-to-english-cardinal (- number bot)))))
        (when (and (cl-plusp top) (cl-plusp bot))
          (setq result (concat result " ")))
        (let ((tens (truncate bot 10))
              (ones (% bot 10)))
          (cond ((= bot 12)
                 (setq result (concat result "twelfth")))
                ((= tens 1)
                 (setq result (concat result
                                      (elt *cardinal-teens* ones)
                                      "th")))
                ((and (zerop tens) (cl-plusp ones))
                 (setq result (concat result (elt *ordinal-ones* ones))))
                ((and (zerop ones)(cl-plusp tens))
                 (setq result (concat result (elt *ordinal-tens* tens))))
                ((cl-plusp bot)
                 (setq result (concat result
                                      (elt *cardinal-tens* tens)
                                      "-"
                                      (elt *ordinal-ones* ones))))
                ((cl-plusp number)
                 (setq result (concat result "th")))
                (t
                 (setq result (concat result "zeroth")))))))
    (when (called-interactively-p 'any)
      (print result))
    result))

