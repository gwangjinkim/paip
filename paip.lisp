
;;;;;;;;;;;;;;;;;;;
;; Ch2
;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;
;; 2.2 random sentence generator
;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;
;; straightforward solution
;;;;;;;;;;;;;;;;;;;

(defun sentence ()    (append (noun-phrase) (verb-phrase)))
(defun houn-phrase () (append (Article) (Noun)))
(defun verb-phrase () (append (Verb) (noun-phrase)))
(defun Article ()     (one-of '(the a)))
(defun Noun ()        (one-of '(man ball woman table)))
(defun Verb ()        (one-of '(hit took saw liked)))

(defun one-of (set)
  "Pick one element of set and make a list of it."
  (list (random-elt set)))

(defun random-elt (choices)
  "Choose an element from a list of at random."
  (elt choices (random (length choices))))

;; see what is happening:
(trance sentence
        noun-phrase
        verb-phrase
        article
        noun
        verb)
(sentence)


(defun Adj* ()  (if (= (random 2) 0)
                    nil
                    (append (Adj) (Adj*))))

(defun PP* ()   (if (random-elt '(t nil))
                    (append (PP) (PP*))
                    nil))

(defun noun-phrase () (append (Article) (Adj*) (Noun) (PP*)))
(defun PP () (append (Prep) (noun-phrase)))
(defun Adj () (one-of '(big little blue green adiabatic)))
(defun Prep () (one-of '(to in by with on)))


;;;;;;;;; two wrong definitione of Adj*
;; (defun Adj* ()
;;   "Warning - incorrect definition of Adjectives."
;;   (one-of '(nil (append (Adj) (Adj*)))))
;; 
;; (defun Adj* ()
;;   "Warning - incorrect definition of Adjectives."
;;   (one-of (list nil (append (Adj) (Adj*)))))
;; 
;; the first returns literal `(append (Adj) (Adj*))`
;; the second endless loop!


;;;;;;;;;;;;;;;;;;;
;; rule-based solution
;;;;;;;;;;;;;;;;;;;


(defparameter *simple-grammar*
  '((sentence        -> (non-phrase verb-phrase))
    (noun-phrase     -> (Article Noun))
    (verb-phrase     -> (Verb noun-phrase))
    (Article         -> the a)
    (Noun            -> man ball woman table)
    (Verb            -> hit took saw liked))
  "A grammar for a trivial subset of English.")

(defvar *grammar* *simple-grammar*
  "The grammar used by generate. Initially, this is 
  *simple-grammar*, but we can switch to other grammars.")

;; `->` is purely decorative

;; `assoc` is a key and it selects the sub-list out
;; (assoc 'noun *grammar*) ;; returns: (NOUN -> MAN BAL WOMAN TABLE)

;; although rules are simply implemented as lists,
;; it is good to introdues a layer of abstraction
;; by defining functions to operate on the rules.

;; we need three functions, 
;;  one to get right-hand side of a rule
;;  one for left-hand side
;;  one for look up all ossible rewrites (right-handsides) for a category

(defun rule-rhs (rule)
  "The right-hand side of a rule."
  (rest (rest rule)))

(defun rule-lhs (rule)
  "The left-hand side of a rule."
  (first rule))

(defun rewrites (category)
  "Return a list of possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

;; these functions make program more readable and changing representaion
;; of rules easier.

;; define function generating the sentences `generate`

;; 3 cases:
;; (1) simplest case: symbol is passed, one random pick of rewrites
;; (2) no possible rewrites: it must be a symbol - a word and not a grammatical cagetory.
;;        and we want it to left alone. (actually: return list of input word,
;;        beaus ewe want all results to be list of words.
;; (3) if rewrites contains a list, we want to generate from that.
;;        so `generate` should also accept list of words
;;           each elemnt of which it `generate`s (recursivity).

;; first clause handles (3)
;; second clause handles (1)
;; third cluase handles (2)

(defun generate (phrase)
  "Generate a random sentence or phrase"
  (cond ((listp phrase)
         (mappend #'generate phrase))
        ((rewrites phrase)
         (generate (random-elt (rewrites phrase))))
        (t
         (list phrase))))

;; this function is short, but dense with information:
;; the craft of programming includes knowning what NOT to write, as well as what to write

;; this style of programming is called:
;; data-driven programming
;; because data (list of rewrites aociated with category) drives what program does next.

;; natural and esay-to-use style in Lisp, leading to concise and extensible programs.
;; because it is always possible to add a new piece of data with a new association
;; without having to modify the original program.



;; `(rewrites phrase)` is called twoice 


(defun generate (phrase)
  "Generate a random sentence or phrase"
  (let ((rewrites-phrase (rewrites phrase)))
    (cond ((listp phrase)
         (mappend #'generate phrase))
        (rewrites-phrase
         (generate (random-elt rewrites-phrase)))
        (t
         (list phrase))))

(defun generate (phrase)
  "Generate a random sentence or phrase"
  (let ((choices nil))
    (cond ((listp phrase)
         (mappend #'generate phrase))
        ((setf choices (rewrites phrase))
         (generate (random-elt choices)))
        (t (list phrase))))


;; using different data for same program

(defparameter *bigger-grammar*
  '((sentence        -> (non-phrase verb-phrase))
    (noun-phrase     -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase     -> (Verb noun-phrase PP*))
    (PP*             -> () (PP PP*))
    (Adj*            -> () (Adj Adj*))
    (PP              -> (Prep noun-phrase))
    (Prep            -> to in by with on)
    (Adj             -> big little blue green aiabatic)
    (Article         -> the a)
    (Noun            -> man ball woman table)
    (Verb            -> hit took saw liked)
    (Pronoun         -> he she it these those that))
  "A grammar for a richer subset of English.")

(setf *grammar* *bigger-grammar*)

(generate 'sentence)
(generate 'sentence)
(generate 'sentence)

;; "with he" instead of "with him"
;; sensible and silly output not distinguished


;; using same data fro different programs

(defun generat-tree (phrase)
  "Generate a rndom sentence or phrase, 
  with a complete parse tree."
  (cond ((listp phrase)
         (mapcar #'generate-tree phrase))
        ((rewrites phrase)
         (conse phrase
                (generate-tree (random-elt (rwrites phrase)))))
        (t (list phrase))))

(generate-tree 'sentence)
(generate-tree 'sentence)


(defun generat-all (phrase)
  "Generate a list of all possible expansions of this phrase."
  (cond ((null phrase) (list nil))
        ((listp phrase)
         (combine-all (generate-all (first phrase))
                      (generate-all (rest phrase))))
        ((rewrites phrase)
         (mappend #'generate-all (rewrites phrase)))
        (t (list (list phrase)))))

(defun combine-all (xlist ylist)
  "Return a list of lists formed by appending a y to an x.
  E.g., (combine-all '((a) (b)) '((1) (2)))
  -> ((A 1) (B 1) (A 2) (B 2))."
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (append x y)) xlist))
           ylist))

(defun mappend (fn the-list)
   (apply #'append (mapcar fn the-list)))

(generate-all 'article)  ;; ((the) (a))
(generate-all 'noun)     ;; ((man) (ball) (woman) (table))
(generate-all 'noun-phrase)
;; ((a man) (a ball) (a woman) (a table)
;;  (the man) (the ball) (the woman) (the table))
(length (generat-all 'sentence)) ;; 256
;; 2 articles x 4 nouns x 4 verbs x 4
;; article - noun - verb - article - noun
;; (2 x 4 x 4 x 2 x 4 = 256)

;; write another trivial grammar for another language

;; combine-all is actually cross-product
;; write cross-product and write combine-all in terms of is.
;; the moral is to make your code as general as possible
;; because you never know what you may want to do with it next!


(defun combine-each (x lst)
  (mapcar #'(lambda (y) (list x y)) lst))

(defun once-flatten (lst)
  (cond ((null lst) nil)
        ((listp (car lst))
         (append (car lst) (once-flatten (rest lst))))
        (t (cons (car lst) (once-flatten (rest lst))))))

(defun cross-product (fn xlist ylist)
  "Return the cross-product of two lists combining each element
   with each of the other."
  (mapcar #'(lambda (lst) (apply fn lst)) 
          (once-flatten 
            (mapcar #'(lambda (x) (combine-each x ylist)) 
                    xlist))))

(defun combine-all (xlist ylist)
  (cross-product xlist ylist))

(cross-product #'* '(1 2 3 4) '(10 20 30 40))
(cross-product #'+ '(1 2 3 4) '(10 20 30 40))
(cross-product #'list '(1 2 3 4) '(10 20 30 40))




