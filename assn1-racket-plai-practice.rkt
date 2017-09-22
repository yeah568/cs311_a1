#lang plai

;;;; TODO: PROBLEM #1 ;;;;
;;
;; Complete the function.  This should be a simple one!

;; compute-greater-power : positive positive -> positive
;; Computes the greater of x^y and y^x (i.e., x raised to
;; the power of y and y raised to the power of x).
;;
;; Hint: look up the identifier "expt" in the helpdesk.
(define (compute-greater-power x y)
  (max (expt x y) (expt y x)))

; Trivial case
(test (compute-greater-power 1 1) 1)

; Cases where order matters.
(test (compute-greater-power 1 2) 2)
(test (compute-greater-power 2 1) 2)

; Cases where order and operation both matter.
(test (compute-greater-power 2 3) 9)
(test (compute-greater-power 3 2) 9)

; An extra case just to feel confident.
; (Note that in this case the smaller of x and y
; raised to the larger "wins".)
(test (compute-greater-power 4 5) 1024)



;;;; TODO: PROBLEM #2 ;;;;
;;
;; Complete the function.
;; You'll certainly want to look up string functions to make this work.
;; You could also convert the strings to lists and work with them in that form!

;; match-length : string string -> natural

;; Compute the number of letters exactly matching at the start of the two strings
;; (before there's a difference).
;;
;; E.g., (match-length "wesselton" "weasel-town") evaluates to 2.
(define (match-length string1 string2)
  (if (equal? (string-length string1) (string-length string2))
    (if (string=? string1 string2)
      (string-length string1)
      (match-length
        (substring string1 0 (- (string-length string1) 1))
        (substring string2 0 (- (string-length string2) 1))
      )
    )
    (match-length
      (substring string1 0 (min (string-length string1) (string-length string2)))
      (substring string2 0 (min (string-length string1) (string-length string2)))
    )
  ))

; Trivial cases.
(test (match-length "" "") 0)
(test (match-length "" "elsa") 0)
(test (match-length "elsa" "") 0)

; Simple one-character match/non-match.
(test (match-length "a" "a") 1)
(test (match-length "a" "b") 0)

; Matching prefixes.
(test (match-length "wesselton" "weasel-town") 2)

; One is a prefix of the other.
(test (match-length "weasel" "weasel-town") 6)
(test (match-length "weasel-town" "weasel") 6)

; Exactly matching longer strings.
(test (match-length "Sven" "Sven") 4)  ; they're both Sven?!

; Case matters
(test (match-length "a" "A") 0)



;;;; TODO: PROBLEM #3 ;;;;
;;
;; Complete the function.  You'll want to remind yourself of
;; some list functions for this one!


;; interleave : (listof any?) (listof any?) -> (listof any?)
;; Interleave the elements of list1 and list2.
;;
;; So, give back a list with the first element of list1, then the
;; first of list2, then the second of list1, then the second of list2,
;; and so on.
;;
;; If one list ends before the other, include all leftover elements of
;; the other list in a row.
(define (interleave list1 list2)
  (if (empty? list1)
    list2
    (if (empty? list2)
      list1
      (append (list (first list1)) (list (first list2)) (interleave (rest list1) (rest list2)))
    )
  )
)


(test (interleave empty empty) empty)
(test (interleave '(1 2 3) empty) '(1 2 3))
(test (interleave empty '(1 2 3)) '(1 2 3))
(test (interleave '(1) '(2)) '(1 2))
(test (interleave '(2) '(1)) '(2 1))
(test (interleave '(1 3) '(2)) '(1 2 3))
(test (interleave '(1 3 5 7) '(2 4 6 8 9)) '(1 2 3 4 5 6 7 8 9))
(test (interleave '(1 3 5 7 8 9) '(2 4 6)) '(1 2 3 4 5 6 7 8 9))

; Not just numbers!
(test (interleave '(z y x w) '("a" 1 "and a" 2 "and a" 3))
      '(z "a" y 1 x "and a" w 2 "and a" 3))


;;;; TODO: PROBLEM #4 ;;;;
;;
;; Complete the function.  More list practice!

;; contains-sequence : (list-of symbol?) (list-of symbol?) -> boolean?
;; Determine whether data contains the elements from sequence in order (but not necessarily right next to each other).
(define (contains-sequence data sequence)
  (if (empty? data)
    (if (not (empty? sequence))
      false
      true
    )
    (if (empty? sequence)
      true
      (if (equal? (first data) (first sequence))
        (contains-sequence (rest data) (rest sequence))
        (contains-sequence (rest data) sequence)
      )
    )
  )
)

;; True tests, trivial though complex
;; plus repeated symbols.
(test (contains-sequence empty empty) true)
(test (contains-sequence '(a) empty) true)
(test (contains-sequence '(a b) '(a)) true)
(test (contains-sequence '(a b) '(b)) true)
(test (contains-sequence '(a b c) '(a c)) true)
(test (contains-sequence '(a b c) '(a b)) true)
(test (contains-sequence '(a b c) '(a b c)) true)
(test (contains-sequence '(a b a) '(a a)) true)

;; False test, trivial though complex.
(test (contains-sequence empty '(a)) false)
(test (contains-sequence '(a) '(b)) false)
(test (contains-sequence '(a b) '(b a)) false)
(test (contains-sequence '(a b) '(a a)) false)


;;;; TODO: PROBLEM #5 ;;;;
;;
;; Thoroughly test this function.  Do NOT use check-expect.
;; Use plai's test construct instead.

(define-type Thingy
  [a-thingy (really? boolean?)]
  [other-thingy (no? boolean?) (maybe? boolean?) (sub-thingy Thingy?)])

;; all-thingied : Thingy -> boolean?
;; Determines whether all the booleans in the Thingy are true.
;;
;; Note: type-case is REALLY handy for functions over define-type types.
;; However, you can also access fields with functions like other-thingy-sub-thingy
;; which, given an other-thingy returns its sub-thingy field.
(define (all-thingied thingy)
  (type-case Thingy thingy
    [a-thingy (b) b]
    [other-thingy (n m sub-thingy) (and n m (all-thingied sub-thingy))]))

(test (all-thingied (a-thingy true)) true)
(test (all-thingied (a-thingy false)) false)
(test (all-thingied (other-thingy true true (a-thingy true))) true)
(test (all-thingied (other-thingy false false (a-thingy false))) false)
(test (all-thingied (other-thingy true true (a-thingy false))) false)
(test (all-thingied (other-thingy true false (a-thingy true))) false)
(test (all-thingied (other-thingy true true (other-thingy true true (a-thingy true)))) true)
(test (all-thingied (other-thingy true true (other-thingy true true (a-thingy false)))) false)






;;;; TODO: PROBLEM #6 ;;;;
;;
;; Time for define-type.  This is a big one.
;; find-species works well with a HtDP-like "template" approach.
;; is-extinct? should be easy using find-species.
;; common-ancestor is fairly tricky, but let the test cases guide you!
;;
;; Complete find-species, is-extinct?, and common-ancestor.


;; The "tree of life" is the biological tree of species charting
;; the evolution of one species from another.
(define-type tree-of-life
  [empty-tree]
  [species (name string?) (extinct? boolean?) (child1 tree-of-life?) (child2 tree-of-life?)])

(define human-species-ToL (species "human" false (empty-tree) (empty-tree)))
(define troll-species-ToL (species "troll" true (empty-tree) (empty-tree)))
(define three-species-ToL (species "missing-link" true human-species-ToL troll-species-ToL))

;; find-species : string tree-of-life -> (or false tree-of-life)
;; Produces the tree-of-life node representing the named species if it exists. Else, produces false.
;; (Note: we could actually make the return type more specific: it's false or species.)
;;
;; Precondition: the species with the given name appears AT MOST once (i.e., zero or one time).
(define (find-species name tree)
  ;; Use a type-case!
  (type-case tree-of-life tree
    [empty-tree () false]
    [species (n e c1 c2)
      (if (equal? name n)
        tree
        (or (find-species name c1) (find-species name c2))
      )
    ]
  )
)

(test (find-species "elsa" (empty-tree)) false)
(test (find-species "elsa" human-species-ToL) false)
(test (find-species "elsa" three-species-ToL) false)
(test (find-species "human" human-species-ToL) human-species-ToL)
(test (find-species "human" three-species-ToL) human-species-ToL)
(test (find-species "troll" three-species-ToL) troll-species-ToL)



;; is-extinct? : string tree-of-life -> boolean
;; Determines whether the given species is recorded as exctinct in the given tree.
;;
;; Precondition: the species with the given name appears exactly once in the tree.
;; Hint: use find-species!
(define (is-extinct? name tree)
  (species-extinct? (find-species name tree)))

(test (is-extinct? "troll" three-species-ToL) true)
(test (is-extinct? "human" three-species-ToL) false)

;; common-ancestor : string string tree-of-life -> (or false tree-of-life)
;; Returns the node of the closest common ancestor OR false if one or both species does not exist in the tree.
;; DOES NOT NEED TO BE EFFICIENT.
;;
;; One bonus point available after the assignment deadline
;; for posting a well-written, efficient version with comments
;; to explain how and why the design works.
;;
;; Precondition: each named species appears AT MOST once (i.e., zero or one time).
(define (common-ancestor name1 name2 tree)
  (type-case tree-of-life tree
    [empty-tree () false]
    [species (n e c1 c2)
      (if (equal? n name1)
        (if (or (find-species name2 c1) (find-species name2 c2) (equal? n name2))
          tree
          false
        )
        (if (equal? n name2)
          (if (or (find-species name1 c1) (find-species name1 c2) (equal? n name1))
            tree
            false
          )
          (if (and (find-species name1 c1) (find-species name2 c1))
            (common-ancestor name1 name2 c1)
            (if (and (find-species name1 c2) (find-species name2 c2))
              (common-ancestor name1 name2 c2)
              (if (or
                    (and (find-species name1 c1) (find-species name2 c2))
                    (and (find-species name1 c2) (find-species name2 c1))
                  )
                  tree
                  false
              )
            )
          )
        )
      )
    ]
  )
)

; Neither appears
(test (common-ancestor "elsa" "anna" (empty-tree)) false)
(test (common-ancestor "elsa" "anna" human-species-ToL) false)
(test (common-ancestor "elsa" "anna" three-species-ToL) false)

; Only one appears (both orders)
(test (common-ancestor "elsa" "human" human-species-ToL) false)
(test (common-ancestor "human" "elsa" human-species-ToL) false)
(test (common-ancestor "elsa" "human" three-species-ToL) false)
(test (common-ancestor "human" "elsa" three-species-ToL) false)

; One is THIS node, other in subtree.
(test (common-ancestor "missing-link" "human" three-species-ToL) three-species-ToL)
(test (common-ancestor "human" "missing-link" three-species-ToL) three-species-ToL)
(test (common-ancestor "missing-link" "troll" three-species-ToL) three-species-ToL)
(test (common-ancestor "troll" "missing-link" three-species-ToL) three-species-ToL)

; Both appear in different subtrees.
(test (common-ancestor "troll" "human" three-species-ToL) three-species-ToL)
(test (common-ancestor "human" "troll" three-species-ToL) three-species-ToL)


; Both appear in the same subtree.
(define subtree-appearances-right (species "goo" false
                                           (species "foo" true (empty-tree) (empty-tree))
                                           three-species-ToL))
(define subtree-appearances-left (species "goo" false
                                          three-species-ToL
                                          (species "foo" true (empty-tree) (empty-tree))))

(test (common-ancestor "troll" "human" subtree-appearances-right) three-species-ToL)
(test (common-ancestor "human" "troll" subtree-appearances-right) three-species-ToL)
(test (common-ancestor "troll" "human" subtree-appearances-left) three-species-ToL)
(test (common-ancestor "human" "troll" subtree-appearances-left) three-species-ToL)
(test (common-ancestor "missing-link" "human" subtree-appearances-right) three-species-ToL)
(test (common-ancestor "human" "missing-link" subtree-appearances-right) three-species-ToL)
(test (common-ancestor "missing-link" "human" subtree-appearances-left) three-species-ToL)
(test (common-ancestor "human" "missing-link" subtree-appearances-left) three-species-ToL)

; Both are ONE node.
(test (common-ancestor "troll" "troll" troll-species-ToL) troll-species-ToL)
(test (common-ancestor "troll" "troll" subtree-appearances-left) troll-species-ToL)






;;;; TODO: PROBLEM #7 ;;;;
;;
;; Here's a small EBNF specification for a language.  Finish the
;; abstract syntax (a define-type named OE for "order expression") for
;; the language.
;;
;; <expr> ::= (group <expr> <expr>)
;;          | (sequentially <expr> <expr>)
;;          | (together <expr> <expr>)
;;          | (join <expr> <expr>)
;;          | (arrive <expr>)
;;          | (give <expr> name <id> in <expr>)
;;          | <string>
;;          | <id>
;;
;; Notes: Use "name" as the name of the (give <expr> name <id> in
;; <expr>) variant.  Assume that a <string> is just a Racket string,
;; and an <id> is just a Racket symbol.  We suggest you call the
;; <string> variant string-literal and the <id> variant id-ref.

(define-type OE
  [group (expr1 OE?) (expr2 OE?)]
  [sequentially (expr1 OE?) (expr2 OE?)]
  [together (expr1 OE?) (expr2 OE?)]
  [join (expr1 OE?) (expr2 OE?)]
  [arrive (expr OE?)]
  [name (expr1 OE?) (id id-ref?) (expr2 OE?)]
  [id-ref (expr symbol?)]
)
