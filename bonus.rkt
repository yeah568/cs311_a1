#lang plai

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

;; get-ancestors : (string tree-of-life) -> (listof tree-of-life?)
;; Returns a list of the parents of a species, starting from the highest in the tree.
(define (get-ancestors name tree)
	(type-case tree-of-life tree
		[empty-tree () empty]
		[species (n e c1 c2)
			(if (equal? n name)
				tree
				(local [
						(define sublist1 (get-ancestors name c1))
						(define sublist2 (get-ancestors name c2))
					]
					(if (not (empty? sublist1))
						(cons tree sublist1)
						(if (not (empty? sublist2))
							(cons tree sublist2)
							empty
						)
					)
				)
			)
		]
	)
)

;; deepest-common-ancestor : (listof tree-of-life) (listof tree-of-life) -> (or false tree-of-life)
(define (deepest-common-ancestor a1 a2)
	(local [(define common-ancestors (take-common-prefix (list a1) (list a2)))]
		(if (empty? common-ancestors)
			false
			(last common-ancestors)
		)
	)
)

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
			(deepest-common-ancestor (get-ancestors name1 tree) (get-ancestors name2 tree))
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






