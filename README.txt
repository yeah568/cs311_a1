CPSC 311 Assignment 1

James Liu, q6x8, 28629111
I have read and complied with the collaboration policy.

1. What is it about using find-species in the implementation of common-ancestor that can cause common-ancestor to become inefficient?

In the implementation with find-species, the basic approach requires calling find-species at every level, which means that you need to recompute the tree traversal many times. 

2. What do you most want to get out of this course?
A deeper understanding of what makes for useful and practical features in a language - I constantly see debates about one language being "better" than another, and it'd be nice to be able to put in some substantial thoughts into the merits of a language over another. Additioanlly, I hope to learn about how to design a language to be useful to others, in developing a DSL for a program, for example.

3. What's something cool, interesting, frustrating, or tangentially relevant that this assignment made you think about?

As someone who has spent many years since CPSC 110 programming in languages like JavaScript, Python, and C#, I'm reminded of the fact that any problem that can be done iteratively can also be done recursively. My natural thought process is to think of things in an iterative manner - for each character in a string, for each element in a list, etc. However, as Racket is a functional language, you generally use recursive solutions, forcing me to change my way of thinking from iterative to recursive solutions.


Bonus question completed in bonus.rkt.