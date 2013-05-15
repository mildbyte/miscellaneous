(define (make-tree-helper)
    (define me '())
    (define root-node '())
    (define brackets? #f)
    (lambda (op . args) (case op
        ('set-me (set! me (car args)))
        ('get-root-node root-node)
        ('set-root-node (set! root-node (car args)))
        ('set-brackets (set! brackets? (car args)))
        ('search (nsearch root-node (car args)))
        ('insert 
            (let ((node (car args)))
                (node 'set-tree me)
                (set! root-node (ninsert! root-node node '()))))
        ('minimum (nminimum root-node))
        ('maximum (nmaximum root-node))
        ('set-key
            (let* ((k (car args))
                   (v (cadr args))
                   (node (me 'search k)))
                (if (null? node)
                    (me 'insert (make-node k v))
                    (node 'set-value v))))
        ('predecessor (npredecessor (car args)))
        ('successor (nsuccessor (car args)))
        ('delete (ndelete! (car args)))
        ('print (nprint root-node brackets?))
        ('pretty-print (nprettyprint root-node)))))

(define (make-tree)
    (define t (make-tree-helper))
    (t 'set-me t)
    t)

(define (make-node k v)
    (define key k)
    (define value v)
    (define left-child '())
    (define right-child '())
    (define parent '())
    (define tree '())
    (lambda (op . args) (case op
        ('get-key k)
        ('set-key (set! k (car args)))
        ('get-value v)
        ('set-value (set! v (car args)))
        ('get-right-child right-child)
        ('set-right-child (set! right-child (car args)))
        ('get-left-child left-child)
        ('set-left-child (set! left-child (car args)))
        ('get-parent parent)
        ('set-parent (set! parent (car args)))
        ('get-tree tree)
        ('set-tree (set! tree (car args))))))

(define (greater? a b) (string>? a b))

(define (nsearch node key)
    (cond
        ((null? node) '())
        ((equal? key (node 'get-key)) node)
        ((greater? key (node 'get-key)) (nsearch (node 'get-right-child) key))
        (else (nsearch (node 'get-left-child) key))))

(define (nprint node brackets?)
    (define (np node) 
        (if (not (null? node))
            (begin
                (if brackets? (print "("))
                (np (node 'get-left-child))
                (if brackets? (print "), "))
                (print (node 'get-key))
                (if brackets? (print ", ("))
                (np (node 'get-right-child))
                (if brackets? (print ")")))))
    (np node))

(define (nprettyprint node)
    (define (nspaces n) (if (> n 0) (begin (print " ") (nspaces (- n 1)))))
    (define (spaceprint level str) (nspaces level) (print str) (newline))
    (define (helper node level)
        (if (null? node) (spaceprint level ".")
            (begin
                (spaceprint level (node 'get-key))
                (helper (node 'get-left-child) (+ level 2))
                (helper (node 'get-right-child) (+ level 2)))))
    (helper node 0))

(define (ninsert! node toinsert currparent)
    (cond
        ((null? node)
            (toinsert 'set-parent currparent)
            toinsert)
        ((greater? (toinsert 'get-key) (node 'get-key)) 
            (node 'set-right-child (ninsert! (node 'get-right-child) toinsert node))
            node)
        (else 
            (node 'set-left-child (ninsert! (node 'get-left-child) toinsert node))
            node)))

(define (nminimum n) (if (null? (n 'get-left-child)) n (nminimum (n 'get-left-child))))
(define (nmaximum n) (if (null? (n 'get-right-child)) n (nmaximum (n 'get-right-child))))

(define (nseekupright n prev)
    (cond 
        ((null? n) '())
        ((eq? (n 'get-left-child) prev) n)
        (else (nseekupright (n 'get-parent) n))))

 (define (nseekupleft n prev)
    (cond 
        ((null? n) '())
        ((eq? (n 'get-right-child) prev) n)
        (else (nseekupleft (n 'get-parent) n))))       

(define (npredecessor n)
    (if (null? (n 'get-left-child))
        (nseekupleft (n 'get-parent) n) 
        (nmaximum (n 'get-left-child))))
 
(define (nsuccessor n)
    (if (null? (n 'get-right-child))
        (nseekupright (n 'get-parent) n) 
        (nminimum (n 'get-right-child))))

(define (ndelete! n)
    (let* ((parent (n 'get-parent))
           (child 
             (cond 
                 ((null? parent) '())
                 ((eq? (parent 'get-right-child) n) 'set-right-child)
                 (else 'set-left-child)))
             (new-node
                 (cond
                    ((and (null? (n 'get-left-child)) (null? (n 'get-right-child)))
                        '())
                    ((null? (n 'get-left-child))
                        (n 'get-right-child))
                    ((null? (n 'get-right-child))
                        (n 'get-left-child))
                    (else
                        (let* ((succ (nsuccessor n))
                               (newtodelete (if (null? succ) (npredecessor n) succ)))
                              (n 'set-key (newtodelete 'get-key))
                              (n 'set-value (newtodelete 'get-value))
                              (ndelete! newtodelete)
                              n)))))
          (if (not (null? child)) 
            (parent child new-node)
            (if (not (null? new-node)) ((new-node 'get-tree) 'set-root-node new-node)))
          (new-node 'set-parent parent)))
                  
(define dinotree (make-tree))
(for-each (lambda (letter) (dinotree 'insert (make-node letter '()))) (list "D" "I" "N" "O" "S" "A" "U" "R"))

(define (printdino)
    (print "The tree is now ")
    (newline)
    (dinotree 'print)
    (newline)
    (dinotree 'pretty-print)
    (newline))


(print "Tree created.\n")
(printdino)

(define dpred (dinotree 'predecessor (dinotree 'search "D")))
(print "The predecessor of D is " (dpred 'get-key) "\n")

(print "Deleting I.\n")
(dinotree 'delete (dinotree 'search "I"))
(printdino)

(define dmax (dinotree 'maximum))
(print "The maximum is " (dmax 'get-key) "\n")

(print "Inserting Z.\n")
(dinotree 'insert (make-node "Z" '()))
(printdino)

(define rsucc (dinotree 'successor (dinotree 'search "R")))
(print "The successor of R is " (rsucc 'get-key) "\n")

(print "Deleting S.\n")
(dinotree 'delete (dinotree 'search "S"))
(printdino)

(print "Inserting T.\n")
(dinotree 'insert (make-node "T" '()))
(printdino)
