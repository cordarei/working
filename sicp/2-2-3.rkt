#lang racket

#|
     *Figure 2.8:* A solution to the eight-queens puzzle.

          +---+---+---+---+---+---+---+---+
          |   |   |   |   |   | Q |   |   |
          +---+---+---+---+---+---+---+---+
          |   |   | Q |   |   |   |   |   |
          +---+---+---+---+---+---+---+---+
          | Q |   |   |   |   |   |   |   |
          +---+---+---+---+---+---+---+---+
          |   |   |   |   |   |   | Q |   |
          +---+---+---+---+---+---+---+---+
          |   |   |   |   | Q |   |   |   |
          +---+---+---+---+---+---+---+---+
          |   |   |   |   |   |   |   | Q |
          +---+---+---+---+---+---+---+---+
          |   | Q |   |   |   |   |   |   |
          +---+---+---+---+---+---+---+---+
          |   |   |   | Q |   |   |   |   |
          +---+---+---+---+---+---+---+---+

     *Exercise 2.42:* The "eight-queens puzzle" asks how to place eight
     queens on a chessboard so that no queen is in check from any other
     (i.e., no two queens are in the same row, column, or diagonal).
     One possible solution is shown in *Note Figure 2-8::.  One way to
     solve the puzzle is to work across the board, placing a queen in
     each column.  Once we have placed k - 1 queens, we must place the
     kth queen in a position where it does not check any of the queens
     already on the board.  We can formulate this approach recursively:
     Assume that we have already generated the sequence of all possible
     ways to place k - 1 queens in the first k - 1 columns of the
     board.  For each of these ways, generate an extended set of
     positions by placing a queen in each row of the kth column.  Now
     filter these, keeping only the positions for which the queen in
     the kth column is safe with respect to the other queens.  This
     produces the sequence of all ways to place k queens in the first k
     columns.  By continuing this process, we will produce not only one
     solution, but all solutions to the puzzle.

     We implement this solution as a procedure `queens', which returns a
     sequence of all solutions to the problem of placing n queens on an
     n*n chessboard.  `Queens' has an internal procedure `queen-cols'
     that returns the sequence of all ways to place queens in the first
     k columns of the board.
|#

          (define (queens board-size)
            (define (queen-cols k)
              (if (= k 0)
                  (list empty-board)
                  (filter
                   (lambda (positions) (safe-first? k positions))
                   (append-map
                    (lambda (rest-of-queens)
                      (map (lambda (new-row)
                             (adjoin-position new-row k rest-of-queens))
                           (enumerate-interval 1 board-size)))
                    (queen-cols (- k 1))))))
            (queen-cols board-size))

#|
     In this procedure `rest-of-queens' is a way to place k - 1 queens
     in the first k - 1 columns, and `new-row' is a proposed row in
     which to place the queen for the kth column.  Complete the program
     by implementing the representation for sets of board positions,
     including the procedure `adjoin-position', which adjoins a new
     row-column position to a set of positions, and `empty-board',
     which represents an empty set of positions.  You must also write
     the procedure `safe?', which determines for a set of positions,
     whether the queen in the kth column is safe with respect to the
     others.  (Note that we need only check whether the new queen is
     safe--the other queens are already guaranteed safe with respect to
     each other.)
|#

(define (enumerate-interval a b)
  (if (= a b)
      (list b)
      (cons a (enumerate-interval (+ a 1) b))))

(define (adjoin-position r c board)
  (cons r board))

(define empty-board '())

(define (safe? k board)
  (define b (reverse board))
  (define kth-row (list-ref b (- k 1)))
  (for/and ([r b]
            [c (in-range 1 (+ 1 (length board)))])
    (cond
      [(= k c) #t]
      [(= r kth-row) #f]
      [(= (abs (- k c)) (abs (- kth-row r))) #f]
      [else #t])))

(define (safe-first? _ board)
  (let ((k (length board))
        (kth (car board)))
    (for/and ([r (cdr board)]
              [c (in-range (- k 1) 0 -1)])
      (cond
        [(= k c) #t]
        [(= r kth) #f]
        [(= (abs (- k c)) (abs (- kth r))) #f]
        [else #t]))))
