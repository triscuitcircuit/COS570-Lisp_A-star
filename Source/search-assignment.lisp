;;;;;;;;;;;;;;;;;;;;;;;
;;Libraries
;;;;;;;;;;;;;;;;;;;;;;;
(load "simple-pqueue.lisp")
(load "messages.lisp")
;;;;;;;;;;;;;;;;;;;;;;;

;; Traveling Salesman Domain
;; Inspired by
;; Paradigms of Artificial Intelligence Programming: Case Studies in Common Lisp by Peter Norvig (1992)

;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cost-timer( ti  &body body)
  "Checks timed variable to enable or disable timing for input function code which is (body)"
  ;; (cost-timer t (function)) to run a function with a timer
  ;; (cost-timer nil (function)) to run a function without a timer
  ;; example: (cost-timer t (frontier self))
  `(cond
     (ti
      (time(progn ,@body)))
     (t(progn ,@body)))
  )
;;;;;;;;;;;;;;;;;;;;;;;

;; Function from Paip
(defun distance (point1 point2)
  "The Euclidean distance between two points.
  The points are coordinates in n-dimensional space."
  (sqrt (reduce #'+ (mapcar #'(lambda (a b) (expt (- a b) 2))
                            point1 point2))))


;;;;;;;;;;;;;;;;;;;;;;;
;;Exported Functions
;;;;;;;;;;;;;;;;;;;;;;;
(export 'simulate-other)
(export 'simulate-test)
(defun simulate-other()
  )
(defun simulate-test()
  )
;;;;;;;;;;;;;;;;;;;;;;;


(defclass a*-domain ()
  ((blocks :accessor blocks :initform nil :initarg :blocks)
   (visited-list :accessor visited-list :initform '())
   (goal :accessor goal :initform '( 3 4) :initarg :goal)
   (open-list :accessor open-list :initform (make-instance `priority-queue :compare-function #'cmp-nodes))
   (command-list :accessor command-list :initform '())
   (bounds :accessor bounds :initform '(25 25) :initarg :bounds )
   (goal-found :accessor goal-found :initform nil)
   (ti :accessor ti :initform t :initarg :ti)
   (robo-loc :accessor robo-loc :initform '(1 1) :initarg :robo-loc)
   (name :accessor name :initform (sym:new-symbol 'a*-search))))


;;;;;;;;;;;;;;;;;;;;;;;
;; A* Helper Classes 
;;;;;;;;;;;;;;;;;;;;;;;
(defclass a-node ()
  (
   (command :accessor command :initform nil :initarg :command)
   (coord :accessor coord :initform '(1 1) :initarg :coord)
   (parent :accessor parent :initform nil :initarg :parent)
   (cost :accessor cost :initform 100 :initarg :cost)
   (g :accessor g :initform 0 )
   ))
(defmethod cmp-nodes ((node1 a-node)(node2 a-node))
  (< (cost node1) (cost node2)))
(defmethod cmp-nodes-same ((node1 a-node)(node2 a-node))
  (equal  (cost node1)(cost node2)))
(defmethod print-node ((self a-node))
  (with-slots (parent coord cost) self
    (format t "parent:(~s) coord:(~s) cost:(~s) ~%" parent coord cost)
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;
