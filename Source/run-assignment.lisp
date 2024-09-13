;;;;;;;;;;;;;;;;;;;;;;;
;;Libraries
;;;;;;;;;;;;;;;;;;;;;;;
(load "simulator.lisp")
(load "messages.lisp")
(load "new-symbol.lisp")
(load "search-assignment.lisp")
(load "simple-pqueue.lisp")
;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions & Macros
;;;;;;;;;;;;;;;;;;;;;;;

(defun sensor-value(percept direction)
  "Checks given association percept list for a given sensor direction"
  (case direction
    (:forward-sensor (cadr (assoc :foward-sensor percept)))
    (:right (cadr (assoc :right-bump percept)))
    (:left (cadr (assoc :left-bumo percept)))
    (:forward (cadr (assoc :front-bump percept)))
    (:rear (cadr (assoc :rear-bump percept)))
    )
  )

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

;;;;;;;;;;;;;;;;;;;;;;;
(defmethod gen-rand-and-list((self sim:simulator) goal number)
  "generates list of object locations while adding them to the world. Checks to see if its a goal"
  (with-slots (sim:world) self
  (let((obs-list nil) (loc nil))
    (dotimes (i number)
      (setq loc (sim:random-empty-location self))
      (if (not(eql goal loc))
	  (push loc obs-list)
	  (sim:add-object sim:world (make-instance 'sim:object :location loc))))
    obs-list)))
;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;
;;Exported Functions
;;;;;;;;;;;;;;;;;;;;;;;

;;exported test function
(defun test()
  "Testing robots function- debug purposes"
  (setq sim (sim:create-simulator :size '(25 25)))
  
  (setq obs-list (gen-rand-and-list sim '(3 10) 25))
  
  
  (setq robo (make-instance 'uniform-agent
			    :blocks obs-list :ti t 
			    :location '(1 1) :robo-loc '(1 1)
			    :orientation :North :goal '(3 10)))
  (sim:add-robot sim :robot robo :random-location nil :random-orientation nil)  
  (build robo)
  (format t "Nodes created for agent: ~s ~%" (nodes-created robo))
  (sim:run sim :for 30 :sketch-each t)
  )

;; run-robots function
;; takes no input and runs the simulation full of robots
;; - all the different robots
;; access the print function from the simulator
(defun simulate()
  "Creates simulators and loads agents"
  (let*(
	;; no obstacle simulator
	(sim-no-obs  (sim:create-simulator :size '(25 25)))
	;; random obstacle simulator
	(sim-rand-blocks (sim:create-simulator :size '(25 25) :num-obstacles 10))
	;; predefined obstacle simulator
	(obs-list '( (3 2) (4 2) (7 2) (8 2) (9 2) (10 2) (2 4) (2 5) (2 6) (2 7) (2 8) (2 9) (2 10)
		   (3 3) (23 23) (22 23) (21 23) (20 23) (19 23) (18 23) (17 23) (16 23) (15 23)
		    (23 22) (23 21) (23 20) (23 19) (23 18) (23 17) (23 16) (23 15)))
	;; simulator with defined obstacles
	(sim-pre-block (sim:create-simulator :size '(25 25) :obstacle-locations obs-list))
	(run-robots sim-no-obs)
	;(run-robots sim-rand-blocks)
	(run-robots sim-pre-block)
  )))

(defmacro run-robots(sim)
  (run-reflex sim)
  (run-hillclimb sim)
  (run-model-reflex sim))

;; run-other function
;; takes no input
;; runs the other A* domain besides robot world
(defun run-other()
  (simulate-other)
  )

(export 'run-robots)
(export 'run-other)
(export 'test)
;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;
;; Support Runner Functions
;;;;;;;;;;;;;;;;;;;;;;;
(defun run-hillclimb(sim)
  "Runs the hill-climb agent based on a given simulator"
  (setq robo (make-instance 'hcl-agent 
			    :loc '(1 1 ) :goal '(3 4) :location '(1 1) :orientation :North))
  (sim:add-robot sim :robot robo :random-location nil :random-orientation nil)  
  (time
   (sim:run sim :for 10 :sketch-each t)))

(defun run-reflex(sim)
  "Runs a timed reflex agent based on given simulator input"
  (setq robo (make-instance 'reflex-agent
			    :location '(1 1) :orientation :North))
  (sim:add-robot sim :robot robo :random-location nil :random-orientation nil)  
  (time(sim:run sim :for 10 :sketch-each t)))

(defun run-model-reflex(sim)
  "Runs a timed model based reflex agent based on a given simulator as input"
  (setq robo (make-instance 'model-agent
			    :location '(1 1) :orientation :North))
  (sim:add-robot sim :robot robo :random-location nil :random-orientation nil)  
  (time(sim:run sim :for 10 :sketch-each t)))

(defun run-uniform-cost (sim)
"Runs a timed uniform cost agent  based on a given simulator as input"
  (setq robo (make-instance 'uniform-agent
			    :location '(1 1) :orientation :North))
  (sim:add-robot sim :robot robo :random-location nil :random-orientation nil)  
  (time(sim:run sim :for 10 :sketch-each t))  
  )

(defun run-a* (sim)
"Runs a timed a*-agent cost agent  based on a given simulator as input"
  (setq robo (make-instance 'a*-agent
			    :location '(1 1) :orientation :North))
  (sim:add-robot sim :robot robo :random-location nil :random-orientation nil)  
  (time(sim:run sim :for 10 :sketch-each t))  
  )


;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;
;;Reflex Agent
;;;;;;;;;;;;;;;;;;;;;;;

(defclass reflex-agent (sim:robot)
  ((name :accessor name :initform (sym:new-symbol 'reflex))))

(defmethod sim:agent-program ((self reflex-agent) percept)
  
  (with-slots (name) self    
    (cond
      ( (not(sensor-value percept :forward)) :forward)
      ( (sensor-value percept :forward) :right)
      ( (in-corner percept) :turn-right)
      )
    ))

(defun in-corner (percept)
  "checks to see if the robot is in a corner based on percepts"
  (or (and (sensor-value percept :forward-sensor)
	   (or (sensor-value percept :left) (sensor-value percept :right)))
      (and (sensor-value percept :back)
	   (or (sensor-value percept :left) (sensor-value percept :right)))))
;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;
;;Model based  reflex agent
;;;;;;;;;;;;;;;;;;;;;;;

(defclass model-agent (sim:robot)
  ((name :accessor name :initform (sym:new-symbol 'model-based))
   (prev-sense :accessor prev-sense :initform 0 :initarg :prev-sense)))

(defmethod sim:agent-program ((self model-agent) percept)
  (with-slots (name  prev-sense) self
    (cond
      ((not(sensor-value percept :forward-sensor))
       (setq prev-sense 0)
       :forward)
      ((and (sensor-value percept :forward-sensor) (sensor-value percept :right-bump))
       (setq prev-sense 1)
       :left)
      ((and (not (sensor-value percept :right))(eq prev-sense 0))
       :right)
      ((eq prev-sense 1)
       :left)
      )))    

;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;
;;Hill-Climb Agent 
;;;;;;;;;;;;;;;;;;;;;;;
(defclass hcl-agent (sim:robot)
  ((loc :accessor loc :initform nil :initarg :loc)
   (goal :accessor goal :initform '( 3 4) :initarg :goal)
   (name :accessor name :initform (sym:new-symbol 'hcl-agent))))

(defmethod sim:agent-program ((self hcl-agent) percept)
  (with-slots (name sim:location goal) self
    (let (
	  (chv_dir '(100 nil))
	  (arr (heuristic sim:location goal percept :forward :backward :left :right)))
      (loop for n from 0 to 3
	    do (if (<= (car (aref arr n)) (car chv_dir))
		   (setq chv_dir  (aref arr n))))
      (format t "Heuristic array: ~s chv_dir:~s ~%" arr chv_dir)
      (cadr chv_dir)
      )))

(defun heuristic (current goal percept &rest perceptv)
  "takes the current location of the robot and the percept values and builds an array of values and direction"
  (setf rtr (make-array '(4)))
  (let ((iteration 0))
    (loop for n in perceptv do
      (let ((dX 0)(dY 0)(cmb 0))
	(cond
	  ((equal n :forward)(setq current (list (car current) (+ (cadr current ) 1))))
	  ((equal n :backward)(setq current (list (car current) (- (cadr current ) 1))))
	  ((equal n :right)(setq current (list (+ (car current) 1) (cadr current ) )))
	  ((equal n :left)(setq current (list (- (car current)1) (cadr current )))))
	(setq dX (abs (- (car current) (car goal))))
	(setq dY (abs (- (cadr current) (cadr goal))))
	(setq cmb (+ dX dY))
	(if (sensor-value percept n)
	    (setq cmb 100))  
	(setf (aref rtr iteration) (list cmb n))
	(setq iteration (+ iteration 1))
	))
    )
  rtr
  )
;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;
;;Uniform Search Agent 
;;;;;;;;;;;;;;;;;;;;;;;
(defclass uniform-agent (sim:robot)
  ((blocks :accessor blocks :initform nil :initarg :blocks)
   (open-list :accessor open-list :initform (make-instance `priority-queue :compare-function #'cmp-nodes))
   (visited-list :accessor visited-list :initform '())
   (command-list :accessor command-list :initform '())
   (bounds :accessor bounds :initform '(25 25) :initarg :bounds)
   (goal-found :accessor goal-found :initform nil)
   (ti :accessor ti :initform t :initarg :ti)
   (goal :accessor goal :initform '(3 4) :initarg :goal)
   (robo-loc :accessor robo-loc :initform '(1 1) :initarg :robo-loc)
   (nodes-created :accessor nodes-created :initform 0)
   (name :accessor name :initform (sym:new-symbol 'uniform-agent))))

(defmethod sim:agent-program ((self uniform-agent) percept)
  (with-slots (command-list) self
    (cond
      ((not command-list)
       :nop)
      (t (command(pop command-list)))
      )))

(defmethod build((self uniform-agent))
  "Building first node and starting the frontier function"
  (let ((start-node nil))
    (with-slots (open-list robo-loc visited-list ti) self
      (setq start-node (make-instance 'a-node :parent nil :coord robo-loc :command :nop))
      (setf (slot-value start-node 'cost) (node-score self start-node))
      (push robo-loc visited-list)
      (enqueue open-list start-node)
      (cost-timer ti (frontier self)))))

(defmethod frontier((self uniform-agent))
  (with-slots (open-list command-list goal-found) self
    (let(
	 (node (dequeue open-list))
	 (new-loc '())
	 (pass 0)
	 (curr-score nil))
      (loop while (and node (not goal-found)) do
	(if (> pass 0)
	    (setq node (dequeue open-list)))
	(cond ((goal-check self node)
	       (progn
		 (setq goal-found T )) )
	      (t
	       (setq curr-score (cost node))
	       ;;Left Node
	       (setq new-loc (NewLoc '(-1 0) node))
	       (generation self new-loc :left curr-score node)
	       ;;Right Node
	       (setq new-loc (NewLoc '(1 0) node))
	       (generation self new-loc :right curr-score node)
	       ;;Forward Node
	       (setq new-loc (NewLoc '(0 1) node))
	       (generation self new-loc :forward curr-score node)
	       ;;Backward Node
	       (setq new-loc (NewLoc '(0 -1) node))
	       (generation self new-loc :backward curr-score node)
	       (setq pass (+ pass 1))
	       )
	      )
	(if (and node (goal-found self))
	    (progn
	      (traceback self node)
	      (setq command-list (reverse command-list))
	      )))))
  )

(defmethod generation((self uniform-agent) new-loc command curr-score Parentnode)
  "generates new node for the queue using the node supplied as a parent"
  (with-slots (open-list visited-list goal-found nodes-created) self
    (let ((node nil))
      (setq node (make-instance 'a-node :parent Parentnode :coord new-loc :command command))
      (setf (slot-value node 'g) (+ (slot-value Parentnode 'g) 1))
      (setf (slot-value node 'cost) (node-score self node))
      
      (if (and (not(is-visited self new-loc )) (is-obstacle self new-loc) (in-bounds self new-loc)
	       (not(goal-found self)) ;(<= (slot-value node 'cost) curr-score)
	       )
	  (progn
	    (incf nodes-created)
	    (enqueue open-list node)
	    (push new-loc visited-list)
	    ))
      ))
  )

(defmethod traceback ((self uniform-agent) node)
  "traces back to first node based off of a given node"
  (with-slots (command-list) self
    (cond
      ((not (eql (parent node) nil))
       (traceback self (parent node))
       (push node command-list)
       )
      )
    ))

(defmethod in-bounds((self uniform-agent) loc)
  "checks if location is in bounds "
  (with-slots (bounds) self
    (cond( (and (> (car loc) 0) (> (cadr loc) 0) (< (car loc) (car bounds)) (< (cadr loc) (cadr bounds) )) t)
	 (t nil))
    )
  )

(defmethod node-score((self uniform-agent) node)
  "Assigns score based on g- the parents value- and Manhattan distance heuristic"
  1 )

(defmethod is-visited ((self uniform-agent) node)
  "Checks to see if location is in the visited list"
  (with-slots (visited-list) self
    (cond ((member node visited-list :test 'equal ) t)
	  (t nil))))
;;
;;
(defmethod is-obstacle ((self uniform-agent) loc)
  "checks to see if location is in obstacle list"
  (with-slots (blocks) self
    (cond (
	   (member loc blocks :test 'equal) nil)
	  (t t))))

(defmethod goal-check((self uniform-agent) node)
  "checks if the nodes location is the goal"
  (with-slots (goal) self
    (if (equal (slot-value node 'coord) goal) t nil))
  )

;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;
;; A* Search Agent
;;;;;;;;;;;;;;;;;;;;;;;
(defclass a*-search (sim:robot)
  ((blocks :accessor blocks :initform nil :initarg :blocks)
   (visited-list :accessor visited-list :initform '())
   (goal :accessor goal :initform '( 3 4) :initarg :goal)
   (open-list :accessor open-list :initform (make-instance `priority-queue :compare-function #'cmp-nodes))
   (command-list :accessor command-list :initform '())
   (bounds :accessor bounds :initform '(25 25) :initarg :bounds )
   (goal-found :accessor goal-found :initform nil)
   (ti :accessor ti :initform t :initarg :ti)
   (nodes-created :accessor nodes-created :initform 0)
   (robo-loc :accessor robo-loc :initform '(1 1) :initarg :robo-loc)
   (name :accessor name :initform (sym:new-symbol 'a*-search))))

(defmethod sim:agent-program ((self a*-search) percept)
  (with-slots (command-list) self
    (cond
      ((eql (length command-list) 0)
       :nop)
      (t (command (pop command-list)))))
  )

(defmethod build ((self a*-search))
  (with-slots (open-list robo-loc visited-list ti) self
    ;;start state
    ;; looop to pop
    ;; not a goal -> compute cost
    ;; find a goal done
    (let ((start-node nil))
      (setq start-node (make-instance `a-node :parent nil :coord robo-loc :command :nop))
      (setf (slot-value start-node 'cost) (node-score self start-node))
      (push robo-loc visited-list)
      (enqueue open-list start-node)
      (cost-timer ti (frontier-take-two self)))
    )
  )
(defmethod printFrontier((self a*-search))
  (with-slots (open-list) self
    (print-queue open-list)
    )
  )
;; Builds the frontier from the queue with recursion
(defmethod frontier-take-two((self a*-search))
  (with-slots (open-list command-list goal-found) self
    (let(
	 (node (dequeue open-list))
	 (new-loc '())
	 (pass 0)
	 (curr-score nil))
      (loop while (and node (not goal-found)) do	
	(if (> pass 0)
	    (setq node (dequeue open-list)))
	(cond ((and node (goal-check self node))
	       (progn
		 (setq goal-found T )) )
	      (t
	       (setq curr-score (cost node))
	       ;;Left Node
	       (setq new-loc (NewLoc '(-1 0) node))
	       (generation self new-loc :left curr-score node)
	       ;;Right Node
	       (setq new-loc (NewLoc '(1 0) node))
	       (generation self new-loc :right curr-score node)
	       ;;Forward Node
	       (setq new-loc (NewLoc '(0 1) node))
	       (generation self new-loc :forward curr-score node)
	       ;;Backward Node
	       (setq new-loc (NewLoc '(0 -1) node))
	       (generation self new-loc :backward curr-score node)
	       (setq pass (+ pass 1))
	       
	       )))
	(if (and node (goal-found self))
	    (progn
	      (traceback self node)
	      (setq command-list (reverse command-list))
	      )))))

(defmethod generation((self a*-search) new-loc command curr-score Parentnode)
  (with-slots (open-list visited-list goal-found nodes-created) self
    (let ((node nil))
      (setq node (make-instance 'a-node :parent Parentnode :coord new-loc :command command))
      (setf (slot-value node 'g) (+ (slot-value Parentnode 'g) 1))
      (setf (slot-value node 'cost) (node-score self node))
      (if (and (not(is-visited self new-loc )) (is-obstacle self new-loc) (in-bounds self new-loc)
	       (not(goal-found self)) ;(<= (slot-value node 'cost) curr-score)
	       )
	  (progn
	    (incf nodes-created)
	    (enqueue open-list node)
	    (push new-loc visited-list))))))

(defmethod traceback ((self a*-search) node)
  "traces back to first node based off of a given node"
  (with-slots (command-list) self
    (cond
      ((not (eql (parent node) nil))
       (traceback self (parent node))
       (push node command-list)
       )
      )
    ))
;; '(0 1)
(defun NewLoc (addLoc parentNode)
  "adds the location to ParentLocation to create a new location"
  (let ((dX 0) (dY 0))
    (setq dX (+ (car addLoc)(car (slot-value Parentnode 'coord))))
    (setq dY (+ (cadr addLoc)(cadr (slot-value Parentnode 'coord))))
    (list dX dY)
    ))

(defmethod in-bounds((self a*-search) loc)
  "checks if location is in bounds "
  (with-slots (bounds) self
    (cond( (and (> (car loc) 0) (> (cadr loc) 0) (< (car loc) (car bounds)) (< (cadr loc) (cadr bounds) )) t)
	 (t nil))
    )
  )

(defmethod node-score((self a*-search) node)
  "Assigns score based on g- the parents value- and Manhattan distance heuristic"
  (+ 
   (slot-value node 'g)
   (node-heuristic self (slot-value node 'coord))))

(defmethod node-heuristic((self a*-search) loc)
  "manhatten distance heurisitc functions"
  (with-slots (goal) self
    (let ((dX 0)(dY 0))
      (setq dX (abs (- (car loc) (car goal))))
      (setq dY (abs (- (cadr loc)(cadr goal))))
      (+ dX dY))))

(defmethod is-visited ((self a*-search) node)
  "Checks to see if location is in the visited list"
  (with-slots (visited-list) self
    (cond ((member node visited-list :test 'equal) t)
	  (t nil))))
;;
;;
(defmethod is-obstacle ((self a*-search) loc)
  "checks to see if location is in obstacle list"
  (with-slots (blocks) self
    (cond (
	   (member loc blocks :test 'equal) nil)
	  (t t))))

(defmethod goal-check((self a*-search) node)
  "checks if the nodes location is the goal"
  (with-slots (goal) self
    (if (equal (slot-value node 'coord) goal) t nil))
  )
;;;;;;;;;;;;;;;;;;;;;;;

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

