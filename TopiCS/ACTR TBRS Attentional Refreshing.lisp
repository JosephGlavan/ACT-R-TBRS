
;;;; AUTHOR: Joseph J Glavan ; j.glavan4@gmail.com

;;;; ACT-R6 model of Barrouillet et al (2007) experiment 3
;;;; Subjects had to make parity or location judgments while
;;;;    holding consonants in working memory. Implements the
;;;;    Time-based Resource-sharing model of WM.


;; Stuff to include at the top
(defun mp-time-ms () (* 1000 (mp-time)))
(defun ms->seconds (foo) (* 0.001 foo))
(defvar *INITIAL-STATE*)
(load "bl-inhibition_tweaked.lisp") ; tweaked to work on the appropriate timescale
(with-open-file (myfile "saved_seed.lsp") (setf *INITIAL-STATE* (read myfile)))
;; Can't set ACT-R's random seed until after the model has been loaded so do it from within betal-wrapper
(setf *RANDOM-STATE* *INITIAL-STATE*)


;;;;;;;; GLOBAL PARAMETERS ;;;;;;;;

(defvar *VERBAL-RECALL*) ; whether the model should verbally respond or use the keyboard for recall phase

(setf *VERBAL-RECALL* t)

;; Logging variables
(defvar *LOG-FILE-NAME*)
(defvar *TLOG-FILE-NAME*)
(defvar *RT*) ; the response time
(defvar *RESPONSE*) ; the key pressed
(defvar *LOCATION*) ; the location of the number to be processed i.e. TOP / BOTTOM
(defvar *DISTRACTOR*) ; the number to be processed
(defvar *TARGET*) ; the letter to be remembered
(defvar *TRIAL*) ; index of the current series
(defvar *TAR-LIST*) ; list of targets in the current series
(defvar *SERIES*) ; number of trials in the given set
(defvar *SET*) ; the index of series repetitions
(defvar *TRAINING*) ; bool (string) flag: 0 real, 1 training phase
(defvar *MODEL-RUN*) ; index of repetitions of the current parameter config during current simulation
(defvar *CONDITION*) ; experimental condition, either PARITY or SPATIAL
(defvar *NUM-STIM*) ; the number of processing stimuli between target letters: [4, 6, 8]
(defvar *PARAM-CONFIG*) ; index of unique parameter configurations


;; Other globals
(defvar *SCREEN-X*)
(defvar *SCREEN-Y*)
(defvar *START-RT*)
(defvar *ASTERISK-DISPLAY-TIME*)
(defvar *TARGET-DELAY*)
(defvar *TARGET-DISPLAY-TIME*)
(defvar *RECALLED*)
(defvar *VIS-LOC*)
(defvar *LIST-INDEX*)
(defvar *CORRECT-COUNT*)
(defvar *RT-TRACKER*)
(defvar *ACC-TRACKER*)
(defvar *PENALTY*)
(defvar *RESPONSE-BASED-REWARD*)
(defvar *NULL-RESPONSE*)
(defvar *GLOBAL-REWARD*)
(defvar *TPT-TRACKER*)
(defvar *NULL-TRACKER*)
(defvar *LTM-BL*)

(setf *SCREEN-X* 1000)
(setf *SCREEN-Y* 1000)
(setf *ASTERISK-DISPLAY-TIME* 750)
(setf *TARGET-DELAY* 500)
(setf *TARGET-DISPLAY-TIME* 1500)
(setf *LIST-INDEX* 0)
(setf *CORRECT-COUNT* 0)
(setf *RT-TRACKER* '())
(setf *ACC-TRACKER* '())
(setf *PENALTY* -1.0)
(setf *RESPONSE-BASED-REWARD* t)
(setf *NULL-RESPONSE* nil)
(setf *GLOBAL-REWARD* 0.0)
(setf *TPT-TRACKER* '())
(setf *LTM-BL* '(1250 -1000000))
(setf *NULL-TRACKER* 0)


;;;; HELPER FUNCTIONS

(defun round-to (number precision)
  (let ((div (expt 10 precision)))
    (float (/ (funcall #'round (* number div)) div))))

(defun myseq (start &key (b nil) (lo nil) (m nil) (prec 5))
  (assert (or (and b lo) (and b m) (and lo m)))
  (if (not b) (loop for n from start to m by (/ (- m start) (- lo 1)) collect (round-to n prec))
      (if (not lo) (loop for n from start to m by b collect (round-to n prec))
                       (loop for n from 0 to (- lo 1) collect (round-to (+ start (* n b)) prec)))))

(defun timestamp ()
  (multiple-value-bind (sec min hr day mon yr dow dst-p tz) (get-decoded-time) 
    (format nil "~2,'0d_~2,'0d_~d_~2,'0d_~2,'0d_~2,'0d" mon day yr hr min sec)))

(defun nshuffle (sequence)
  (loop for i from (length sequence) downto 2
        do (rotatef (elt sequence (random i))
                    (elt sequence (1- i))))
  sequence)

(defun mypop (myplace mylist)
  (if (> myplace 0)
      (pop (nthcdr myplace mylist))
      (pop mylist)))

(defun repeat (x y)
  (if (zerop y) 
      nil 
      (cons x (repeat x (- y 1)))))

(defun loc (location)
  (coerce
   (if (equal location 'bottom)
       (list (round (/ *SCREEN-X* 2)) (round (* .25 *SCREEN-Y*)))
       (list (round (/ *SCREEN-X* 2)) (round (* .75 *SCREEN-Y*)))) 'simple-vector))

(defun distractor-pt (n)
  (cond ((eq n 1) 4.267)
        ((eq n 2) 2.133)
        ((eq n 3) 1.422)
        ((eq n 4) 1.067)
        ((eq n 6) .711)
        ((eq n 8) .533)
        ((eq n 9) .474)
        ((eq n 10) .427)
        ((eq n 11) .388)
        ((eq n 12) .355)))

(defun distractor-dt (n)
  (cond ((eq n 1) 2.133)
        ((eq n 2) 1.067)
        ((eq n 3) .711)
        ((eq n 4) .533)
        ((eq n 6) .356)
        ((eq n 8) .267)
        ((eq n 9) .237)
        ((eq n 10) .213)
        ((eq n 11) .194)
        ((eq n 12) .178)))


;;;; Check for resp == nil
(defun correct-p (c n loc resp)
  (if (equal c "PARITY") (if (equal 0 (mod (parse-integer n) 2)) (if (equal resp "f") nil (if (equal resp "j") t))
                          (if (equal resp "j") nil (if (equal resp "f") t)))
      (if (equal c "SPATIAL") (if (equal loc "BOTTOM") (if (equal resp "f") t (if (equal resp "j") nil))
                               (if (equal loc "TOP") (if (equal resp "j") t (if (equal resp "f") nil))))
      "ERROR")))

(defun std-dev (samples)
  (let* ((n (length samples))
	 (mean (/ (reduce #'+ samples) n))
	 (tmp (mapcar (lambda (x) (expt (- x mean) 2)) samples)))
    (sqrt (/ (reduce #'+ tmp) n))))

(defun recursive-sum (L) (if L (+ (car L) (recursive-sum (cdr L))) 0))

(defun mean (L) (if L (float (/ (recursive-sum L) (list-length L))) 0)) ; Arguably should return NIL instead of 0 for an empty list but this will be cleaner for my data

(defun elem-mult (x y) 
  (let ((mx (copy-list x)) (my (copy-list y)) (result '()))
    (while mx (push (* 1.0 (pop mx) (pop my)) result))
    (reverse result)))

(defun reg-coef (x y) ; = (mean(x*y) - mean(x)*mean(y)) / (mean(x^2) - mean(x)^2)
  (if (eq 0 (- (mean (mapcar #'(lambda (y) (expt y 2)) x)) (expt (mean x) 2))) 999999999
      (* 1.0 (/ (- (mean (elem-mult x y)) (* (mean x) (mean y))) (- (mean (mapcar #'(lambda (y) (expt y 2)) x)) (expt (mean x) 2))))))

(defun model-rmse (cl s cond-key)
  (if (eq (type-of cl) 'cons) t (setf cl (list cl)))
  (if (eq (type-of s) 'cons) t (setf s (list s)))
  (if (eq (type-of cond-key) 'cons) t (setf cond-key (list cond-key)))
  (defvar ha '()) (defvar hb '())
  (dolist (i cond-key) (cond ((equal i "PARITY") (progn (push 8.04 ha) (push -7.82 hb)))
                             ((equal i "SPATIAL") (progn (push 7.84 ha) (push -7.68 hb)))
                             ((equal i "CONTROL") (progn (push 8.13 ha) (push -8.33 hb)))
                             (t (progn (push nil ha) (push nil hb)))))
  (if (= (list-length cl) (list-length s) (list-length cond-key))
      (float (sqrt (/ (recursive-sum (mapcar #'(lambda (w x y z) (expt (- w (+ y (/ (* x z) 6900.0))) 2)) s cl ha hb)) (list-length cl))))
      nil))



;;;;;;;; RESPONSE HANDLERS ;;;;;;;;
(defmethod rpm-window-key-event-handler ((win rpm-window) key)
  (setf *RT* (- (get-time) *START-RT*))
  (if *RESPONSE-BASED-REWARD*
      (if (equal *CONDITION* "PARITY") (progn (if (eq 1 (mod (parse-integer *DISTRACTOR*) 2))
                                                  (if (equal *RESPONSE* "f") (trigger-reward *GLOBAL-REWARD*) (trigger-reward 0))
                                                  (if (equal *RESPONSE* "j") (trigger-reward *GLOBAL-REWARD*) (trigger-reward 0)))
                                                  ;(if (equal *RESPONSE* "f") (trigger-reward (ms->seconds *RT*)) (trigger-reward (ms->seconds (* *PENALTY* *RT*))))
                                                  ;(if (equal *RESPONSE* "j") (trigger-reward (ms->seconds *RT*)) (trigger-reward (ms->seconds (* *PENALTY* *RT*)))))
                                         ;(spp attend :u 1000)
                                         )
          (if (equal *CONDITION* "SPATIAL") (progn (if (equal *LOCATION* "BOTTOM")
                                                       (if (equal *RESPONSE* "f") (trigger-reward *GLOBAL-REWARD*) (trigger-reward 0))
                                                       (if (equal *RESPONSE* "j") (trigger-reward *GLOBAL-REWARD*) (trigger-reward 0)))
                                                       ;(if (equal *RESPONSE* "f") (trigger-reward (ms->seconds *RT*)) (trigger-reward (ms->seconds (* *PENALTY* *RT*))))
                                                       ;(if (equal *RESPONSE* "j") (trigger-reward (ms->seconds *RT*)) (trigger-reward (ms->seconds (* *PENALTY* *RT*)))))
                                              ;(spp attend :u 1000)
                                              )
              (print "CONDITION NOT RECOGNIZED! REWARD NOT TRIGGERED!"))))
  ;(setf *RESPONSE* key) ; set through globals...idk why keyboard and hands aren't working
)

(defmethod device-speak-string ((win rpm-window) text)
  (let ((myrt (- (get-time) *START-RT*)) (myresp text))
    ;(trigger-reward (* .001 myrt))
    (push (list myresp myrt) *RECALLED*)
    (setf *START-RT* (get-time)))
)


;;;;;;;; ENVIRONMENT HANDLERS ;;;;;;;;

;; Default variables for testing
(defvar test-dist)
(setf *PARAM-CONFIG* 0)
(setf *NUM-STIM* 4)
(setf *CONDITION* "PARITY")
(setf *MODEL-RUN* 1)
(setf *TRAINING* "0")
(setf *SET* 2)
(setf *SERIES* 1)
(setf *TAR-LIST* '("b" "c"))
(setf test-dist
      (list
      '(("1" #(600 100)) ("2" #(600 100)) ("3" #(600 900)) ("4" #(600 900)))
      '(("5" #(600 100)) ("6" #(600 100)) ("7" #(600 900)) ("8" #(600 900)))
      )
)

;;;; CONSTRUCT TRIALS
(defvar consonants)
(defvar odd-nums)
(defvar even-nums)
(defvar *FOUR-STIM*)
(defvar *SIX-STIM*)
(defvar *EIGHT-STIM*)
;(list-length consonants) == 20
(setf consonants '("b" "c" "d" "f" "g" "h" "j" "k" "l" "m" "n" "p" "q" "r" "s" "t" "v" "w" "x" "z"))
(setf odd-nums '("1" "3" "5" "7" "9")) ;   length = 5
(setf even-nums '("2" "4" "6" "8" "10")) ; length = 5

(setf *EIGHT-STIM* '())
(setf *SIX-STIM* '())
(setf *FOUR-STIM* '())


;; A function that takes the number of sets series distractors
; Going to be lazy and just randomly select everything instead of worrying that stimuli are super controlled and blocked across all conditions (i.e. using random effects) because I'm going to simulate like 1000 times
(defun generate-stimuli (nsets nseries ndistractors)
  (let ((return-list '())) ; List of sets
    (dotimes (r nsets)
      (let ((k (- nsets r)) (remp '())) ; temporary list for the current set
        (dotimes (s nseries)
          (let ((target-list '()) (dist-list '())) ; temporary list for the current series' target-list and dist-list. Combine the two at the very end to make the current series
            (dotimes (u k) ; for each target in the series
              (push (nth (random (list-length consonants)) consonants) target-list) ; add a target letter
              (push (let ((tar-dists '())) ; create list of distractors to go with each target
                      (dotimes (v ndistractors) ; for as many distractors in this condition
                        (push (list (if (eq (random 2) 0) (nth (random (list-length odd-nums)) odd-nums) (nth (random (list-length even-nums)) even-nums))
                                    (if (eq (random 2) 0) (loc 'bottom) (loc 'top)))
                              tar-dists)) ; add ndistractor distractor pairs to tar-dists
                      tar-dists) ; make sure the let statement returns tar-dists
                    dist-list) ; push each tar-dist onto dist-lists
              )
            (push (list target-list dist-list) remp)))
        (push remp return-list)))
    return-list))

;; A function that takes a number distractors and produces number distractors until a minimum number of trials has been built
; Similar to above just don't have to mess with targets
(defun generate-training-stimuli (ndistractors min-trials)
  (let ((nreps (ceiling min-trials ndistractors)) (return-list '()))
    (dotimes (i nreps) 
      (let ((temp '()))
        (dotimes (j ndistractors)
          (push (list (if (eq (random 2) 0) (nth (random (list-length odd-nums)) odd-nums) (nth (random (list-length even-nums)) even-nums))
                      (if (eq (random 2) 0) (loc 'bottom) (loc 'top)))
                temp))
        (push temp return-list)))
    return-list))


;;; RUN A LIST OF TARGETS WITH ACCOMPANYING DISTRACTORS THEN RECORD RECALL PERFORMANCE
;;;;; LOGS ALL BEHAVIORAL DATA AND RETURNS 1 FOR CORRECT RECALL AND 0 FOR INCORRECT

(defun run-series (target-stimuli distractor-stimuli)
  (defvar mytarget)
  (defvar this-dist-list)
  (defvar count)
  (defvar this-dist)
  
  ;; Pre-series, get ready
  (set-hand-location left 4 4)
  (set-hand-location right 7 4)
  
  (let ((w (open-exp-window 'win :visible nil)))
    (install-device w)
    (setf *VIS-LOC* (/ *SCREEN-Y* 2))
    (clear-buffer 'visual-location)
    (add-text-to-exp-window :text "*" :x (/ *SCREEN-X* 2) :y (/ *SCREEN-Y* 2))
    (proc-display)
    (if (equal *CONDITION* "PARITY") (goal-focus parity-goal)
        (if (equal *CONDITION* "SPATIAL") (goal-focus spatial-goal)
            (setf *CONDITION* "ERROR")))
    (suppress-warnings (run-full-time (/ *ASTERISK-DISPLAY-TIME* 1000.0) :real-time nil))
    (clear-exp-window)
    (proc-display)
    (suppress-warnings (run-full-time (/ *TARGET-DELAY* 1000.0) :real-time nil))

    ;; Run the target-distractor series
    (let ((mytarlist (copy-list target-stimuli)) (mydistlist (copy-list distractor-stimuli)) (series-tpt 0))
      (while (not (eq mytarlist nil))
             (setf mytarget (pop mytarlist))
             (setf *START-RT* (get-time))
             (setf *RECALLED* '())
             (setf *VIS-LOC* (/ *SCREEN-Y* 2))
             (clear-buffer 'visual-location)
             (add-text-to-exp-window :text mytarget :x (/ *SCREEN-X* 2) :y (/ *SCREEN-Y* 2))
             (proc-display)
             (suppress-warnings (run-full-time (/ *TARGET-DISPLAY-TIME* 1000.0) :real-time nil))
             (clear-exp-window)
             (proc-display)
             (suppress-warnings (run-full-time (/ *TARGET-DELAY* 1000.0) :real-time nil))
             
             (setf this-dist-list (pop mydistlist))
             (setf count 0)
             (while (not (eq this-dist-list nil))
                    (incf count)
                    (setf this-dist (pop this-dist-list))
                    (setf *DISTRACTOR* (first this-dist))
                    (if (> (svref (second this-dist) 1) (/ *SCREEN-Y* 2)) (setf *LOCATION* "TOP") (setf *LOCATION* "BOTTOM"))
                    (setf *RESPONSE* nil)
                    (setf *RT* -1)
                    (setf *START-RT* (get-time))
                    (setf *VIS-LOC* (svref (second this-dist) 1))
                    (clear-buffer 'visual-location)
                    (add-text-to-exp-window :text *DISTRACTOR* :x (svref (second this-dist) 0) :y (svref (second this-dist) 1))
                    (proc-display :clear t)
                    (trigger-reward nil)
                    (suppress-warnings (run-full-time (distractor-pt *NUM-STIM*) :real-time nil))
                    (clear-exp-window)
                    (proc-display)
                    (suppress-warnings (run-full-time (distractor-dt *NUM-STIM*) :real-time nil))
                    
                    (cond ((equal *CONDITION* "PARITY") (if (eq 1 (mod (parse-integer *DISTRACTOR*) 2))
                                                            ;(if (and (equal *RESPONSE* "f") (> *RT* 0)) (push 1 (nth (- (/ *NUM-STIM* 2) 2) *ACC-TRACKER*)) (push 0 (nth (- (/ *NUM-STIM* 2) 2) *ACC-TRACKER*)))
                                                            ;(if (and (equal *RESPONSE* "j") (> *RT* 0)) (push 1 (nth (- (/ *NUM-STIM* 2) 2) *ACC-TRACKER*)) (push 0 (nth (- (/ *NUM-STIM* 2) 2) *ACC-TRACKER*)))))
                                                            (if (and (equal *RESPONSE* "f") (> *RT* 0)) (push 1 *ACC-TRACKER*) (push 0 *ACC-TRACKER*))
                                                            (if (and (equal *RESPONSE* "j") (> *RT* 0)) (push 1 *ACC-TRACKER*) (push 0 *ACC-TRACKER*))))
                          ((equal *CONDITION* "SPATIAL") (if (equal *LOCATION* "BOTTOM")
                                                             ;(if (and (equal *RESPONSE* "f") (> *RT* 0)) (push 1 (nth (+ (/ *NUM-STIM* 2) 1) *ACC-TRACKER*)) (push 0 (nth (+ (/ *NUM-STIM* 2) 1) *ACC-TRACKER*)))
                                                             ;(if (and (equal *RESPONSE* "j") (> *RT* 0)) (push 1 (nth (+ (/ *NUM-STIM* 2) 1) *ACC-TRACKER*)) (push 0 (nth (+ (/ *NUM-STIM* 2) 1) *ACC-TRACKER*))))))
                                                             (if (and (equal *RESPONSE* "f") (> *RT* 0)) (push 1 *ACC-TRACKER*) (push 0 *ACC-TRACKER*))
                                                             (if (and (equal *RESPONSE* "j") (> *RT* 0)) (push 1 *ACC-TRACKER*) (push 0 *ACC-TRACKER*)))))
            
                    (if (> *RT* 0) (progn (push *RT* *RT-TRACKER*) (setf series-tpt (+ series-tpt *RT*))) (progn (setf *NULL-TRACKER* (+ *NULL-TRACKER* 1)) (setf series-tpt (+ (distractor-pt *NUM-STIM*) (distractor-dt *NUM-STIM*) series-tpt)))) ; (setf *NULL-RESPONSE* t))
                    )
             )
      (push (* 1.0 (/ series-tpt *SET*)) *TPT-TRACKER*)
      )

      ;; Run the recall segment

    (setf *RECALLED* '())
    (setf *START-RT* (get-time))
    (setf *VIS-LOC* (/ *SCREEN-Y* 2))
    (clear-buffer 'visual-location)
    (add-text-to-exp-window :text "Recall" :x (/ *SCREEN-X* 2) :y (/ *SCREEN-Y* 2))
    (proc-display)
    (suppress-warnings (run 20))
    (clear-exp-window)
    
    ; Log the recall data
    (let ((recall-list '()) (myreclist (reverse (copy-list *RECALLED*))) (mytarlist (copy-list *TAR-LIST*)))
      (while (not (eq mytarlist nil))
             (defvar myrecall)
             (defvar myrt)
             (defvar mytarget)
             (if (eq myreclist nil)
                 (progn (setf myrecall nil) (setf myrt nil))
                 (progn (setf myrecall (first (car myreclist)))
                   (setf myrt (second (pop myreclist)))))
             (setf mytarget (pop mytarlist))
             (push myrecall recall-list)
             )

      ;; Returns 1 for correct, 0 for incorrect
      (if (equal (reverse recall-list) *TAR-LIST*) 1 0)
      )
    )
  )


(defun run-set (set-of-series-stimuli)
  (let ((myseries (copy-list set-of-series-stimuli)) (this-series nil) (series-count 0) (corr-count 0))
    (while (not (eq myseries nil))
           (incf series-count)
           (setf this-series (pop myseries))
           (setf *SERIES* series-count)
           (setf *TAR-LIST* (first this-series))
           (setf corr-count (+ corr-count (run-series (first this-series) (second this-series))))
           )
    (setf *CORRECT-COUNT* (+ *CORRECT-COUNT* corr-count))
    (> corr-count 0)
    )
)


(defun run-spec-condition (p-s-condition num-stim-condition stimuli)
  (setf *CONDITION* p-s-condition)
  (setf *NUM-STIM* num-stim-condition)
  (setf *CORRECT-COUNT* 0)
  (let ((mystim (copy-list stimuli)) (this-set nil) (set-count 0) (continue t))
    (while (and (not (eq mystim nil)) continue)
      (incf set-count)
      (setf *SET* set-count)
      (setf this-set (pop mystim))
      (setf continue (run-set this-set))
      )
    )
  (float (/ *CORRECT-COUNT* 3))
)


; 24, 16, 12 blocks for 4, 6, 8 stim
(defun training (p-s-condition num-stim-condition mystimuli1 mystimuli2)
  (setf *TRAINING* "1") ;(sgp :v t :act t)
  (setf *CONDITION* p-s-condition)
  (setf *NUM-STIM* num-stim-condition)
  (if (equal *CONDITION* "PARITY") (goal-focus parity-goal)
      (if (equal *CONDITION* "SPATIAL") (goal-focus spatial-goal)
          (setf *CONDITION* "ERROR")))   
  (let ((bcount 0) (stimuli (copy-list mystimuli1)))
    (let ((w (open-exp-window 'win :visible nil)))
      (install-device w)
      (setf *RESPONSE-BASED-REWARD* t)
      (while (not (eq stimuli nil))
             (incf bcount)
             (let ((this-block (pop stimuli)) (tcount 0))
               (while (not (eq this-block nil))
                      (incf tcount)
                      (let ((this-dist (pop this-block)))
                        (setf *DISTRACTOR* (first this-dist))
                        (if (> (svref (second this-dist) 1) (/ *SCREEN-Y* 2)) (setf *LOCATION* "TOP") (setf *LOCATION* "BOTTOM"))
                        (setf *RESPONSE* nil)
                        (setf *RT* -1)
                        (setf *START-RT* (get-time))
                        (setf *VIS-LOC* (svref (second this-dist) 1))
                        (clear-buffer 'visual-location)
                        (add-text-to-exp-window :text *DISTRACTOR* :x (svref (second this-dist) 0) :y (svref (second this-dist) 1))
                        (proc-display :clear t)
                        (trigger-reward nil)
                        (suppress-warnings (run-full-time (distractor-pt *NUM-STIM*) :real-time nil))
                        (clear-exp-window)
                        (proc-display)
                        (suppress-warnings (run-full-time (distractor-dt *NUM-STIM*) :real-time nil))
                        (if (not (> *RT* 0)) (progn (trigger-reward 0) ;(trigger-reward (* *PENALTY* (+ (distractor-pt *NUM-STIM*) (distractor-dt *NUM-STIM*)))) ;(spp attend :u 1000)
                                               ))
                
                        )
                      )
               )
             )

      (setf *RESPONSE-BASED-REWARD* nil)
      )
    (setf stimuli (copy-list mystimuli2))
    (setf *SET* 0)
    (while (not (eq stimuli nil))
           (let ((this-set (pop stimuli)))
             (incf *SET*)
             (run-set this-set))))
  (setf *TRAINING* "0")
)


(defun run-experiment (n)
  (let ((cond-scores '()) (this-score 0))
    (dotimes (i n)
      (setf *MODEL-RUN* (+ 1 i))
      (setf this-score 0)
      ; 2 (CONDITION) X 3 (NUM-STIM)
      ;; Parity
      (reset)
      (training "PARITY" 4 *FOUR-TRAINING-1* *FOUR-TRAINING-2*)
      (setf this-score (+ this-score (expt (- 5.16 (run-spec-condition "PARITY" 4 *FOUR-STIM*)) 2)))
      (reset)
      (training "PARITY" 6 *SIX-TRAINING-1* *SIX-TRAINING-2*)
      (setf this-score (+ this-score (expt (- 4.58 (run-spec-condition "PARITY" 6 *SIX-STIM*)) 2)))
      (reset)
      (training "PARITY" 8 *EIGHT-TRAINING-1* *EIGHT-TRAINING-2*)
      (setf this-score (+ this-score (expt (- 3.69 (run-spec-condition "PARITY" 8 *EIGHT-STIM*)) 2)))
      
      ;; Spatial
      (reset)
      (training "SPATIAL" 4 *FOUR-TRAINING-1* *FOUR-TRAINING-2*)
      (setf this-score (+ this-score (expt (- 5.56 (run-spec-condition "SPATIAL" 4 *FOUR-STIM*)) 2)))
      (reset)
      (training "SPATIAL" 6 *SIX-TRAINING-1* *SIX-TRAINING-2*)
      (setf this-score (+ this-score (expt (- 5.52 (run-spec-condition "SPATIAL" 6 *SIX-STIM*)) 2)))
      (reset)
      (training "SPATIAL" 8 *EIGHT-TRAINING-1* *EIGHT-TRAINING-2*)
      (setf this-score (+ this-score (expt (- 4.60 (run-spec-condition "SPATIAL" 8 *EIGHT-STIM*)) 2)))
      (push this-score cond-scores)
      )
    (/ (recursive-sum cond-scores) n)
    )
)


;;;; General wrapper (designed to be used by MindModeling)
(defun mm-wrapper (&key run n bll ans rt inhibition-scale inhibition-decay egs alpha reward le lf blc ac ad)
  ;; Fix random seeds at the beginning of each betal-wrapper call. This ensures the runs always start from the same place.
  ;; Resetting the seeds outside of the loop allows multiple runs to simulate multiple individuals
  (setf *RANDOM-STATE* *INITIAL-STATE*)
  (set-parameter-value (gethash :seed *act-r-parameters-table*) (list (ash 28874058 run) 0))
  (defvar stimuli-training-1)
  (defvar stimuli-training-2)
  (defvar stimuli-general)
  (defvar myrc) (defvar mynd) (defvar num-dist) (defvar resp-cond)
  (setf num-dist '(1 2 3 4 6 8 9 10 11 12))
  (setf resp-cond '("PARITY" "SPATIAL"))

  (let ((this-score 0) (myhash (make-hash-table :test 'equal)) 
        (cond-spans (repeat '() (* (list-length resp-cond) (list-length num-dist)))) (cond-rts (repeat '() (* (list-length resp-cond) (list-length num-dist)))) 
        (cond-tpt (repeat '() (* (list-length resp-cond) (list-length num-dist)))) (cond-acc (repeat '() (* (list-length resp-cond) (list-length num-dist)))))
    (dotimes (i n)
      (setf *MODEL-RUN* (+ 1 i))    
      (dotimes (rc (list-length resp-cond))
        (dotimes (nd (list-length num-dist))
          (setf myrc (nth rc resp-cond))
          (setf mynd (nth nd num-dist))

          ;; Generate stimuli
          (setf stimuli-training-1 (generate-training-stimuli mynd 96))
          (setf stimuli-training-2 (generate-stimuli 2 3 mynd))
          (setf stimuli-general (generate-stimuli 10 3 mynd))
 
          ;; Reset the model
          (setf *NULL-RESPONSE* nil)
          (reset) (mp-real-time-management :allow-dynamics t)
          ;; Set model parameters
          (suppress-warnings
           (set-parameter-value (gethash :bll *act-r-parameters-table*) bll)
           (set-parameter-value (gethash :ans *act-r-parameters-table*) ans)
           (set-parameter-value (gethash :rt *act-r-parameters-table*) rt)
           (set-parameter-value (gethash :egs *act-r-parameters-table*) egs)
           (set-parameter-value (gethash :alpha *act-r-parameters-table*) alpha)
           (setf *GLOBAL-REWARD* reward)
           (set-parameter-value (gethash :le *act-r-parameters-table*) le)
           (set-parameter-value (gethash :lf *act-r-parameters-table*) lf)
           (set-parameter-value (gethash :blc *act-r-parameters-table*) blc)
           (set-parameter-value (gethash :sim-hook *act-r-parameters-table*) #'(lambda (x y) (if (chunk-slot-equal x y) 0 (if (and (typep x 'float) (typep y 'float)) (+ ac (log (expt (+ (sqrt (expt (- x y) 2)) 1) (* -1 ad)))) -1000))))
           (set-parameter-value (gethash :inhibition-scale *act-r-parameters-table*) inhibition-scale)
           (set-parameter-value (gethash :inhibition-decay *act-r-parameters-table*) inhibition-decay))
          (set-all-base-levels (first *LTM-BL*) (second *LTM-BL*))
          (spp-fct (list '(already-encoded wait-for-id no-guess-ret-nf no-guess-ret-pr below-no-guess-ret-sr-nw above-no-guess-ret-sr-nw) :u *GLOBAL-REWARD*))

          ;; Run the model          
          (training myrc mynd stimuli-training-1 stimuli-training-2)
          (setf *RT-TRACKER* '()) (setf *TPT-TRACKER* '()) (setf *ACC-TRACKER* '())
          (push (run-spec-condition myrc mynd stimuli-general) (nth (+ (* rc (list-length num-dist)) nd) cond-spans))
          
          (if (not (null *ACC-TRACKER*)) (push (mean *ACC-TRACKER*) (nth (+ (* rc (list-length num-dist)) nd) cond-acc)) (push 0 (nth (+ (* rc (list-length num-dist)) nd) cond-acc)))
          (if (not (null *RT-TRACKER*)) (push (mean *RT-TRACKER*) (nth (+ (* rc (list-length num-dist)) nd) cond-rts)) (progn (setf this-score 10000) (setf *NULL-RESPONSE* t)))
          (if (not (null *TPT-TRACKER*)) (push (mean *TPT-TRACKER*) (nth (+ (* rc (list-length num-dist)) nd) cond-tpt)) (progn (setf this-score 10000) (setf *NULL-RESPONSE* t)))
          )
        )
      )
    ;; Save results in hash table
    (if *NULL-RESPONSE*
        (progn (setf (gethash "RMSE" myhash) this-score) (setf (gethash "TE" myhash) this-score)
          (dolist (myrc resp-cond) (setf (gethash (concatenate 'string myrc "-SCL") myhash) -1))
          (dolist (myrc resp-cond)
            (dolist (mynd num-dist)
              (setf (gethash (concatenate 'string myrc (write-to-string mynd) "-SPAN") myhash) -1)
              (setf (gethash (concatenate 'string myrc (write-to-string mynd) "-RT") myhash) -1)
              (setf (gethash (concatenate 'string myrc (write-to-string mynd) "-TPT") myhash) -1)
              (setf (gethash (concatenate 'string myrc (write-to-string mynd) "-ACC") myhash) -1)
              (setf (gethash (concatenate 'string myrc (write-to-string mynd) "-CL") myhash) -1))))

        (progn (setf cond-spans (mapcar #'mean cond-spans)) (setf cond-tpt (mapcar #'mean cond-tpt)) (setf cond-cl (mapcar #'(lambda (x) (* 1.0 (/ x 6900))) cond-tpt)) (setf cond-rts (mapcar #'mean cond-rts)) (setf cond-acc (mapcar #'mean cond-acc))
          (setf (gethash "RMSE" myhash) (+ this-score (model-rmse cond-tpt cond-spans (flatten (mapcar #'(lambda (x) (repeat x (list-length num-dist))) resp-cond)))))
          (dotimes (rc (list-length resp-cond)) (setf (gethash (concatenate 'string (nth rc resp-cond) "-SCL") myhash) (reg-coef (subseq cond-cl (* rc (list-length num-dist)) (* (list-length num-dist) (+ rc 1))) (subseq cond-spans (* rc (list-length num-dist)) (* (list-length num-dist) (+ rc 1))))))
          (dotimes (rc (list-length resp-cond))
            (dotimes (nd (list-length num-dist))
              (setf myrc (nth rc resp-cond)) (setf mynd (nth nd num-dist))
              (setf (gethash (concatenate 'string myrc (write-to-string mynd) "-SPAN") myhash) (nth (+ (* rc (list-length num-dist)) nd) cond-spans))
              (setf (gethash (concatenate 'string myrc (write-to-string mynd) "-RT") myhash) (nth (+ (* rc (list-length num-dist)) nd) cond-rts))
              (setf (gethash (concatenate 'string myrc (write-to-string mynd) "-TPT") myhash) (nth (+ (* rc (list-length num-dist)) nd) cond-tpt))
              (setf (gethash (concatenate 'string myrc (write-to-string mynd) "-ACC") myhash) (nth (+ (* rc (list-length num-dist)) nd) cond-acc))
              (setf (gethash (concatenate 'string myrc (write-to-string mynd) "-CL") myhash) (nth (+ (* rc (list-length num-dist)) nd) cond-cl))
              ))
          (setf (gethash "TE" myhash) (round-to (+ this-score (/ (abs (- 5.16 (nth 4 cond-spans))) .78) (/ (abs (- 4.58 (nth 5 cond-spans))) 1.23) (/ (abs (- 3.69 (nth 6 cond-spans))) .63) 
                                          (/ (abs (- 5.56 (nth 14 cond-spans))) .75) (/ (abs (- 5.52 (nth 15 cond-spans))) .62) (/ (abs (- 4.60 (nth 16 cond-spans))) .82)
                                          (/ (abs (- 628 (nth 4 cond-rts))) 117) (/ (abs (- 551 (nth 5 cond-rts))) 53) (/ (abs (- 483 (nth 6 cond-rts))) 32) 
                                          (/ (abs (- 484 (nth 14 cond-rts))) 61) (/ (abs (- 387 (nth 15 cond-rts))) 41) (/ (abs (- 361 (nth 16 cond-rts))) 39)
                                          (/ (abs (- 2467 (nth 4 cond-tpt))) 400) (/ (abs (- 3251 (nth 5 cond-tpt))) 316) (/ (abs (- 3724 (nth 6 cond-tpt))) 218) 
                                          (/ (abs (- 1928 (nth 14 cond-tpt))) 233) (/ (abs (- 2297 (nth 15 cond-tpt))) 239) (/ (abs (- 2827 (nth 16 cond-tpt))) 266)
                                          (let ((par-slope (reg-coef (list (nth 4 cond-cl) (nth 5 cond-cl) (nth 6 cond-cl)) (list (nth 4 cond-spans) (nth 5 cond-spans) (nth 6 cond-spans))))
                                                (spa-slope (reg-coef (list (nth 14 cond-cl) (nth 15 cond-cl) (nth 16 cond-cl)) (list (nth 14 cond-spans) (nth 15 cond-spans) (nth 16 cond-spans)))))
                                            (+ (if (<= (- (atan -7.82) (atan par-slope)) 0) (* (/ 4 (abs (- (atan -7.82) (/ pi 2)))) (abs (- (atan -7.82) (atan par-slope)))) (* (/ 4 (abs (- (atan -7.82) (/ pi -2)))) (abs (- (atan -7.82) (atan par-slope)))))
                                               (if (<= (- (atan -7.68) (atan spa-slope)) 0) (* (/ 4 (abs (- (atan -7.68) (/ pi 2)))) (abs (- (atan -7.68) (atan spa-slope)))) (* (/ 4 (abs (- (atan -7.68) (/ pi -2)))) (abs (- (atan -7.68) (atan spa-slope)))))))
                                          (recursive-sum (mapcar #'(lambda (x) (if (< x .8) (/ (* (abs (- .8 x)) 4) .3) 0)) (list (nth 4 cond-acc) (nth 5 cond-acc) (nth 6 cond-acc) (nth 14 cond-acc) (nth 15 cond-acc) (nth 16 cond-acc)))))
                                                12))
          )
        )
    myhash)
  )


(defun run-series-ccl (target-stimuli cl)
  (defvar mytarget)
  (defvar count)

  ;; Pre-series, get ready
  (set-hand-location left 4 4)
  (set-hand-location right 7 4)

  (let ((w (open-exp-window 'win :visible nil)))
    (goal-focus parity-goal) ; Need a goal chunk but it doesn't matter which one
    (install-device w)
    (setf *VIS-LOC* (/ *SCREEN-Y* 2))
    (clear-buffer 'visual-location)
    (add-text-to-exp-window :text "*" :x (/ *SCREEN-X* 2) :y (/ *SCREEN-Y* 2))
    (proc-display)
    (suppress-warnings (run-full-time (/ *ASTERISK-DISPLAY-TIME* 1000.0) :real-time nil))
    (clear-exp-window)
    (proc-display)
    (suppress-warnings (run-full-time (/ *TARGET-DELAY* 1000.0) :real-time nil))

    ;; Run the target-distractor series
    (let ((mytarlist (copy-list target-stimuli)) (ili (+ (distractor-pt 1) (distractor-dt 1) (ms->seconds *TARGET-DELAY*))))
      (while (not (eq mytarlist nil))
             (setf mytarget (pop mytarlist))
             (setf *START-RT* (get-time))
             (setf *RECALLED* '())
             (setf *VIS-LOC* (/ *SCREEN-Y* 2))
             (clear-buffer 'visual-location)
             (add-text-to-exp-window :text mytarget :x (/ *SCREEN-X* 2) :y (/ *SCREEN-Y* 2))
             (proc-display)
             (suppress-warnings (run-full-time (/ *TARGET-DISPLAY-TIME* 1000.0) :real-time nil))
             (clear-exp-window)
             (proc-display)
             (suppress-warnings (run-full-time (/ *TARGET-DELAY* 1000.0) :real-time nil))
          
          
             ;;; Lock out maintenance
             (let ((saved-chunk (no-output (buffer-chunk goal))))
               (goal-focus halt)
               (suppress-warnings (run-full-time (* cl ili) :real-time nil))
               ;;; Unlock maintenance
               (goal-focus-fct (car saved-chunk))
               (suppress-warnings (run-full-time (- (* ili (- 1.0 cl)) (ms->seconds *TARGET-DELAY*)) :real-time nil)))
             
             )
      (push (* cl ili 1000.0) *TPT-TRACKER*)
      )

    ;; Run the recall segment
    (setf *RECALLED* '())
    (setf *START-RT* (get-time))
    (setf *VIS-LOC* (/ *SCREEN-Y* 2))
    (clear-buffer 'visual-location)
    (add-text-to-exp-window :text "Recall" :x (/ *SCREEN-X* 2) :y (/ *SCREEN-Y* 2))
    (proc-display)
    (suppress-warnings (run 200))
    (clear-exp-window)
      
    ; Log the recall data
    (let ((recall-list '()) (myreclist (reverse (copy-list *RECALLED*))) (mytarlist (copy-list *TAR-LIST*)))
      (while (not (eq mytarlist nil))
             (defvar myrecall)
             (defvar myrt)
             (defvar mytarget)
             (if (eq myreclist nil)
                 (progn (setf myrecall nil) (setf myrt nil))
                 (progn (setf myrecall (first (car myreclist)))
                   (setf myrt (second (pop myreclist)))))
             (setf mytarget (pop mytarlist))
             (push myrecall recall-list)
             )
        

      ;; Returns 1 for correct, 0 for incorrect
      (if (equal (reverse recall-list) *TAR-LIST*) 1 0)
      )
    )
  )


(defun run-set-ccl (set-of-series-stimuli cl)
  (let ((myseries (copy-list set-of-series-stimuli)) (this-series nil) (series-count 0) (corr-count 0))
    (while (not (eq myseries nil))
           (incf series-count)
           (setf this-series (pop myseries))
           (setf *SERIES* series-count)
           (setf *TAR-LIST* (first this-series))
           (setf corr-count (+ corr-count (run-series-ccl (first this-series) cl)))
           )
    (setf *CORRECT-COUNT* (+ *CORRECT-COUNT* corr-count))
    (> corr-count 0)
    )
)


(defun run-spec-condition-ccl (stimuli cl)
  (setf *CORRECT-COUNT* 0)
  (let ((mystim (copy-list stimuli)) (this-set nil) (set-count 0) (continue t))
    (while (and (not (eq mystim nil)) continue)
      (incf set-count)
      (setf *SET* set-count)
      (setf this-set (pop mystim))
      (setf continue (run-set-ccl this-set cl))
      )
    )
  (float (/ *CORRECT-COUNT* 3))
)


(defun training-ccl (mystimuli2 cl)
  (setf *TRAINING* "1")
  (let ((stimuli (copy-list mystimuli2)))
    (setf *SET* 0)
    (while (not (eq stimuli nil))
      (let ((this-set (pop stimuli)))
        (incf *SET*)
        (run-set-ccl this-set cl))))
  (setf *TRAINING* "0")
)


(defun mm-wrapper-ccl (&key run n bll ans rt inhibition-scale inhibition-decay egs alpha reward le lf blc ac ad)
  ;; Fix random seeds at the beginning of each betal-wrapper call. This ensures the runs always start from the same place.
  ;; Resetting the seeds outside of the loop allows multiple runs to simulate multiple individuals
  (setf *RANDOM-STATE* *INITIAL-STATE*)
  (set-parameter-value (gethash :seed *act-r-parameters-table*) (list (ash 28874058 run) 0))
  (defvar stimuli-training-1)
  (defvar stimuli-training-2)
  (defvar stimuli-general)
  (defvar myrc) (defvar mynd) (defvar num-dist) (defvar resp-cond)
  (setf num-dist (myseq 0.0 :m 0.925 :b .025))
  (setf resp-cond '("CONTROL"))

  (let ((this-score 0) (myhash (make-hash-table :test 'equal)) 
        (cond-spans (repeat '() (* (list-length resp-cond) (list-length num-dist)))) (cond-rts (repeat '() (* (list-length resp-cond) (list-length num-dist)))) 
        (cond-tpt (repeat '() (* (list-length resp-cond) (list-length num-dist)))) (cond-acc (repeat '() (* (list-length resp-cond) (list-length num-dist)))))
    (dotimes (i n)
      (setf *MODEL-RUN* (+ 1 i))    
      (dotimes (rc (list-length resp-cond))
        (dotimes (nd (list-length num-dist))
          (setf myrc (nth rc resp-cond))
          (setf cl (nth nd num-dist))

          (setf *CONDITION* "CONTROL")
          (setf *NUM-STIM* 0)

          ;; Generate stimuli
          (setf stimuli-training-2 (generate-stimuli 2 3 0))
          (setf stimuli-general (generate-stimuli 10 3 0))
 
          ;; Reset the model
          (setf *NULL-RESPONSE* nil)
          (reset) (mp-real-time-management :allow-dynamics t)
          ;; Set model parameters
          (suppress-warnings
           (set-parameter-value (gethash :bll *act-r-parameters-table*) bll)
           (set-parameter-value (gethash :ans *act-r-parameters-table*) ans)
           (set-parameter-value (gethash :rt *act-r-parameters-table*) rt)
           (set-parameter-value (gethash :egs *act-r-parameters-table*) egs)
           (set-parameter-value (gethash :alpha *act-r-parameters-table*) alpha)
           (setf *GLOBAL-REWARD* reward)
           (set-parameter-value (gethash :le *act-r-parameters-table*) le)
           (set-parameter-value (gethash :lf *act-r-parameters-table*) lf)
           (set-parameter-value (gethash :blc *act-r-parameters-table*) blc)
           (set-parameter-value (gethash :sim-hook *act-r-parameters-table*) #'(lambda (x y) (if (chunk-slot-equal x y) 0 (if (and (typep x 'float) (typep y 'float)) (+ ac (log (expt (+ (sqrt (expt (- x y) 2)) 1) (* -1 ad)))) -1000))))
           (set-parameter-value (gethash :inhibition-scale *act-r-parameters-table*) inhibition-scale)
           (set-parameter-value (gethash :inhibition-decay *act-r-parameters-table*) inhibition-decay))
          (set-all-base-levels (first *LTM-BL*) (second *LTM-BL*))
          (spp-fct (list '(already-encoded wait-for-id no-guess-ret-nf no-guess-ret-pr below-no-guess-ret-sr-nw above-no-guess-ret-sr-nw) :u *GLOBAL-REWARD*))


          ;; Run the model          
          (training-ccl stimuli-training-2 cl)
          (setf *RT-TRACKER* '()) (setf *TPT-TRACKER* '()) (setf *ACC-TRACKER* '())
          (push (run-spec-condition-ccl stimuli-general cl) (nth (+ (* rc (list-length num-dist)) nd) cond-spans))
          
          (if (not (null *ACC-TRACKER*)) (push (mean *ACC-TRACKER*) (nth (+ (* rc (list-length num-dist)) nd) cond-acc)) (push 0 (nth (+ (* rc (list-length num-dist)) nd) cond-acc)))
          (if (not (null *RT-TRACKER*)) (push (mean *RT-TRACKER*) (nth (+ (* rc (list-length num-dist)) nd) cond-rts)) (push 0 (nth (+ (* rc (list-length num-dist)) nd) cond-acc)))
          (if (not (null *TPT-TRACKER*)) (push (mean *TPT-TRACKER*) (nth (+ (* rc (list-length num-dist)) nd) cond-tpt)) (push 0 (nth (+ (* rc (list-length num-dist)) nd) cond-acc)))
          )
        )
      )
    ;; Save results in hash table
    (if *NULL-RESPONSE*
        (progn (setf (gethash "RMSE" myhash) this-score)
          (dolist (myrc resp-cond) (setf (gethash (concatenate 'string myrc "-SCL") myhash) -1))
          (dolist (myrc resp-cond)
            (dolist (mynd num-dist)
              (setf (gethash (concatenate 'string myrc (write-to-string mynd) "-SPAN") myhash) -1)
              (setf (gethash (concatenate 'string myrc (write-to-string mynd) "-TPT") myhash) -1)
              (setf (gethash (concatenate 'string myrc (write-to-string mynd) "-CL") myhash) -1))))

        (progn (setf cond-spans (mapcar #'mean cond-spans)) (setf cond-tpt (mapcar #'mean cond-tpt)) (setf cond-cl (mapcar #'(lambda (x) (* 1.0 (/ x 6900))) cond-tpt)) (setf cond-rts (mapcar #'mean cond-rts)) (setf cond-acc (mapcar #'mean cond-acc))
          (setf (gethash "RMSE" myhash) (+ this-score (model-rmse cond-tpt cond-spans (flatten (mapcar #'(lambda (x) (repeat x (list-length num-dist))) resp-cond)))))
          (dotimes (rc (list-length resp-cond)) (setf (gethash (concatenate 'string (nth rc resp-cond) "-SCL") myhash) (reg-coef (subseq cond-cl (* rc (list-length num-dist)) (* (list-length num-dist) (+ rc 1))) (subseq cond-spans (* rc (list-length num-dist)) (* (list-length num-dist) (+ rc 1))))))
          (dotimes (rc (list-length resp-cond))
            (dotimes (nd (list-length num-dist))
              (setf myrc (nth rc resp-cond)) (setf mynd (nth nd num-dist))
              (setf (gethash (concatenate 'string myrc (write-to-string mynd) "-SPAN") myhash) (nth (+ (* rc (list-length num-dist)) nd) cond-spans))
              (setf (gethash (concatenate 'string myrc (write-to-string mynd) "-TPT") myhash) (nth (+ (* rc (list-length num-dist)) nd) cond-tpt))
              (setf (gethash (concatenate 'string myrc (write-to-string mynd) "-CL") myhash) (nth (+ (* rc (list-length num-dist)) nd) cond-cl))
              ))
          )
        )
    myhash)
  )



;;;;;;;; BEGIN MODEL DEFINITION ;;;;;;;;

(clear-all)

(define-model continuous-span-model

;;;;;;;; DEFINE MODEL PARAMETERS ;;;;;;;;

(suppress-warnings (sgp :v nil
     :vwt nil ; need to debug display
     :act nil ; need to debug activations
     :ult nil ; need to debug utilities
     :sact nil
     :crt nil ; need to debug conflict resolution
     ))

;; FIXED PARAMETERS ; currently defaults
(sgp :ol nil ; :OL MUST BE NIL OR A NUMBER FOR INHIBITION TO WORK
     :mp 1 
     :mas nil
     :randomize-time 3
     :enable-inhibition t
     :ul t
     :esc t
     :epl t
     )

;;;;;;;; DEFINE MEMORY CHUNKS ;;;;;;;;

(chunk-type goal which-list condition state location response)
(chunk-type stimulus string type)
(chunk-type target string parent list episode)
(chunk-type num-fact string parity)
(chunk-type resp-rule condition response)
(chunk-type stop)
(define-chunks
   (odd isa chunk) (even isa chunk)
   (below isa chunk) (above isa chunk) (center isa chunk)
   (letter isa chunk) (number isa chunk) (special isa chunk)
   (parity isa chunk) (spatial isa chunk)
   (attended isa chunk) (encoded isa chunk)
   (finish-target isa chunk) (get-num-fact isa chunk) (get-resp-rule isa chunk)
   (eval-resp isa chunk) (wait isa chunk) (start isa chunk) (locked isa chunk)
)
(add-dm
   ; response rules
   (resp-odd isa resp-rule condition odd response "f")
   (resp-even isa resp-rule condition even response "j")
   (resp-below isa resp-rule condition below response "f")
   (resp-above isa resp-rule condition above response "j")
   ; number facts
   (nf-one isa num-fact string "1" parity odd)
   (nf-two isa num-fact string "2" parity even)
   (nf-three isa num-fact string "3" parity odd)
   (nf-four isa num-fact string "4" parity even)
   (nf-five isa num-fact string "5" parity odd)
   (nf-six isa num-fact string "6" parity even)
   (nf-seven isa num-fact string "7" parity odd)
   (nf-eight isa num-fact string "8" parity even)
   (nf-nine isa num-fact string "9" parity odd)
   (nf-ten isa num-fact string "10" parity even)
   ; stimuli
   (asterisk isa stimulus string "*" type special)
   (recall isa stimulus string "Recall" type special)
   (one isa stimulus string "1" type number) (two isa stimulus string "2" type number)
   (three isa stimulus string "3" type number) (four isa stimulus string "4" type number)
   (five isa stimulus string "5" type number) (six isa stimulus string "6" type number)
   (seven isa stimulus string "7" type number) (eight isa stimulus string "8" type number)
   (nine isa stimulus string "9" type number) (ten isa stimulus string "10" type number)
   (b isa stimulus string "b" type letter) (c isa stimulus string "c" type letter)
   (d isa stimulus string "d" type letter) (f isa stimulus string "f" type letter)
   (g isa stimulus string "g" type letter) (h isa stimulus string "h" type letter)
   (j isa stimulus string "j" type letter) (k isa stimulus string "k" type letter)
   (l isa stimulus string "l" type letter) (m isa stimulus string "m" type letter)
   (n isa stimulus string "n" type letter) (p isa stimulus string "p" type letter)
   (q isa stimulus string "q" type letter) (r isa stimulus string "r" type letter)
   (s isa stimulus string "s" type letter) (t isa stimulus string "t" type letter)
   (v isa stimulus string "v" type letter) (w isa stimulus string "w" type letter)
   (x isa stimulus string "x" type letter) (z isa stimulus string "z" type letter)
   ; goals
   (parity-goal isa goal condition parity state start)
   (spatial-goal isa goal condition spatial state start)
   (halt isa goal condition nil state locked)
)


;;;;;;;; DEFINE PRODUCTIONS ;;;;;;;;

;(p find) buffer stuffing should take care of any need to look for anything
(p attend ; moves visual attention to the stuffed vis-location
   =goal>
      isa   goal
   =visual-location>
      isa   visual-location
   ?visual-location> ; might not need
      buffer   unrequested
      attended   nil
   ?visual>
      state    free
!eval! t ; block this from compiling into prior distractors' productions
==>
   =goal>
      state   attended
   +visual>
      isa   move-attention
      screen-pos   =visual-location
)

;;;;;;;; NEED 3 ENCODE PRODUCTIONS TO STORE STIMULUS LOCATION ;;;;;;;;

(p encode-above    ; Get the chunk representation of the visual stimulus
   =goal>
      isa   goal
   =visual>
      isa   text
      value   =value
      screen-pos   =loc
 !eval! (> *VIS-LOC* (+ 100 (/ *SCREEN-Y* 2)))
==>
   =goal>
      state   encoded
      location   above
   +retrieval>
      isa   stimulus
      string    =value
)
(p encode-below    ; Get the chunk representation of the visual stimulus
   =goal>
      isa   goal
   =visual>
      isa   text
      value   =value
      screen-pos   =loc
 !eval! (< *VIS-LOC* (- (/ *SCREEN-Y* 2) 100))
==>
   =goal>
      state   encoded
      location   below
   +retrieval>
      isa   stimulus
      string    =value
)
(p encode-center    ; Get the chunk representation of the visual stimulus
   =goal>
      isa   goal
   =visual>
      isa   text
      value   =value
      screen-pos   =loc
 !eval! (and (< *VIS-LOC* (+ 100 (/ *SCREEN-Y* 2))) (> *VIS-LOC* (- (/ *SCREEN-Y* 2) 100)))
==>
   =goal>
      state   encoded
      location   center
   +retrieval>
      isa   stimulus
      string    =value
)


;;;;;;;; STIMULUS IS ASTERISK ;;;;;;;;

(p ret-asterisk    ; Stimulus is an asterisk
   =goal>
      isa   goal
      state   encoded
   =retrieval>
      isa   stimulus
      type   special
      string   "*"
!safe-eval! (setf *LIST-INDEX* (+ 1 *LIST-INDEX*))
==>
!safe-bind! =l (write-to-string (* 1000 *LIST-INDEX*)) ; encoding list to a unique space of representations (i.e. as.string(ints > 1000))
   =goal>
      response   start
      which-list   =l     
)

;;;;;;;; STIMULUS IS A LETTER ;;;;;;;;

(p first-letter   ; Just read the first new letter, now build its list representation
   =goal>
      isa goal
      state   encoded
      response   start
      which-list   =l
   =retrieval>
      isa   stimulus
      type   letter
      string   =t
   ?imaginal>
      state   free
   ?vocal>
      state   free
==>
!safe-bind! =e (ms->seconds (mp-time-ms))
   =goal>
      state   finish-target
      response   nil
   +vocal>
      isa   speak
      string   =t
   +imaginal>
      isa   target
      string   =t
      parent   start
      list   =l
      episode   =e
)
(p new-letter   ; Just read a new letter, now build its list representation
   =goal>
      isa goal
      state   encoded
    - response   start
      which-list   =l
   =retrieval>
      isa   stimulus
      type   letter
      string   =t
   ?imaginal>
      state   free
   ?vocal>
      state   free
==>
!safe-bind! =e (ms->seconds (mp-time-ms))
   =goal>
      state   finish-target
   +vocal>
      isa   speak
      string   =t
   +imaginal>
      isa   target
      string   =t
      list   =l
      episode   =e
)
(p finish-new-letter   ; Link new last item to its parent
   =goal>
      isa   goal
      state   finish-target
   ?imaginal>
      state   free
   =imaginal>
      isa   target
==>
   =goal>
      state   nil
   -imaginal>
)


;;;;;;;; STIMULUS IS A NUMBER ;;;;;;;;

;;;; These fire in all conditions
(p blind-guess-f
   =goal>
      isa   goal
      state   encoded
    - location   center
      response   nil
   ?manual>
      state   free
==>
   =goal>
      state   nil
      response   nil
   +manual>
      isa   punch
      hand   left
      finger   index
!eval! (setf *RESPONSE* "f")
)

(p blind-guess-j 
   =goal>
      isa   goal
      state   encoded
    - location   center
      response   nil
   ?manual>
      state   free
==>
   =goal>
      state   nil
      response   nil
   +manual>
      isa   punch
      hand   right
      finger   index
!eval! (setf *RESPONSE* "j")
)

(p wait-for-id ; Wait to identify the number (before possibly guessing)
   =goal>
      isa   goal
      state   encoded
    - location   center
   ?retrieval>
      state   busy
==>
   =goal>
      state   get-num-fact
)

;; Possible case of encoding completes before I can fire the wait-for-id production (which is crazy but let's handle that extreme case so that it's not forced to blind guess)
(p already-encoded
   =goal>
      isa   goal
      state   encoded
    - location   center
   =retrieval>
      isa   stimulus
      type   number
==>
   =goal>
      state   get-num-fact
   =retrieval>
)


;;;; PARITY CONDITION
(p guess-num-odd ; Guess after identifying number, retrieve num-fact
   =goal>
      isa   goal
      condition   parity
      state   get-num-fact
      response   nil
   =retrieval>
      isa   stimulus
      type   number
      string   =t
   ?manual>
      state   free
==>
   =goal>
      state   nil
      response   nil
   +manual>
      isa   punch
      hand   left
      finger   index
!safe-eval! (setf *RESPONSE* "f")
)

(p guess-num-even ; Guess after identifying number, retrieve num-fact
   =goal>
      isa   goal
      condition   parity
      state   get-num-fact
      response   nil
   =retrieval>
      isa   stimulus
      type   number
      string   =t
   ?manual>
      state   free
==>
   =goal>
      state   nil
      response   nil
   +manual>
      isa   punch
      hand   right
      finger   index
!safe-eval! (setf *RESPONSE* "j")
)

(p no-guess-ret-nf ; Retrieve num-fact (no guess)
   =goal>
      isa   goal
      condition   parity
      state   get-num-fact
   =retrieval>
      isa   stimulus
      type   number
      string   =t
==>
   =goal>
      state   get-resp-rule
   +retrieval>
      isa   num-fact
      string   =t
)

(p ret-odd-guess-odd ; Guess after retrieving parity
   =goal>
      isa   goal
      condition   parity
      state   get-resp-rule
      response   nil
   =retrieval>
      isa   num-fact
      parity   odd
   ?manual>
      state   free
==>
   =goal>
      state   nil
      response   nil
   +manual>
      isa   punch
      hand   left
      finger   index
!safe-eval! (setf *RESPONSE* "f")
)

(p ret-even-guess-even ; Guess after retrieving parity
   =goal>
      isa   goal
      condition   parity
      state   get-resp-rule
      response   nil
   =retrieval>
      isa   num-fact
      parity   even
   ?manual>
      state   free
==>
   =goal>
      state   nil
      response   nil
   +manual>
      isa   punch
      hand   right
      finger   index
!safe-eval! (setf *RESPONSE* "j")
)
(p ret-odd-guess-even ; Guess after retrieving parity
   =goal>
      isa   goal
      condition   parity
      state   get-resp-rule
      response   nil
   =retrieval>
      isa   num-fact
      parity   odd
   ?manual>
      state   free
==>
   =goal>
      state   nil
      response   nil
   +manual>
      isa   punch
      hand   right
      finger   index
!safe-eval! (setf *RESPONSE* "j")
)

(p ret-even-guess-odd ; Guess after retrieving parity
   =goal>
      isa   goal
      condition   parity
      state   get-resp-rule
      response   nil
   =retrieval>
      isa   num-fact
      parity   even
   ?manual>
      state   free
==>
   =goal>
      state   nil
      response   nil
   +manual>
      isa   punch
      hand   left
      finger   index
!safe-eval! (setf *RESPONSE* "f")
)

(p no-guess-ret-pr ; Retrieve resp-rule (no guessing)
   =goal>
      isa   goal
      condition   parity
      state   get-resp-rule
   =retrieval>
      isa   num-fact
      parity   =p
==>
   =goal>
      state   eval-resp
   +retrieval>
      isa   resp-rule
      condition   =p
)


;;;; SPATIAL CONDITION
(p below-guess-below-nw ; Guess, don't wait for stim id
   =goal>
      isa   goal
      condition   spatial
      state   encoded
      location   below
      response   nil
   ?manual>
      state   free
==>
   =goal>
      state   nil
      response   nil
   +manual>
      isa   punch
      hand   left
      finger   index
!safe-eval! (setf *RESPONSE* "f")
)

(p above-guess-above-nw ; Guess, don't wait for stim id
   =goal>
      isa   goal
      condition   spatial
      state   encoded
      location   above
      response   nil
   ?manual>
      state   free
==>
   =goal>
      state   nil
      response   nil
   +manual>
      isa   punch
      hand   right
      finger   index
!safe-eval! (setf *RESPONSE* "j")
)

(p below-guess-above-nw ; Guess, don't wait for stim id
   =goal>
      isa   goal
      condition   spatial
      state   encoded
      location   below
      response   nil
   ?manual>
      state   free
==>
   =goal>
      state   nil
      response   nil
   +manual>
      isa   punch
      hand   right
      finger   index
!safe-eval! (setf *RESPONSE* "j")
)

(p above-guess-below-nw ; Guess, don't wait for stim id
   =goal>
      isa   goal
      condition   spatial
      state   encoded
      location   above
      response   nil
   ?manual>
      state   free
==>
   =goal>
      state   nil
      response   nil
   +manual>
      isa   punch
      hand   left
      finger   index
!safe-eval! (setf *RESPONSE* "f")
)

(p below-no-guess-ret-sr-nw ; Retrieve resp-rule (no guessing), don't wait for stim id
   =goal>
      isa   goal
      condition   spatial
      state   encoded
      location   below
==>
   =goal>
      state   eval-resp
   +retrieval>
      isa   resp-rule
      condition   below
)

(p above-no-guess-ret-sr-nw ; Retrieve resp-rule (no guessing), don't wait for stim id
   =goal>
      isa   goal
      condition   spatial
      state   encoded
      location   above
==>
   =goal>
      state   eval-resp
   +retrieval>
      isa   resp-rule
      condition   above
)

;;;; Same rules can be used in either condition to evaluate responses
(p safe-response-f ; Respond according to resp-rule
   =goal>
      isa   goal
      state   eval-resp
      response   nil
   =retrieval>
      isa   resp-rule
      response   "f"
   ?manual>
      state   free
==>
   =goal>
      state   nil
      response   nil
   +manual>
      isa   punch
      hand   left
      finger   index
!safe-eval! (setf *RESPONSE* "f")
)

(p safe-response-j ; Respond according to resp-rule
   =goal>
      isa   goal
      state   eval-resp
      response   nil
   =retrieval>
      isa   resp-rule
      response   "j"
   ?manual>
      state   free
==>
   =goal>
      state   nil
      response   nil
   +manual>
      isa   punch
      hand   right
      finger   index
!safe-eval! (setf *RESPONSE* "j")
)


;;;;;;;; MEMORY REACTIVATION PRODUCTIONS ;;;;;;;;

(p refresh-trace    ; Bottle neck is free so try refreshing
   =goal>
      isa   goal
    - state   recall 
    - state   finish-target
    - state   locked ; For fixed cl enforced by environment code hack
      which-list   =l
   ?retrieval>
    - state   busy
      buffer   empty
   ?visual-location>
    - attended   nil
   ?visual>
    - state   busy
      buffer   empty
==>
   =goal>
   +retrieval>
      isa   target
      list   =l
)
(p continue-refresh    ; Continue the refreshing loop
   =goal>
      isa   goal
    - state   finish-target
    - state   recall 
    - state   locked ; For fixed cl enforced by environment code hack
      which-list   =l
   =retrieval>
      isa   target
      episode   =e
      string   =s
   ?visual-location>
    - attended   nil
   ?visual>
    - state   busy
      buffer   empty
==>
   =goal>
   +retrieval> 
      isa   target 
      list   =l
      episode   =e
)
;(p continue-refresh-vb    ; Continue the refreshing loop
;   =goal>
;      isa   goal
;    - state   finish-target
;    - state   recall
;    - state   locked ; For fixed cl enforced by environment code hack
;      which-list   =l
;   =retrieval>
;      isa   target
;      episode   =e
;      string   =s
;   ?visual-location>
;      state   free
;   ?visual>
;      buffer   empty
;   ?vocal>
;      state   busy
;   ?aural>
;      buffer empty
;==>
;   =goal>
;   +retrieval> 
;      isa   target
;      list   =l
;      episode   =e
;)
;(p continue-refresh-ab    ; Continue the refreshing loop
;   =goal>
;      isa   goal
;    - state   finish-target
;    - state   recall
;    - state   locked ; For fixed cl enforced by environment code hack
;      which-list   =l
;   =retrieval>
;      isa   target
;      episode   =e
;      string   =s
;   ?visual-location>
;      state   free
;   ?visual>
;      buffer   empty
;   ?aural>
;      buffer   empty
;   ?vocal>
;    - state   busy
;==>
;   =goal>
;   +retrieval> ;=retrieval
;      isa   target 
;      episode   =e
;      list   =l
;   +vocal>
;      isa   subvocalize
;      string   =s
;)
;(p articulatory-rehearsal
;   =goal>
;      isa   goal
;    - state   finish-target
;    - state   recall 
;    - state   locked ; For fixed cl enforced by environment code hack
;      which-list   =l
;   =retrieval>
;      isa   target
;   ?visual-location>
;      state   free
;   ?visual>
;      buffer   empty
;   ?vocal>
;    - state   busy
;   =aural>
;      isa   sound
;      kind   word
;      content   =s
;==>
;   =goal>
;   +retrieval> 
;      isa   target 
;      list   =l
;      string   =s
;   +vocal>
;      isa   subvocalize
;      string   =s
;)
;(p articulatory-repetition    ; Retrieval is busy doing non-maintenance stuff so just continue to loop in standby 
;   =goal>
;      isa   goal
;    - state   finish-target
;    - state   recall 
;      which-list   =l
;   ?retrieval>
;      state   busy
;   ?visual-location>
;      state   free
;   ?visual>
;      buffer   empty
;   ?vocal>
;    - state   busy
;   =aural>
;      isa   sound
;      kind   word
;      content   =s
;==>
;   =goal>
;   +vocal>
;      isa   subvocalize
;     string   =s
;)

;(p articulatory-repetition-ccl   ; Alternate version to the above that should continue to work while retrieval is locked out ; I need both versions so that A-rep is always allowed (except for finish-target and recall)
;   =goal>
;      isa   goal
;      state   locked ; Takes the place of the retrieval query because locked essentially means that "the central bottleneck is in use by non-maintenance"
;   ?visual-location>
;      state   free
;   ?visual>
;      buffer   empty
;   ?vocal>
;    - state   busy
;   =aural>
;      isa   sound
;      kind   word
;      content   =s
;==>
;   =goal>
;   +vocal>
;      isa   subvocalize
;     string   =s
;)


;;;;;;;; RECALL TARGET LIST PRODUCTIONS ;;;;;;;; 

(p begin-recall    ; Stimulus is "Recall"
   =goal>
      isa   goal
      state   encoded
      which-list   =l
   =retrieval>
      isa   stimulus
      type   special
      string   "Recall"
   ?aural-location>
      state   free
==>
!eval! (setf *RECALLED* '()) ; just in case
   =goal>
      state   recall
   +retrieval>
      isa   target
      parent   start
      list   =l
)

(p verbal-recall
   =goal>
      isa   goal
      state   recall
      which-list   =l
   =retrieval>
      isa   target
      episode   =e
      string   =s
   ?vocal>
      state   free
!safe-eval! *VERBAL-RECALL*
==>
   =goal>
   +retrieval> 
      isa   target 
      list   =l
      episode   =e
    - string   =s
   +vocal>
      isa   speak
      string   =s
)

(p manual-recall
   =goal>
      isa   goal
      state   recall
      which-list   =l
   =retrieval>
      isa   target
      episode   =e
      string   =s
   ?manual>
      state   free
!safe-eval! (not *VERBAL-RECALL*)
==>
   =goal>
   +retrieval> 
      isa   target
      list   =l
      episode   =e
    - string   =s
   +manual>
      isa   press-key
      key   =s
!safe-eval! (setf *RESPONSE* =s)
)

(p fail-recall    ; Recall loop fails
   =goal>
      isa   goal
      state   recall
   ?retrieval>
      state   error
==>
   +goal>
      isa   stop
   -aural-location>
   -aural>
   -vocal>
!eval! (reset-audio-module (get-module :audio))
!eval! (set-audloc-default :attended nil :onset lowest)
)
(p finish-recall    ; Made N responses so wait
   =goal>
      isa   goal
      state   recall
!safe-eval! (>= (list-length *RECALLED*) *SET*)
==>
   +goal>
      isa   stop
   -aural-location>
   -aural>
   -vocal>
!eval! (reset-audio-module (get-module :audio))
!eval! (set-audloc-default :attended nil :onset lowest)
)

;;;;;;;; PHONOLOGICAL LOOP PRODUCTIONS ;;;;;;;;

;(p reset-aural-location
;   ?aural-location>
;      state   error
;==>
;   +aural>
;      isa   clear
;)
;(p move-aural-attention
;   =goal>
;      isa   goal
;    - state   recall
;   =aural-location>
;      isa   audio-event
;   ;?aural-location>
;   ;   buffer   requested
;   ?aural>
;    - state   busy
;      buffer   empty
;==>
;   =goal>
;   +aural>
;      isa   sound
;      event   =aural-location
;)
      

;;;;;;;; SET UTILITY VALUES ;;;;;;;;

(spp attend :u 1000)
(spp encode-center :u 1000)
(spp encode-above :u 1000)
(spp encode-below :u 1000)
(spp ret-asterisk :u 1000)
(spp first-letter :u 1000)
(spp new-letter :u 1000)
(spp finish-new-letter :u 1000)
(spp begin-recall :u 1000)
;(spp move-aural-attention :u 800)

(set-audloc-default :attended nil :onset lowest)
)