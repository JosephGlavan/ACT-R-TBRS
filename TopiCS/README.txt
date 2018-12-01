README for ACT-R TBRS model to accompany 

Glavan & Houpt (2019) An Integrated Working Memory Model for Time-Based Resource-Sharing. Topics in Cognitive Science.

The model was constructed under ACT-R version r875 (which is ancient at time of writing but that’s what MindModeling.org had available to use at the time) so the code has some hacks for dealing with bugs that have since been fixed in the ACT-R source. It should be fairly straightforward to modify the model code to work with the latest version of ACT-R 6.0 or later, but this has not been tested.


To reproduce the results from the TopiCS paper, do the following:

1) Launch LISP and load the “load-act-r-6.lisp” file from the ACT-R source directory.

2) Load the appropriate model file (either attentional refreshing-only or articulatory rehearsal).

3a) The “mm-wrapper” function is a general function to simulate n subjects in every condition of the task. It returns its results in a hash table. It takes the following keyword arguments (values used by Glavan & Houpt in parentheses):
	run : used to advance the random seed so that multiple subjects can be simulated with multiple function calls*. Useful for parallelization.
	n : how many virtual subjects to run with one function call
	bll (0.5) : base-level learning parameter that controls decay rate. 
	ans (0.3) : activation noise parameter
	rt (0.0) : retrieval threshold
	inhibition-scale (1.0) : scaling parameter for base-level inhibition
	inhibition-decay (5.7) : decay parameter for base-level inhibition
	egs (1.0) : utility noise parameter
	alpha  (0.2) : utility learning rate
	reward (7.0) : difference in utility for correct vs. incorrect responses
	le (0.3) : latency exponent parameter
	lf (0.7) : latency factor parameter
	blc (15.0) : base-level constant
	ac (0.0) : temporal association constant. Should be kept at 0.0 unless you really know what you’re doing (see Glavan (2017) p. 56)
	ad (6.0) : episodic selectivity parameter

3b) The “mm-wrapper-ccl” does the same as “mm-wrapper-ccl” except that instead of simulating the parity and spatial tasks, cognitive load is simulated from 0.0 to 0.925 in 0.025 increments.


*Calling mm-wrapper five times with the values [1,2,3,4,5] for run and n=1 should be equivalent to calling it once with run=1 and n=5.
