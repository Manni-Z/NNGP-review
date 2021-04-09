"""
The following work based on the code of Lee et al. (2018) at
https://github.com/brain-research/nngp

*run_experiments.py has main function from line 225-228 like the following:

def main(argv):
  del argv  # Unused
  hparams = set_default_hparams().parse(FLAGS.hparams)
  run_nngp_eval(hparams, FLAGS.experiment_dir)
  
*Add a new arguement 'hparams' and comment out line 277 as the following

def main(argv, hparams):
  del argv  # Unused
  #hparams = set_default_hparams().parse(FLAGS.hparams)
  run_nngp_eval(hparams, FLAGS.experiment_dir)

*Comment out line 231 and 232. 

Similarly for Fig. 7, add a new argument to main to iteratively change the value of FLAGS.num_train.

"""

import run_experiments
import tensorflow as tf

# change nonlinearity='relu' to nonlinearity='tanh' to get the result of tanh. 
i=0
for w in range(0, 25):
  i+=0.25
  j=0
  for b in range(0, 20):
    j+=0.2
    run_gilbreth.main(argv=0, hparams=tf.contrib.training.HParams(
      nonlinearity='relu', weight_var=i, bias_var=j, depth=25))
    
