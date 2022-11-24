# The ndl2 (Naive Discriminative Learning version 2) package

The ndl2 package contains a fast implementation of the Rescorla-Wagner model of
learning ([read about the RW model here][1]) and of the Danks equilibrium
weights equations ([read the paper here][2]).

The original paper that describes this model can be downloaded from [here][3].
You should use this paper as a citation when reporting ndl models calculated by
ndl2:
Baayen, R. H., Milin, P., Filipovic Durdevic, D., Hendrix, P. and Marelli, M.
(2011), An amorphous model for morphological processing in visual comprehension
based on naive discriminative learning. Psychological Review 118, 438-482.


## Documentation

See the R documentation for more information and examples on how to use ndl2.
Have a look into the doc folder for install instructions and  if you are
interested in the development of ndl2. We hope to add a very extensive vignette
to the package in the near future.

## Python version `pyndl`

There is a [python version called `pyndl`][4], which added learning for continues inputs and outputs and has easier support for huge weight matrices and Unicode.


[1]: http://en.wikipedia.org/wiki/Rescorla%E2%80%93Wagner_model
[2]: http://repository.cmu.edu/philosophy/94/
[3]: http://www.ualberta.ca/~baayen/publications/BaayenEtAlPsychReview.pdf
[4]: https://github.com/quantling/pyndl
