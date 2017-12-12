# Dynamic programming algorithms for sequence alignment

## Note

I intentionally chose Haskell as programming language for this project, 
to showcase its power in implementing DP algorithms. Haskell implements
lazy evaluation, which automatically results in memoization of computed
values, therefore the programmer gets dynamic programming for free. 

Because the dynamic programming is implicit, it is not necessary to build
up a table. Indeed, if entries are immediately computed, it would break
lazy evaluation and hence break the algorithm. 

Follow https://docs.haskellstack.org/en/stable/README/ to install Haskell, 
then compile and run with `bash run.sh`. The result is stored in `out`. 
