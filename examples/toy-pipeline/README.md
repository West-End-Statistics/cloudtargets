# Toy pipeline

Eight 15-second jobs spread over `workers = 4` worker slots. With 2-vCPU machines
this uses two of them. The `where` target counts jobs per machine, so a result
with two host names shows the work really was distributed.

```r
setwd("examples/toy-pipeline")
targets::tar_make()
targets::tar_read(where)
```

Costs a few cents; machines are deleted when `tar_make()` finishes.
