# mlr3bam
R package implementing Big Additive Model (BAM) learners for the mlr3 ecosystem.

## Installation

```r
remotes::install_github("Tino-Rg/mlr3bam")
```

## Usage

```r
library(mlr3)  
library(mlr3bam)
```

### Classification

```r
task = mlr3::tsk("sonar")  
learner = LearnerClassifBam$new()
learner$param_set$set_values(
    k = 5,
    discrete = TRUE,
    nthreads = 1
)  
learner$train(task)  
pred = learner$predict(task)  
print(pred)
```

### Regression

```r
task = mlr3::tsk("mtcars")  
learner = LearnerRegrBam$new() 
learner$param_set$set_values(
    k = 3,
    method = "fREML"
)  
learner$train(task)   
pred = learner$predict(task)    
print(pred)
```

## Related work

Course projects wiki: https://github.com/tdhock/2026-01-aa-grande-echelle/wiki/projets

This learner uses the mgcv package for training and prediction: https://cran.r-project.org/package=mgcv