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
    formula = Class ~ s(V1, k = 5) + s(V2, k = 4) + V3,
    method = "fREML"
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
    formula = mpg ~ s(disp, k = 3) + s(hp, k = 4) + cyl,
    method = "fREML"
)
learner$train(task)   
pred = learner$predict(task)    
print(pred)
```

## Related work

Course projects wiki: https://github.com/tdhock/2026-01-aa-grande-echelle/wiki/projets

This learner uses the mgcv package for training and prediction: https://cran.r-project.org/package=mgcv