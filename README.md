# mlr3bam
R package implementing Big Additive Model (BAM) learners for the mlr3 ecosystem.

## Installation

remotes::install_github("Tino-Rg/mlr3bam")

## Usage

library(mlr3)  
library(mlr3bam)

### Classification

task = mlr3::tsk("sonar")  
learner = LearnerClassifBam$new()  
learner$train(task)  
pred = learner$predict(task)  
print(pred)

### Regression

task = mlr3::tsk("mtcars")  
learner = LearnerRegrBam$new()   
learner$train(task)   
pred = learner$predict(task)    
print(pred)

## Related work

Course projects wiki: https://github.com/tdhock/2026-01-aa-grande-echelle/wiki/projets

This learner uses the mgcv package for training and prediction: https://cran.r-project.org/package=mgcv