
packages <- 
  c(
    "magrittr",
    "rpart",
    "rpart.plot",
    "ggplot2",
    "caret",
    "tidyverse",
    "dplyr",
    "haven", 
    "foreign",
    "haven",
   'ggplot2movies'
  )

lapply(
  packages,
  library,
  character.only = TRUE
)
library(gbm)
library(class)
library(stream)
#library(RWeka)
library(nopp)
library(randomForest)
require(neuralnet)
library(caret)
library(cluster)
library(factoextra)
library(rattle)
library(C50)
library("mlogit")
library(ggmosaic)
library(e1071)
library(scorecard)
library(pROC)
library(nnet)
library(ggplot2)
library(gridExtra)
library(ggmosaic)
library(ClustOfVar)
library(perturb)
library(scorecard)
library("VIM")
library("randomForest")
library(tree)
library(modeest)
library(data.table)
require(gridExtra)

