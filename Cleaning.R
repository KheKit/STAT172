rm(list = ls()) # clean global environment
# load packages:
library(rpart)
library(rpart.plot)
library(randomForest)
library(ggplot2)
library(pROC)
library(RColorBrewer)

# read in customer churn data
mara <- read.csv(choose.files(), header = T, stringsAsFactors = TRUE)
summary(mara)

unique(mara$Wall21)

# test for git push
# test change for sam

#hello this is a test