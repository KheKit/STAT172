rm(list = ls()) # clean global environment
# load packages:
library(rpart)
library(rpart.plot)
library(randomForest)
library(ggplot2)
library(pROC)
library(RColorBrewer)

# read in customer churn data
mara <- read.csv("D:/Drake Spring 2021/Drake STAT172/Final_Project/MarathonData.csv", stringsAsFactors = TRUE)
summary(mara)

unique(mara$Wall21)

# test for git push