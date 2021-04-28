rm(list = ls()) # clean global environment
# load packages:
library(rpart)
library(rpart.plot)
library(randomForest)
library(ggplot2)
library(pROC)
library(RColorBrewer)

mara <- read.csv(file.choose(), header = T, stringsAsFactors = TRUE)
View(mara)
summary(mara)

unique(mara$Wall21)

mara$speed <- ifelse(mara$CATEGORY == "A" | mara$CATEGORY == "B", "Fast", "Slow")
