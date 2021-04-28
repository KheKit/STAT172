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
# for Mac
mara <- read.csv(file.choose(), header = T, stringsAsFactors = TRUE)
View(mara)
summary(mara)

unique(mara$Wall21)

#
mara$speed <- mara$CATEGORY
mara$speed[mara$CATEGORY == "A"] <- "Fast"
mara$speed[mara$CATEGORY == "B"] <- "Fast"
mara$speed[mara$CATEGORY == "C"] <- "Slow"
mara$speed[mara$CATEGORY == "D"] <- "Slow"
