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

# Creating our response variable
mara$speed <- ifelse(mara$CATEGORY == "A" | mara$CATEGORY == "B", "Fast", "Slow")

unique(mara$Category)

