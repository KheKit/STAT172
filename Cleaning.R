rm(list = ls()) # clean global environment
# load packages:
library(rpart)
library(rpart.plot)
library(randomForest)
library(ggplot2)
library(pROC)
library(RColorBrewer)

# Read in the marathon data
mara <- read.csv(file.choose(), header = T, stringsAsFactors = TRUE)
View(mara)
summary(mara)


### CLEAN DATA ###

# Creating our response variable
mara$speed <- ifelse(mara$CATEGORY == "A" | mara$CATEGORY == "B", "Fast", "Slow")

# Make a binary cross training variable
mara$CrossTraining_bin <- ifelse(mara$CrossTraining == "", "No", "Yes")

# Fixing the piece of data in sp4week that had "11125.00000"
mara$sp4week[82] <- 11.125
