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

# Manually enter the missing Category values
mara$Category[26] = "WAM"
mara$Category[35] = "MAM"
mara$Category[52] = "MAM"
mara$Category[54] = "MAM"
mara$Category[74] = "MAM"
mara$Category[81] = "MAM"

# Make a binary cross training variable
mara$CrossTraining_bin <- ifelse(mara$CrossTraining == "", "No", "Yes")

# Fixing the piece of data in sp4week that had "11125.00000"
mara$sp4week[82] <- 11.125

# Making the "-" in Wall21 to be NA in order to get the average to eventually replace those
mara$Wall21[mara$Wall21 == " -   "] <- NA
mara$Wall21 <- as.numeric(as.character(mara$Wall21))
avg_Wall <- mean(mara$Wall21, na.rm = TRUE)
mara$Wall21[is.na(mara$Wall21)] <- avg_Wall

