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


### EXPLORATORY #####
# Univariate
ggplot(data = mara,aes(x = speed)) + 
  geom_bar(fill = "steelblue") +
  ggtitle("Speed") +
  geom_text(stat="count", aes(label=..count..), vjust=1.6, color="white", size=3.5) +
  labs(x = "Speed", y = "Frequency") +
  theme_bw()
  

ggplot(data = mara) + 
  geom_histogram(aes(x = Wall21)) +
  labs(x = "Wall21", y = "Count")

ggplot(data = mara) + 
  geom_bar(aes(x = Category)) +
  labs(x = "Category", y = "Count")

ggplot(data = mara) + 
  geom_bar(aes(x = CrossTraining_bin)) +
  labs(x = "Cross Training?", y = "Count")

ggplot(data = mara) + 
  geom_histogram(aes(x = sp4week)) +
  labs(x = "sp4Week", y = "Count")

ggplot(data = mara) + 
  geom_histogram(aes(x = km4week)) +
  labs(x = "km4Week", y = "Count")


# Add y - speed
ggplot(data = mara) + 
  geom_histogram(aes(x = Wall21, fill = speed)) +
  labs(x = "Wall21", y = "Count")

ggplot(data = mara) + 
  geom_bar(aes(x = Category, fill = speed), position = "fill") +
  labs(x = "Category", y = "Count")

ggplot(data = mara) + 
  geom_bar(aes(x = CrossTraining_bin, fill = speed), position = "fill") +
  labs(x = "Cross Training?", y = "Count")

ggplot(data = mara) + 
  geom_histogram(aes(x = sp4week, fill = speed)) +
  labs(x = "sp4Week", y = "Count")

ggplot(data = mara) + 
  geom_histogram(aes(x = km4week, fill = speed)) +
  labs(x = "km4Week", y = "Count")



#### ANALYSIS ######
# test tree
RNGkind(sample.kind = "default")
set.seed(741852)
train.idx <- sample(x = 1:nrow(mara), size = floor(.8*nrow(mara)))
train.df = mara[train.idx,]
test.df = mara[-train.idx,]

# fit our tree
set.seed(741852)
ctree <- rpart(speed ~ Category+km4week+sp4week+CrossTraining_bin+Wall21,
               data = train.df,
               method = "class")
rpart.plot(ctree)
