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

####################################################################################
### CLEAN DATA #################################################
#######################################################

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

# Create new variable of ratio of first half marathon to second half marathon
mara$halfratio <- mara$Wall21 / ( mara$MarathonTime - mara$Wall21)

# Remove unused variables
mara$id <- NULL
mara$Marathon <- NULL
mara$Name <- NULL
mara$CATEGORY <- NULL
mara$CrossTraining <- NULL
mara$Wall21 <- NULL

write.csv(mara, "/Users/smoon/Desktop/track.csv")

##################################################################################
### EXPLORATORY ############################################################
#### Visualization ###################################################
############################################################

# Univariate
ggplot(data = mara,aes(x = speed)) + 
  geom_bar(fill = "steelblue") +
  ggtitle("Speed") +
  geom_text(stat="count", aes(label=..count..), vjust=1.6, color="white", size=5) +
  labs(x = "Speed", y = "Frequency") +
  theme_bw()

ggplot(data = mara, aes(x = halfratio)) + 
  geom_histogram(fill = "steelblue") +
  ggtitle("Histogram of Half Ratio") +
  labs(x = "halfratio", y = "Count") +
  theme_bw()

ggplot(data = mara, aes(x = Category)) + 
  geom_bar(fill = "steelblue") +
  ggtitle("Age Category") +
  geom_text(stat="count", aes(label=..count..), vjust=1.4, color="white", size=4) +
  labs(x = "Category", y = "Frequency") +
  theme_bw()
  

ggplot(data = mara, aes(x = CrossTraining_bin)) + 
  geom_bar(fill = "steelblue") +
  ggtitle("Number of Cross Traning") +
  geom_text(stat="count", aes(label=..count..), vjust=1.6, color="white", size=5) +
  labs(x = "Cross Training", y = "Count") +
  theme_bw()

ggplot(data = mara, aes(x = sp4week)) + 
  geom_histogram(fill = "steelblue") +
  ggtitle("Histogram of sp4Week") +
  labs(x = "sp4Week", y = "Count") +
  theme_bw()

ggplot(data = mara, aes(x = km4week)) + 
  geom_histogram(fill = "steelblue") +
  ggtitle("Histogram of km4Week") +
  labs(x = "km4Week", y = "Count") +
  theme_bw()

# Add y - speed
ggplot(data = mara) + 
  geom_histogram(aes(x = halfratio, fill = speed)) +
  ggtitle("Half Ratio With Speed") +
  labs(x = "halfratio", y = "Count") +
  theme_minimal()

# using proportion 
ggplot(data = mara) + 
  geom_histogram(aes(x = halfratio, fill = speed), position = "fill") +
  ggtitle("Proportion of Half Ratio With Speed") +
  labs(x = "halfratio", y = "Count") +
  scale_fill_brewer("Speed", palette = "Dark2") +
  theme_minimal()
  
ggplot(data = mara) + 
  geom_bar(aes(x = Category, fill = speed), position = "fill") +
  ggtitle("Proportion of Category With Speed") +
  labs(x = "Category", y = "Percentage") +
  scale_fill_brewer("Speed", palette = "Dark2") +
  theme_minimal()

ggplot(data = mara) + 
  geom_bar(aes(x = CrossTraining_bin, fill = speed), position = "fill") +
  ggtitle("Proportion of Cross Training With Speed") +
  labs(x = "Cross Training", y = "Percentage") +
  scale_fill_brewer("Speed", palette = "Dark2") +
  theme_minimal()

ggplot(data = mara) + 
  geom_histogram(aes(x = sp4week, fill = speed)) +
  ggtitle("Histogram of sp4Week With Speed") +
  labs(x = "sp4Week", y = "Count") +
  scale_fill_brewer("Speed", palette = "Dark2") +
  theme_minimal()

ggplot(data = mara) + 
  geom_histogram(aes(x = km4week, fill = speed)) +
  ggtitle("Histogram of km4Week With Speed") +
  labs(x = "km4Week", y = "Count") +
  scale_fill_brewer("Speed", palette = "Dark2") +
  theme_minimal()

# Scatter plot
ggplot(data = mara) +
  geom_point(aes(x = sp4week, y = km4week, col = Category)) +
  ggtitle("sp4Week Vs km4Week") +
  labs(x = "Sp4Week", y = "Km4Week") +
  scale_color_brewer("Category", palette = "Dark2") +
  theme_bw()

ggplot(data = mara) +
  geom_point(aes(x = km4week, y = MarathonTime, col = Category)) +
  geom_smooth(aes(x = km4week, y = MarathonTime)) +
  ggtitle("km4Week Vs Marathon Time") +
  labs(x = "km4Week", y = "Marathon Time") +
  scale_color_brewer("Category", palette = "Dark2") +
  theme_bw()

#######################################################################
#### ANALYSIS ###############################################
# test tree ###################################
#######################################

RNGkind(sample.kind = "default")
set.seed(741852)
train.idx <- sample(x = 1:nrow(mara), size = floor(.8*nrow(mara)))
train.df = mara[train.idx,]
test.df = mara[-train.idx,]

# fit our tree
set.seed(741852)
ctree <- rpart(speed ~ Category+km4week+sp4week+CrossTraining_bin+halfratio,
               data = train.df,
               method = "class")
rpart.plot(ctree)


############################################################################
## Random Forest ###############################################
###################################################

mara$speed <- factor(mara$speed)
mara$CrossTraining_bin <- factor(mara$CrossTraining_bin)

# set seed
RNGkind(sample.kind = "default")
set.seed(741852)
# training and testing
train.idx <- sample(x = 1:nrow(mara), size = floor(.8*nrow(mara)))
train.df = mara[train.idx,]
test.df = mara[-train.idx,]


# temporary forest
tempforest <- randomForest(speed ~ Category + km4week + sp4week + CrossTraining_bin + halfratio,
                           data = train.df,
                           ntree = 1000,
                           mtry = 4)

# Tuning #
mtry <- seq(1,5)
# make room for OOB error, m value
keeps <- data.frame(m = rep(NA, length(mtry)),
                    OOB_err_rate = rep(NA, length(mtry)))

for (idx in 1:length(mtry)) {
  print(paste0("trying m = ", mtry[idx]))
  forest <- randomForest(speed ~ Category + km4week + sp4week + CrossTraining_bin + halfratio,
                         data = train.df,
                         ntree = 1000, 
                         mtry = mtry[idx])
  
  keeps[idx, "m"] <- mtry[idx]
  
  keeps[idx, "OOB_err_rate"] <- mean(predict(forest) != train.df$speed)
}

# Plot OOB error rate ~ m (mtry) value
qplot(m, OOB_err_rate, geom = c("line", "point"), data = keeps) +
  theme_bw() + labs(x = "m (mtry) value", y = "OOB error rate") +
  scale_x_continuous(breaks = c(1:10))

finalforest <- randomForest(speed ~ Category + km4week + sp4week + CrossTraining_bin + halfratio,
                       data = train.df,
                       ntree = 1000, 
                       mtry = 5,
                       importance = TRUE)


# Create an ROC curve ("Fast" is pos event)

pi_hat_forest <- predict(finalforest, test.df, type = "prob")[,"Fast"]
rocCurve <- roc(response = test.df$speed, 
                predictor = pi_hat_forest,
                levels = c("Slow", "Fast"))

plot(rocCurve, print.auc = TRUE, print.thres = TRUE)
# .871 (1, .571)
# AUC: .818

# If we set our threshold at .871, 
# When a runner is not fast, we are going to correctly predict that they are not 
#   fast 100% of the time.  NOTE: This is because we have such a small data set!!!
# When a runner is fast, we are going to correctly identify that about 
#   57% of the time

varImpPlot(finalforest, type = 1)
# sp4week, then km4week, then Category, then CrossTraining_bin

#################################################################################
### Modeling ########################################################
### Using Binomial (Bernoulli) ########################################
################################################

# Make a binary speed variable
mara$speed_bin <- ifelse(mara$speed == "Fast", 1, 0)


m1 <- glm(data = mara, speed_bin ~ km4week + sp4week + halfratio + Category + CrossTraining_bin, family = "binomial")
summary(m1)
#GLM with all possible explanatory variables. AIC = 86.493

m2 <- glm(data = mara, speed_bin ~ km4week + sp4week + halfratio + Category, family = "binomial")
summary(m2)
#GLM without CrossTrain_bin. Took out due to high p-value of 0.325194 (and it's last on the 
#   variable importance plot. AIC = 85.472

m3 <- glm(data = mara, speed_bin ~ km4week + sp4week + halfratio, family = "binomial")
summary(m3)
#GLM without CrossTrain_bin and Category. Took category out due to high p-values across all categories. AIC = 79.112.
#   Lowest AIC!  Makes sense because it's the top 3 in the variable importance plot.

# halfratio has a large standard error...

m4 <- glm(data = mara, speed_bin ~ km4week + sp4week, family = "binomial")
summary(m4)
# Took out halfratio because it has a large standard error (complete separation), and
#   it doesn't quite make sense to include it since it is something that occurrs after
#   the race has started.  AIC: 78.197 - Smallest AIC - use for interpretations!!


