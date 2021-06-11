housing <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data")
colnames(housing) <- c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS","RAD","TAX","PRATIO","B","LSTAT","MDEV")
summary(housing)
plot(housing)
names(housing)
#
# I'm just getting an all x by all y to get a visual of what the relationships look like
# above. Most of the data looks to be useful, except for the following ones:
# • Charles river access (but that is binary)
# • Highway access (I guess that should be expected)
# • Tax rate (appears to be very lopsided, almost binary)
# The Boston data frame has 506 rows and 14 columns.
#Correlation matrix
install.packages("corrplot")
library(corrplot)
corrplot(cor(housing), method="number", tl.cex=0.5)
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  text(0.5, 0.5, txt)
}
#pairs(cbind(CRIM,ZN,INDUS,CHAS,NOX,RM,AGE,DIS,RAD,TAX,PTRATIO,B,LSTAT,MDEV),lower.panel=panel.cor,pch=18)
pairs(housing,lower.panel=panel.cor,pch=18)
# The highest correlations occurred with RAD (access to highway) and TAX (rate
# per $1,000). Unfortunately, I don't think I can exclude these from the dataset.
# The remaining variables are well within range.

#75 percent as the cutoff: 75 percent of the data will be for training and 25 percent for testing.
# Where the data is inherently geographic, it is better to make good samples that use
# percentages of geographic areas, but that data is not available. I am assuming that
# the data is inherently randomly organized, so random partitioning using the median
# house value as the index is workable.
# The caret package has a data partitioning function available, createDataPartition.
# The function works on a vector of values, selects the records of interest as per your
# parameters, and produces a vector of the indices selected. We can then extract the
# records of interest into training and testing sets.
#### The vector passed to createDataPartition is assumed to be sorted, so you must sort the data ahead of time.####
# I think this is a little problematic, as records would now most likely be clumped
# together geographically. I chose to split up the housing based on median value
# (MDEV). It seemed to have a good enough range that a randomized selection process
# would pull values from all different areas. I thought many of the other values would
# tend towards certain geographic pockets. Let's first install the caret package:

housing <- housing[order(housing$MDEV),]
install.packages("caret")
install.packages("ggplot2")
install.packages("Rcpp")
library(Rcpp)
library(caret)
library(ggplot2)
# The partitioning process uses a random number to select records. If we use set.
# seed, we can reproduce the partitioning example that takes place, since we are
# specifying the random starting place, as shown here:
set.seed(3277)
trainingIndices <- createDataPartition(housing$MDEV, p=0.75,list=FALSE)
housingTraining <- housing[trainingIndices,]
housingTesting <- housing[-trainingIndices,]
nrow(housingTraining)
nrow(housingTesting)

# First, we will use linear regression, lm. This model will provide a baseline for our
# testing, as shown in the following code:
linearModel <- lm(MDEV ~ CRIM + ZN + INDUS + CHAS + NOX + RM + AGE +
                    DIS + RAD + TAX + PRATIO + B + LSTAT, data=housingTraining)
summary(linearModel)
# It is interesting that AGE does not appear to be a true factor. Similarly, TAX and B
# have minimal impact.
predicted <- predict(linearModel,newdata=housingTesting)
summary(predicted)
summary(housingTesting$MDEV)
plot(predicted,housingTesting$MDEV)
# We appear to have close to a 45-degree regression with predicted versus actual.
# There is an offset.
sumofsquares <- function(x) {
   return(sum(x^2))
   }
diff <- predicted - housingTesting$MDEV
sumofsquares(diff)
# The sumofsquares result is the sum of the squares of the differences between
# predicted and actual values. The 3,000+ values over a few hundred observations
# don't sound particularly accurate, but we can try out other methods to see if we
# can arrive at a better mode. So, we will use this to compare results among the
# models going forward.

#LOGISTIC REGRESSION

lr <- glm(MDEV ~ CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS +
            RAD + TAX + PRATIO + B + LSTAT, data=housingTraining)
summary(lr)
predicted2 <- predict(lr,newdata=housingTesting)
summary(predicted2)
plot(predicted2,housingTesting$MDEV)
diff <- predicted2 - housingTesting$MDEV
sumofsquares(diff)
# We end up with exactly the same results! This shows that linear regression and
# logistic regression boil down to the same underlying modeling algorithm.

#RESIDUALS
# We can look at the residuals for the model (built into the result of the lm function).
# Note, we can use the resid function against any of the model-fitting functions
# available, as follows:
# The following plot shows a nice average of near zero for the residuals until we get to
# the higher values:
plot(resid(linearModel))


#LEAST SQUARE REGRESSION

# Least squares regression uses a line of the form b0 + b1*x as the line formula. Here,
# we have b0 as the intercept and b1 as the slope of the line. Using the same data, we
# can run a least squares regression using R functions directly.

# Let's assign our variables to the normal x and Y for a least squares regression (makes
# later calculations cleaner), as follows:
x <- housingTesting$MDEV
Y <- predicted
# Now, we will calculate our b0 and b1 from our x and Y, as follows:
b1 <- sum((x-mean(x))*(Y-mean(Y)))/sum((x-mean(x))^2)
b0 <- mean(Y)-b1*mean(x)
c(b0,b1)
#Let's plot the raw data using the following command:
plot(x,Y)
#We can add a least squares regression line to the plot, as follows:
abline(c(b0,b1),col="blue",lwd=2)
# There isn't a great match between the testing data and the prediction. The least
# squares line looks too flat.

# RELATIVE IMPORTANCE

# We can calculate the relative importance of the variables we used in the model using
# the relaimpo package. The relative importance of the variables used in our model
# will tell you which variables are providing the most effect on your results. In other
# words, out of all of the variables available, which should we pay the most attention
# to. Most of the time, you can only afford to investigate a few. In this case, maybe we
# are a buyer looking to see what factors are most affecting the value of houses and
# direct our search where those factors are maximized.
install.packages("relaimpo")
library(relaimpo)
calc.relimp(linearModel,type=c("lmg","last","first","pratt"),
            rela=TRUE)
# In the relative-importance metrics, we see computed values for each of the possible
# parameters in our model. This is what the parameters are about:
#   • The lmg column is the coefficient of the variable from the model.
# • The last column (also called usefulness) looks at what the effect of adding
# this variable into the model would be, effectively removing it, on the other
# variables. We are looking for the last values greater than lmg, as those
# variables are generating more effect. This would include NOX, DIS, RAD,
# PRATIO, and LSTAT.
# • The first column (squared covariance between y and the variable) looks at the
# variable as if none of the other variables were present in the model. We are
# interested in cases where the first column value is greater than lmg, as those
# variables are truly generating more effect. These include CRIM, ZN, INDUS,
# NOX, AGE, RAD, and B.
# • The pratt column (product of the standard coefficient and the correlation)
# is based on Pratt's contribution in 1987. The downfall is that negative values
# need to be ignored as not applicable. We are again looking for pratt values
# over lmg such as CRIM, ZN, RM, and PRATIO.
# The most interesting part of the results is the detail that the variables provided only
# explain 76 percent of the value. This is a pretty good number, but we did not end up
# being accurate.

#STEPWISE REGRESSION

# Stepwise regression is the process of adding/removing variables from the regression
# model, adjusting for the effect of doing so, and continuing to evaluate each of the
# variables involved. With forward stepwise regression, we start with an empty model
# and successively add each of the variables, gauge their effect, decide whether they
# remain in the model, and move on to the next variable. In backward regression,
# we start with the full model variable set and successively attempt to remove each,
# gauging their effect.
install.packages("MASS")
library(MASS)
# The results of the step(s) show the process of adding/removing variables from the
# model and the result of doing so leading up to the final best set of variables for the
# model. Let's use the stepAIC function to perform the same as follows:
step <- stepAIC(linearModel, direction="both")
# It is interesting to see that INDUS (percentage of industrial zoning) has the largest
# effect in this model and LSTAT (lower-income status population) is really negligible.

# K-NEAREST NEIGHBOR CLASSIFICATION

install.packages("class")
library(class)
knnModel <- knn(train=housingTraining, test=housingTesting,
                cl=housingTraining$MDEV)
summary(knnModel)
# Interpreting the data goes as follows: five entries for the 20.8 bucket, four entries for
# the 14.9 bucket, and so on. The buckets with the most hits are portrayed first, and
# then in the order of decreasing occurrence.
plot(knnModel)

# NAIVE BAYES

# Naïve Bayes is the process of determining classifiers based on probability, assuming
# the features are independent (the assumption is the naïve part).
install.packages("e1071")
library(e1071)
# We produce our estimates/model calling upon the naiveBayes function in much
# the same manner as we did for the previous regression: the idea is that median value
# is the product of all the associated data. We use the training data as the basis for the
# model, as shown here:
nb <- naiveBayes(MDEV ~ CRIM + ZN + INDUS + CHAS + NOX + RM + AGE +
                   DIS + RAD + TAX + PRATIO + B + LSTAT, data=housingTraining)
# We can examine the parameters generated for the effect of each variable as follows:
nb$tables$TAX
# The apriori value of the Naïve Bayes result contains the class distribution for the
# dependent variable. We can see this visually by plotting the result. I think it looks
# very similar to the previous knn model result: again, we have the tight overlap in
# the middle with both tails skewed. This does match our data. We can plot the result
# using the following command:
plot(nb$apriori)

# THE TRAIN METHOD (sf.260) (also look Yangchang Zhao (Eds.)case studies, 
# Nutshell Sanfrancisco, Yanchang Zhao and justin p.285)

# A standard method to develop a model, regardless of technique, is the train
# method. The train method has only one required parameter—sample data.
# All the other parameters are optional.
# Some of the parameters of the train method are described in the following table:
# Parameter - Description
# (x) This is the sample data
# (y) This is the vector of outcomes
# (form) This is the formula in the format result ~ var1 + var2 …
# (data) This is the dataframe where variables referenced in the formula can be taken
# (weights) This is the vector of case weights if applicable for the model
# (subset) This is the vector of indices to use for training
# (method) This can contain any of several methods listed at http://topepo.github.
# io/caret/bytag.html
set.seed(3277)
install.packages("kernlab")
library(kernlab)
bootControl <- trainControl(number = 200)
svmFit <- train(housingTraining[,-14], housingTraining[,14], method="svmRadial", tuneLength=5, trControl=bootControl, scaled=FALSE)
svmFit
predicted <- predict(svmFit$finalModel,newdata=housingTesting[,-14])
plot(housingTesting,predicted)
svmFit$finalModel

# K-MEANS CLUSTERING


km <- kmeans(housingTraining[,1:13], 1)
km
# Using our
# remaining test data, we can predict which cluster the test data will be applied to.
# We can use the clue package for testing out the k-means model, as shown here:
install.packages("clue")
library(clue)
cl_predict(km,housingTesting[,-14])
housingTesting[,14]

# DECISION TREES

# There are several decision tree packages available in R. For this example, we use
# the housing regression data. A package to produce decision trees from regression
# data is rpart.

library(rpart)
housingFit <- rpart(MDEV ~ CRIM + ZN + INDUS + CHAS + NOX + RM +
                    AGE + DIS + RAD + TAX + PRATIO + B + LSTAT, method="anova",
                    data=housingTraining)
#We can see the decision tree in a plot using the following command:
plot(housingFit)
text(housingFit, use.n=TRUE, all=TRUE, cex=.8)
# While the plotting is not great, you can follow the decision points to arrive at a
# valuation fairly easily.
# Let's generate the predicted values using the test data, as follows:
treePredict <- predict(housingFit,newdata=housingTesting)
# We will verify the correctness of the model (using the sum of squares defined in the
# previous section), as follows:
diff <- treePredict - housingTesting$MDEV
sumofsquares <- function(x) {return(sum(x^2))}
sumofsquares(diff)
# Just for comparison, this is a worse result than direct linear regression. Maybe if the
# data were non-continuous, this would be a better modeling technique.

#AdaBOOST

install.packages("ada")
library(ada)
adaModel <- ada(x=housingTraining[,-14],y=housingTraining$class,test.x=housingTesting[,-14],test.y=housingTesting$class)
adaModel

# NEURAL NETWORK

install.packages('neuralnet')
library(neuralnet)
nnet <- neuralnet(MDEV ~ CRIM + ZN + INDUS + CHAS + NOX + RM + AGE
                  + DIS + RAD + TAX + PRATIO + B + LSTAT,housingTraining, hidden=10,
                  threshold=0.01)
# However, when I originally ran this, I saw the following error:
#   Warning message:
#   algorithm did not converge in 1 of 1 repetition(s) within the stepmax
# Unfortunately, there does not appear to be any tried-and-trusted method for
# converging. It takes some adjusting, and every iteration takes several minutes,
# so it takes some patience as well.
# I ended up using a simpler function call, as shown here:
nnet <- neuralnet(MDEV ~ CRIM + ZN + INDUS + CHAS + NOX + RM + AGE +
                    DIS + RAD + TAX + PRATIO + B + LSTAT,housingTraining)
plot(nnet, rep="best")
# We evaluate the neural net performance much like the other methods, except that we
# use the compute function. as follows:
results <- compute(nnet, housingTesting[,-14])
diff <- results$net.result - housingTesting$MDEV
sumofsquares(diff)
# Unfortunately, this method is by far the worst performer among the models. I am
# assuming the data does not match up with the requirements for a neural net.


# RANDOM FORESTS

# Random forests is an algorithm where each data point is developed into a large
# number of trees (in a forest) and the results are combined for a model.
install.packages("randomForest")
library(randomForest)
forestFit <- randomForest(MDEV ~ CRIM + ZN + INDUS + CHAS + NOX + RM
                          + AGE + DIS + RAD + TAX + PRATIO + B + LSTAT, data=housingTraining)
forestPredict <- predict(forestFit,newdata=housingTesting)
diff <- forestPredict - housingTesting$MDEV
sumofsquares(diff)
# If we gather the results of the sumofsquares test from the models in the chapter, we
# come across the following findings:
#   • 3,555 from the linear regression
# • 3,926 from the decision tree
# • 11,016 from the neural net
# • 2,464 from the forest
# The forest model produced the best-fitting data.