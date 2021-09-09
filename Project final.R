data <- read.csv("drugs_data.csv")
str(data)
data$Class <- as.factor(data$Class)#I transform Class as factor
summary(data)

library(ggplot2)
ggplot(data, aes(Nscore, X)) +
  geom_point(aes(colour = Class)) +
  xlab("Nscore") +
  ylab("Number of row")

#I remove the variable X otherwise the classification would be driven mainly by this variable
#with cutoff the number of row where the data change from value 0 to value 1 of the variable Class
data <- data[, -1] 

set.seed(10)
#the data are divided in train(50%), validation(25%) and test sets(25%)
n<-nrow(data)
ind1 <- sample(c(1:n), round(n/2))
ind2 <- sample((c(1:n)[-ind1]), round(n/4))
ind3 <- setdiff(c(1:n), c(ind1, ind2))
train.data <- data[ind1,]
valid.data <- data[ind2,]
test.data <- data[ind3,]

#some exploratory analysis
library(tidyverse)

pairs(train.data, lower.panel = NULL)

#I divide the data in order to have plots between one variable vs all the others and I will also 
#distinguish data by colour based on Class value, in order to see if there is any separation 
#in the data. Instead of repeating the code many times i will create a function.
#a is the variable that will be plot versus all the others and b will be equal to variable Class
function.one <- function(a, b) { 
  gathered.data <- train.data %>%
    as_tibble() %>%
    gather(key = "variable", value = "value", -{{a}}, -{{b}})
  ggplot(gathered.data, aes(x = value, y = {{a}})) +
    geom_point(aes(color = {{b}})) +
    facet_wrap(~variable) 
}

function.one(Age, Class)
function.one(Education, Class)
function.one(X.Country, Class)
function.one(Ethnicity, Class)
function.one(Nscore, Class)
function.one(Escore, Class)
function.one(Oscore, Class)
function.one(Ascore, Class)
function.one(Cscore, Class)
function.one(Impulsive, Class)
function.one(SS, Class)



#let's start with knn
library(class)
corr.class.rate<-numeric(55)#i will check the best value of k using train data and validation data
for(k in 1:55)
{
  pred.class<-knn(train.data[,-12], valid.data[,-12], train.data[,12], k=k)
  corr.class.rate[k]<-sum((pred.class==valid.data$Class))/length(pred.class)
}
plot(c(1:55),corr.class.rate,type="l",
     main="Correct Classification Rates for the Test Data for a range of k",
     xlab="k",ylab="Correct Classification Rate",cex.main=0.7)
     abline(v = which.max(corr.class.rate), col = "red")
which.max(corr.class.rate)
#the best value for k is 30 so I'll make prediction using validation data
model.knn <- knn(train.data[, -12], valid.data[, -12], train.data[, 12], k=30)
#I calculate the correct classification rate
rateKNN <- sum((model.knn==valid.data$Class))/length(model.knn)



#Classification trees
library(rpart)
library(rpart.plot)

#I first create a single tree classification using train data
single.tree <- rpart(Class ~ ., data = train.data, method = "class")
rpart.plot(single.tree, type = 2, extra = 4)
# I validate the first tree using validation data
single.tree.valid <- predict(single.tree, valid.data, "class")
table.singletree <- table(single.tree.valid, valid.data$Class)
#I now calculate the correct classification rate of this model
CCR.singletree<- sum(diag(table.singletree))/sum(table.singletree)



library(randomForest)
#Model trees are characterised by a high variance and they can be influenced by the way the data 
#have been split. Bagging helps with this issue so I create a model with RandomForest using 
#train data
set.seed(10)
bag.forest <- randomForest(Class ~ ., data = train.data, mtry = 10, ntree = 200)
bag.forest
#I now validate this model using validation data set
bag.forest.valid <- predict(bag.forest, valid.data, "class")
table.bagforest <- table(bag.forest.valid, valid.data$Class)
#I calculate the correct classification rate of this model
CCR.bagforest <- sum(diag(table.bagforest))/sum(table.bagforest)



#I now create a model in Random Forest and see if there is a dominant predictor
set.seed(10)
random.forest <- randomForest(Class ~ ., data = train.data, ntree = 200)
random.forest
#I now validate the model using validation data set
rand.for.valid <- predict(random.forest, valid.data, "class")
table.RF <- table(rand.for.valid, valid.data$Class)
#I calculate the error rate for this model
CCR.randforest <- sum(diag(table.RF))/sum(table.RF)



library(forcats)
library(caret)

#The following graph is for comparing the two model Bagging and Random Forest
rf_tb <- tibble(var = rownames(importance(random.forest)),
                    `Random Forest` = importance(random.forest)[,1]) %>%
  left_join(tibble(var = rownames(importance(random.forest)),
                       Bagging = importance(bag.forest)[,1])) %>%
  mutate(var = fct_reorder(var, Bagging)) %>%
  gather(model, gini, -var)

ggplot(rf_tb,aes(var, gini, color = model, shape=model)) +
  geom_point() +
  coord_flip() +
  labs(title = "Predicting use of drugs",
       x = NULL,
       y = "Average decrease in the Gini Index",
       color = "Method",shape="Model")



#SVM
library(e1071)

#I start with a linear SVM. I create the model using train set. It takes few seconds to run.
tune.linear <- tune.svm(Class~., data = train.data, type = "C-classification", kernel = "linear",
                        cost = c(0.001, 0.05, 0.01, 0.1, 1, 10, 100))
summary(tune.linear)
tune.linear$best.model #this is the model performing best
#I validate the model using validation set
svm.linear <- predict(tune.linear$best.model, newdata = valid.data)
#I calculate the error rate for the model
CCR.linear<- sum(diag(table(svm.linear, valid.data$Class)))/length(svm.linear)



#The second model I create is a polynomial SVM using certain values of C, Gamma and Coef0
#It takes few seconds to run
tune.poly.1 <- tune.svm(Class ~ ., data = train.data, type = "C-classification", 
                      kernel = "polynomial",cost = c(0.001, 0.01, 0.1),
                      gamma = c(1, 3, 5), degree = 2,
                      coef0 = c(0, 1, 2))
summary(tune.poly.1)
#I validate this model using validation set
svm.poly.1 <- predict(tune.poly.1$best.model, newdata = valid.data)
#I calculate the error rate
CCR.poly.1 <- sum(diag(table(svm.poly.1, valid.data$Class)))/length(svm.poly.1)



#I now try a second polynomial SVM using different parameters 
#It takes few seconds to run
tune.poly.2 <- tune.svm(Class ~ ., data = train.data, type = "C-classification", 
                      kernel = "polynomial",cost = c(10, 100),
                      gamma = c(0.01, 0.05, 0.1), degree = 2,
                      coef0 = c(3, 4, 5))
summary(tune.poly.2)
#I validate the model with the validation set
svm.poly.2 <- predict(tune.poly.2$best.model, newdata = valid.data)
#I calculate the error rate
CCR.poly.2 <- sum(diag(table(svm.poly.2, valid.data$Class)))/length(svm.poly.2)



#I finally create a Radial SVM using train data
#It takes few more seconds to run than the previous models
tune.radial <- tune.svm(Class ~ ., data = train.data, type = "C-classification",
                        kernel = "radial", cost = c(0.01, 0.1, 1, 10, 100),
                        gamma = c(0,5, 1, 2, 3, 4), coef0 = c(0, 1, 2, 3, 4, 5))
summary(tune.radial)
#I evaluate the model with evaluation set
svm.radial <- predict(tune.radial$best.model, newdata = valid.data)
#I calculate the error rate
CCR.radial <- sum(diag(table(svm.radial, valid.data$Class)))/length(svm.radial)



#I will now choose the best SVM models to use it to make a plot and have a visualisation of 
#classification
SVM.matrix <- matrix(c(CCR.linear, CCR.poly.1, CCR.poly.2, CCR.radial), 4, 1)
rownames(SVM.matrix) <- c("SVM Linear", "SVM Polynomial1", "SVM Polynomial2", "SVM Radial")
SVM.matrix



#SVM Linear has the highest Correct Classification Rate so It will be used to create a plot
plot(tune.linear$best.model, train.data, Oscore~Ascore)
SVM.linear.table <- (table(valid.data$Class, svm.linear))
SVM.linear.rates <- sweep(SVM.linear.table, 1, apply(SVM.linear.table, 1, sum), "/")
SVM.linear.rates



#I now compare the Correct Classification Rate of all models. I will choose the one with the 
#highest Correct Classification rate and I will use it to make prediction using test data set.
CCR.matrix <- matrix(c(rateKNN, CCR.singletree, CCR.bagforest, CCR.randforest, CCR.linear, 
                       CCR.poly.1, CCR.poly.2, CCR.radial), 8, 1)
rownames(CCR.matrix) <- c("kNN", "Single Tree", "Bagging", "Random Forest","SVM Linear",
                          "SVM Polynomial1", "SVM Polynomial2", "SVM Radial")
round(CCR.matrix, 4)



#I choose kNN model because it has the highest correct classification rate and I am going to 
#make prediction on it using test data set
set.seed(10)
predict.test <-  knn(train.data[, -12], test.data[, -12], train.data[, 12], k=30)
table.test <- table(test.data$Class, predict.test)
#I calculate the test rate
rate.kNN <- sum(diag(table.test))/length(test.data$Class)
rate.kNN 

cross.class.rate <- sweep(table.test, 1, apply(table.test, 1, sum), "/")
sensitivity <- cross.class.rate[2,2]
specificity <- cross.class.rate[1,1]
sensitivity
specificity
#sensitivity and specificity are quite high and both close to 80%. In this case it means that
#the model classify well both people who never used or used at some point drugs with a little
#higher misclassification error for those who never used drugs.