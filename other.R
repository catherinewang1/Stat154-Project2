# Question 3

#----------------------------------------------------------------------------
#-------------------------- Question 3a -------------------------------------
#----------------------------------------------------------------------------


## ----------------------------------------------------------------------------
## Some optimization
## ----------------------------------------------------------------------------
## CV generic takes a lot of time to run, most of it is from splitting the data 
# into squares, to speed up computation, we save the list of squares from both
# METHOD 1 and METHOD 2 into intermediate MapIndices objects
if(TRUE) {
  
  method1_train = read.csv("data/train.csv")
  method1_train = method1_train[(method1_train$expert_label) != 0, ]
  method1_val = read.csv("data/validation.csv")
  method1_val = method1_val[(method1_val$expert_label) != 0, ]
  method1_test = read.csv("data/test.csv")
  method1_test = method1_test[(method1_test$expert_label) != 0, ]
  
  method2_train = rbind(read.csv("data/train2.csv"), read.csv("data/train3.csv"))
  method2_train = method2_train[(method2_train$expert_label != 0), ]
  method2_val = rbind(read.csv("data/validation2.csv"), read.csv("data/validation3.csv"))
  method2_val = method2_val[(method2_val$expert_label != 0), ]
  method2_test = read.csv("data/image1.csv")
  method2_test = method2_test[(method2_test$expert_label != 0), ]
  # METHOD 1: Get Method1MapIndices (to speed computation)
  #divide the data into squares by METHOD 1
  train = read.csv("data/train.csv")
  validation = read.csv("data/validation.csv")
  test = read.csv("data/test.csv")
  combined = rbind(train, validation)
  rownames(method1_train) = NULL
  dat = method1_train
  dat = train[(train$expert_label != 0), ]
  
  dat$x10 = floor(dat$x / 40)* 40
  dat$y10 = floor(dat$y / 40)* 40
  d = dat %>% group_by(imageNum, x10, y10) %>% summarise(count = n())
  mapIndices <- vector("list", nrow(d))
  z = vector("list", nrow(d))
  for(i in 1:nrow(d)) {
    imageNumval = d$imageNum[i]
    xval = d$x10[i]
    yval = d$y10[i]
    key = paste0(xval,",", yval)
    indices = which(dat$imageNum == imageNumval & dat$x10 == xval & dat$y10 == yval)
    mapIndices[[i]] <- indices
  }
  Method1MapIndices = mapIndices
  
  
  
  # METHOD 2: Get Method2MapIndices (to speed computation)
  #divide the data into squares by METHOD 2
  image2 = read.csv("data/image2.csv")
  image3 = read.csv("data/image3.csv")
  method2_train = rbind(image2, image3)
  rownames(method2_train) = NULL
  dat = method2_train
  dat = method2_train[(method2_train$expert_label != 0), ]
  
  dat$x10 = floor(dat$x / 40)* 40
  dat$y10 = floor(dat$y / 40)* 40
  d = dat %>% group_by(imageNum, x10, y10) %>% summarise(count = n())
  mapIndices <- vector("list", nrow(d))
  z = vector("list", nrow(d))
  for(i in 1:nrow(d)) {
    imageNumval = d$imageNum[i]
    xval = d$x10[i]
    yval = d$y10[i]
    key = paste0(xval,",", yval)
    indices = which(dat$imageNum == imageNumval & dat$x10 == xval & dat$y10 == yval)
    mapIndices[[i]] <- indices
  }
  Method2MapIndices = mapIndices
}


CVgeneric_Optimized <- function(generic_classifier, training_features, training_labels, K, lossFunction, mapIndices) {
  #for each fold, find the loss
  set.seed(154)
  losses = c()
  folds = createFolds(1:nrow(d), k=K)
  for(k in 1:K) {
    CVtrain = unlist(mapIndices[-folds[[k]]])
    CVvalid = unlist(mapIndices[folds[[k]]])
    
    #get the inputs for the classifier
    temp_features_val = training_features[CVvalid, ]
    temp_features_train = training_features[CVtrain, ]
    
    temp_labeled_val = training_labels[CVvalid]
    temp_labeled_train = training_labels[CVtrain]
    
    #run the classifier
    mod_fit = generic_classifier(temp_features_train, temp_labeled_train)
    predicted = predict(mod_fit, newdata=temp_features_val)
    loss = lossFunction(predicted, temp_labeled_val)
    losses = c(losses, loss)
  }
  #return
  return(losses)
}

K = 10

## ----------------------------------------------------------------------------
## SVM
## ----------------------------------------------------------------------------
generic_svm <- function(X, y) {
  total_dat = cbind(X, y)
  return(svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE))
}
loss_svm <- function(yhat, y) {
  if("class" %in% names(yhat)) {
    return(mean(yhat$class != y)) 
  } else {
    return(mean(yhat != y))
  }
}
#function to get CV for qda classification
getCVsvm <- function(dataInput, mapIndices) {
  dat = dataInput[(dataInput$expert_label != 0), ]
  rownames(dat) = NULL
  
  training_features = dat[,5:12]
  training_labels = as.factor(dat$expert_label)
  
  CVgeneric_Optimized(generic_svm, training_features, training_labels, K, loss_svm, 
                      mapIndices)
}
#get svm CV Folds losses (inaccuracy)
ptm <- proc.time()
svmLossesMethod1 = getCVsvm(method1_train, Method1MapIndices)
svmLossesMethod2 = getCVsvm(method2_train, Method2MapIndices)
svm_ptm = proc.time() - ptm

#get svm Test loss (inaccuracy)
svm_testMod = svm(as.factor(expert_label) ~ ., data =  method1_train[,4:12])
svm_testLoss1 = mean(predict(svm_testMod, newdata = method1_test[,5:12]) != method1_test$expert_label)

svm_testMod = svm(as.factor(expert_label) ~ ., data =  method2_train[,4:12])
svm_testLoss2 = mean(predict(svm_testMod, newdata = method2_test[,5:12])$class != method2_test$expert_label)

#total CV results
CVresultsSVM = data.frame(CVFold = c("Test","Average Folds", 1:K),
                          svm1 = c(svm_testLoss1, mean(svmLossesMethod1), svmLossesMethod1),
                          svm2 = c(svm_testLoss2, mean(svmLossesMethod2), svmLossesMethod2))
names(CVresultsSVM) = c("Data/CV Fold", "SVM Method 1", "SVM Method 2")
write.csv(CVresultsSVM, "CVresults/CVsvm.csv", row.names = FALSE)


#Run this -----------------------------
library(ggplot2)
library(dplyr)
library(caret)
library(class)
library(e1071)
goodFeatures = c("NDAI", "RadianceAngleBF", "SD")
subset <- function(dat, fraction) {
  nrows = dim(dat)[1]
  dat[sample(nrows, fraction*nrows),]
}
loss1 <- function(x,y) {mean(x != y)}
#-------------------------------------
# KNN-------------------------------------------------
CVgeneric_Optimized <- function(generic_classifier, training_features, training_labels, K, lossFunction, mapIndices, hyperparameter, knn = FALSE) {
  #for each fold, find the loss
  set.seed(154)
  losses = c()
  folds = createFolds(1:nrow(d), k=K)
  for(k in 1:K) {
    CVtrain = unlist(mapIndices[-folds[[k]]])
    CVvalid = unlist(mapIndices[folds[[k]]])
    
    #get the inputs for the classifier
    temp_features_val = training_features[CVvalid, ]
    temp_features_train = training_features[CVtrain, ]
    
    temp_labeled_val = training_labels[CVvalid]
    temp_labeled_train = training_labels[CVtrain]
    #run the classifier
    if (knn) {
      predicted = generic_classifier(temp_features_train, temp_features_val, temp_labeled_train, hyperparameter)
    } else {
      mod_fit = generic_classifier(temp_features_train, temp_labeled_train)
      predicted = predict(mod_fit, newdata=temp_features_val)
    }
    loss = lossFunction(predicted, temp_labeled_val)
    losses = c(losses, loss)
  }
  losses
}


generic_knn <- function(trainx, testx, trainy, k) {
  knn(trainx, testx, trainy, k)
}

#function to get CV for knn classification
getCVknn <- function(dataInput, mapIndices, hyperparameter) {
  rownames(dataInput) = NULL
  
  training_features = dataInput[, 5:12]
  training_labels = as.factor(dataInput$expert_label)
  
  hyperparameter %>% sapply(function(h) CVgeneric_Optimized(generic_knn, training_features, training_labels, K, loss1, 
                                                  mapIndices, h, knn))
}
#get knn CV Folds losses (inaccuracy)
ptm <- proc.time()
knnLossesMethod1 = getCVknn(method1_train, Method1MapIndices) #K x k
knnLossesMethod2 = getCVknn(method2_train, Method2MapIndices)
knn_ptm = proc.time() - ptm

#get knn Test loss (inaccuracy)

test = method1_test
train = subset(rbind(method1_train, method1_val), 1/2)
knn_testPred1 = knn(train[, 5:12], test[, 5:12], train$expert_label, )
knn_testLoss1 = loss1(knn_testPred1, test$expert_label)

test = method2_test[1:dim(method2_test)[1]/1e2, ]
train = method2_train[1:dim(method2_train)[1]/1e2, ]
knn_testPred2 = knn(train[, names(train)!= "expert_label"], test, train$expert_label, )
knn_testLoss2 = loss1(knn_testPred2, test$expert_label)

#total CV results
CVresultsknn = data.frame(CVFold = c("Test","Average Folds", 1:K),
                          knn1 = c(knn_testLoss1, mean(knnLossesMethod1), knnLossesMethod1),
                          knn2 = c(knn_testLoss2, mean(knnLossesMethod2), knnLossesMethod2))
names(CVresultsknn) = c("Data/CV Fold", "knn Method 1", "knn Method 2")
write.csv(CVresultsMethod, "CVresults/CVknn.csv", row.names = FALSE)
# -------------------------------------------------


# 4a-----------------------------------------------
train$expert_label = factor(train$expert_label)
set.seed(1)
cv = tune(svm, expert_label ~ . , data=train[, 4:12], kernel ="linear",
            ranges = list(cost=seq(0.01, 100, length.out = 5) ))
performances = cv10$performances
ggplot(performances, aes(x = cost, y = error)) + geom_line() + labs(title = "Cross-Validation Error Across Cost")
bestfit = cv10$best.model
yhat = predict(bestfit, test)
labhat = ifelse(yhat > 0.5, 1, -1)
loss1(labhat, test$expert_label)
# -------------------------------------------------
# 4b-----------------------------------------------
correct = labhat == test$expert_label
data.frame(correct, test[, 5:12]) %>%
  gather(-correct, key = "Feature", value = "Value") %>%
  ggplot(aes(x = Value, y = correct)) +
  geom_point() +
  facet_wrap(~ Feature, scales = "free")
# -------------------------------------------------