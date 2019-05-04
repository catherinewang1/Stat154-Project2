#Run this -----------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(caret)
library(class)
library(e1071)
goodFeatures = c("NDAI", "RadianceAngleBF", "SD")
subset <- function(dat, fraction) {
  nrows = dim(dat)[1]
  dat[sample(nrows, fraction*nrows),]
}
loss1 <- function(x,y) {mean(x != y)}
CVgeneric_Optimized <- function(generic_classifier, 
  training_features, training_labels, K, lossFunction, 
  mapIndices, hyperparameter = NULL, knn = FALSE, kernel = "linear") {
  #for each fold, find the loss
  set.seed(154)
  losses = c()
  folds = createFolds(1:nrow(d), k=K)
  for(k in 1:K) {
    cat(paste0(paste(k, hyperparameter, sep = "->"), '\n'))
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
    } else if (!is.null(hyperparameter)) {
      mod_fit = generic_classifier(temp_features_train, temp_labeled_train, hyperparameter, kernel = kernel)
      predicted = predict(mod_fit, newdata=temp_features_val)
    } else {
      mod_fit = generic_classifier(temp_features_train, temp_labeled_train)
      predicted = predict(mod_fit, newdata=temp_features_val)
    }
    loss = lossFunction(predicted, temp_labeled_val)
    losses = c(losses, loss)
  }
  losses
}
#function to get CV for classifiers with hyperparameter
getCVhyperparam <- function(generic_classifier, dataInput, 
        mapIndices, hyperparameter, knn = FALSE, kernel = "linear") {
  rownames(dataInput) = NULL
  training_features = dataInput[, 5:12]
  training_labels = as.factor(dataInput$expert_label)
  hyperparameter %>% sapply(function(h) {
    CVgeneric_Optimized(generic_classifier, 
    training_features, training_labels, K, loss1, mapIndices, h, knn, kernel = kernel)
  })
}
summarizeData <- function(dat, hyperparameters, hyperparamName, K) {
  hyperparamerror = dat %>% apply(2, mean)
  indexminerror = which.min(hyperparamerror)
  list(bestparamcverror = dat[,indexminerror], meanerror = hyperparamerror, besthyperparam = hyperparameters[indexminerror])
}
#-------------------------------------

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
if (TRUE) {
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
  
  #get mode function from: https://www.tutorialspoint.com/r/r_mean_median_mode.htm
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  ## Make the data smaller
  method1 = rbind(method1_train, method1_val)
  method1$x10 = floor(method1$x / 5) * 5 
  method1$y10 = floor(method1$y / 5) * 5
  
  method1_small_train = method1 %>% group_by(imageNum, x10, y10) %>%
    summarise(expert_label = getmode(expert_label), NDAI = mean(NDAI), SD = mean(SD), CORR = mean(CORR),
              RadianceAngleDF = mean(RadianceAngleDF), RadianceAngleCF = mean(RadianceAngleCF),
              RadianceAngleBF = mean(RadianceAngleBF), RadianceAngleAF = mean(RadianceAngleAF),
              RadianceAngleAN = mean(RadianceAngleAN))
  method1_small_train = data.frame(method1_small_train)
  
  method2 = rbind(method2_train, method2_val)
  method2$x10 = floor(method2$x / 20) * 20
  method2$y10 = floor(method2$y / 20) * 20
  
  method2_small_train = method2 %>% group_by(imageNum, x10, y10) %>%
    summarise(expert_label = getmode(expert_label), NDAI = mean(NDAI), SD = mean(SD), CORR = mean(CORR),
              RadianceAngleDF = mean(RadianceAngleDF), RadianceAngleCF = mean(RadianceAngleCF),
              RadianceAngleBF = mean(RadianceAngleBF), RadianceAngleAF = mean(RadianceAngleAF),
              RadianceAngleAN = mean(RadianceAngleAN))
  method2_small_train = data.frame(method2_small_train)
  
  #svm_mod = svm(as.factor(expert_label) ~ ., data =  method1_small_train[,4:12], cost = .001)
  #svm_testLoss1 = mean(predict(svm_mod, newdata = method1_test[,5:12]) != method1_test$expert_label)
  
  ## SVM CV with K=10
  if(TRUE) {
    dat = method1_small_train
    dat = dat[(dat$expert_label != 0), ]
    
    dat$x10 = floor(dat$x10 / 40)* 40
    dat$y10 = floor(dat$y10 / 40)* 40
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
    dat = method2_small_train
    dat = dat[(dat$expert_label != 0), ]
    
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
}
K = 10


# KNN-------------------------------------------------
generic_knn <- function(trainx, testx, trainy, k) {
  knn(trainx, testx, trainy, k)
}
k = 1:10
#get knn CV Folds losses (inaccuracy)
ptm <- proc.time()
cvKknn1 = getCVhyperparam(generic_knn, method1_small_train, Method1MapIndices, k, TRUE) #K x k
cvKknn2 = getCVhyperparam(generic_knn, method2_small_train, Method1MapIndices, k, TRUE) #K x k
knn_ptm = proc.time() - ptm

summaryknn1 = summarizeData(cvKknn1, k, "K", K)
ggplot() + geom_line(aes(x = k, y = summaryknn1$meanerror)) + labs(title = "CV Error Across K for KNN (Method 1)")
summaryknn2 = summarizeData(cvKknn2, k, "K", K)
ggplot() + geom_line(aes(x = k, y = summaryknn2$meanerror)) + labs(title = "CV Error Across K for KNN (Method 2)")

#get knn Test loss (inaccuracy)
knn_testPred1 = knn(method1_small_train[, 5:12], method1_test[, 5:12], 
                    method1_small_train$expert_label, summaryknn1$besthyperparam)
knn_testLoss1 = loss1(knn_testPred1, method1_test$expert_label)

knn_testPred2 = knn(method2_small_train[, 5:12], method2_test[, 5:12], 
                    method2_small_train$expert_label, summaryknn2$besthyperparam)
knn_testLoss2 = loss1(knn_testPred2, method2_test$expert_label)

#total CV results
CVresultsknn = data.frame(CVFold = c("Test","Average Folds", 1:K),
                          knn1 = c(knn_testLoss1, mean(summaryknn1$bestparamcverror), summaryknn1$bestparamcverror),
                          knn2 = c(knn_testLoss2, mean(summaryknn2$bestparamcverror), summaryknn2$bestparamcverror))
names(CVresultsknn) = c("Data/CV Fold", "knn Method 1", "knn Method 2")
write.csv(CVresultsMethod, "CVresults/CVknn.csv", row.names = FALSE)
# -------------------------------------------------


# 4a-----------------------------------------------
generic_svm <- function(X, y, hyperparameter, kernel = "linear") {
  y = factor(y)
  data = cbind(X,y)
  if (kernel == "linear") {
    svm(y ~ ., data = data, kernel = kernel, cost = hyperparameter)
  } else if (kernel == "radial") {
    svm(y ~ ., data = data, kernel = kernel, gamma = hyperparameter)
  } else {
    svm(y ~ ., data = data, kernel = kernel, degree = hyperparameter)
  }
}
C = seq(0.01, 100, length.out = 20)
training1 = method1_small_train
training1$expert_label = factor(training1$expert_label)
cvcostsvm1 = getCVhyperparam(generic_svm, training1, Method1MapIndices, C)
summarysvm1 = summarizeData(cvcostsvm1, C, "C", K)
ggplot() + geom_line(aes(x = C, y = summarysvm1$meanerror)) + 
      labs(title = "CV Error Across Cost for SVM (Method 1)", x = "Cost", y = "Mean CV error")

svm_mod1 = svm(expert_label ~ ., data = training1[4:12], cost = summarysvm1$besthyperparam)
test1 = method1_test
test1$expert_label = factor(test1$expert_label)
labhat1 = predict(svm_mod1, newdata = test1[,5:12])
svm_testAccuracy1 = mean(labhat1 == test1$expert_label)

plot(svm_mod1, subset(test1, 1/10), CORR ~ NDAI)
dev.off()
plot(svm_mod1, subset(test1, 1/10), CORR ~ RadianceAngleDF)
plot(svm_mod1, subset(test1, 1/10), NDAI ~ RadianceAngleAF)
# -------------------------------------------------
# 4b-----------------------------------------------
result1 = 1:length(labhat1) %>% sapply(function(i) {
    yhat = labhat1[i]
    y = method1_test$expert_label[i]
    if (yhat == y) {
      "Correct"
    } else if (yhat == 1 && y == -1) {
      "False Positive"
    } else {
      "False Negative"
    }
  })
data1 = cbind(result1, method1_test[5:12])
data1 = subset(data1, 1/8)
data1 %>%
  gather(-result1, key = "Features", value = "value") %>%
  ggplot(aes(x = value, y = result1)) + geom_point(aes(alpha = 0.5)) +
  facet_wrap(~ Features, scales = "free") +
  labs(title = "Features vs. Prediction Results (Method 1)", 
       y = "Correctness of Predictions")
# -------------------------------------------------
# 4c-----------------------------------------------
deg = 2:4
cvpolysvm = getCVhyperparam(generic_svm, training1, Method1MapIndices, hyperparameter = deg, kernel = "polynomial")
summarypolysvm = summarizeData(cvpolysvm, deg, "Degree", K)
ggplot() + geom_line(aes(x = deg, y = summarypolysvm$meanerror)) + 
  labs(title = "CV Error Across Degree for SVM with Polynomial Kernel", x = "Degree", y = "Mean CV error")

polymod = svm(expert_label ~ ., data = training1[4:12], degree = summarypolysvm$besthyperparam, kernel = "polynomial")
labhatpoly = predict(polymod, newdata = test1[,5:12])
svmpolyAccuracy = mean(labhatpoly == test1$expert_label)


gamma = 1:20
cvradialsvm = getCVhyperparam(generic_svm, training1, Method1MapIndices, hyperparameter = gamma, kernel = "radial")
summaryradialsvm = summarizeData(cvradialsvm, gamma, "Gamma", K)
ggplot() + geom_line(aes(x = gamma, y = summaryradialsvm$meanerror)) + 
  labs(title = "CV Error Across Gamma for SVM with Radial Kernel", x = "Gamma", y = "Mean CV error")

radialmod = svm(expert_label ~ ., data = training1[4:12], gamma = summaryradialsvm$besthyperparam)
labhatradial = predict(radialmod, newdata = test1[,5:12])
svmradialAccuracy = mean(labhatradial == test1$expert_label)
# -------------------------------------------------
# 4d-----------------------------------------------
training2 = method2_small_train
training2$expert_label = factor(training2$expert_label)
cvcostsvm2 = getCVhyperparam(generic_svm, training2, Method2MapIndices, C)
summarysvm2 = summarizeData(cvcostsvm2, C, "C", K)
ggplot() + geom_line(aes(x = C, y = summarysvm2$meanerror)) + 
  labs(title = "CV Error Across C for SVM (Method 2)", x = "Cost", y = "Mean CV error")

svm_mod2 = svm(expert_label ~ ., data = training2[4:12], cost = summarysvm2$besthyperparam)
test2 = method2_test
test2$expert_label = factor(test2$expert_label)
labhat2 = predict(svm_mod2, newdata = test2[,5:12])
svm_testAccuracy2 = mean(labhat2 == test2$expert_label)

plot(svm_mod2, subset(test2, 1/10), CORR ~ NDAI)
dev.off()
plot(svm_mod2, subset(test2, 1/10), CORR ~ RadianceAngleDF)
plot(svm_mod2, subset(test2, 1/10), NDAI ~ RadianceAngleAF)

result2 = 1:length(labhat2) %>% sapply(function(i) {
  yhat = labhat2[i]
  y = method2_test$expert_label[i]
  if (yhat == y) {
    "Correct"
  } else if (yhat == 1 && y == -1) {
    "False Positive"
  } else {
    "False Negative"
  }
})
data2 = cbind(result2, method2_test[5:12])
data2 = subset(data2, 1/8)
data2 %>%
  gather(-result2, key = "Features", value = "value") %>%
  ggplot(aes(x = value, y = result2)) + geom_point(aes(alpha = 0.5)) +
  facet_wrap(~ Features, scales = "free") +
  labs(title = "Features vs. Prediction Results (Method 2)", 
       y = "Correctness of Predictions")
# -------------------------------------------------