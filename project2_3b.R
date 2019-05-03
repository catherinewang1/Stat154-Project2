library(ggplot2)
library(dplyr)
library(gridExtra)
library(GGally)
library(caret)
library(hash)
library(MASS)
library(pROC)
library(e1071)

method1_train = read.csv("data/train.csv")
method1_train = method1_train[(method1_train$expert_label) != 0, ]
method1_val = read.csv("data/validation.csv")
method1_val = method1_val[(method1_val$expert_label) != 0, ]
method1_test = read.csv("data/test.csv")
method1_test = method1_test[(method1_test$expert_label) != 0, ]

method2_train = rbind(read.csv("data/train2.csv"), read.csv("data/train3.csv"))
method2_train = method2_train[(method2_train$expert_label != 0), ]
method2_val = rbind(read.csv("data/validation2.csv"), read.csv("data/validation3.csv"))
metho2_val = method2_val[(method2_val$expert_label != 0), ]
method2_test = read.csv("data/image1.csv")
method2_test = method2_test[(method2_test$expert_label != 0), ]



getFPRandTPR <- function(yprob, y, cutoffs) {
  #returns a dataframe with the cutoffs, False Positive, True Positive
  # consider 1 as Positive, -1 as Negative
  FP = c()
  TP = c()
  for(i in 1:length(cutoffs)) {
    yhat = as.integer(yprob > cutoffs[i])
    TP = c(TP, sum((yhat == 1) & (y == 1)))
    FP = c(FP, sum((yhat == 1) & (y != 1)))
  }
  P = sum(y == 1) #number of real positive cases 
  N = sum(y == -1) #number of real negative cases
  return(data.frame(cutoffs = cutoffs,
                    FPR = FP/N,
                    TPR = TP/P))
}

getDistCorner <- function(ROC_df) {
  #gets distance from the corner (0,1)
  return(sqrt((ROC_df$FPR - 0)^2 + (ROC_df$TPR - 1)^2))
}


cutoffs = seq(0, 1, .005)
## Logistic
## ----------------------------------------------------------------------------
log_mod1 = train(as.factor(expert_label) ~ ., data =  method1_train[,4:12], method="glm", family="binomial")
log_mod1_predicted = predict(log_mod1, type = "prob", newdata = method1_test[,5:12])
log_ROC_1 = getFPRandTPR(log_mod1_predicted["1"], method1_test$expert_label, cutoffs)
index_min = which.min(getDistCorner(log_ROC_1))
ROC_log1 = ggplot(log_ROC_1, aes(x = FPR, y = TPR)) + geom_line()+
       annotate("point", x = log_ROC_1$FPR[index_min], y = log_ROC_1$TPR[index_min], colour = "blue") +
       labs(title=paste0("ROC: Logistic Regression (Method 1) / Best Cutoff:",log_ROC_1$cutoff[index_min])) 

log_mod2 = train(as.factor(expert_label) ~ ., data =  method2_train[,4:12], method="glm", family="binomial")
log_mod2_predicted = predict(log_mod2, type = "prob", newdata = method2_test[,5:12])
log_ROC_2 = getFPRandTPR(log_mod2_predicted["1"], method2_test$expert_label, cutoffs)
index_min = which.min(getDistCorner(log_ROC_2))
ROC_log2 = ggplot(log_ROC_2, aes(x = FPR, y = TPR)) + geom_line()+
  annotate("point", x = log_ROC_2$FPR[index_min], y = log_ROC_2$TPR[index_min], colour = "blue") +
  labs(title=paste0("ROC: Logistic Regression (Method 2) / Best Cutoff:",log_ROC_2$cutoff[index_min])) 

## LDA
## ----------------------------------------------------------------------------
lda_mod1 = lda(as.factor(expert_label) ~ ., data =  method1_train[,4:12])
lda_mod1_predicted = predict(lda_mod1, type = "prob", newdata = method1_test[,5:12])$posterior
lda_ROC_1 = getFPRandTPR(lda_mod1_predicted[,"1"], method1_test$expert_label, cutoffs)
index_min = which.min(getDistCorner(lda_ROC_1))
ROC_lda1 = ggplot(lda_ROC_1, aes(x = FPR, y = TPR)) + geom_line()+
  annotate("point", x = lda_ROC_1$FPR[index_min], y = lda_ROC_1$TPR[index_min], colour = "blue") +
  labs(title=paste0("ROC: LDA (Method 1) / Best Cutoff:",lda_ROC_1$cutoff[index_min])) 

lda_mod2 = lda(as.factor(expert_label) ~ ., data =  method2_train[,4:12])
lda_mod2_predicted = predict(lda_mod2, type = "prob", newdata = method2_test[,5:12])$posterior
lda_ROC_2 = getFPRandTPR(lda_mod2_predicted[,"1"], method2_test$expert_label, cutoffs)
index_min = which.min(getDistCorner(lda_ROC_2))
ROC_lda2 = ggplot(lda_ROC_2, aes(x = FPR, y = TPR)) + geom_line()+
  annotate("point", x = lda_ROC_2$FPR[index_min], y = lda_ROC_2$TPR[index_min], colour = "blue") +
  labs(title=paste0("ROC: LDA (Method 2) / Best Cutoff:",lda_ROC_2$cutoff[index_min])) 

## QDA
## ----------------------------------------------------------------------------
qda_mod1 = qda(as.factor(expert_label) ~ ., data =  method1_train[,4:12])
qda_mod1_predicted = predict(qda_mod1, type = "prob", newdata = method1_test[,5:12])$posterior
qda_ROC_1 = getFPRandTPR(qda_mod1_predicted[,"1"], method1_test$expert_label, cutoffs)
index_min = which.min(getDistCorner(qda_ROC_1))
ROC_qda1 = ggplot(qda_ROC_1, aes(x = FPR, y = TPR)) + geom_line()+
  annotate("point", x = qda_ROC_1$FPR[index_min], y = qda_ROC_1$TPR[index_min], colour = "blue") +
  labs(title=paste0("ROC: QDA (Method 1) / Best Cutoff:",qda_ROC_1$cutoff[index_min])) 

qda_mod2 = qda(as.factor(expert_label) ~ ., data =  method2_train[,4:12])
qda_mod2_predicted = predict(qda_mod2, type = "prob", newdata = method2_test[,5:12])$posterior
qda_ROC_2 = getFPRandTPR(qda_mod2_predicted[,"1"], method2_test$expert_label, cutoffs)
index_min = which.min(getDistCorner(qda_ROC_2))
ROC_qda2 = ggplot(qda_ROC_2, aes(x = FPR, y = TPR)) + geom_line()+
  annotate("point", x = qda_ROC_2$FPR[index_min], y = qda_ROC_2$TPR[index_min], colour = "blue") +
  labs(title=paste0("ROC: QDA (Method 2) / Best Cutoff:",qda_ROC_2$cutoff[index_min])) 

## SVM
## ----------------------------------------------------------------------------
#NOTE: SVM doesn't give probabilities, but we set the Cost value to different values and determine the
#      resulting model's TPR and FPR. In addition, SVM takes a long time, so we train on the reduced
#      data set where an original 10x10 pixel square creates a new data unit
## 
## -------------------------Make the data smaller---------------------------------------------------
method1_train = read.csv("data/train.csv")
method1_train = method1_train[(method1_train$expert_label) != 0, ]
method1_val = read.csv("data/validation.csv")
method1_val = method1_val[(method1_val$expert_label) != 0, ]
method1_test = read.csv("data/test.csv")
method1_test = method1_test[(method1_test$expert_label) != 0, ]

method2_train = rbind(read.csv("data/train2.csv"), read.csv("data/train3.csv"))
method2_train = method2_train[(method2_train$expert_label != 0), ]
method2_val = rbind(read.csv("data/validation2.csv"), read.csv("data/validation3.csv"))
metho2_val = method2_val[(method2_val$expert_label != 0), ]
method2_test = read.csv("data/image1.csv")
method2_test = method2_test[(method2_test$expert_label != 0), ]

#get mode function from: https://www.tutorialspoint.com/r/r_mean_median_mode.htm
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

method1 = rbind(method1_train, method1_val)
method1$x10 = floor(method1$x / 10) * 10
method1$y10 = floor(method1$y / 10) * 10

method1_small_train = method1 %>% group_by(imageNum, x10, y10) %>%
  summarise(expert_label = getmode(expert_label), NDAI = mean(NDAI), SD = mean(SD), CORR = mean(CORR),
            RadianceAngleDF = mean(RadianceAngleDF), RadianceAngleCF = mean(RadianceAngleCF),
            RadianceAngleBF = mean(RadianceAngleBF), RadianceAngleAF = mean(RadianceAngleAF),
            RadianceAngleAN = mean(RadianceAngleAN))
method1_small_train = data.frame(method1_small_train)


method2 = rbind(method2_train, method2_val)
method2$x10 = floor(method2$x / 10) * 10
method2$y10 = floor(method2$y / 10) * 10

method2_small_train = method2 %>% group_by(imageNum, x10, y10) %>%
  summarise(expert_label = getmode(expert_label), NDAI = mean(NDAI), SD = mean(SD), CORR = mean(CORR),
            RadianceAngleDF = mean(RadianceAngleDF), RadianceAngleCF = mean(RadianceAngleCF),
            RadianceAngleBF = mean(RadianceAngleBF), RadianceAngleAF = mean(RadianceAngleAF),
            RadianceAngleAN = mean(RadianceAngleAN))
method2_small_train = data.frame(method2_small_train)

## -----------------------CREATE ROC CURVE-----------------------------------------------------
svm_mod1 = svm(as.factor(expert_label) ~ ., data =  method1_small_train[,4:12], probability=TRUE)
svm_mod1_predictObject = predict(svm_mod1, newdata = method1_test[,5:12], probability=TRUE)
svm_mod1_predicted = attributes(svm_mod1_predictObject)[["probabilities"]][,"1"]
svm_ROC_1 = getFPRandTPR(svm_mod1_predicted, method1_test$expert_label, cutoffs)
index_min = which.min(getDistCorner(svm_ROC_1))
ROC_svm1 = ggplot(svm_ROC_1, aes(x = FPR, y = TPR)) + geom_line()+
  annotate("point", x = svm_ROC_1$FPR[index_min], y = svm_ROC_1$TPR[index_min], colour = "blue") +
  labs(title=paste0("ROC: SVM (Method 1) / Best Cutoff:",svm_ROC_1$cutoff[index_min])) 

svm_mod2 = svm(as.factor(expert_label) ~ ., data =  method2_small_train[,4:12], probability=TRUE)
svm_mod2_predictObject = predict(svm_mod2, newdata = method2_test[,5:12], probability=TRUE)
svm_mod2_predicted = attributes(svm_mod2_predictObject)[["probabilities"]][,"1"]
svm_ROC_2 = getFPRandTPR(svm_mod2_predicted, method2_test$expert_label, cutoffs)
index_min = which.min(getDistCorner(svm_ROC_2))
ROC_svm2 = ggplot(svm_ROC_2, aes(x = FPR, y = TPR)) + geom_line()+
  annotate("point", x = svm_ROC_2$FPR[index_min], y = svm_ROC_2$TPR[index_min], colour = "blue") +
  labs(title=paste0("ROC: SVM (Method 2) / Best Cutoff:",svm_ROC_2$cutoff[index_min])) 


#ROC_svm2 + geom_label(aes(x = svm_ROC_2$FPR[index_min], y = svm_ROC_2$TPR[index_min], label="asdf"), position = "bottom-right")


png(filename = "imgs/Q3b_ROC.png", width = 1920, height = 1080)
grid.arrange(ROC_log1, ROC_lda1, ROC_qda1, ROC_svm1, 
             ROC_log2, ROC_lda2, ROC_qda2, ROC_svm2, nrow = 2)
dev.off()

grid.arrange(ROC_log1, ROC_log2, 
             ROC_lda1, ROC_lda2, 
             ROC_qda1, ROC_qda2, nrow = 2)









