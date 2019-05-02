#Stat 154 Project 2
library(ggplot2)
library(dplyr)
library(gridExtra)
library(GGally)
library(caret)
library(hash)
library(MASS)
library(pROC)
library(e1071)


#read in the raw data
image1 = read.table("image_data/image1.txt", sep = "", header = FALSE)
image2 = read.table("image_data/image2.txt", sep = "", header = FALSE)
image3 = read.table("image_data/image3.txt", sep = "", header = FALSE)

#label the column names
column_names = c("y", "x", "expert_label", "NDAI", "SD", "CORR",
                 "RadianceAngleDF", "RadianceAngleCF", "RadianceAngleBF", "RadianceAngleAF", "RadianceAngleAN")
colnames(image1) = column_names
colnames(image2) = column_names
colnames(image3) = column_names
image1 = cbind(imageNum=1, image1)
image2 = cbind(imageNum=2, image2)
image3 = cbind(imageNum=3, image3)

image_all = rbind(image1, image2, image3)
write.csv(image1, "data/image1.csv", row.names = FALSE)
write.csv(image2, "data/image2.csv", row.names = FALSE)
write.csv(image3, "data/image3.csv", row.names = FALSE)
write.csv(image_all, "data/image_all.csv", row.names = FALSE)

#----------------------------------------------------------------------------
#-------------------------- Question 1b -------------------------------------
#----------------------------------------------------------------------------

#--------- Summarise the data (ie % points for the different classes)

summary1 = image1 %>% group_by(expert_label) %>% summarise(Image1_count = n(), Image1_prop = n() / nrow(image1))
summary2 = image2 %>% group_by(expert_label) %>% summarise(Image2_count = n(), Image2_prop = n() / nrow(image2))
summary3 = image3 %>% group_by(expert_label) %>% summarise(Image3_count = n(), Image3_prop = n() / nrow(image3))
summary_total = image_combined %>% group_by(expert_label) %>% summarise(Total_count = n(), Total_prop = n() / nrow(image_combined))
summary_table = cbind(summary1, summary2[,c(2,3)], summary3[,c(2,3)], summary_total[c(2,3)])

rownames(summary_table) = NULL #c("Not Cloud", "Unlabeled", "Cloud")
rownames(summary_table) = c("Not Cloud", "Unlabeled", "Cloud")
png(filename="imgs/Fig1b1.png")
grid.table(summary_table)
dev.off()

#--------- Plot well-labeled beautiful maps

colors_cloud = c("skyblue4","black","ghostwhite")
#plot image 1
q1b_image1 <- ggplot(image1, aes(x = x, y = y)) + geom_raster(aes(fill=expert_label)) + 
  scale_fill_gradientn(colours=colors_cloud) + 
  labs(title="Image 1: Cloud/Not Cloud in Coordinate Space", x = "x-Coord", y = "y-Coord") + 
  theme_classic()

#plot image 2
q1b_image2 <- ggplot(image2, aes(x = x, y = y)) + geom_raster(aes(fill=expert_label)) + 
  scale_fill_gradientn(colours=colors_cloud) + 
  labs(title="Image 2: Cloud/Not Cloud in Coordinate Space", x = "x-Coord", y = "y-Coord") + 
  theme_classic()

#plot image 3
q1b_image3 <- ggplot(image3, aes(x = x, y = y)) + geom_raster(aes(fill=expert_label)) + 
  scale_fill_gradientn(colours=colors_cloud) + 
  labs(title="Image 3: Cloud/Not Cloud in Coordinate Space", x = "x-Coord", y = "y-Coord") + 
  theme_classic()

#get appropriate legend
q1b_legendplot <- ggplot(image1, aes(x=x, y=y)) + geom_point(aes(colour=factor(expert_label))) +
  scale_colour_manual(values = colors_cloud) + theme(legend.direction = "horizontal") +
  #scale_colour_discrete(name = "Cloud Label", labels = c("Not Cloud", "Unlabeled", "Cloud")) +
  labs(colour = "Cloud Label")

#extract legend: https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}


mylegend<-g_legend(q1b_legendplot)

png(filename="imgs/Fig1b2.png", width = 1920, height = 1080, units = "px", pointsize = 12)
q1b_finalplot <- grid.arrange(arrangeGrob(q1b_image1 + theme(legend.position="none"),
                               q1b_image2 + theme(legend.position="none"),
                               q1b_image3 + theme(legend.position="none"),
                               nrow=1),
                   mylegend, nrow=2,heights=c(10, 1))

dev.off()

#----------------------------------------------------------------------------
#-------------------------- Question 1c -------------------------------------
#----------------------------------------------------------------------------
set.seed(154)
png(filename="imgs/Fig1c1.png", width = 1920, height = 1080, units = "px", pointsize = 12)
ggpairs(image1[sample(1:nrow(image1), 100),], aes(colour = factor(expert_label), alpha = 0.4), title="Pairplot for Image 1")
dev.off()
#ggpairs(image2[sample(1:nrow(image2), 100),], aes(colour = factor(expert_label), alpha = 0.4), title="Pairplot for Image 2")
#ggpairs(image3[sample(1:nrow(image3), 100),], aes(colour = factor(expert_label), alpha = 0.4), title="Pairplot for Image 3")


print(by(image_all, image_all$expert_label, summary))

####### QUESTION 2 ########
#----------------------------------------------------------------------------
#-------------------------- Question 2a -------------------------------------
#----------------------------------------------------------------------------
set.seed(154)


#image 1:
x_square = floor(image1$x / 40)*40
y_square = floor(image1$y / 40)*40
squares = expand.grid(x = unique(x_square), y = unique(y_square))

indices_randomized = sample(1:nrow(squares))
n = nrow(squares)
train_indices = indices_randomized[1:(floor(.7*n))]
validation_indices = indices_randomized[((floor(.7*n))+1):(floor(.85*n))]
test_indices = indices_randomized[((floor(.85*n))+1):n]

train_chosen = squares[train_indices,]
validation_chosen = squares[validation_indices, ]
test_chosen = squares[test_indices, ]

train_1 = NULL
for(i in 1:nrow(train_chosen)) {
  x_chosen = train_chosen[i, "x"]
  y_chosen = train_chosen[i, "y"]
  train_1 = rbind(train_1, image1[(x_square == x_chosen & y_square == y_chosen), ])
}
validation_1 = NULL
for(i in 1:nrow(validation_chosen)) {
  x_chosen = validation_chosen[i, "x"]
  y_chosen = validation_chosen[i, "y"]
  validation_1 = rbind(validation_1, image1[(x_square == x_chosen & y_square == y_chosen), ])
}
test_1 = NULL
for(i in 1:nrow(test_chosen)) {
  x_chosen = test_chosen[i, "x"]
  y_chosen = test_chosen[i, "y"]
  test_1 = rbind(test_1, image1[(x_square == x_chosen & y_square == y_chosen), ])
}
#train_1 = cbind(imageNum = 1, train_1)
write.csv(train_1, "data/train1.csv", row.names = FALSE)
write.csv(validation_1, "data/validation1.csv", row.names = FALSE)
write.csv(test_1, "data/test1.csv", row.names = FALSE)


#image 2:
x_square = floor(image2$x / 40)*40
y_square = floor(image2$y / 40)*40
squares = expand.grid(x = unique(x_square), y = unique(y_square))

indices_randomized = sample(1:nrow(squares))
n = nrow(squares)
train_indices = indices_randomized[1:(floor(.7*n))]
validation_indices = indices_randomized[((floor(.7*n))+1):(floor(.85*n))]
test_indices = indices_randomized[((floor(.85*n))+1):n]

train_chosen = squares[train_indices,]
validation_chosen = squares[validation_indices, ]
test_chosen = squares[test_indices, ]

train_2 = NULL
for(i in 1:nrow(train_chosen)) {
  x_chosen = train_chosen[i, "x"]
  y_chosen = train_chosen[i, "y"]
  train_2 = rbind(train_2, image2[(x_square == x_chosen & y_square == y_chosen), ])
}
validation_2 = NULL
for(i in 1:nrow(validation_chosen)) {
  x_chosen = validation_chosen[i, "x"]
  y_chosen = validation_chosen[i, "y"]
  validation_2 = rbind(validation_2, image2[(x_square == x_chosen & y_square == y_chosen), ])
}
test_2 = NULL
for(i in 1:nrow(test_chosen)) {
  x_chosen = test_chosen[i, "x"]
  y_chosen = test_chosen[i, "y"]
  test_2 = rbind(test_2, image2[(x_square == x_chosen & y_square == y_chosen), ])
}
#train_2 = cbind(imageNum = 2, train_2)
write.csv(train_2, "data/train2.csv", row.names = FALSE)
write.csv(validation_2, "data/validation2.csv", row.names = FALSE)
write.csv(test_2, "data/test2.csv", row.names = FALSE)

#image 3:
x_square = floor(image3$x / 40)*40
y_square = floor(image3$y / 40)*40
squares = expand.grid(x = unique(x_square), y = unique(y_square))

indices_randomized = sample(1:nrow(squares))
n = nrow(squares)
train_indices = indices_randomized[1:(floor(.7*n))]
validation_indices = indices_randomized[((floor(.7*n))+1):(floor(.85*n))]
test_indices = indices_randomized[((floor(.85*n))+1):n]

train_chosen = squares[train_indices,]
validation_chosen = squares[validation_indices, ]
test_chosen = squares[test_indices, ]

train_3 = NULL
for(i in 1:nrow(train_chosen)) {
  x_chosen = train_chosen[i, "x"]
  y_chosen = train_chosen[i, "y"]
  train_3 = rbind(train_3, image3[(x_square == x_chosen & y_square == y_chosen), ])
}
validation_3 = NULL
for(i in 1:nrow(validation_chosen)) {
  x_chosen = validation_chosen[i, "x"]
  y_chosen = validation_chosen[i, "y"]
  validation_3 = rbind(validation_3, image3[(x_square == x_chosen & y_square == y_chosen), ])
}
test_3 = NULL
for(i in 1:nrow(test_chosen)) {
  x_chosen = test_chosen[i, "x"]
  y_chosen = test_chosen[i, "y"]
  test_3 = rbind(test_3, image3[(x_square == x_chosen & y_square == y_chosen), ])
}
#train_3 = cbind(imageNum = 3, train_3)
write.csv(train_3, "data/train3.csv", row.names = FALSE)
write.csv(validation_3, "data/validation3.csv", row.names = FALSE)
write.csv(test_3, "data/test3.csv", row.names = FALSE)

#total dataset
train = rbind(train_1, train_2, train_3)
validation = rbind(validation_1, validation_2, validation_3)
test = rbind(test_1, test_2, test_3)
write.csv(train, "data/train.csv", row.names = FALSE)
write.csv(validation, "data/validation.csv", row.names = FALSE)
write.csv(test, "data/test.csv", row.names = FALSE)

#sanity checks
## plot the map of the training data (put into a if statement so it can be easily compressed)
if(TRUE) {
  colors_cloud = c("skyblue4","black","ghostwhite")
  #plot image 1
  q1b_image1 <- ggplot(train_1, aes(x = x, y = y)) + geom_raster(aes(fill=expert_label)) + 
    scale_fill_gradientn(colours=colors_cloud) + 
    labs(title="Image 1 Train: Cloud/Not Cloud in Coordinate Space", x = "x-Coord", y = "y-Coord") + 
    theme_classic()
  
  #plot image 2
  q1b_image2 <- ggplot(train_2, aes(x = x, y = y)) + geom_raster(aes(fill=expert_label)) + 
    scale_fill_gradientn(colours=colors_cloud) + 
    labs(title="Image 2 Train: Cloud/Not Cloud in Coordinate Space", x = "x-Coord", y = "y-Coord") + 
    theme_classic()
  
  #plot image 3
  q1b_image3 <- ggplot(train_3, aes(x = x, y = y)) + geom_raster(aes(fill=expert_label)) + 
    scale_fill_gradientn(colours=colors_cloud) + 
    labs(title="Image 3 Train: Cloud/Not Cloud in Coordinate Space", x = "x-Coord", y = "y-Coord") + 
    theme_classic()
  
  #get appropriate legend
  q1b_legendplot <- ggplot(image1, aes(x=x, y=y)) + geom_point(aes(colour=factor(expert_label))) +
    scale_colour_manual(values = colors_cloud) + theme(legend.direction = "horizontal") +
    #scale_colour_discrete(name = "Cloud Label", labels = c("Not Cloud", "Unlabeled", "Cloud")) +
    labs(colour = "Cloud Label")
  
  #extract legend: https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots
  #https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
  g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
  
  
  mylegend<-g_legend(q1b_legendplot)
  png(filename="imgs/Fig2a1.png", width = 1920, height = 1080, units = "px", pointsize = 12)
  q1b_finalplot <- grid.arrange(arrangeGrob(q1b_image1 + theme(legend.position="none"),
                                            q1b_image2 + theme(legend.position="none"),
                                            q1b_image3 + theme(legend.position="none"),
                                            nrow=1),
                                mylegend, nrow=2,heights=c(10, 1))
  dev.off()
  
}

#check dimensions of the training and test against original
if(TRUE) {
  dim(train_1)
  dim(validation_1)
  dim(test_1)
  dim(image1)
  head(train_1)
  
  dim(train_2)
  dim(validation_2)
  dim(test_2)
  dim(image2)
  head(train_2)
  
  dim(train_3)
  dim(validation_3)
  dim(test_3)
  dim(image3)
  head(train_3)
}


##METHOD 2: CHOOSE 1 image to be test, split other 2 into training and validation
set.seed(154039827)
image_test_num = sample(c(1, 2, 3), 1) # = 1
method2_test = image1
method2_train = rbind(train_2, train_3)
method2_validation = rbind(test_2, validation_2, test_3, validation_3)

#----------------------------------------------------------------------------
#-------------------------- Question 2b -------------------------------------
#----------------------------------------------------------------------------
#classify all the points in validation and test set as -1 (cloud free). This is a `trivial` classifier
validation_labeled = validation[validation$expert_label != 0, ]
test_labeled = test[test$expert_label != 0, ]
temp_labeled = rbind(validation_labeled, test_labeled)
temp = rbind(validation, test)
accuracy_table = data.frame(dataset = c("Validation", "Test", "Val&Test Combined"),
                            accuracy = c(mean(validation$expert_label == -1), mean(test$expert_label == -1), mean(temp$expert_label == -1)),
                            accuracy_labeled = c(mean(validation_labeled$expert_label == -1),
                                                 mean(test_labeled$expert_label == -1),
                                                 mean(temp_labeled$expert_label == -1)))
colnames(accuracy_table) = c("dataset", "Prop Correct {-1,1}", "Prop Correct {-1, 0, 1}")
grid.table(accuracy_table)
dev.off()



#----------------------------------------------------------------------------
#-------------------------- Question 2d -------------------------------------
#----------------------------------------------------------------------------
set.seed(154)
CVgeneric_genericMatrix <- function(generic_classifier, training_features, training_labels, K, lossFunction) {
  #assumes generic_classifier(X, y) as inputs
  #     while some generic functions take in a formula (y~.,X),
  #     most generic classifiers are okay with these inputs as well
  #     If classifier only takes formula, then write a helper function as in:
  #     generic1 <- function(X, y) {return(train(y ~ ., data =  X, method="glm", family="binomial"))}
  #     writing it with X,y rather than a formula allows other inputs into the generic function 
  #     like method, family, etc...
  #lossFunction(yhat, y): takes in yhat, y as inputs
  #returns a vector length K of the loss of each fold
  #Note: CV should be split through part 2a's method, however this depends on which method was used
  #      to split. Therefore, that split cannot be implemented GENERICALLY, which means it's up to the
  #      user to input the correct matrices.
  folds = createFolds(training_labels, k=K)
  losses = c()
  for(i in 1:K) {
    #get the inputs for the classifier
    temp_features_val = training_features[folds[[i]], ]
    temp_features_train = training_features[-folds[[i]], ]
    temp_labeled_val = training_labels[folds[[i]]]
    temp_labeled_train = training_labels[-folds[[i]]]
    
    #run the classifier
    mod_fit = generic_classifier(temp_features_train, temp_labeled_train)
    predicted = predict(mod_fit, newdata=temp_features_val)
    loss = lossFunction(predicted, temp_labeled_val)
    losses = c(losses, loss)
  }
  return(losses)
}


# Now: CVgeneric for generic classifier, but specific to our 
# training data and how we specifically split it
# ie there is an extra input (whole training), which has the same number of rows as the 
# training_features but with the x and y coordinate values (so we know how to divide the data)
CVgeneric <- function(generic_classifier, training_features, training_labels, K, lossFunction, train_whole) {
  #divide the data into K folds through method 1 
  dat = train_whole
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
  #for each fold, find the loss
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

#An example of how to run CV K=10 with logisitic regression on the entire training data set
# Note: we assume that generic_classifier takes in TrainingData (X), and Labels (y), so it
#       is up to the user to wrap R training functions to include other options such as
#       method="glm", family="binomial (How to wrap it is shown below in generic1)
# Note: we also added an input train_whole which requires the rows to be the same data points as in 
#       training_features and training_labels, but requires the x, y coordinat data. This is because
#       we split the data through squares, so we need to select squares (not just pixels)
## ----------------------------------------------------------------------------
## Example on how to use CVgeneric
## ----------------------------------------------------------------------------
dat = train[(train$expert_label != 0), ]
rownames(dat) = NULL
generic1 <- function(X, y) {
  total_dat = cbind(X, y)
  return(train(y ~ ., data =  total_dat, method="glm", family="binomial"))
}

generic2 <- function(X, y) {
  total_dat = cbind(X, y)
  return(lda(y ~ ., data =  total_dat))
}
generic_classifier = generic2 #lm(method="glm", family = "binomial")

training_features = dat[,5:11]
training_labels = as.factor(dat$expert_label)
K = 10

loss1 <- function(x,y) {mean(x != y)}
lossFunction = loss1
train_whole = dat
CVgeneric(generic_classifier, training_features, training_labels, K, lossFunction, train_whole)



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
  # METHOD 1: Get Method1MapIndices (to speed computation)
  #divide the data into squares by METHOD 1
  train = read.csv("data/train.csv")
  validation = read.csv("data/validation.csv")
  test = read.csv("data/test.csv")
  method1_train = rbind(train, validation)
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
## LOGISTIC REGRESSION
## ----------------------------------------------------------------------------
generic_logistic <- function(X, y) {
  total_dat = cbind(X, y)
  return(train(y ~ ., data =  total_dat, method="glm", family="binomial"))
}
loss_logisitc <- function(yhat, y) {
  if("class" %in% names(yhat)) {
    return(mean(yhat$class != y)) 
  } else {
    return(mean(yhat != y))
  }
}
#function to get CV for logistic regression
getCVLogistic <- function(dataInput, mapIndices) {
  dat = dataInput[(dataInput$expert_label != 0), ]
  rownames(dat) = NULL
  
  training_features = dat[,5:12]
  training_labels = as.factor(dat$expert_label)
  
  CVgeneric_Optimized(generic_logistic, training_features, training_labels, K, loss_logisitc, 
                      mapIndices)
}
#get Logistic CV Folds losses (inaccuracy)
ptm <- proc.time()
LogisticLossesMethod1 = getCVLogistic(method1_train, Method1MapIndices)
LogisticLossesMethod2 = getCVLogistic(method2_train, Method2MapIndices)
logistic_ptm = proc.time() - ptm

#get Logistic Test loss (inaccuracy)
test = test[(test$expert_label != 0), ]
logistic_testMod = train(as.factor(expert_label) ~ ., data =  test[,4:12], method="glm", family="binomial")
logistic_testLoss1 = mean(predict(logistic_testMod) != test$expert_label)

test = image1
test = test[(test$expert_label != 0), ]
logistic_testMod = train(as.factor(expert_label) ~ ., data =  test[,4:12], method="glm", family="binomial")
logistic_testLoss2 = mean(predict(logistic_testMod) != test$expert_label)

#total CV results
CVresultsLogistic = data.frame(CVFold = c("Test", "Average Folds", 1:K),
                       Logisticregression1 = c(logistic_testLoss1, mean(LogisticLossesMethod1), LogisticLossesMethod1),
                       Logisticregression2 = c(logistic_testLoss2, mean(LogisticLossesMethod2), LogisticLossesMethod2))
names(CVresultsLogistic) = c("Data/CV Fold", "Logistic (Cutoff .5) Method 1", "Logistic (Cutoff .5) Method 2")
write.csv(CVresultsLogistic, "CVresults/CVLogistic.csv", row.names = FALSE)


## ----------------------------------------------------------------------------
## LDA
## ----------------------------------------------------------------------------
generic_lda <- function(X, y) {
  total_dat = cbind(X, y)
  return(lda(y ~ ., data =  total_dat))
}
loss_lda <- function(yhat, y) {
  if("class" %in% names(yhat)) {
    return(mean(yhat$class != y)) 
  } else {
    return(mean(yhat != y))
  }
}
#function to get CV for lda classification
getCVlda <- function(dataInput, mapIndices) {
  dat = dataInput[(dataInput$expert_label != 0), ]
  rownames(dat) = NULL
  
  training_features = dat[,5:12]
  training_labels = as.factor(dat$expert_label)
  
  CVgeneric_Optimized(generic_lda, training_features, training_labels, K, loss_lda, 
                      mapIndices)
}
#get lda CV Folds losses (inaccuracy)
ptm <- proc.time()
ldaLossesMethod1 = getCVlda(method1_train, Method1MapIndices)
ldaLossesMethod2 = getCVlda(method2_train, Method2MapIndices)
lda_ptm = proc.time() - ptm

#get lda Test loss (inaccuracy)
test = test[(test$expert_label != 0), ]
lda_testMod = lda(as.factor(expert_label) ~ ., data =  test[,4:12])
lda_testLoss1 = mean(predict(lda_testMod)$class != test$expert_label)

test = image1
test = test[(test$expert_label != 0), ]
lda_testMod = lda(as.factor(expert_label) ~ ., data =  test[,4:12])
lda_testLoss2 = mean(predict(lda_testMod)$class != test$expert_label)

#total CV results
CVresultsLDA = data.frame(CVFold = c("Test","Average Folds", 1:K),
                          lda1 = c(lda_testLoss1, mean(ldaLossesMethod1), ldaLossesMethod1),
                          lda2 = c(lda_testLoss2, mean(ldaLossesMethod2), ldaLossesMethod2))
names(CVresultsLDA) = c("Data/CV Fold", "LDA Method 1", "LDA Method 2")
write.csv(CVresultsLDA, "CVresults/CVlda.csv", row.names = FALSE)




## ----------------------------------------------------------------------------
## QDA
## ----------------------------------------------------------------------------
generic_qda <- function(X, y) {
  total_dat = cbind(X, y)
  return(qda(y ~ ., data =  total_dat))
}
loss_qda <- function(yhat, y) {
  if("class" %in% names(yhat)) {
    return(mean(yhat$class != y)) 
  } else {
    return(mean(yhat != y))
  }
}
#function to get CV for qda classification
getCVqda <- function(dataInput, mapIndices) {
  dat = dataInput[(dataInput$expert_label != 0), ]
  rownames(dat) = NULL
  
  training_features = dat[,5:12]
  training_labels = as.factor(dat$expert_label)
  
  CVgeneric_Optimized(generic_qda, training_features, training_labels, K, loss_qda, 
                      mapIndices)
}
#get qda CV Folds losses (inaccuracy)
ptm <- proc.time()
qdaLossesMethod1 = getCVqda(method1_train, Method1MapIndices)
qdaLossesMethod2 = getCVqda(method2_train, Method2MapIndices)
qda_ptm = proc.time() - ptm

#get qda Test loss (inaccuracy)
test = test[(test$expert_label != 0), ]
qda_testMod = qda(as.factor(expert_label) ~ ., data =  test[,4:12])
qda_testLoss1 = mean(predict(qda_testMod)$class != test$expert_label)

test = image1
test = test[(test$expert_label != 0), ]
qda_testMod = qda(as.factor(expert_label) ~ ., data =  test[,4:12])
qda_testLoss2 = mean(predict(qda_testMod)$class != test$expert_label)

#total CV results
CVresultsQDA = data.frame(CVFold = c("Test","Average Folds", 1:K),
                          qda1 = c(qda_testLoss1, mean(qdaLossesMethod1), qdaLossesMethod1),
                          qda2 = c(qda_testLoss2, mean(qdaLossesMethod2), qdaLossesMethod2))
names(CVresultsQDA) = c("Data/CV Fold", "QDA Method 1", "QDA Method 2")
write.csv(CVresultsQDA, "CVresults/CVqda.csv", row.names = FALSE)



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
test = test[(test$expert_label != 0), ]
ptm <- proc.time()
svm_testMod = svm(as.factor(expert_label) ~ ., data =  test[,4:12])
svm_testLoss1 = mean(predict(svm_testMod) != test$expert_label)
svm_ptm = proc.time() - ptm
test = image1
test = test[(test$expert_label != 0), ]
svm_testMod = svm(as.factor(expert_label) ~ ., data =  test[,4:12])
svm_testLoss2 = mean(predict(svm_testMod)$class != test$expert_label)

#total CV results
CVresultsSVM = data.frame(CVFold = c("Test","Average Folds", 1:K),
                          svm1 = c(svm_testLoss1, mean(svmLossesMethod1), svmLossesMethod1),
                          svm2 = c(svm_testLoss2, mean(svmLossesMethod2), svmLossesMethod2))
names(CVresultsSVM) = c("Data/CV Fold", "SVM Method 1", "SVM Method 2")
write.csv(CVresultsSVM, "CVresults/CVsvm.csv", row.names = FALSE)


#Display the results so far
cbind(CVresultsLogistic, CVresultsLDA[,c(2,3)], CVresultsQDA[,c(2,3)], CVresultsSVM[,c(2,3)])

library(rpud)
rpusvm(test[,5:12], as.factor(test$expert_label), type="eps-regression", scale=TRUE)

