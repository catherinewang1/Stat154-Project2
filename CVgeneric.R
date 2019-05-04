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
