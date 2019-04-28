#Stat 154 Project 2
library(ggplot2)
library(dplyr)
library(gridExtra)
library(GGally)



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

image_combined = rbind(image1, image2, image3)

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
grid.table(summary_table)

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

q1b_finalplot <- grid.arrange(arrangeGrob(q1b_image1 + theme(legend.position="none"),
                               q1b_image2 + theme(legend.position="none"),
                               q1b_image3 + theme(legend.position="none"),
                               nrow=1),
                   mylegend, nrow=2,heights=c(10, 1))

#----------------------------------------------------------------------------
#-------------------------- Question 1c -------------------------------------
#----------------------------------------------------------------------------
set.seed(154)
ggpairs(image1[sample(1:nrow(image1), 100),], aes(colour = factor(expert_label), alpha = 0.4), title="Pairplot for Image 1")
#ggpairs(image2[sample(1:nrow(image2), 100),], aes(colour = factor(expert_label), alpha = 0.4), title="Pairplot for Image 2")
#ggpairs(image3[sample(1:nrow(image3), 100),], aes(colour = factor(expert_label), alpha = 0.4), title="Pairplot for Image 3")


####### QUESTION 2 ########
#----------------------------------------------------------------------------
#-------------------------- Question 2a -------------------------------------
#----------------------------------------------------------------------------
set.seed(154)
p = .8

#image 1:
x_square = floor(image1$x / 10)*10
y_square = floor(image1$y / 10)*10
squares = expand.grid(x = unique(x_square), y = unique(y_square))
chosen_indices = (sample(1:nrow(squares), floor(p*nrow(squares))))
chosen = squares[chosen_indices,]
notchosen = squares[-chosen_indices, ]
train_1 = NULL
for(i in 1:nrow(chosen)) {
  x_chosen = chosen[i, "x"]
  y_chosen = chosen[i, "y"]
  train_1 = rbind(train_1, image1[(x_square == x_chosen & y_square == y_chosen), ])
}
test_1 = NULL
for(i in 1:nrow(notchosen)) {
  x_notchosen = notchosen[i, "x"]
  y_notchosen = notchosen[i, "y"]
  test_1 = rbind(test_1, image1[(x_square == x_notchosen & y_square == y_notchosen), ])
}
write.csv(train_1, "data/train1.csv")
write.csv(test_1, "data/test1.csv")

#image 2:
x_square = floor(image2$x / 10)*10
y_square = floor(image2$y / 10)*10
squares = expand.grid(x = unique(x_square), y = unique(y_square))
chosen_indices = (sample(1:nrow(squares), floor(p*nrow(squares))))
chosen = squares[chosen_indices,]
notchosen = squares[-chosen_indices, ]
train_2 = NULL
for(i in 1:nrow(chosen)) {
  x_chosen = chosen[i, "x"]
  y_chosen = chosen[i, "y"]
  train_2 = rbind(train_2, image2[(x_square == x_chosen & y_square == y_chosen), ])
}
test_2 = NULL
for(i in 1:nrow(notchosen)) {
  x_notchosen = notchosen[i, "x"]
  y_notchosen = notchosen[i, "y"]
  test_2 = rbind(test_2, image2[(x_square == x_notchosen & y_square == y_notchosen), ])
}
write.csv(train_2, "data/train2.csv")
write.csv(test_2, "data/test2.csv")

#image 3:
x_square = floor(image3$x / 10)*10
y_square = floor(image3$y / 10)*10
squares = expand.grid(x = unique(x_square), y = unique(y_square))
chosen_indices = (sample(1:nrow(squares), floor(p*nrow(squares))))
chosen = squares[chosen_indices,]
notchosen = squares[-chosen_indices, ]
train_3 = NULL
for(i in 1:nrow(chosen)) {
  x_chosen = chosen[i, "x"]
  y_chosen = chosen[i, "y"]
  train_3 = rbind(train_3, image3[(x_square == x_chosen & y_square == y_chosen), ])
}
test_3 = NULL
for(i in 1:nrow(notchosen)) {
  x_notchosen = notchosen[i, "x"]
  y_notchosen = notchosen[i, "y"]
  test_3 = rbind(test_3, image3[(x_square == x_notchosen & y_square == y_notchosen), ])
}
write.csv(train_3, "data/train3.csv")
write.csv(test_3, "data/test3.csv")

train = rbind(train_1, train_2, train_3)
test = rbind(test_1, test_2, test_3)
write.csv(train, "data/train.csv")
write.csv(test, "data/test.csv")

#sanity checks
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

q1b_finalplot <- grid.arrange(arrangeGrob(q1b_image1 + theme(legend.position="none"),
                                          q1b_image2 + theme(legend.position="none"),
                                          q1b_image3 + theme(legend.position="none"),
                                          nrow=1),
                              mylegend, nrow=2,heights=c(10, 1))

dim(train_1)
dim(test_1)
dim(image1)
head(train_1)

dim(train_2)
dim(test_2)
dim(image2)
head(train_2)

dim(train_3)
dim(test_3)
dim(image3)
head(train_3)

#----------------------------------------------------------------------------
#-------------------------- Question 2b -------------------------------------
#----------------------------------------------------------------------------
