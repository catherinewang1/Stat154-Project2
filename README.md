# Stat154-Project2

Repo for Spring 2019 Stat 154 Project 2
+ Catherine Wang- catherine_wang at berkeley.edu
+ Yijin Wang- 

## Final Report: 

The Final Report pdf is the final submission for this project. (This pdf was generated on google docs, and the corresponding word file is also included for reproducibility.)

## Reproducibility:
To reproduce all the figures, graphs, tables, and results from the final report, run through the R code in the `project2.R` file. (Make sure you have installed the required libraries such as `ggplot`, `dplyr`, `gridExtra`, `GGally`, etc.)


+ note: in the script, it saves the images in between in the `imgs` folder, however these may result in lower quality (resolution) images than the actual final report contains. To get the higher quality images, run the code and zoom in in RStudio. 
+ note: splitting the data into training and testing takes a while to run. To quicken the process, this data has been saved in `data/*.csv`. In R, simply load the data by `read.csv("data/*.csv")`. To read in the data easily, follow the code shown below:

```
#to read in the data from Image 1 (replace the 1 with 2/3 to get Image2/3)
image1 = read.csv("data/image1.csv")
train1 = read.csv("data/train1.csv")
validation1 = read.csv("data/validation1.csv")
test1 = read.csv("data/test1.csv")

#to read in the data from all 
image_all = read.csv("data/image_all.csv")
train = read.csv("data/train.csv")
validation = read.csv("data/validation.csv")
test = read.csv("data/test.csv")
```

