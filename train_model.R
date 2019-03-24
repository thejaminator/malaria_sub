#Initially we used this but then we ported it to python on google colab
#because training the other base model comparisons was too computationally expensive on
#a laptop

setwd("C:/Users/user/Google Drive/unilaptop/r projects/malaria")

#load the file we saved from the data_prep script
load(file="feature_matrix.Rda")


#you will probably have to install these packages
library(reticulate)
library(dplyr)
library(keras)
library(stringi)
library(pbapply)
library(caTools)

set.seed(100)

#i set the python path to where i installed keras
use_python("C:/Users/user/ANACON~3/envs/rstudio/python.exe", required = TRUE)
use_condaenv("rstudio", conda="auto")
py_config()

#train test split
# 70% train, 30% test split. IMPT TO RANDOMIZE OR ELSE ALL YOUR TEST DATA BELONGS TO ONE TYPE 
total_feature_matrix <- total_feature_matrix[sample(1:nrow(total_feature_matrix)), ]
#actually we dont need to randomly sample again since its already shuffled
train_index <- sample(1:nrow(total_feature_matrix), 0.8 * nrow(total_feature_matrix))
test_index <- setdiff(1:nrow(total_feature_matrix), train_index)

train = total_feature_matrix[train_index,]
test = total_feature_matrix[test_index,]



# in data_prep we added a label to the data frame, now we need to seperate into x and y
#x is your training variable, y is the target variable to predict 
test_x<-select(test, -"label")
test_y <- select(test, "label")

#repeat for training dataset
train_x<-select(train, -"label")
train_y <- as.matrix(select(train, "label")) #keras can't take in dataframe so you got to change it

dim(train)
#got to reshape it first for the CNN to understand c(number, dim, dim, channels)
train_array <- as.matrix(train_x)
train_array <- array_reshape(train_array, c(nrow(train_array),50, 50, 1))

test_array<-as.matrix(test_x)
test_array <- array_reshape(test_array,c(nrow(test_array),50, 50, 1))

#train_array <- reshape(x, shape(-1, 50, 50, 1))
#checking if split done right 
dim(train_y)
dim(test_x)
dim(test_y)
dim(train_array)

# Initialize a sequential model 
model <- keras_model_sequential()

# Add layers to the model
model %>%
  #first layer is the convultion layer
  #remember that our images were 50 x 50, grayscale so the input shape would be
  #50 x 50 x 1. If they were RGB( having colour) it would be 50 x 50 x3
  layer_conv_2d(16,kernel_size = c(3,3), activation = 'relu', input_shape = c(50,50,1)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(16,kernel_size = c(3,3), activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  
  #flatten layer serves as the connection to the dense layer
  layer_flatten() %>%
  #hidden layer
  layer_dense(units = 16, activation = 'relu') %>% 
  #final layer must decide the range of probability  0 (bad cells) or towards 1 (goods cells)
  #we use sigmoid cos it gives value btw 0 and 1
  layer_dense(units = 1, activation = 'sigmoid')
summary(model)

#We compile the model. Since this is a binary classification problem, we use
#binary_crossentropy. Multiple categorical? categorical_crossentropy
model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)

#fit the model 
history <- model %>% fit(
  train_array,
  train_y, 
  epochs = 10, 
  batch_size = 10, 
  validation_split = 0.2,
  verbose= 1 #see progress bar
)

# Save model
model %>% save_model_hdf5("CNN.hdf5")

plot(history)
# Save model history
save(history,file="train_history.Rda")
plot(history)
# Save test images' data for analysis
save(test_array,file="test_array.Rda")
# Save ground truth for comparison later
save(test_y,file="ground_truth.Rda")




