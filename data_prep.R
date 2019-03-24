#This file is to convert the 50% of images to a .csv called half_data.csv for submission
library(EBImage)
library(stringr)
library(pbapply)

#set folder to the one containing the cell_images
setwd("C:/Users/user/Google Drive/unilaptop/r projects/malaria")

bad_files = list.files("cell_images/Parasitized",full.names = TRUE,pattern = "\\.png$")
good_files = list.files("cell_images/Uninfected",full.names = TRUE,pattern = "\\.png$")



#function to covnert images to 1d vectors
flatten <- function(imgname,width=50,height=50,dir){
  img <- readImage(imgname)
  try(img_resized <- resize(img, w = width ,h = height))
  ## Set to grayscale (normalized to max)
  grayimg <- channel(img_resized, "gray")
  ## Get the image as a matrix
  img_matrix <- grayimg@.Data
  ## Coerce to a vector (row-wise)
  img_vector <- as.vector(t(img_matrix))
  return(img_vector)
}

#create the dataframe of the 1d vectors by appending them into dataframe 
bad_df <- pblapply(bad_files,flatten)
bad_feature_matrix <- do.call(rbind, bad_df)
bad_feature_matrix <- as.data.frame(bad_feature_matrix)
#label bad cells as 1
bad_feature_matrix$label <- 1

#repeat for good cells
good_df <- pblapply(good_files,flatten)
good_feature_matrix <- do.call(rbind, good_df)
good_feature_matrix <- as.data.frame(good_feature_matrix)
#label good cells as 0
good_feature_matrix$label <- 0

#concat the dataframes
total_feature_matrix <- rbind(bad_feature_matrix,good_feature_matrix)
save(total_feature_matrix,file="feature_matrix.Rda")


#for generating the file to send to github
half_size = nrow(total_feature_matrix)/2
sample_index <- sample(1:nrow(total_feature_matrix), half_size)
half_feature_matrix = total_feature_matrix[sample_index,]
#i saved as csv to upload to google colab, called half_data.csv (bad naming i know)
write.csv(half_feature_matrix, 'half_data.csv')



load(file="feature_matrix.Rda")
#for generating the 10% file 
tenth_size = nrow(total_feature_matrix)/10
sample_index <- sample(1:nrow(total_feature_matrix), tenth_size)
length(sample_index)
tenth_feature_matrix = total_feature_matrix[sample_index,]
save(tenth_feature_matrix,file="tenth_feature_matrix.Rda")
