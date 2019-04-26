# Malaria Submission

See the [pdf](Artificial-Intelligence-Offers-a-Better-Way-to-Diagnose-Malaria.pdf) for the report

## 1. data_prep.R 

Script to process the images into matrix, for use in subsequent files. Requires the download of theses files 
https://www.kaggle.com/iarunava/cell-images-for-detecting-malaria/downloads/cell-images-for-detecting-malaria.zip/
(No need since the 10% of the entire feature matrix generated is in tenth_feature_matrix.Rda)

## 2. tenth_feature_matrix.Rda

The 10% of the entire feature matrix generated by data_prep.R, for use
in train_model.R and model_images.R

## 3.train_model.R 

Script to initially train the CNN. Creates model r_cnn.hdf5 for use in model_images. Eventually, because of the computational intensity to train the other base models, it 
was superseceded by the Google Colab Workbook. 

## 4.Link to Model Training Notebook 

https://colab.research.google.com/drive/1kNMixUoJhd4tQQBVFLmizZJMNON1rv3I
The CSV containing 50% of the image data was uploaded to github and the notebook will fetch the file from there, so there should be no need to download anything to your computer to run the colab notebook. Please let me know if there is something wrong with it. 

Trains the CNN, feature engineering and the other base models. Also visualizes the layers of the CNN
The results were downloaded to model_results.csv for analysis in model_images.R and plotting.R


## 5.model_images.R 

Generate the images of the cells and see if they are correctly classified. Also determines the defined optimal threshold.

## 6. plotting.R 

create ROC plots
