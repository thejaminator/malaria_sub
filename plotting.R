library(AUC)
library(ggplot2)
library(caret)
library(ROCR)
source("model_images.R")



setwd("C:/Users/user/Google Drive/unilaptop/r projects/malaria_sub")
df = read.csv("model_results.csv")
ground_truth=as.factor(df$ground_truth)

#Decide on threshold for model to use
get_threshold_prediction<-function(probability,threshold){
  if (probability>threshold){
    return (1)
  }else{
    return  (0)
  }
}



#naive bayes no feature engineering
cm_nb_pixels = confusionMatrix(as.factor(df$gnb_pixels_predictions), ground_truth, dnn = c("Prediction", "Reference"),positive="1")
cm_nb_pixels
#nive bayes with feature engineering (hog)
cm_nb_hog = confusionMatrix(as.factor(df$gnb_hog_predictions), ground_truth, dnn = c("Prediction", "Reference"),positive="1")
cm_nb_hog
#log no feature engin
cm_log_pixels = confusionMatrix(as.factor(df$log_reg_pixels_predictions), ground_truth, dnn = c("Prediction", "Reference"),positive="1")
cm_log_pixels
#log with feature engin
cm_log_hog = confusionMatrix(as.factor(df$log_reg_hog_predictions), ground_truth, dnn = c("Prediction", "Reference"),positive="1")
cm_log_hog

#simple cnn
r_cnn_predictions = sapply(df$r_cnn_proba,get_threshold_prediction,0.50)
cm_r_cnn = confusionMatrix(as.factor(r_cnn_predictions), ground_truth, dnn = c("Prediction", "Reference"),positive="1")
cm_r_cnn
#more complex cnn
colab_cnn_predictions = sapply(df$colab_cnn_proba,get_threshold_prediction,0.50)
cm_colab_cnn = confusionMatrix(as.factor(colab_cnn_predictions), ground_truth, dnn = c("Prediction", "Reference"),positive="1")
cm_colab_cnn


dev.off()
ground_truth=as.factor(df$ground_truth)
roc_obj = (roc(as.factor(df$log_reg_pixels_proba),ground_truth))
auc=auc(roc_obj)
plot(roc_obj, main=paste("AUC = ",round(auc,digits=4)))


#MULTIPLE ROC

# List of predictions
preds_list <- list(df$log_reg_pixels_proba,df$log_reg_hog_proba,df$r_cnn_proba)

# List of actual values (same for all)
m <- length(preds_list)
actuals_list <- rep(list(ground_truth), m)

# Plot the ROC curves
pred <- prediction(preds_list, actuals_list)
rocs <- performance(pred, "tpr", "fpr")
aucs <- performance(pred, measure = "auc")
round(slot(aucs,'y.values')[[2]],digits=3)
plot(rocs, col = as.list(1:m), main = "Test Set ROC Curves")
legend(x = "bottomright" ,
       legend = c(paste("Log Reg",round(slot(aucs,'y.values')[[1]],digits=3)),
                  c(paste("Log Reg + Feat Eng",round(slot(aucs,'y.values')[[2]],digits=3))),
                    c(paste("CNN",round(slot(aucs,'y.values')[[3]],digits=3)))
                  ),
       cex=1.5,
       fill = 1:m)
       #legend = c("Decision Tree", "Bagged Trees", "Random Forest", "GBM"),fill = 1:m)

       