library(cluster)
library(factoextra)
library(clusterCrit)
library(tidyr)
library(caret)
library(mlbench)
library(ClusterR)
#### K-Means
###################LOADING DATASETS

dataset1_train <- readRDS(file = "dataset1_train.Rds")
dataset1_test <- readRDS(file = "dataset1_test.Rds")

dataset_norm_train<-cbind(X_train_norm,y_train)
dataset_norm_test<-cbind(X_test_norm,y_test)

dataset_stand_train<-cbind(X_train_stand,y_train)
dataset_stand_test<-cbind(X_test_stand,y_test)

dataset_robstand_train<-cbind(X_train_robstand,y_train)
dataset_robstand_test<-cbind(X_test_robstand,y_test)

dataset_corr_train<-cbind(X_train_corr,y_train)
dataset_corr_test<-cbind(X_test_corr,y_test)

dataset_robcorr_train<-cbind(X_train_robcorr,y_train)
dataset_robcorr_test<-cbind(X_test_robcorr,y_test)

dataset_pca_norm_train<-cbind(X_train_pca_norm,y_train)
dataset_pca_norm_test<-cbind(X_test_pca_norm,y_test)

dataset_pca_stand_train<-cbind(X_train_pca_stand,y_train)
dataset_pca_stand_test<-cbind(X_test_pca_stand,y_test)

dataset_robpca_robstand_train<-cbind(X_train_robpca_robstand,y_train)
dataset_robpca_robstand_test<-cbind(X_test_robpca_robstand,y_test)

dataset_pca_corr_train<-cbind(X_train_pca_corr,y_train)
dataset_pca_corr_test<-cbind(X_test_pca_corr,y_test)

dataset_robpca_robcorr_train<-cbind(X_train_robpca_robcorr,y_train)
dataset_robpca_robcorr_test<-cbind(X_test_robpca_robcorr,y_test)

dataset_ycor_train<-cbind(X_train_ycor,y_train)
dataset_ycor_test<-cbind(X_test_ycor,y_test)

#######All########
results_means(dataset1_train[,-31],dataset1_train, dataset1_test[,-31], dataset1_test)

#######NORM########
results_means(dataset_norm_train[,-31],dataset_norm_train, dataset_norm_test[,-31], dataset_norm_test)

######STAND########
results_means(dataset_stand_train[,-31],dataset_stand_train, dataset_stand_test[,-31], dataset_stand_test)

########ROBSTAND###
results_means(dataset_robstand_train[,-31],dataset_robstand_train, dataset_robstand_test[,-31], dataset_robstand_test)

############CORR###
results_means(dataset_corr_train[,-16],dataset_corr_train, dataset_corr_test[,-16], dataset_corr_test)


###########ROBCORR###
results_means2(dataset_robcorr_train[,-17],dataset_robcorr_train, dataset_robcorr_test[,-17], dataset_robcorr_test)


############PCA_NORM#########
results_means(dataset_pca_norm_train[,-6],dataset_pca_norm_train, dataset_pca_norm_test[,-6], dataset_pca_norm_test)

############PCA_STAND#########
results_means(dataset_pca_stand_train[,-6],dataset_pca_stand_train, dataset_pca_stand_test[,-6], dataset_pca_stand_test)

############ROBPCA_ROBSTAND#########
results_means(dataset_robpca_robstand_train[,-5],dataset_robpca_robstand_train, dataset_robpca_robstand_test[,-5], dataset_robpca_robstand_test)

############PCA_CORR#########
results_means2(dataset_pca_corr_train[,-7],dataset_pca_corr_train, dataset_pca_corr_test[,-7], dataset_pca_corr_test)

############ROBPCA_ROBCORR#########
results_means2(dataset_robpca_robcorr_train[,-6],dataset_robpca_robcorr_train, dataset_robpca_robcorr_test[,-6], dataset_robpca_robcorr_test)

###########YCor################
results_means(dataset_ycor_train[,-4],dataset_ycor_train, dataset_ycor_test[,-4], dataset_ycor_test)

##############Melhor modelo -- PCA NORM 
set.seed(47)
km<-kmeans(dataset_pca_norm_train[,-6],centers=2,nstart=30)
#visualize the clusters
#plot results of final k-medoids model
fviz_cluster(km, data = dataset_pca_norm_train[,-6])+
  scale_color_manual(values = c("royalblue1", "brown1"))+
  scale_fill_manual(values = c("royalblue1", "brown1"))

