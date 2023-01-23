# ------------------------------------------------------------- #
#                    Supervised Learning                        #
# ------------------------------------------------------------- #


library(caret)
library(DiagrammeR)
library(klaR)
library(rattle)
library(xgboost)


path = './datasets' 
source("load_datasets.R")


### ====================== Methods ========================= ####

#10-fold cross validation to choose hyperparameters

classifier <- function(clf, CV = TRUE, tune_grid = NULL, X_train, y_train) {

  train_control <- trainControl(method = 'none')
  if (isTRUE(CV)) {
    train_control <- trainControl(method = "cv", number = 10, savePredictions = 'final')
  } 

  set.seed(92626)
  model <- train(x = X_train, y = as.factor(y_train[, 1]),
                method = clf, metric = 'Accuracy',
                tuneGrid = tune_grid, trControl = train_control,
                preProcess = NULL)
  
  if (!is.null(tune_grid)) {
    print(paste('------------------------ Best', clf ,'Model -------------------------'))
    print(model$bestTune)
  }
  return (model)
}


performance_meas <- function(predicted, real) {
  
  aux <- confusionMatrix(data = predicted, reference = real)
  
  print(aux$table)
  
  return (data.frame(
    value = c(aux$overall['Accuracy'], aux$byClass['Sensitivity'],
              aux$byClass['Specificity'], aux$byClass['Balanced Accuracy'],
              aux$byClass['Precision'], aux$byClass['F1'])))
}


### ================= Choose Classifier ==================== ####


## K-Nearest Neighbors
tune_grid <- expand.grid(k = seq(from = 3, to = 19, by = 2))  # best k is 5

knn_model <- classifier('knn', tune_grid = tune_grid, X_train = X_train_stand, y_train = y_train)
knn_pred <- predict(knn_model, X_test_stand)
performance_meas(knn_pred, as.factor(y_test[, 1]))  # Accuracy = 0.9649123


## Naive Bayes
tune_grid <- expand.grid(
  fL = c(0, 0.5, 1.0), 
  usekernel = TRUE, 
  adjust = c(0, 0.5, 1.0)
  )

nb_model <- classifier('nb', tune_grid = tune_grid, X_train = X_train_stand, y_train = y_train)
nb_pred <- predict(nb_model, X_test_stand)
performance_meas(nb_pred, as.factor(y_test[, 1]))  # Accuracy = 0.9473684


## Linear Discriminant Analysis
lda_model <- classifier('lda', X_train = X_train_stand, y_train = y_train)
lda_pred <- predict(lda_model, X_test_stand)
performance_meas(lda_pred, as.factor(y_test[, 1]))  # Accuracy = 0.9707602

## Quadratic Discriminant Analysis
qda_model <- classifier('qda', X_train = X_train_stand, y_train = y_train)
qda_pred <- predict(qda_model, X_test_stand)
performance_meas(qda_pred, as.factor(y_test[, 1]))  # Accuracy = 0.9590643

## Decision Tree

dtree_model <- classifier('rpart', X_train = X_train_stand, y_train = y_train)
dtree_pred <- predict(dtree_model, X_test_stand)
performance_meas(dtree_pred, as.factor(y_test[, 1]))  # Accuracy = 0.9415205

dev.new(width=5, height=8, unit="in")
plot(dtree_model$finalModel, uniform=TRUE,
     main="Classification Tree")
text(dtree_model$finalModel, use.n.=TRUE, all=TRUE, cex=.8)


## X-Gboost
tune_grid <- expand.grid(
  nrounds = seq(from = 10, to = 100, by = 10),
  max_depth = c(3:7),
  eta = c(0.025, 0.05, 0.1),
  gamma = 0,
  subsample = c(0.5, 1),
  colsample_bytree = c(0.5, 0.75, 1),
  min_child_weight = c(1, 2)
)

xgboost_model <- classifier('xgbTree', tune_grid = tune_grid, X_train = X_train_stand, y_train = y_train)
xgboost_pred <- predict(xgboost_model, X_test_stand)
performance_meas(xgboost_pred, as.factor(y_test[, 1]))  # Accuracy = 0.9824561


# > Best classifier: X-Gboost



### =================== Graphic Plots ===================== ####

# Accuracy vs #Neighbors
plot(knn_model)

# XGBoost Tree
xgb.plot.tree(model = xgboost_model$finalModel, trees = 1)



### =================== Choose Dataset ===================== ####

xgboost_bestmodel <- function(X_train, y_train) {
  
  best_params <- expand.grid(
    nrounds = 100,
    max_depth = 4,
    eta = 0.1,
    gamma = 0,
    subsample = 0.5,
    colsample_bytree = 1,
    min_child_weight = 0.5
  )
  
  set.seed(42)
  bestmodel <- train(x = X_train , y = as.factor(y_train),
                     method = 'xgbTree', metric = 'Accuracy',
                     tuneGrid = best_params,
                     trControl = trainControl(method = 'none', savePredictions = TRUE),
                     preProcess = NULL, verbose = 0)
  return(bestmodel)
}



datasets <- c('_norm', '_stand', '_robstand', '_corr', '_robcorr', '_pbcorr', 
              '_pca_norm', '_pca_stand', '_robpca_robstand', 
              '_pca_corr', '_robpca_robcorr')

for (name in datasets) {
  
  xgboost <- xgboost_bestmodel(X_train = get(paste('X_train', name, sep = '')),
                               y_train = y_train[, 1])
  
  xgboost_pred <- predict(xgboost, get(paste('X_test', name, sep = '')))
  print(paste('DATASET:', name))
  print(performance_meas(xgboost_pred, as.factor(y_test[, 1])))
}


# > Best Dataset: _robstand (Accuracy = 0.9883041) with 30 variables
# corr (Accuracy = 0.9824561) with 15 variables
# pbcorr (Accuracy = 0.9532164) with 3 variables - the one with least variables




