# ------------------------------------------------------------- #
#                   Breast Cancer Prediction                    #
# https://www.kaggle.com/uciml/breast-cancer-wisconsin-data     #
# ------------------------------------------------------------- #

library(GGally)
library(ggplot2)
library(RColorBrewer)
library(ggsci)
library(wesanderson)
library(corrplot)
library(reshape)
library(sfsmisc)
library(varrank)
library(rrcov)
library(infotheo)
library(aricode)
library(robcor)
library(caret)
library(MASS)
library(psych)




### ================ Preliminary Analysis ================== ####

dataset <- read.csv(file = "data.csv")

dataset <- dataset[, colSums(is.na(dataset)) == 0]

summary(dataset)


dataset$diagnosis <- ifelse(dataset$diagnosis == "B", 0, 1)

summary(dataset[dataset$diagnosis == 0, ]) # Benign(B)
summary(dataset[dataset$diagnosis == 1, ]) # Malignant(M)


## Balance - dependent variable
counts <- table(dataset$diagnosis)
data <- data.frame(
  category = c("Malignant", "Benign"),
  count <- c(
    counts[names(counts) == 1],
    counts[names(counts) == 0]
  )
)

ggplot(data, aes(x = category, y = count, fill = category)) +
  geom_col(position = position_dodge()) +
  scale_fill_manual(values = c("royalblue1", "brown1")) +
  ggtitle("Quantity of Breast Cancer Diagnosis") +
  theme_bw() +
  labs(
    x = "Type of Tumour",
    y = "Number of Observations"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold")
  )


## Divide the 30 variables in 3 categories: mean values, standard error, and worst values
dt.melt <- melt(dataset[, 1:3], id = c("id", "diagnosis")) # ???
dt.melt.mean <- melt(dataset[, 1:12], id = c("id", "diagnosis"))
dt.melt.se <- melt(dataset[, c(1:2, 13:22)], id = c("id", "diagnosis"))
dt.melt.w <- melt(dataset[, c(1:2, 23:32)], id = c("id", "diagnosis"))

## Histograms
# Mean
ggplot(dt.melt.mean, aes(value, fill = as.factor(diagnosis))) +
  geom_histogram(bins = 30, alpha = 0.8) +
  scale_fill_manual(values = c("royalblue1", "brown1")) +
  facet_wrap(~variable, ncol = 2, scales = "free") +
  labs(fill = "Diagnosis") +
  ggtitle("Histogram Mean Values") +
  theme_bw() +
  labs(
    x = "Value",
    y = "Number of Observations"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold")
  )

# Standard Error
ggplot(dt.melt.se, aes(value, fill = as.factor(diagnosis))) +
  geom_histogram(bins = 30, alpha = 0.8) +
  scale_fill_manual(values = c("royalblue1", "brown1")) +
  facet_wrap(~variable, ncol = 2, scales = "free") +
  ggtitle("Histogram Standard Error Values") +
  theme_bw() +
  labs(fill = "Diagnosis") +
  labs(
    x = "Value",
    y = "Number of Observations"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold")
  )

# Worst
ggplot(dt.melt.w, aes(value, fill = as.factor(diagnosis))) +
  geom_histogram(bins = 30, alpha = 0.8) +
  scale_fill_manual(values = c("royalblue1", "brown1")) +
  facet_wrap(~variable, ncol = 2, scales = "free") +
  ggtitle("Histogram Worst Values") +
  labs(fill = "Diagnosis") +
  theme_bw() +
  labs(
    x = "Value",
    y = "Number of Observations"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold")
  )


## Boxplots
# Mean
ggplot(dt.melt.mean, aes(variable, value, fill = as.factor(diagnosis))) +
  geom_boxplot() +
  facet_wrap(~variable, ncol = 5, scales = "free") +
  scale_fill_manual(values = c("royalblue1", "brown1")) +
  theme(strip.text.x = element_blank()) +
  labs(fill = "Diagnosis") +
  ggtitle("Boxplot Mean Values") +
  theme_bw() +
  labs(
    x = "",
    y = "Value"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold")
  )

# Standard Error
ggplot(dt.melt.se, aes(variable, value, fill = as.factor(diagnosis))) +
  geom_boxplot() +
  facet_wrap(~variable, ncol = 5, scales = "free") +
  scale_fill_manual(values = c("royalblue1", "brown1")) +
  theme(strip.text.x = element_blank()) +
  labs(fill = "Diagnosis") +
  ggtitle("Boxplot Standard Error Values") +
  theme_bw() +
  labs(
    x = "",
    y = "Value"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold")
  )

# Worst
ggplot(dt.melt.w, aes(variable, value, fill = as.factor(diagnosis))) +
  geom_boxplot() +
  facet_wrap(~variable, ncol = 5, scales = "free") +
  scale_fill_manual(values = c("royalblue1", "brown1")) +
  theme(strip.text.x = element_blank()) +
  labs(fill = "Diagnosis") +
  ggtitle("Boxplot Worst Values") +
  theme_bw() +
  labs(
    x = "",
    y = "Value"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold")
  )



## Correlations
options(repr.plot.width = 20, repr.plot.height = 20, repr.plot.res = 10)

# All values
corrplot(cor(dataset[, c(3:32)]),
  method = "square",
  type = "lower",
  order = "hclust",
  tl.col = "black",
  addCoef.col = "black",
  number.cex = 0.5,
  tl.cex = 0.5,
  title = "Correlation Matrix Between All Variables",
  diag = FALSE, # hide correlation on principal diagonal
  mar = c(0, 0, 1, 0)
)




### ================== Train/Test split ==================== ####

nrows <- NROW(dataset)

set.seed(218)
index <- sample(1:nrows, 0.7 * nrows)

train <- dataset[index, ] # 398 train data (70%)
test <- dataset[-index, ] # 171 test data (30%)

prop.table(table(train$diagnosis)) # train set proportion of maligns and benigns
prop.table(table(test$diagnosis)) # test set proportion of maligns and benigns

# Features
X_train <- train[-(1:2)]
X_test <- test[-(1:2)]
# Labels
y_train <- train$diagnosis
y_test <- test$diagnosis




### =================== Pre-processing ===================== ####

## Normalization - Min Max Scaler
min_max_scaling <- function(train, test) {
  min_vals <- sapply(train, min)
  range1 <- sapply(train, function(x) diff(range(x)))

  train_scaled <- data.frame(matrix(nrow = nrow(train), ncol = ncol(train)))

  for (i in seq_len(ncol(train))) {
    column <- (train[, i] - min_vals[i]) / range1[i]
    train_scaled[i] <- column
  }

  colnames(train_scaled) <- colnames(train)

  # scale the testing data using the min and range of the train data
  test_scaled <- data.frame(matrix(nrow = nrow(test), ncol = ncol(test)))

  for (i in seq_len(ncol(test))) {
    column <- (test[, i] - min_vals[i]) / range1[i]
    test_scaled[i] <- column
  }

  colnames(test_scaled) <- colnames(test)

  return(list(train = train_scaled, test = test_scaled))
}

norm <- min_max_scaling(X_train, X_test)

X_train_norm <- norm$train
X_test_norm <- norm$test



## Standardization (mean + sd)
standard_scaling <- function(train, test) {
  meann <- sapply(train, mean)
  stand_dev <- sapply(train, sd)

  train_scaled <- data.frame(matrix(nrow = nrow(train), ncol = ncol(train)))

  for (i in seq_len(ncol(train))) {
    column <- (train[, i] - meann[i]) / stand_dev[i]
    train_scaled[i] <- column
  }

  colnames(train_scaled) <- colnames(train)

  # scale the testing data using the mean and standard deviation of the train data
  test_scaled <- data.frame(matrix(nrow = nrow(test), ncol = ncol(test)))

  for (i in seq_len(ncol(test))) {
    column <- (test[, i] - meann[i]) / stand_dev[i]
    test_scaled[i] <- column
  }

  colnames(test_scaled) <- colnames(test)

  return(list(train = train_scaled, test = test_scaled))
}

stand <- standard_scaling(X_train, X_test)

X_train_stand <- stand$train
X_test_stand <- stand$test



## Standardization (median + MAD)
standard_scaling_rob <- function(train, test) {
  meann <- sapply(train, median)
  stand_dev <- sapply(train, mad)

  train_scaled <- data.frame(matrix(nrow = nrow(train), ncol = ncol(train)))

  for (i in seq_len(ncol(train))) {
    column <- (train[, i] - meann[i]) / stand_dev[i]
    train_scaled[i] <- column
  }

  colnames(train_scaled) <- colnames(train)

  # scale the testing data using the mean and standard deviation of the train data
  test_scaled <- data.frame(matrix(nrow = nrow(test), ncol = ncol(test)))

  for (i in seq_len(ncol(test))) {
    column <- (test[, i] - meann[i]) / stand_dev[i]
    test_scaled[i] <- column
  }

  colnames(test_scaled) <- colnames(test)

  return(list(train = train_scaled, test = test_scaled))
}

robstand <- standard_scaling_rob(X_train, X_test)

X_train_robstand <- robstand$train
X_test_robstand <- robstand$test



## Pearson Correlation
set.seed(42)
correlationMatrix <- cor(X_train)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.8)
X_train_corr <- X_train[, -highlyCorrelated]
X_test_corr <- X_test[, -highlyCorrelated]

## Robust Correlation
corrplot(cov2cor(cov.rob(X_train, method = "mcd")$cov),
  method = "square",
  type = "lower",
  order = "hclust",
  tl.col = "black",
  addCoef.col = "black",
  number.cex = 0.5,
  tl.cex = 0.5,
  title = "Robust Correlation Matrix Between All Variables",
  diag = FALSE,
  mar = c(0, 0, 1, 0)
)

set.seed(42)
correlationMatrixrob <- cov2cor(cov.rob(X_train, method = "mcd")$cov)
highlyCorrelatedrob <- findCorrelation(correlationMatrixrob, cutoff = 0.8)
X_train_robcorr <- X_train[, -highlyCorrelatedrob]
X_test_robcorr <- X_test[, -highlyCorrelatedrob]


# cor Pearson
print(highlyCorrelated[order(highlyCorrelated)]) # 1  2  3  5  6  7  8 13 14 16 21 23 24 27 28
names(X_train[setdiff(highlyCorrelated, highlyCorrelatedrob)]) # "concavity_worst" "radius_mean" "area_se" "smoothness_mean"

# robcor (cov using MCD)
print(highlyCorrelatedrob[order(highlyCorrelatedrob)]) # 2  3  4  6  7  8 11 13 16 21 23 24 26 28
names(X_train[setdiff(highlyCorrelatedrob, highlyCorrelated)]) # "compactness_worst" "area_mean" "radius_se"


## Point-Bisserial Correlation - works exactly like pearson correlation
biserial(X_train, y_train) 
highlyCorrelated95 <- c(8,21,23,28)  # above 0.95
cor(X_train[,c(8,21,23,28)])
highlycorrelatedfinal <- c(21,23,28)  # without concave.points_mean;

X_train_pbcorr <- X_train[,highlycorrelatedfinal]
X_test_pbcorr <- X_test[,highlycorrelatedfinal]



## PCA
screePlot <- function(pca.eig) {
  manualcolors <- c(
    "magenta", "mediumorchid1", "indianred1", "lightsalmon", "cornflowerblue", "chartreuse",
    "seagreen1", "moccasin", "mediumvioletred", "seagreen", "cadetblue1",
    "darkolivegreen1", "tan2", "tomato3", "#7CE3D8", "gainsboro"
  )
  ggplot(pca.eig, aes(x = index, y = prop_varex)) +
    geom_bar(stat = "identity", aes(fill = index), colour = "black") +
    geom_path(aes(x = index.cont), size = 1, colour = "Gray50") +
    geom_point(size = 3) +
    labs(
      x = "Principal Component",
      y = "Percentage of Variance Explained"
    ) +
    scale_fill_manual(values = manualcolors) +
    theme_bw(base_size = 20) +
    theme(legend.position = "none")
}

## Classic PCA (normalized data)
pc.Class_norm <- PcaClassic(X_train_norm, cov.control = NULL, crit.pca.distances = 0.999)
summary(pc.Class_norm) # 5 PC - 0.87858

pca.eig <- as.data.frame(pc.Class_norm$eigenvalues)
pca.eig$index <- as.factor(1:nrow(pca.eig))
pca.eig$index.cont <- 1:nrow(pca.eig)
pca.eig$prop_varex <- pc.Class_norm$eigenvalues / sum(pc.Class_norm$eigenvalues)

screePlot(pca.eig[1:10, ])  # we keep 5 PC

X_train_pca_norm <- pc.Class_norm$scores[, 1:5]
pca_fit <- predict(pc.Class_norm, X_test_norm)
X_test_pca_norm <- pca_fit[, 1:5]


## Classic PCA (standardization)
pc.Class_stand <- PcaClassic(X_train_stand, cov.control = NULL, crit.pca.distances = 0.999)
summary(pc.Class_stand) # 5 PC - 0.84823

pca.eig <- as.data.frame(pc.Class_stand$eigenvalues)
pca.eig$index <- as.factor(1:nrow(pca.eig))
pca.eig$index.cont <- 1:nrow(pca.eig)
pca.eig$prop_varex <- pc.Class_stand$eigenvalues / sum(pc.Class_stand$eigenvalues)

screePlot(pca.eig[1:10, ])  # we keep 5 PC

X_train_pca_stand <- pc.Class_stand$scores[, 1:5]
pca_fit <- predict(pc.Class_stand, X_test_stand)
X_test_pca_stand <- pca_fit[, 1:5]


## Robust PCA (Robust standardization)
pc.ROBPCA_robstand <- PcaHubert(X_train_robstand, k = 10, crit.pca.distances = 0.999)
summary(pc.ROBPCA_robstand) # 4 PC - 0.85690

pca.eig <- as.data.frame(pc.ROBPCA_robstand$eigenvalues)
pca.eig$index <- as.factor(1:nrow(pca.eig))
pca.eig$index.cont <- 1:nrow(pca.eig)
pca.eig$prop_varex <- pc.ROBPCA_robstand$eigenvalues / sum(pc.ROBPCA_robstand$eigenvalues)

screePlot(pca.eig[1:10, ])  # we keep 4 PC

X_train_robpca_robstand <- pc.ROBPCA_robstand$scores[, 1:4]
pca_fit <- predict(pc.ROBPCA_robstand, X_test_robstand)
X_test_robpca_robstand <- pca_fit[, 1:4]




## Classic PCA (Pearson correlation)

# Applying scaling beforehand
stand <- standard_scaling(X_train_corr, X_test_corr)
X_train_corr_stand <- stand$train
X_test_corr_stand <- stand$test

pc.Class_corr <- PcaClassic(X_train_corr_stand, cov.control = NULL, crit.pca.distances = 0.999)
summary(pc.Class_corr) # 6 PC - 0.88023

pca.eig <- as.data.frame(pc.Class_corr$eigenvalues)
pca.eig$index <- as.factor(1:nrow(pca.eig))
pca.eig$index.cont <- 1:nrow(pca.eig)
pca.eig$prop_varex <- pc.Class_corr$eigenvalues / sum(pc.Class_corr$eigenvalues)

screePlot(pca.eig[1:10, ]) # we keep 6 PC

X_train_pca_corr <- pc.Class_corr$scores[, 1:6]
pca_fit <- predict(pc.Class_corr, X_test_corr_stand)
X_test_pca_corr <- pca_fit[, 1:6]


## Robust PCA (Robust Correlation)

# Applying scaling beforehand
robstand <- standard_scaling_rob(X_train_robcorr, X_test_robcorr)
X_train_robcorr_robstand <- robstand$train
X_test_robcorr_robstand <- robstand$test
  
pc.ROBPCA_robcorr <- PcaHubert(X_train_robcorr_robstand, k = 10, crit.pca.distances = 0.999)
summary(pc.ROBPCA_robcorr)  # 5 PC - 0.86579

pca.eig <- as.data.frame(pc.ROBPCA_robcorr$eigenvalues)
pca.eig$index <- as.factor(1:nrow(pca.eig))
pca.eig$index.cont <- 1:nrow(pca.eig)
pca.eig$prop_varex <- pc.ROBPCA_robcorr$eigenvalues / sum(pc.ROBPCA_robcorr$eigenvalues)

screePlot(pca.eig[1:10, ])  # we keep 5 PC

X_train_robpca_robcorr <- pc.ROBPCA_robcorr$scores[, 1:5]
pca_fit <- predict(pc.ROBPCA_robcorr, X_test_robcorr_robstand)
X_test_robpca_robcorr <- pca_fit[, 1:5]



# Some samples of the obtained scores
print(X_train_pca_norm[1:5, 1:5])
print(X_train_pca_stand[1:5, 1:5])
print(X_train_robpca_robstand[1:5, 1:4])
print(X_train_pca_corr[1:5, 1:6])
print(X_train_robpca_robcorr[1:5, 1:5])


######



### ================== Datasets Export ===================== ####

path_out = './datasets'
dir.create(path_out)


datasets <- c('y_train', 
              'X_train_norm', 'X_train_stand', 'X_train_robstand', 'X_train_corr', 'X_train_robcorr',
              'X_train_pbcorr',
              'X_train_pca_norm', 'X_train_pca_stand', 'X_train_robpca_robstand', 
              'X_train_pca_corr', 'X_train_robpca_robcorr',
              
              'y_test', 
              'X_test_norm', 'X_test_stand', 'X_test_robstand', 'X_test_corr', 'X_test_robcorr', 
              'X_test_pbcorr',
              'X_test_pca_norm', 'X_test_pca_stand', 'X_test_robpca_robstand', 
              'X_test_pca_corr', 'X_test_robpca_robcorr')


for (name in datasets) {
  
  data <- eval(as.name(name))
  
  if (substring(name, 1, 1) == "y") {data <- data.frame(diagnosis = eval(as.name(name)))}
  
  write.csv(data, file.path(path_out, paste(name,'.csv', sep='')), row.names=FALSE)
}







