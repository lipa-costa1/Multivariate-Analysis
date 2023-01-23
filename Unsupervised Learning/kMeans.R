#### K-Means
library(cluster)
library(factoextra)
library(clusterCrit)
library(tidyr)
library(caret)
library(mlbench)
library(ClusterR)

############FUNCTIONS FOR CLUSTERING VALIDATION 
#(the df is the dataset without the diagnosis column)
######Elbow Method

elbow<-function(df){
  
  errors = data.frame(
    numero_clusters = seq(1:7),
    error = NA
  )
  
  for(i in 1:7){
    cluster <-  kmeans(df, centers = i, nstart = 30)
    
    errors$error[i] = cluster$tot.withinss
  }
  return(errors)
}

######Silhouette Method and Other Methods - Davies Bound-index, Dunn-index, C-index


best_cluster <- function(df) {
  
  ss=c()
  scorefunc = data.frame(
    numero_clusters = c(2:7),
    scores = NA,
    cs = NA,
    dbs = NA,
    dunns = NA
    
  )
  
  for(i in 1:6){
    km <-  kmeans(df, centers = i+1, nstart = 30)
    ss <- silhouette(km$cluster,dist = dist(df))
    scorefunc$scores[i] = mean(ss[,3])
    aux <- intCriteria(as.matrix(df),km$cluster,c("C_index","Davies_Bouldin","Dunn"))
    scorefunc$cs[i]<-aux$c_index
    scorefunc$dbs[i]<-aux$davies_bouldin
    scorefunc$dunns[i]<-aux$dunn
  }
  
  return(scorefunc)
  
}

###### MINIMIZAR A WITHIN E MAXIMIZAR A BETWEEN Method


within_between<-function(df){
  
  ssc = data.frame(
    numero_clusters = seq(1:7),
    within_ss = NA,
    between_ss = NA
  )
  
  for(i in 1:7){
    cluster <-  kmeans(df, centers = i, nstart = 30)
    
    ssc$within_ss[i] = mean(cluster$withinss)
    ssc$between_ss[i] = cluster$betweenss
  }
  return(ssc)
}


############FUNCTIONS FOR PERFORMANCE MEASURES
#(the predicted and real must in in "B" and "M" factors)
performance_meas<-function(predicted,real){
  
  performance_m = data.frame(
    medidas = c("Accuracy", "Sensitivity/Recall", "Specificity", "Balanced Accuracy",
                "Precision",  "F1-score"),
    valor = NA
  )
  aux<-confusionMatrix(data=predicted,reference=real)
  performance_m$valor[1]<-aux$overall[1]
  performance_m$valor[2]<-aux$byClass[1]
  performance_m$valor[3]<-aux$byClass[2]
  performance_m$valor[4]<-aux$byClass[11]
  performance_m$valor[5]<-precision(data=predicted,reference=real)
  performance_m$valor[6]<-aux$byClass[7]
  return(performance_m)
}

#####Purity function

ClusterPurity <- function(clusters, classes) {
  sum(apply(table(classes, clusters), 2, max)) / length(clusters)
}

########

############FUNCTIONS FOR PREDICT
#euclidian distance

predict.kmeans <- function(object, newdata){
  centers <- object$centers
  n_centers <- nrow(centers)
  dist_mat <- as.matrix(dist(rbind(centers, newdata)))
  dist_mat <- dist_mat[-seq(n_centers), seq(n_centers)]
  max.col(-dist_mat)
}


#############


######LOADING DATASETS
dataset1_train <- readRDS(file = "dataset1_train.Rds")
dataset1_test <- readRDS(file = "dataset1_test.Rds")
dataset_stand_train<-cbind(X_train_stand,y_train)
dataset_stand_test<-cbind(X_test_stand,y_test)


#####Choosing the optimal k

#One way of determing the number is by using the elbow method.
#This method is based on running several K-means algorithm with different ks.
#If we plot the overall error for the different number of clusters we will see 
#that the more clusters that we add to the algorithm the 
#lower that the total error will be.
#However, the error will know the decrease in a uniformed way: 
#we will reach one point where the error does not decrease as much
#as it does on the previous step. That is the elbow of the graph and
#that's the number of clusters that we should choose.


######Elbow Method

elbow(dataset_stand_train[,-31])

ggplot(elbow(dataset_stand_train[,-31]), aes(numero_clusters, error)) + geom_line() + geom_point() + 
  theme_minimal() + labs(title = "Total within-cluster sum of squares by number of clusters", x="Number of Clusters", y="Total Within-Cluster SS") + scale_x_continuous(breaks = seq(1,30))

#As we can see, the error decreases a lot when we pass from 1 cluster to 
#two and from two clusters to three. 
#However, the total error does not decrease that much when we add a fourth cluster. So, we should look for three clusters on our dataset.


######Silhouette Method and Other Methods - Davies Bound-index, Dunn-index, C-index

#Average silhouette method computes the average silhouette of observations
#for different values of k. The optimal number of clusters k is the one
#that maximize the average silhouette over a range of possible values for k.


best_cluster(dataset_stand_train[,-31])
plot(c(2:7), type='b', best_cluster(dataset_stand_train[,-31])$scores, xlab='Number of clusters', ylab='Average Silhouette Width', frame=FALSE)

######Minimizar a Within e Maximizar a Between Method

f<-gather(within_between(dataset_stand_train[,-31]), key = "measurement", value = value,-numero_clusters)
ggplot(f, aes(x=numero_clusters, y=value, fill = measurement)) + geom_bar(stat = "identity", position = "dodge") + ggtitle("Cluster Model Comparison") + xlab("Number of Clusters") + ylab("Total Sum of Squares") + scale_x_discrete(name = "Number of Clusters", limits = c("1", "2", "3", "4", "5", "6", "7"))


########EVALUATE THE PREVISÃO with k=2


results_means1<-function(df_train_semy,df_train_comy,df_test_semy,df_test_comy){
  
  set.seed(47)
  km2<-kmeans(df_train_semy, centers = 2, nstart = 30)
  
  #Performance Measures - In the training set
  
  pre_kmeans <- km2$cluster
  pre_kmeans <- factor(ifelse(pre_kmeans == 1,"B","M")) #1-beligno, 2-maligno
  yy_train <- factor(ifelse(df_train_comy[,length(colnames(df_train_comy))] == 0,"B","M"))
  print("Performance Measures - In the training set")
  print(performance_meas(pre_kmeans,yy_train))
  print(confusionMatrix(data=pre_kmeans,reference=yy_train)$table)
  
  r <- rbind(
    kmeans = c(
      purity=ClusterPurity(pre_kmeans,yy_train),
      entropy=entropy(pre_kmeans,yy_train)$U
    )
  )
  
  print(r)
  
  #Performance Measures - In the test set
  
  test_preds <- predict(km2, df_test_semy)
  test_preds <- factor(ifelse(test_preds == 1,"B","M")) #1-beligno, 2-maligno
  yy_test <- factor(ifelse(df_test_comy[,length(colnames(df_test_comy))] == 0,"B","M"))
  print("Performance Measures - In the test set")
  print(performance_meas(test_preds,yy_test))
  print(confusionMatrix(data=test_preds,reference=yy_test)$table)

  
}

results_means2<-function(df_train_semy,df_train_comy,df_test_semy,df_test_comy){
  
  set.seed(47)
  km2<-kmeans(df_train_semy, centers = 2, nstart = 30)
  
  #Performance Measures - In the training set
  
  pre_kmeans <- km2$cluster
  pre_kmeans <- factor(ifelse(pre_kmeans == 2,"B","M")) #2-benigno, 1-maligno
  yy_train <- factor(ifelse(df_train_comy[,length(colnames(df_train_comy))] == 0,"B","M"))
  print("Performance Measures - In the training set")
  print(performance_meas(pre_kmeans,yy_train))
  print(confusionMatrix(data=pre_kmeans,reference=yy_train)$table)
  
  r <- rbind(
    kmeans = c(
      purity=ClusterPurity(pre_kmeans,yy_train),
      entropy=entropy(pre_kmeans,yy_train)$U
    )
  )
  
  print(r)
  
  #Performance Measures - In the test set
  
  test_preds <- predict(km2, df_test_semy)
  test_preds <- factor(ifelse(test_preds == 2,"B","M")) #2-benigno, 1-maligno
  yy_test <- factor(ifelse(df_test_comy[,length(colnames(df_test_comy))] == 0,"B","M"))
  print("Performance Measures - In the test set")
  print(performance_meas(test_preds,yy_test))
  print(confusionMatrix(data=test_preds,reference=yy_test)$table)
  
  
}

results_means1(dataset_stand_train[,-31],dataset_stand_train, dataset_stand_test[,-31], dataset_stand_test)

####plots feios
set.seed(47)
km<-kmeans(dataset_stand_train[,-31],centers=2,nstart=30)
plotcluster(dataset_stand_train[,-31], km$cluster,col=km$cluster)


########CLUSTER with k=3

set.seed(47)
km3<-kmeans(dataset_stand_train[,-31], centers = 3, nstart = 30)
plotcluster(dataset_stand_train[,-31], km3$cluster,col=km3$cluster)
test_preds3 <- predict(km3, dataset_stand_test[,-31])

###ou

set.seed(47)
km33<-kmeans(rbind(dataset_stand_train[,-31],dataset_stand_test[,-31]), centers = 3, nstart = 30)
plotcluster(rbind(dataset_stand_train[,-31],dataset_stand_test[,-31]), km33$cluster,col=km33$cluster)



