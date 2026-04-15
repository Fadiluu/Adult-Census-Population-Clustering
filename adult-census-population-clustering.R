#0. Importing Libraries ----
install.packages("tidyverse")
install.packages("fastDummies")
install.packages("cluster") 
install.packages("factoextra")
install.packages("plotly")
install.packages("pheatmap") 
install.packages('igraph')
install.packages('mclust')
install.packages("dbscan")
install.packages("splitstackshape")
install.packages("pheatmap")

library(ggplot2)
library(dplyr)
library(fastDummies)
library(cluster)
library(factoextra)
library(plotly)
library(pheatmap)
library(igraph)
library(mclust)
library(dbscan)
library(tidyverse)
library(splitstackshape)
library(pheatmap)
library(ggplot2)


#1. Loading and Pre-processing Data ----
  
  main_data<-read.csv("adult.csv", header = FALSE)
  
  # class(data)
  # View(data)

  #1.1 Naming and removing unecessary columns
  colnames(main_data) <- c("age", "workclass", "fnlwgt", "education", 
                           "education_num", "marital_status", "occupation", 
                           "relationship", "race", "sex","capital_gain", 
                           "capital_loss", "hours_per_week", "native_country", 
                           "income")
  
  main_data<-select(main_data, -c("fnlwgt", "education_num","occupation",
                                  "relationship", "capital_gain", "capital_loss",
                                  "native_country","hours_per_week"))
  
  # View(main_data)
  
  #1.2 Dealing with NA values
  main_data[main_data==""]<-NA
  
  View(main_data)
  
  colSums(is.na(main_data))
  
  main_data<-main_data[complete.cases(main_data), ]
  
  colSums(is.na(main_data))
  
  #1.3 Sampling
  ggplot(main_data) +
    aes(x = workclass) +
    geom_bar() +
    labs(x = "Work Class",
         y = "Count",
         title = "Work Class Frequency")

  ggplot(main_data) +
    aes(x = education) +
    geom_bar() +
    labs(x = "education",
         y = "Count",
         title = "Education Frequency")

  ggplot(main_data) +
    aes(x = race) +
    geom_bar() +
    labs(x = "race",
         y = "Count",
         title = " Race Frequency")
  
  set.seed(937)
  data<-stratified(main_data, group=c("workclass","education","race"),
                                           size=0.01,replace=FALSE)
  
  View(data)
  summary(data)
  
  #1.4 Recoding column values
  income<-recode(data$income, "<=50K" = 0, ">50K" = 1, .default = 0)
  # income
  
  # unique(data$workclass)
  workclass<-recode(data$workclass, "Self-emp-not-inc" = "Self-emp", 
                    "Self-emp-inc" = "Self-emp", "Federal-gov" = "gov",
                    "State-gov" = "gov", "Local-gov" = "gov",
                    .default = data$workclass)
  # workclass
  
  # unique(data$marital_status)
  marital_status<-recode(data$marital_status, "Never-married" = "Single", 
                         "Married-civ-spouse" = "Married", 
                         "Married-spouse-absent" = "Married", 
                         "Married-AF-spouse" = "Married",
                         .default = data$marital_status)
  # marital_status
  
  # unique(data$education)
  #HS = High School, MS = Middle School, PS = Primary School
  education<-recode(data$education, "11th" = "HS", "12th" = "HS",
                    "9th" = "HS", "7th-8th" = "MS", "5th-6th" = "MS",
                    "1st-4th" = "PS", .default = data$education)
  # education
  
  data$income<-income
  data$workclass<-workclass
  data$marital_status<-marital_status
  data$education<-education
  
  View(data)
  
  #1.4 One-hot Encoding
  numerical<-data %>% select_if(is.numeric)
  # View(numerical)
  categorical<-data %>% select_if(is.character)
  # View(categorical)

  one_hot_encoded<-dummy_cols(categorical,remove_first_dummy = TRUE)
  # View(one_hot_encoded)
  one_hot_encoded<-one_hot_encoded[,6:27]
  # View(one_hot_encoded)

  data<-cbind(numerical,one_hot_encoded)
  View(data)

  #1.5 Scaling/Normalization
  # data[,1:24]<-scale(data[,1:24])
  
  scaled_data <- scale(data[, 1:24])
  data[, 1:24] <- as.data.frame(scaled_data)
  View(data)
  
#2. Estimation of number of clusters ----
  
  #2.1 Sum of Square
  fit_km<-list()
  sum_square<-list()
  
  for(i in 1:10){
    ?kmeans
    fit_km[[i]]<-kmeans(data, i, nstart = 5, iter.max = 10)
    sum_square[[i]]<-fit_km[[i]]$betweenss/fit_km[[i]]$totss
  }
  plot(1:10, sum_square, type='b', ylab='between SS/total SS') #k=5
  
  #2.2 Elbow method
  ?fviz_nbclust
  fviz_nbclust(data,kmeans,"wss") #k=5
  fviz_nbclust(data,kmeans,"gap_stat") #k=1
  fviz_nbclust(data,kmeans,"silhouette") #k=8
  
  #2.3 Statistical model
  fit_m<-Mclust(data)
  plot(fit_m) #k=2
  
#3. K-means Clustering ----
  
  set.seed(123)
  k<-4
  
  kmeans_result <- kmeans(data, centers = k, nstart = 25, iter.max = 100)
  
  # Convert data matrix back to data frame
  data <- as.data.frame(data)
  
  data$cluster <- as.factor(kmeans_result$cluster)
  
  fviz_cluster(kmeans_result, data = data[, -ncol(data)], geom = "point", stand = FALSE)
  
  # Analyzing Clusters
  cluster_centers <- kmeans_result$centers
  print(cluster_centers)
  
  cluster_distribution <- table(data$cluster)
  print(cluster_distribution)
  
  demographic_distribution <- data %>%
    group_by(cluster) %>%
    summarise(
      age_mean = mean(age),
      proportion_high_income = mean(income),
      workclass_distribution = list(table(workclass)),
      marital_status_distribution = list(table(marital_status)),
      education_distribution = list(table(education)),
      race_Asian_Pac_Islander_distribution = list(table('race_ Asian-Pac-Islander')),
      race_Black_distribution = list(table('race_ Black')),
      race_Other_distribution = list(table('race_ Other')),
      race_White_distribution = list(table('race_ White')),
      sex_distribution = list(table('sex_ Male'))  
    )
  
  print(demographic_distribution)
  
  
  ##Age Dist.
  ggplot(data, aes(x = cluster, y = age, fill = cluster)) +
    geom_boxplot() +
    labs(title = "Age Distribution by Cluster")
  ## Income Dist.
  ggplot(data, aes(x = cluster, fill = as.factor(income))) +
    geom_bar(position = "fill") +
    labs(title = "Income Distribution by Cluster", y = "Proportion")
  
  workclass_distribution_plot <- data %>%
    group_by(cluster, workclass_Private, workclass_Self-emp) %>%
    summarise(workclass_count = n()) %>%
    pivot_longer(cols = c(workclass_Private, workclass_Self-emp), names_to = "workclass", values_to = "count") %>%
    ggplot(aes(x = cluster, y = count, fill = workclass)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Workclass Distribution by Cluster")
  
  print(workclass_distribution_plot)
  
  
  marital_status_distribution_plot <- data %>%
    group_by(cluster, marital_status_Married, marital_status_Separated, marital_status_Single, marital_status_Widowed) %>%
    summarise(status_count = n()) %>%
    pivot_longer(cols = c(marital_status_Married, marital_status_Separated, marital_status_Single, marital_status_Widowed), names_to = "marital_status", values_to = "count") %>%
    ggplot(aes(x = cluster, y = count, fill = marital_status)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Marital Status Distribution by Cluster")
  
  print(marital_status_distribution_plot)
  
  education_distribution_plot <- data %>%
    group_by(cluster, education_Assoc-acdm, education_Assoc-voc, education_Bachelors, education_Doctorate, education_HS, education_HS-grad, education_Masters, education_MS, education_Prof-school, education_PS, education_Some-college) %>%
    summarise(count_education = n()) %>%
    pivot_longer(cols = c(education_Assoc-acdm, education_Assoc-voc, education_Bachelors, education_Doctorate, education_HS, education_HS-grad, education_Masters, education_MS, education_Prof-school, education_PS, education_Some-college), names_to = "education", values_to = "count") %>%
    ggplot(aes(x = cluster, y = count, fill = education)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Education Distribution by Cluster")
  
  print(education_distribution_plot)
  
  
  race_distribution_plot <- data %>%
    group_by(cluster, 'race_ Asian-Pac-Islander', 'race_ Black, race_ Other', 'race_ White') %>%
    summarise(count_race = n()) %>%
    pivot_longer(cols = c('race_ Asian-Pac-Islander', 'race_ Black', 'race_ Other', 'race_ White'), names_to = "race", values_to = "count") %>%
    ggplot(aes(x = cluster, y = count, fill = race)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Race Distribution by Cluster")
  
  print(race_distribution_plot)
  
  ##Sex Distribution
  
  sex_distribution_plot <- data %>%
    group_by(cluster, 'sex_ Male') %>%
    summarise(count = n()) %>%
    ggplot(aes(x = cluster, y = count, fill = 'sex_ Male')) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Sex Distribution by Cluster")
  
  print(sex_distribution_plot)
  

  write.csv(cluster_centers, "cluster_centers.csv")
  view("cluster_centers.csv")
  
#4. Hierarchical Clustering ----
  
  ?dist
  ?hclust
  fit_hc<-hclust(dist(data),"ward.D2")
  
  ?cutree
  
  #4.1 getting the clusters
  ## changing to 7 here in order for 7 clusters
  hc_clusters<-cutree(fit_hc,7)
  data$clusters<-hc_clusters
  
  #4.2 visualization
  ?rect.hclust
  plot(fit_hc,hang = TRUE)
  abline(h = 7, col = "red")
  rect.hclust(fit_hc,k=7,border="red")
  
  
  ?fviz_dend
  fviz_dend(as.dendrogram(fit_hc),k=7, cex=0.5,rect_fill = TRUE)
  
  #lets make it look nicer
  fviz_dend(as.dendrogram(fit_hc),k=7, cex=1,
            type="phylogenic",
            phylo_layout="layout_as_tree",lwd=2)
  
  #let's have a look at what is happening with first cluster
  filtered_data_base_c1 <- data[data$clusters == 1]
  print(filtered_data_base_c1)
  write.csv(filtered_data_base_c1,"cluster 1.csv") 
  
  
  filtered_data_base_c2 <- data[data$clusters == 2]
  write.csv(filtered_data_base_c2,"cluster 2.csv") 
  
  filtered_data_base_c3 <- data[data$clusters == 3]
  write.csv(filtered_data_base_c3,"cluster 3.csv") 
  
  filtered_data_base_c4 <- data[data$clusters == 4]
  write.csv(filtered_data_base_c4,"cluster 44.csv") 
  
  filtered_data_base_c5 <- data[data$clusters == 5]
  write.csv(filtered_data_base_c5,"cluster 5.csv") 
  
  filtered_data_base_c6 <- data[data$clusters == 6]
  write.csv(filtered_data_base_c6,"cluster 6.csv") 
  
  filtered_data_base_c7 <- data[data$clusters == 7]
  write.csv(filtered_data_base_c7,"cluster 7.csv")
  
#5. Density Clustering ----
  
  kNNdistplot(data, k = 6)
  abline(h = 0.85, col = "red", lty = 2)
  #eps=h, minPts=num of cols+1
  ?dbscan
  fit_dbscan <- dbscan(data, eps = 0.85, minPts = 9)
  g <- fviz_cluster(fit_dbscan, data = data, 
                    geom = "point", main = "DBSCAN Clustering")
  ggplotly(g)
  
#6. Cluster Evaluation  ----
  
  #6.1 K-means Clustering
  
  set.seed(123)
  k <- 5
  kmeans_result <- kmeans(data, centers = k, nstart = 25, iter.max = 100)
  data <- as.data.frame(data)
  data$cluster <- as.factor(kmeans_result$cluster)
  fviz_cluster(kmeans_result, data = data[, -ncol(data)], geom = "point", stand = FALSE)
  
  # Analyzing Clusters
  cluster_centers <- kmeans_result$centers
  print(cluster_centers)
  cluster_distribution <- table(data$cluster)
  print(cluster_distribution)
  
  # Silhouette Method
  silhouette_1 <- silhouette(kmeans_result$cluster, dist(data))
  fviz_silhouette(silhouette_1, main = 'Silhouette plot', palette = "jco")
  
  #6.2 Density Clustering
  
  #Silhouette Method
  
  ?silhouette
  sihouette<-silhouette(fit_dbscan$cluster,dist(data))
  View(sihouette)
  
  ?fviz_silhouette
  fviz_silhouette(sihouette,
                  main='Silhouette plot',
                  palette = "jco" #colour palette
  )