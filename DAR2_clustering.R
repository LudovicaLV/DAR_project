#overall clustering
library(pheatmap)
library(tidyverse) 
library(cluster)   
library(factoextra) 

#create dataframe with all individual data
newdf_final <- data.frame(matrix(ncol = 7, nrow = 0))

#remove individuals with too little data
final_ind <- c(1:n_ind)[-c(5,21,22)]

for (ind_individual in final_ind){
  #output of measures calculation
  df_ind <- readRDS(paste(".../", ind_individual, ".rds", sep = ""))
  ind_ok_time <- readRDS(file = paste("ind_ok_time", ind_individual, ".rds", sep = ""))
  df_ind$ind_ok_time <- ind_ok_time
  newdf_final <- rbind(newdf_final, df_ind)
}

#remove rows with NAs
newdf_final_noNA <- na.omit(newdf_final)

#select column with measurements
s_df <- scale(newdf_final_noNA[c(2:5)])
data <- as.matrix(s_df)

#analysis to choose the number of clusters - elbow method
fviz_nbclust(data, FUN = hcut, method = "wss", k.max = 15)

#choice of cluster number
nclust <- 7

#clustering functions
dist_mat <- dist(data, method = 'euclidean')
hclust_avg <- hclust(dist_mat, method = 'ward.D')
clusters_row <- cutree(hclust_avg, k = nclust)

table(clusters_row)

###proceed
newdf_final_noNA$clust_tot <- clusters_row
saveRDS(newdf_final_noNA, file = "newdf_final_noNA.rds")
