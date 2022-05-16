#create dataframe with all individual data
newdf_final_point <- data.frame(matrix(ncol = 9, nrow = 0))

final_ind <- c(1:n_ind)[-c(5,21,22)]

for (ind_individual in final_ind){
  df_ind <- readRDS(paste(".../", ind_individual, ".rds", sep = ""))
  ind_ok_time <- readRDS(paste(".../ind_ok_time", ind_individual, ".rds", sep = ""))
  df_ind$ind_ok_time <- ind_ok_time
  
  x_f <- readRDS(paste(".../first_x", ind_individual, "_5.rds", sep = ""))
  y_f <- readRDS(paste(".../first_y", ind_individual, "_5.rds", sep = ""))
  
  df_ind$x_f <- x_f
  df_ind$y_f <- y_f
  
  newdf_final_point <- rbind(newdf_final_point, df_ind)
}

#remove rows with NAs
newdf_final_point_noNA <- na.omit(newdf_final_point)

#create whole dataframe
xy_df <- data.frame(x = newdf_final_point_noNA$x_f, y = newdf_final_point_noNA$y_f, ID = newdf_final_point_noNA$ID)
data <- as.matrix(xy_df[c(1:2)])

#analysis to choose the number of clusters - elbow method
fviz_nbclust(data, FUN = hcut, method = "wss", k.max = 15)

#subcluster number 
nclust <- 3

#clustering functions
dist_mat <- dist(data, method = 'euclidean')
hclust_avg <- hclust(dist_mat, method = 'ward.D')
clusters_row <- cutree(hclust_avg, k = nclust)

table(clusters_row)

xy_df$cluster_point <- clusters_row

colvp <- vector()
colvp[xy_df$cluster_point == 1] <- "magenta" 
colvp[xy_df$cluster_point == 2] <- "gray"
colvp[xy_df$cluster_point == 3] <- "darkgreen"

plot(xy_df$x, xy_df$y, col = colvp, xlab = "X", ylab = "Y", pch = 19)
newdf_final_noNA$start_point_cluster <- clusters_row
saveRDS(newdf_final_noNA, file = "newdf_final_noNA_with_points.rds")

colvp <- vector()
colvp[newdf_final_noNA$start_point_cluster == 1] <- "magenta" 
colvp[newdf_final_noNA$start_point_cluster == 2] <- "gray"
colvp[newdf_final_noNA$start_point_cluster == 3] <- "darkgreen"

plot(as.Date(newdf_final_noNA$date), ID_no_gaps, col = colvp, pch = 19, xlab = "Date", ylab = "ID position")

x1<- xy_df[xy_df$cluster_point == 1,]
x2<- xy_df[xy_df$cluster_point == 2,]
x3<- xy_df[xy_df$cluster_point == 3,]

write.csv(data.frame(X= x1$x, Y = x1$y),paste("x_pink.csv"))
write.csv(data.frame(X= x2$x, Y = x2$y),paste("x_gray.csv"))
write.csv(data.frame(X= x3$x, Y = x3$y),paste("x_green.csv"))
