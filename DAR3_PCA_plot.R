library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

##plot result clustering

ss <- vector()

ss[newdf_final_noNA$clust_tot == 1] <- 1 
ss[newdf_final_noNA$clust_tot == 5] <- 2
ss[newdf_final_noNA$clust_tot == 2] <- 3
ss[newdf_final_noNA$clust_tot == 7] <- 4
ss[newdf_final_noNA$clust_tot == 3] <- 5
ss[newdf_final_noNA$clust_tot == 4] <- 6
ss[newdf_final_noNA$clust_tot == 6] <- 7

colv <- vector()
colv[ss == 1] <- "#ffffb2" 
colv[ss == 2] <- "#fecc5c"
colv[ss == 3] <- "#74a9cf"
colv[ss == 4] <- "#fd8d3c"
colv[ss == 5] <- "#f03b20" 
colv[ss == 6] <- "#bd0026"
colv[ss == 7] <- "#045a8d"

plot(as.Date(newdf_final_noNA$date), newdf_final_noNA$ID, col = colv, pch = 19, xlab = "Date", ylab = "ID position")
newdf_final_noNA$color_new <- colv

#for compact plot
ID_no_gaps <- c()

for (i in 1:4){
  ID_no_gaps[newdf_final_noNA$ID == i] <- i
}

for (i in 6:20){
  ID_no_gaps[newdf_final_noNA$ID == i] <- i - 1
}

for (i in 23:47){
  ID_no_gaps[newdf_final_noNA$ID == i] <- i - 3
}

plot(as.Date(newdf_final_noNA$date), ID_no_gaps, col = newdf_final_noNA$color_new, pch = 19, xlab = "Date", ylab = "ID position")

col_lv <- sort(c("#ffffb2", "#fecc5c", "#74a9cf", "#fd8d3c", "#f03b20", "#bd0026", "#045a8d"))

new_df_pca <- newdf_final_noNA[c(2:5)]
colnames(new_df_pca) <- c("a", "b", "c", "d")
df.pca <- prcomp(new_df_pca, center = TRUE, scale. = FALSE)
summary(df.pca)
ggbiplot(df.pca, ellipse=FALSE, groups=newdf_final_noNA$color_new, alpha = 1, varname.size = 2.0) +
  scale_colour_manual(name="Group", values= col_lv)
