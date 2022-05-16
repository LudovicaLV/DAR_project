#first family

#225 - GG37131 female (mother)
#565 - GG41309 juv
#568 - GG41310 juv 
#567 - GG41312 juv

ind_family1 <- c(14, 41, 42, 43)

clust_new_order <- vector()

clust_new_order[newdf_final_noNA$clust_tot == 1] <- 1
clust_new_order[newdf_final_noNA$clust_tot == 5] <- 2
clust_new_order[newdf_final_noNA$clust_tot == 2] <- 3
clust_new_order[newdf_final_noNA$clust_tot == 7] <- 4
clust_new_order[newdf_final_noNA$clust_tot == 3] <- 5
clust_new_order[newdf_final_noNA$clust_tot == 4] <- 6
clust_new_order[newdf_final_noNA$clust_tot == 6] <- 7

newdf_final_noNA$clust_new_order <- clust_new_order

k <- 1
df <- newdf_final_noNA[newdf_final_noNA$ID == ind_family1[k], ]
plot(as.Date(df$date), rep(k, length(df$ID)), col = df$color_new, pch = 19, xlab = "Date", ylab = "ID position", xlim = c(as.Date("2020-11-19"), as.Date("2022-02-08")), ylim = c(0,5))

k <- 2
df <- newdf_final_noNA[newdf_final_noNA$ID == ind_family1[k], ]
points(as.Date(df$date), rep(k, length(df$ID)), col = df$color_new, pch = 19, xlab = "Date", ylab = "ID position", ylim = c(0,5))

k <- 3
df <- newdf_final_noNA[newdf_final_noNA$ID == ind_family1[k], ]
points(as.Date(df$date), rep(k, length(df$ID)), col = df$color_new, pch = 19, xlab = "Date", ylab = "ID position", ylim = c(0,5))

k <- 4
df <- newdf_final_noNA[newdf_final_noNA$ID == ind_family1[k], ]
points(as.Date(df$date), rep(k, length(df$ID)), col = df$color_new, pch = 19, xlab = "Date", ylab = "ID position", ylim = c(0,5))

par(mfrow = c(1,1))
#mother: 

#Oct 2020-Jan 2021
df <- newdf_final_noNA[newdf_final_noNA$ID == 14, ]
month_l <- c(10,11,12)
df <- df[df$month %in% month_l, ]
df1 <- df[df$year == 2020, ]

df <- newdf_final_noNA[newdf_final_noNA$ID == 14, ]
month_l <- c(1)
df <- df[df$month %in% month_l, ]
df2 <- df[df$year == 2021, ]

df <- rbind(df1, df2)

df_ind <- as.data.frame(table(factor(df$clust_new_order, levels = c(1:7))))
m <- df_ind$Freq
m <- m/sum(m)
v1 <- seq(1,7,1)
barplot(m, ylim = c(0, 0.7), col = c("#ffffb2", "#fecc5c", "#74a9cf", "#fd8d3c", "#f03b20", "#bd0026", "#045a8d"), names.arg = c(),  yaxt="n", cex.lab = 1.5)
axis(2, at=c(0,0.7, lab=c(0, 0.7, 1)))

#Feb-May 2021
df <- newdf_final_noNA[newdf_final_noNA$ID == 14, ]
month_l <- c(2:5)
df <- df[df$month %in% month_l, ]
df_ind <- as.data.frame(table(factor(df$clust_new_order, levels = c(1:7))))
m <- df_ind$Freq
m <- m/sum(m)
v1 <- seq(1,7,1)
barplot(m, ylim = c(0, 0.7), col = c("#ffffb2", "#fecc5c", "#74a9cf", "#fd8d3c", "#f03b20", "#bd0026", "#045a8d"), names.arg = c(),  yaxt="n", cex.lab = 1.5)
axis(2, at=c(0,0.7, lab=c(0, 0.7, 1)))

#June-Sep 2021
df <- newdf_final_noNA[newdf_final_noNA$ID == 14, ]
month_l <- c(6:9)
df <- df[df$month %in% month_l, ]
df <- df[df$year == 2021, ]
df_ind <- as.data.frame(table(factor(df$clust_new_order, levels = c(1:7))))
m <- df_ind$Freq
m <- m/sum(m)
v1 <- seq(1,7,1)
barplot(m, ylim = c(0, 0.7), col = c("#ffffb2", "#fecc5c", "#74a9cf", "#fd8d3c", "#f03b20", "#bd0026", "#045a8d"), names.arg = c(),  yaxt="n", cex.lab = 1.5)
axis(2, at=c(0,0.7, lab=c(0, 0.7, 1)))

#Oct 2021-Jan 2022
df <- newdf_final_noNA[newdf_final_noNA$ID == 14, ]
month_l <- c(10,11,12)
df <- df[df$month %in% month_l, ]
df1 <- df[df$year == 2021, ]

df <- newdf_final_noNA[newdf_final_noNA$ID == 14, ]
month_l <- c(1)
df <- df[df$month %in% month_l, ]
df2 <- df[df$year == 2022, ]

df <- rbind(df1, df2)

df_ind <- as.data.frame(table(factor(df$clust_new_order, levels = c(1:7))))
m <- df_ind$Freq
m <- m/sum(m)
v1 <- seq(1,7,1)
barplot(m, ylim = c(0, 0.7), col = c("#ffffb2", "#fecc5c", "#74a9cf", "#fd8d3c", "#f03b20", "#bd0026", "#045a8d"), names.arg = c(),  yaxt="n", cex.lab = 1.5)
axis(2, at=c(0,0.7, lab=c(0, 0.7, 1)))

## first young

#June-Sep 2021
df <- newdf_final_noNA[newdf_final_noNA$ID == 41, ]
month_l <- c(6:9)
df <- df[df$month %in% month_l, ]
df <- df[df$year == 2021, ]
df_ind <- as.data.frame(table(factor(df$clust_new_order, levels = c(1:7))))
m <- df_ind$Freq
m <- m/sum(m)
v1 <- seq(1,7,1)
barplot(m, ylim = c(0, 0.7), col = c("#ffffb2", "#fecc5c", "#74a9cf", "#fd8d3c", "#f03b20", "#bd0026", "#045a8d"), names.arg = c(),  yaxt="n", cex.lab = 1.5)
axis(2, at=c(0,0.7, lab=c(0, 0.7, 1)))

#second young

#June-Sep 2021
df <- newdf_final_noNA[newdf_final_noNA$ID == 42, ]
month_l <- c(6:9)
df <- df[df$month %in% month_l, ]
df <- df[df$year == 2021, ]
df_ind <- as.data.frame(table(factor(df$clust_new_order, levels = c(1:7))))
m <- df_ind$Freq
m <- m/sum(m)
v1 <- seq(1,7,1)
barplot(m, ylim = c(0, 0.7), col = c("#ffffb2", "#fecc5c", "#74a9cf", "#fd8d3c", "#f03b20", "#bd0026", "#045a8d"), names.arg = c(),  yaxt="n", cex.lab = 1.5)
axis(2, at=c(0,0.7, lab=c(0, 0.7, 1)))

#Oct 2021-Jan 2022
df <- newdf_final_noNA[newdf_final_noNA$ID == 42, ]
month_l <- c(10,11,12)
df <- df[df$month %in% month_l, ]
df1 <- df[df$year == 2021, ]

df <- newdf_final_noNA[newdf_final_noNA$ID == 42, ]
month_l <- c(1)
df <- df[df$month %in% month_l, ]
df2 <- df[df$year == 2022, ]

df <- rbind(df1, df2)

df_ind <- as.data.frame(table(factor(df$clust_new_order, levels = c(1:7))))
m <- df_ind$Freq
m <- m/sum(m)
v1 <- seq(1,7,1)
barplot(m, ylim = c(0, 0.7), col = c("#ffffb2", "#fecc5c", "#74a9cf", "#fd8d3c", "#f03b20", "#bd0026", "#045a8d"), names.arg = c(),  yaxt="n", cex.lab = 1.5)
axis(2, at=c(0,0.7, lab=c(0, 0.7, 1)))

#third young

#June-Sep 2021
df <- newdf_final_noNA[newdf_final_noNA$ID == 43, ]
month_l <- c(6:9)
df <- df[df$month %in% month_l, ]
df <- df[df$year == 2021, ]
df_ind <- as.data.frame(table(factor(df$clust_new_order, levels = c(1:7))))
m <- df_ind$Freq
m <- m/sum(m)
v1 <- seq(1,7,1)
barplot(m, ylim = c(0, 0.7), col = c("#ffffb2", "#fecc5c", "#74a9cf", "#fd8d3c", "#f03b20", "#bd0026", "#045a8d"), names.arg = c(),  yaxt="n", cex.lab = 1.5)
axis(2, at=c(0,0.7, lab=c(0, 0.7, 1)))

#Oct 2021-Jan 2022
df <- newdf_final_noNA[newdf_final_noNA$ID == 43, ]
month_l <- c(10,11,12)
df <- df[df$month %in% month_l, ]
df1 <- df[df$year == 2021, ]

df <- newdf_final_noNA[newdf_final_noNA$ID == 43, ]
month_l <- c(1)
df <- df[df$month %in% month_l, ]
df2 <- df[df$year == 2022, ]

df <- rbind(df1, df2)

df_ind <- as.data.frame(table(factor(df$clust_new_order, levels = c(1:7))))
m <- df_ind$Freq
m <- m/sum(m)
v1 <- seq(1,7,1)
barplot(m, ylim = c(0, 0.7), col = c("#ffffb2", "#fecc5c", "#74a9cf", "#fd8d3c", "#f03b20", "#bd0026", "#045a8d"), names.arg = c(),  yaxt="n", cex.lab = 1.5)
axis(2, at=c(0,0.7, lab=c(0, 0.7, 1)))