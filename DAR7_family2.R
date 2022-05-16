#second family

#228 - GG37113 female (mother)
#428 - GG37139 male (father)
#571 - GG41336 juv
#572- GG41339 juv

ind_family2 <- c(12, 18, 44, 45)

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

newdf_final_noNA$clust_new_order <- ss
newdf_final_noNA$color_new <- colv

k <- 1
df <- newdf_final_noNA[newdf_final_noNA$ID == ind_family2[k], ]
plot(as.Date(df$date), rep(k, length(df$ID)), col = df$color_new, pch = 19, xlab = "Date", ylab = "ID position", xlim = c(as.Date("2020-07-02"), as.Date("2022-01-22")), ylim = c(0,5))

k <- 2
df <- newdf_final_noNA[newdf_final_noNA$ID == ind_family2[k], ]
points(as.Date(df$date), rep(k, length(df$ID)), col = df$color_new, pch = 19, xlab = "Date", ylab = "ID position", ylim = c(0,5))

k <- 3
df <- newdf_final_noNA[newdf_final_noNA$ID == ind_family2[k], ]
points(as.Date(df$date), rep(k, length(df$ID)), col = df$color_new, pch = 19, xlab = "Date", ylab = "ID position", ylim = c(0,5))

k <- 4
df <- newdf_final_noNA[newdf_final_noNA$ID == ind_family2[k], ]
points(as.Date(df$date), rep(k, length(df$ID)), col = df$color_new, pch = 19, xlab = "Date", ylab = "ID position", ylim = c(0,5))


par(mfrow = c(1,1))
#June-Sep 2021

#ind 1
#June-Sep 2020
df <- newdf_final_noNA[newdf_final_noNA$ID == ind_family2[1], ]
month_l <- c(6:9)
df <- df[df$month %in% month_l, ]
df <- df[df$year == 2020, ]
df_ind <- as.data.frame(table(factor(df$clust_new_order, levels = c(1:7))))
m <- df_ind$Freq
m <- m/sum(m)
v1 <- seq(1,7,1)
barplot(m, ylim = c(0, 0.8), col = c("#ffffb2", "#fecc5c", "#74a9cf", "#fd8d3c", "#f03b20", "#bd0026", "#045a8d"), names.arg = c(),  yaxt="n", cex.lab = 1.5)
axis(2, at=c(0,0.8, lab=c(0, 0.8, 1)))

#Ot 2020 - Jan 2021
df <- newdf_final_noNA[newdf_final_noNA$ID == ind_family2[1], ]
month_l <- c(10,11,12)
df <- df[df$month %in% month_l, ]
df1 <- df[df$year == 2020, ]

df <- newdf_final_noNA[newdf_final_noNA$ID == ind_family2[1], ]
month_l <- c(1)
df <- df[df$month %in% month_l, ]
df2 <- df[df$year == 2021, ]

df <- rbind(df1, df2)

df_ind <- as.data.frame(table(factor(df$clust_new_order, levels = c(1:7))))
m <- df_ind$Freq
m <- m/sum(m)
v1 <- seq(1,7,1)
barplot(m, ylim = c(0, 0.8), col = c("#ffffb2", "#fecc5c", "#74a9cf", "#fd8d3c", "#f03b20", "#bd0026", "#045a8d"), names.arg = c(),  yaxt="n", cex.lab = 1.5)
axis(2, at=c(0,0.8, lab=c(0, 0.8, 1)))

#Feb-May 2021
df <- newdf_final_noNA[newdf_final_noNA$ID == ind_family2[1], ]
month_l <- c(2:5)
df <- df[df$month %in% month_l, ]
df_ind <- as.data.frame(table(factor(df$clust_new_order, levels = c(1:7))))
m <- df_ind$Freq
m <- m/sum(m)
v1 <- seq(1,7,1)
barplot(m, ylim = c(0, 0.8), col = c("#ffffb2", "#fecc5c", "#74a9cf", "#fd8d3c", "#f03b20", "#bd0026", "#045a8d"), names.arg = c(),  yaxt="n", cex.lab = 1.5)
axis(2, at=c(0,0.8, lab=c(0, 0.8, 1)))

#June-Sep 2021
df <- newdf_final_noNA[newdf_final_noNA$ID == ind_family2[1], ]
month_l <- c(6:9)
df <- df[df$month %in% month_l, ]
df <- df[df$year == 2021, ]
df_ind <- as.data.frame(table(factor(df$clust_new_order, levels = c(1:7))))
m <- df_ind$Freq
m <- m/sum(m)
v1 <- seq(1,7,1)
barplot(m, ylim = c(0, 0.8), col = c("#ffffb2", "#fecc5c", "#74a9cf", "#fd8d3c", "#f03b20", "#bd0026", "#045a8d"), names.arg = c(),  yaxt="n", cex.lab = 1.5)
axis(2, at=c(0,0.8, lab=c(0, 0.8, 1)))

#Oct 2021-Jan 2022
df <- newdf_final_noNA[newdf_final_noNA$ID == ind_family2[1], ]
month_l <- c(10,11,12)
df <- df[df$month %in% month_l, ]
df1 <- df[df$year == 2021, ]

df <- newdf_final_noNA[newdf_final_noNA$ID == ind_family2[1], ]
month_l <- c(1)
df <- df[df$month %in% month_l, ]
df2 <- df[df$year == 2022, ]

df <- rbind(df1, df2)

df_ind <- as.data.frame(table(factor(df$clust_new_order, levels = c(1:7))))
m <- df_ind$Freq
m <- m/sum(m)
v1 <- seq(1,7,1)
barplot(m, ylim = c(0, 0.8), col = c("#ffffb2", "#fecc5c", "#74a9cf", "#fd8d3c", "#f03b20", "#bd0026", "#045a8d"), names.arg = c(),  yaxt="n", cex.lab = 1.5)
axis(2, at=c(0,0.8, lab=c(0, 0.8, 1)))

#ind 2
#Feb-May 2021
df <- newdf_final_noNA[newdf_final_noNA$ID == ind_family2[2], ]
month_l <- c(2:5)
df <- df[df$month %in% month_l, ]
df_ind <- as.data.frame(table(factor(df$clust_new_order, levels = c(1:7))))
m <- df_ind$Freq
m <- m/sum(m)
v1 <- seq(1,7,1)
barplot(m, ylim = c(0, 0.8), col = c("#ffffb2", "#fecc5c", "#74a9cf", "#fd8d3c", "#f03b20", "#bd0026", "#045a8d"), names.arg = c(),  yaxt="n", cex.lab = 1.5)
axis(2, at=c(0,0.8, lab=c(0, 0.8, 1)))

#June-Sep 2021
df <- newdf_final_noNA[newdf_final_noNA$ID == ind_family2[2], ]
month_l <- c(6:9)
df <- df[df$month %in% month_l, ]
df <- df[df$year == 2021, ]
df_ind <- as.data.frame(table(factor(df$clust_new_order, levels = c(1:7))))
m <- df_ind$Freq
m <- m/sum(m)
v1 <- seq(1,7,1)
barplot(m, ylim = c(0, 0.8), col = c("#ffffb2", "#fecc5c", "#74a9cf", "#fd8d3c", "#f03b20", "#bd0026", "#045a8d"), names.arg = c(),  yaxt="n", cex.lab = 1.5)
axis(2, at=c(0,0.8, lab=c(0, 0.8, 1)))

#ind 3
#June-Sep 2021
df <- newdf_final_noNA[newdf_final_noNA$ID == ind_family2[3], ]
month_l <- c(6:9)
df <- df[df$month %in% month_l, ]
df <- df[df$year == 2021, ]
df_ind <- as.data.frame(table(factor(df$clust_new_order, levels = c(1:7))))
m <- df_ind$Freq
m <- m/sum(m)
v1 <- seq(1,7,1)
barplot(m, ylim = c(0, 0.8), col = c("#ffffb2", "#fecc5c", "#74a9cf", "#fd8d3c", "#f03b20", "#bd0026", "#045a8d"), names.arg = c(),  yaxt="n", cex.lab = 1.5)
axis(2, at=c(0,0.8, lab=c(0, 0.8, 1)))

#ind 4
#June-Sep 2021
df <- newdf_final_noNA[newdf_final_noNA$ID == ind_family2[4], ]
month_l <- c(6:9)
df <- df[df$month %in% month_l, ]
df <- df[df$year == 2021, ]
df_ind <- as.data.frame(table(factor(df$clust_new_order, levels = c(1:7))))
m <- df_ind$Freq
m <- m/sum(m)
v1 <- seq(1,7,1)
barplot(m, ylim = c(0, 0.8), col = c("#ffffb2", "#fecc5c", "#74a9cf", "#fd8d3c", "#f03b20", "#bd0026", "#045a8d"), names.arg = c(),  yaxt="n", cex.lab = 1.5)
axis(2, at=c(0,0.8, lab=c(0, 0.8, 1)))

#Oct 2021-Jan 2022
df <- newdf_final_noNA[newdf_final_noNA$ID == ind_family2[4], ]
month_l <- c(10,11,12)
df <- df[df$month %in% month_l, ]
df1 <- df[df$year == 2021, ]

df <- newdf_final_noNA[newdf_final_noNA$ID == ind_family2[4], ]
month_l <- c(1)
df <- df[df$month %in% month_l, ]
df2 <- df[df$year == 2022, ]

df <- rbind(df1, df2)

df_ind <- as.data.frame(table(factor(df$clust_new_order, levels = c(1:7))))
m <- df_ind$Freq
m <- m/sum(m)
v1 <- seq(1,7,1)
barplot(m, ylim = c(0, 0.8), col = c("#ffffb2", "#fecc5c", "#74a9cf", "#fd8d3c", "#f03b20", "#bd0026", "#045a8d"), names.arg = c(),  yaxt="n", cex.lab = 1.5)
axis(2, at=c(0,0.8, lab=c(0, 0.8, 1)))
