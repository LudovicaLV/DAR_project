##by gender and age
info <- read_csv("/Users/ludovicaluisavissat/Desktop/Owl Ringing Data - nesting and survival 26012022.csv")

setwd("/Users/ludovicaluisavissat/Desktop/Owls_tracks")
file_names = list.files(pattern="*.RData")

names_of_interest <- c()
n <- str_sub(file_names[1],12,nchar(file_names[1])-6)
names_of_interest <- c(names_of_interest, n)

for (i in 2:length(file_names)){
  n <- str_sub(file_names[i],13,nchar(file_names[i])-6)
  names_of_interest <- c(names_of_interest, n)
}

##gender
gv <- c()
for (i in 1:length(names_of_interest)){
  m <- match(names_of_interest[i], info$Number)
  g <- info$Sex[m]
  gv <- c(gv, g)
}

gv[23] <- "Female"

final_gv <- gv[final_ind]

df_fem <- rep(0, 7)
df_mal <- rep(0, 7)

cf <- 0
cm <- 0

for (ind_gv in 1:44){
  ind_g <- final_ind[ind_gv]
  newdf_consider_point <- newdf_final_noNA[newdf_final_noNA$ID == ind_g, ]
  df_gen <- as.data.frame(table(factor(newdf_consider_point$clust_tot, levels = c(1:7))))
  if (final_gv[ind_gv] == "Female"){
    df_fem <- df_fem + df_gen$Freq
    cf <- cf + 1 
  }else{
    df_mal <- df_mal + df_gen$Freq
    cm <- cm + 1
  }
}

m <- df_fem
m <- df_fem/sum(m)
m <- m[c(1,5,2,7,3,4,6)]
v1 <- seq(1,7,1)
barplot(m, col = c("#ffffb2", "#fecc5c", "#74a9cf", "#fd8d3c", "#f03b20", "#bd0026", "#045a8d"), names.arg = c(), ylab = "Frequency", yaxt="n", cex.lab = 1.5)
axis(2, at=c(0,max(m)), lab=c(0, round(max(m), 1)))

m <- df_mal
m <- df_mal/sum(m)
m <- m[c(1,5,2,7,3,4,6)]
v1 <- seq(1,7,1)
barplot(m, col = c("#ffffb2", "#fecc5c", "#74a9cf", "#fd8d3c", "#f03b20", "#bd0026", "#045a8d"), names.arg = c(), ylab = "Frequency", yaxt="n", cex.lab = 1.5)
axis(2, at=c(0,max(m)), lab=c(0, round(max(m), 1)))
m <- matrix(c(df_fem, df_mal), nrow = 7)
cmm <- chisq.test(m)
cmm$p.value

##age
av <- c()
for (i in 1:length(names_of_interest)){
  m <- match(names_of_interest[i], info$Number)
  a <- info$Age[m]
  av <- c(av, a)
}

final_av <- av[final_ind]

df_you <- rep(0, 7)
df_adu <- rep(0, 7)

cy <- 0
ca <- 0

for (ind_av in 1:44){
  ind_a <- final_ind[ind_av]
  newdf_consider_point <- newdf_final_noNA[newdf_final_noNA$ID == ind_a, ]
  df_age <- as.data.frame(table(factor(newdf_consider_point$clust_tot, levels = c(1:7))))
  if (final_av[ind_av] <= 3){
    df_you <- df_you + df_age$Freq
    cy <- cy + 1
  }else{
    df_adu <- df_adu + df_age$Freq
    ca <-  ca + 1
  }
}

m <- df_you
m <- df_you/sum(m)
m <- m[c(1,5,2,7,3,4,6)]
v1 <- seq(1,7,1)
barplot(m, col = c("#ffffb2", "#fecc5c", "#74a9cf", "#fd8d3c", "#f03b20", "#bd0026", "#045a8d"), names.arg = c(), ylab = "Frequency", yaxt="n", cex.lab = 1.5)
axis(2, at=c(0,max(m)), lab=c(0, round(max(m), 1)))

m <- df_adu
m <- df_adu/sum(m)
m <- m[c(1,5,2,7,3,4,6)]
v1 <- seq(1,7,1)
barplot(m, col = c("#ffffb2", "#fecc5c", "#74a9cf", "#fd8d3c", "#f03b20", "#bd0026", "#045a8d"), names.arg = c(), ylab = "Frequency", yaxt="n", cex.lab = 1.5)
axis(2, at=c(0,max(m)), lab=c(0, round(max(m), 1)))

m <- matrix(c(df_you, df_adu), nrow = 7)
cm <- chisq.test(m)
cm$p.value

#by start location
df_pt_list <- list()

for (point_ind in c(1:3)){
  newdf_consider_point <- newdf_final_noNA[newdf_final_noNA$start_point_cluster == point_ind, ]
  df_pt <- as.data.frame(table(factor(newdf_consider_point$clust_tot, levels = c(1:7))))
  df_pt_list[[point_ind]] <- df_pt
}

m <- df_pt_list[[1]]$Freq
m <- df_pt_list[[1]]$Freq/sum(m)
m <- m[c(1,5,2,7,3,4,6)]
v1 <- seq(1,7,1)
barplot(m, col = c("#ffffb2", "#fecc5c", "#74a9cf", "#fd8d3c", "#f03b20", "#bd0026", "#045a8d"), names.arg = c(), ylab = "Frequency", yaxt="n", cex.lab = 1.5)
axis(2, at=c(0,max(m)), lab=c(0, round(max(m), 1)))

m <- df_pt_list[[2]]$Freq
m <- df_pt_list[[2]]$Freq/sum(m)
m <- m[c(1,5,2,7,3,4,6)]
v1 <- seq(1,7,1)
barplot(m, col = c("#ffffb2", "#fecc5c", "#74a9cf", "#fd8d3c", "#f03b20", "#bd0026", "#045a8d"), names.arg = c(), ylab = "Frequency", yaxt="n", cex.lab = 1.5)
axis(2, at=c(0,max(m)), lab=c(0, round(max(m), 1)))

m <- df_pt_list[[3]]$Freq
m <- df_pt_list[[3]]$Freq/sum(m)
m <- m[c(1,5,2,7,3,4,6)]
v1 <- seq(1,7,1)
barplot(m, col = c("#ffffb2", "#fecc5c", "#74a9cf", "#fd8d3c", "#f03b20", "#bd0026", "#045a8d"), names.arg = c(), ylab = "Frequency", yaxt="n", cex.lab = 1.5)
axis(2, at=c(0,max(m)), lab=c(0, round(max(m), 1)))

##all 3 together
m <- matrix(c(df_pt_list[[1]]$Freq, df_pt_list[[2]]$Freq, df_pt_list[[3]]$Freq), nrow = 7)
cm <- chisq.test(m)
cm$p.value

