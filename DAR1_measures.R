#load needed libraries
library(lubridate)
library(tidyverse)
library(toolsForAtlas)

setwd(".../Owls_tracks") 
file_names = list.files(pattern="*.RData")

n_ind <- length(file_names)

for (ind_individual in 1:n_ind){
  print(ind_individual)
  #load the data 
  b1 <- load(paste(".../Owls_tracks/", file_names[ind_individual], sep = ""))
  d <- ringTrack
  unique(d$date)
  
  #create a list (mid_to_mid) of DARs, by selecting the data depending on the night ID
  mid_to_mid <- d %>%
    group_by(Night) %>%
    group_split()
  
  #explore dates
  #  for (i in 1:length(mid_to_mid)){
  #    print(unique(as.Date(mid_to_mid[[i]]$dateTime)))
  #  }
  
  #extract the dates for each DARs
  as_date_v <- list()
  dl <- 1
  
  for (i in 1:length(mid_to_mid)){
    as_date_v[[dl]] <- unique(as.Date(mid_to_mid[[i]]$dateTime))
    dl <- dl + 1
  }
  
  #make sure they are consecutive and that there are only two
  ind_ok <- vector()
  
  for (i in 1:length(as_date_v)){
    if ((as_date_v[[i]][2] - as_date_v[[i]][1]) == 1 && length(as_date_v[[i]]) == 2){
      ind_ok <- c(ind_ok, i)
    }
  }
  
  #select only the ones that start before 9pm and end after 2am
  ind_ok_time <- vector()
  
  for (i in ind_ok){
    h1 <- hour(mid_to_mid[[i]]$dateTime[1])
    l <- length(mid_to_mid[[i]]$dateTime)
    hf <- hour(mid_to_mid[[i]]$dateTime[l])
    if (h1 <= 21 && hf >= 2){
      ind_ok_time <- c(ind_ok_time, i)
    }
  }
  
  saveRDS(mid_to_mid, file = paste("mid_to_mid", ind_individual, ".rds", sep = ""))
  saveRDS(ind_ok_time, file = paste("ind_ok_time", ind_individual, ".rds", sep = ""))
  
  #start extracting the DAR measures
  
  #start and end point (using seg.ID)
  Ncut <- 20
  first_x <- c()
  first_y <- c()
  last_x <- c()
  last_y <- c()
  
  for (i in ind_ok_time){
    DATA <- mid_to_mid[[i]]
    if (any(is.na(unique(DATA$seg.ID))) && length(unique(DATA$seg.ID)) != 1){
      nightSTARTseg <- min(which(DATA$seg.ID == min(DATA$seg.ID,na.rm=T))) # define first point (row number) of the first segment in the data
      nightENDseg <- max(which(DATA$seg.ID == max(DATA$seg.ID,na.rm=T))) # define last point (row number) of the last segment in the data
      Ncut <- min((nightSTARTseg), 20)
      msx <- mean(DATA$X[(nightSTARTseg-Ncut):(nightSTARTseg-1)])
      msy <- mean(DATA$Y[(nightSTARTseg-Ncut):(nightSTARTseg-1)])
      first_x <- c(first_x, msx)
      first_y <- c(first_y, msy)
      Ncut <- min(length(DATA$X), 20)
      mex <- mean(DATA$X[(nightENDseg+1):(nightENDseg+Ncut)])
      mey <- mean(DATA$Y[(nightENDseg+1):(nightENDseg+Ncut)])
      last_x <- c(last_x, mex)
      last_y <- c(last_y, mey)
    }else{
      first_x <- c(first_x, NA)
      first_y <- c(first_y, NA)
      last_x <- c(last_x, NA)
      last_y <- c(last_y, NA)
    }
  }
  
  saveRDS(first_x, file = paste("first_x", ind_individual, "_5.rds", sep = ""))
  saveRDS(first_y, file = paste("first_y", ind_individual, "_5.rds", sep = ""))
  saveRDS(last_x, file = paste("last_x", ind_individual, "_5.rds", sep = ""))
  saveRDS(last_y, file = paste("last_y", ind_individual, "_5.rds", sep = ""))
  
  #and the distance between the two points (start and end)
  d_first_last <- c()
  
  for (i in 1:length(first_x)){
    dd <- sqrt((first_x[i] - last_x[i])^2 + (first_y[i] - last_y[i])^2) 
    d_first_last <- c(d_first_last, dd)
  }
  
  saveRDS(d_first_last, file = paste("d_first_last", ind_individual, "_5.rds", sep = ""))
  
  #maximum displacement (furthest distance from start point)
  #here we use a 5-min subsampling
  max_displ <- vector()
  x_max <- vector()
  y_max <- vector()
  
  for (i in 1:length(ind_ok_time)){
    x_max_search <- vector()
    y_max_search <- vector()
    max_d <- 0
    ind <- ind_ok_time[i]
    #print(i)
    dd_v <- c()
    if (!is.nan(first_x[i]) && !is.nan(first_y[i])){
      
      #add columns which group rows by times (in a frames of 5 minutes)
      ref_time<-min(mid_to_mid[[ind]]$date) 
      adptable<-addTimeDateFields (mid_to_mid[[ind]], "5 min", ref_time) 
      
      subData <- adptable %>% group_by(min5)%>%
        slice(1)%>%
        ungroup() #subset the data table by the times' frames (one point every 5 min)
      
      subData <- subData[order(as.Date(subData$date, format="%Y/%m/%d")),]
      
      dd_v <- sqrt((first_x[i] - subData$X)^2 + (first_y[i] - subData$Y)^2) 
      max_d <- max(dd_v)
      ind_m <- match(max_d, dd_v)
      x_max <- c(x_max, subData$X[ind_m])
      y_max <- c(y_max, subData$Y[ind_m])
      max_displ <- c(max_displ, max_d)
    }else{
      max_displ <- c(max_displ, NA)
      x_max <- c(x_max, NA)
      y_max <- c(y_max, NA)
    }
  }
  
  saveRDS(max_displ, paste("max_displ", ind_individual, "_5.rds", sep = ""))
  saveRDS(x_max, paste("x_max", ind_individual, "_5.rds", sep = ""))
  saveRDS(y_max, paste("y_max", ind_individual, "_5.rds", sep = ""))
  
  #maximum diameter
  max_poss_displ <- vector()
  max_x1 <- vector()
  max_x2 <- vector()
  max_y1 <- vector()
  max_y2 <- vector()
  
  for (i in ind_ok_time){
    #print(i)
    max_x1_c <- 0
    max_x2_c <- 0
    max_y1_c <- 0
    max_y2_c <- 0
    max_d <- 0
    dd_v <- c()
    
    #add columns which group rows by times (in a frames of 5 minutes)
    ref_time<-min(mid_to_mid[[i]]$date)
    adptable<-addTimeDateFields (mid_to_mid[[i]], "5 min", ref_time)
    
    subData <- adptable %>% group_by(min5)%>%
      slice(1)%>%
      ungroup() 
    
    subData <- subData[order(as.Date(subData$date, format="%Y/%m/%d")),]
    l1 <- length(subData$X) - 1
    
    for (j in 1:l1){
      k_init <- j + 1
      for (k in k_init:length(subData$X)){
        dd <- sqrt((subData$X[j] - subData$X[k])^2 + (subData$Y[j] - subData$Y[k])^2) 
        dd_v <- c(dd_v, dd)
        if (max_d < max(dd_v, na.rm = TRUE)){
          max_d <- max(dd_v, na.rm = TRUE)
          max_x1_c <- subData$X[j]
          max_y1_c <- subData$Y[j]
          max_x2_c <- subData$X[k]
          max_y2_c <- subData$Y[k]
        }
      }
    }
    max_poss_displ <- c(max_poss_displ, max_d)
    max_x1 <- c(max_x1, max_x1_c)
    max_x2 <- c(max_x2, max_x2_c)
    max_y1 <- c(max_y1, max_y1_c)
    max_y2 <- c(max_y2, max_y2_c)
  }
  
  saveRDS(max_poss_displ, file = paste("max_poss_displ", ind_individual, "_5.rds", sep = ""))
  saveRDS(max_x1, file = paste("max_x1", ind_individual, "_5.rds", sep = ""))
  saveRDS(max_x2, file = paste("max_x2", ind_individual, "_5.rds", sep = ""))
  saveRDS(max_y1, file = paste("max_y1", ind_individual, "_5.rds", sep = ""))
  saveRDS(max_y2, file = paste("max_y2", ind_individual, "_5.rds", sep = ""))
  
  #maximum width
  
  #distance point/line
  #a: coordinates points
  #b, c: 2 furthest points
  
  dist2d <- function(a,b,c) {
    v1 <- b - c
    v2 <- a - b
    m <- cbind(v1,v2)
    d <- abs(det(m))/sqrt(sum(v1*v1))
  } 
  
  #2 points, one for each region
  
  d_f_tot <- vector()
  ind_df <- vector()
  
  ind_tot_up <- vector()
  ind_tot_down <- vector()
  
  x_tot_up <- vector()
  y_tot_up <- vector()
  
  x_tot_down <- vector()
  y_tot_down <- vector()
  
  #distance point/line
  #a: coordinates points
  #b, c: 2 furthest points
  
  for (i in 1:length(max_x1)){
    ind <- ind_ok_time[i]
    
    #line by 2 points
    
    start_x <- max_x1[i]
    start_y <- max_y1[i]
    
    end_x <- max_x2[i]
    end_y <- max_y2[i]
    
    x <- c(start_x, end_x)
    y <- c(start_y, end_y)
    fit <- lm(y~x)
    lm_coef <- coef(fit)
    q <- lm_coef[1]
    m <- lm_coef[2]
    
    #to find furthest points
    ref_time<-min(mid_to_mid[[ind]]$date)
    adptable<-addTimeDateFields (mid_to_mid[[ind]], "5 min", ref_time) 
    
    subData <- adptable %>% group_by(min5)%>%
      slice(1)%>%
      ungroup() 
    
    subData <- subData[order(as.Date(subData$date, format="%Y/%m/%d")),]
    
    d_f_up <- vector()
    ind_df_up <- vector()
    x_df_up <- vector()
    y_df_up <- vector()
    
    d_f_down <- vector()
    ind_df_down <- vector()
    x_df_down <- vector()
    y_df_down <- vector()
    
    for (j in 1:length(subData$X)){
      a <- c(subData$X[j], subData$Y[j])
      b <- c(start_x, start_y)
      c <- c(end_x, end_y)
      d2 <- dist2d(a, b, c)
      y_c <- m * subData$X[j] + q
      if (y_c > subData$Y[j]){
        d_f_up <- c(d_f_up, d2)
        ind_df_up <- c(ind_df_up, j)
        x_df_up <- c(x_df_up, subData$X[j])
        y_df_up <- c(y_df_up, subData$Y[j])
      }else{
        d_f_down <- c(d_f_down, d2)
        ind_df_down <- c(ind_df_down, j)
        x_df_down <- c(x_df_down, subData$X[j])
        y_df_down <- c(y_df_down, subData$Y[j])
      }
    }
    
    if (length(d_f_up) != 0){
      m_up <- max(d_f_up, na.rm = TRUE)
      match_up <- match(m_up, d_f_up)
      ind_tot_up <- c(ind_tot_up, ind_df_up[match_up])
      x_tot_up <- c(x_tot_up, x_df_up[match_up])
      y_tot_up <- c(y_tot_up, y_df_up[match_up])
    }else{
      ind_tot_up <- c(ind_tot_up, NA)
      x_tot_up <- c(x_tot_up, NA)
      y_tot_up <- c(y_tot_up, NA)
    }
    
    if (length(d_f_down) != 0){
      m_down <- max(d_f_down, na.rm = TRUE)
      match_down <- match(m_down, d_f_down)
      ind_tot_down <- c(ind_tot_down, ind_df_down[match_down])
      x_tot_down <- c(x_tot_down, x_df_down[match_down])
      y_tot_down <- c(y_tot_down, y_df_down[match_down])
    }else{
      ind_tot_down <- c(ind_tot_down, NA)
      x_tot_down <- c(x_tot_down, NA)
      y_tot_down <- c(y_tot_down, NA)
    }
    m_tot <- m_up + m_down
    d_f_tot <- c(d_f_tot, m_tot)
  }
  
  saveRDS(d_f_tot, file = paste("d_f_tot", ind_individual, "_5.rds", sep = ""))
  saveRDS(ind_tot_up, file = paste("ind_df_up", ind_individual, "_5.rds", sep = ""))
  saveRDS(ind_tot_down, file = paste("ind_df_down", ind_individual, "_5.rds", sep = ""))
  saveRDS(x_tot_up, file = paste("x_tot_up", ind_individual, "_5.rds", sep = ""))
  saveRDS(y_tot_up, file = paste("y_tot_up", ind_individual, "_5.rds", sep = ""))
  saveRDS(x_tot_down, file = paste("x_tot_down", ind_individual, "_5.rds", sep = ""))
  saveRDS(y_tot_down, file = paste("y_tot_down", ind_individual, "_5.rds", sep = ""))
  
  
  #DAR dates (first day of the 2)
  date_v <- c()
  for (i in ind_ok_time){
    date_v <- c(date_v, toString(mid_to_mid[[i]]$date[1], origin = "1970-01-01", tz = "UTC"))
  }
  
  #final dataframe with dates and measurements
  df_final_meas <- data.frame(date = date_v, a = d_first_last, b = max_displ, c = max_poss_displ, d =d_f_tot, ID = rep(ind_individual,length(d_first_last)))
  
  saveRDS(df_final_meas, file = paste("df_final", ind_individual, ".rds", sep = ""))
}
