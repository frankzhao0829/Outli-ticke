# Changepoint calculator
setwd('C:\\Users\\zhaoch\\Desktop')

library(data.table)

# Get the libraries we need for this task
library(dplyr)
library(RODBC)
library(sqldf)
library(ggplot2)


#EPRID<-c(92854,111452,111485,111509,113944,114185,115841,115841
#        ,115841,115841,116368,116385,116511,200456,200456
#        ,200865,201349,203188,205364,205461,205526)
#Project_Acronym<-c('NCRF','SVR','GCSS','NIRV','EWMS','HUB','EDW','LDSM','SIDS','VISI'
#                  ,'OVSC','MTS2','RST','MDM','MDMD','RAPD','EGSD','QSPK','SPLN','COA','CCDI')
#iss_metrics_T<-data.frame(EPRID=EPRID
#                        ,Project_Acronym=Project_Acronym
#                        ,Type='TARGET')
#iss_metrics_SQ<-data.frame(EPRID=EPRID
#                          ,Project_Acronym=Project_Acronym
#                          ,Type='SOURCE_QUALIFIER')
#iss_metrics <- rbind(iss_metrics_T,iss_metrics_SQ)
#iss_metrics <- cbind(iss_metrics
#                     ,pDATE=rep(as.Date(0),length(iss_metrics))
#                     ,xAVG=rep(0,length(iss_metrics))
#                     ,xSTD=rep(0,length(iss_metrics))
#                     ,xCount=rep(0,length(iss_metrics)))

Check_Outlier_Modified_Z <- function(data, column){
  
  if (! is.numeric(data[ ,column])) {
    message <- paste(column, "is not numeric!", sep = " ")
    stop(message)
  }
  
  threshold <- 3.5
  
  data <- data %>% 
    mutate(median  = median(data[ ,column][!is.na(data[ ,column])]),
           
           MAD     = median(abs(data[ ,column][!is.na(data[ ,column])] - mean(median))),
           
           Modified_Z_coln = ifelse((data[ ,column] - median) == 0, 0, 
                                    ifelse(is.na(data[ ,column]), NA,
                                    (0.6745*(data[ ,column] - median))/MAD)),
           
           Modified_outl_coln = ifelse(is.na(data[ ,column]), "Missing", 
                                       if_else(abs(Modified_Z_coln) > threshold, "Y", "N"))) %>%
    select(-median, -MAD)
  
  return(data)
}

Check_Outlier_Z <- function(data, column){
  
  if (! is.numeric(data[ ,column])) {
    stop("data is not numeric!")
  }
  
  threshold <- 3.5
  
  data <- data %>% 
    mutate(avg  = mean(data[ ,column][!is.na(data[ ,column])]),
           
           stdev = sd(data[ ,column][!is.na(data[ ,column])]),
           
           Z_col = ifelse(is.na(data[ ,column]), NA,
                           (data[ ,column] - avg)/mean(stdev)),
           
           outl_col = ifelse(is.na(data[ ,column]), "Missing", 
                              if_else(abs(Z_col) > threshold, "Y", "N"))) %>%
    select(-avg, -stdev)
  
  return(data)
}

# Test function
# head(Check_Outlier_Modified_Z(mydata, "Throughput_in_bytes_per_sec"))
# head(Check_Outlier_Z(mydata, "Throughput_in_bytes_per_sec"))

# Load data
mydata <- read.csv("ISS_Aptio_data.csv", header=TRUE, stringsAsFactors = F)

# Format Date time to R datetime
mydata$Start_Time <- as.POSIXct(mydata$Start_Time, format="%d-%b-%y %I.%M.%S %p", tz=Sys.timezone())

# Split data by groups
subsets <- split(mydata, paste(mydata$EPRID, mydata$Project_Acronym, mydata$Type))

# Apply function to each subset of data
subsets <- lapply(subsets, FUN = Check_Outlier_Modified_Z, column = "Throughput_in_bytes_per_sec")
subsets <- lapply(subsets, FUN = Check_Outlier_Z, column = "Throughput_in_bytes_per_sec")

DT_Final <- do.call("rbind", subsets)

DT_Final <- DT_Final %>%
  mutate(T_Mod_ZScore  = Modified_Z_coln,
         T_Mod_Outlier = Modified_outl_coln,
         T_ZScore      = Z_col,
         T_Outlier     = outl_col) %>%
  select(-Modified_Z_coln, -Modified_outl_coln, -Z_col, -outl_col)


# Function to plot multiple ggplots in one chart
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



# Graph for Throughput 
p1 <- ggplot(DT_Final, aes(x= Start_Time, y = Throughput_in_bytes_per_sec, colour = T_Outlier)) + geom_point() +
  ggtitle("Outliers using Normal Z Scores") + scale_color_discrete(name = "Outliers") + 
  scale_y_continuous(labels = scales::comma, name = "Throughput in bytes/sec") + 
  scale_x_datetime(name = "Points by Start Time")

p2 <- ggplot(DT_Final, aes(x= Start_Time, y = Throughput_in_bytes_per_sec, colour = T_Mod_Outlier)) + geom_point() +
  ggtitle("Outliers using Modified Z Scores") + scale_color_discrete(name = "Outliers") + 
  scale_y_continuous(labels = scales::comma, name = "Throughput in bytes/sec") + 
  scale_x_datetime(name = "Points by Start Time")

multiplot(p1, p2, cols = 2)


# DT <- data.table(mydata)

# 
# DT_OUT <- DT[, .(T_AVG=mean(Throughput_in_bytes_per_sec)
#        , T_MED=median(Throughput_in_bytes_per_sec)
#        , T_STDEV=sd(Throughput_in_bytes_per_sec)
#        , T_LEN=length(Throughput_in_bytes_per_sec)
#        , V_AVG=mean(volume_in_Bytes)
#        , V_MED=median(volume_in_Bytes)
#        , V_STDEV=sd(volume_in_Bytes)
#        , V_LEN=length(volume_in_Bytes))
#    , by=.(EPRID,Project_Acronym,Type)]

#DT[EPRID==92854 & Project_Acronym=='NCRF' & Type=='SOURCE_QUALIFIER']

# for(i in 1:nrow(DT_OUT)) {
#   DT_TEMP <-DT[EPRID==DT_OUT[i,EPRID] & Project_Acronym==DT_OUT[i,Project_Acronym] & Type==DT_OUT[i,Type]]
#   DT_TEMP$T_ZSCORE <- (DT$Throughput_in_bytes_per_sec - DT_OUT[i,T_AVG])/DT_OUT[i,T_STDEV]
#   DT_TEMP$V_ZSCORE <- (DT$volume_in_Bytes - DT_OUT[i,V_AVG])/DT_OUT[i,V_STDEV]
#   DT_TEMP$T_OUTLIER <- ""
#   DT_TEMP$V_OUTLIER <- ""
#   
# #    if(DT_TEMP$T_ZSCORE >= threshold) 
# #    {DT_TEMP$T_OUTLIER <- "Y"}
# #  else   
# #    {DT_TEMP$T_OUTLIER <- "N"}
#   
#   if(i==1)
#       {DT_FINAL <- DT_TEMP}
#     else
#       DT_FINAL <- rbind(DT_FINAL,DT_TEMP)
#   }
# 
# # for(i in 1:nrow(DT_FINAL)) {
# #   if ( DT_FINAL[i]$T_ZSCORE > threshold)   {DT_FINAL[i]$T_OUTLIER <- "Y"} 
# #   if ( DT_FINAL[i]$V_ZSCORE > threshold)   {DT_FINAL[i]$V_OUTLIER <- 'Y'}
# #   }
# # 
# # DT_FINAL_T <- subset(DT_FINAL, T_OUTLIER == "Y")




## ylim needs to show the range
## normal points red, outlier points black

#plot(DT$Throughput_in_bytes_per_sec ,main="data",ylab="Throughput")
#par(new=T)
#plot(DT_FINAL_T$Throughput_in_bytes_per_sec,type="b",axes=F,col=3)
#par(new=F)

plot(DT_FINAL$Throughput_in_bytes_per_sec,ylim=range(DT_FINAL$Throughput_in_bytes_per_sec),main="data",ylab="Throughput",col=3)
points(ifelse(T_OUTLIER=='Y',DT_FINAL$Throughput_in_bytes_per_sec,0),col=1,pch=21)



#plot(DT_FINAL_T$Throughput_in_bytes_per_sec ,main="data",ylab="Throughput")
#plot(DT$volume_in_Bytes ,main="data",ylim=c(0,100),ylab="volume")


write.csv(file="chpt_ISS.csv",DT_FINAL)

