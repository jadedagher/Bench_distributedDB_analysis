library(ggplot2)
library(plotly)
library(stringr)
library(tidyr)
library(dplyr)


bench <- function(bench_name, pod_vector, path_file){
  
  # parametres en durs pour les tests
  # bench_name <- "Spanner bench - request per second"
  # path_file <- "./data/spanner_test_timestamp/bench_2/application-pod"
  # pod_vector <- c(1:10)

  application_pod <- NULL
  index_fichier <- pod_vector
  
  #import main data files
  for(index in index_fichier){
    application_pod[index] <- c(paste(path_file, index,".log", sep = ""))
    
    assign(paste("application_pod", index, sep = ""), 
           as.data.frame(read.csv(application_pod[index], header=FALSE, sep=" ")))
  }
  
  #bigdata <- rbind(application_pod1)
  bigdata <- rbind(application_pod1, application_pod2, application_pod3, application_pod4, application_pod5, application_pod6, application_pod7, application_pod8, application_pod9, application_pod10)
  #df reduction col1: timestamp, col2: action
  #df error reduction: col1: timestamp, col2: log level ERROR, col3: action
  error <- data.frame(bigdata$V2, bigdata$V3, bigdata$V7, bigdata$V31)
  bigdata <- data.frame(bigdata$V2, bigdata$V7)
  
  
  
  
  #subseting dataframe by action (getProductByProductId, insertProductOrder, insertOrder, transactionValidation)
  data_getproduct <- subset(bigdata, bigdata$bigdata.V7 == "getProductByProductId")
  #####WARNING#####
  data_insertproduct <- subset(bigdata, bigdata$bigdata.V7 == "insertProductOrder")
  #data_insertproduct <- subset(bigdata, bigdata$bigdata.V7 == "insert")
  #####WARNING#####
  data_insertorder <- subset(bigdata, bigdata$bigdata.V7 == "insertOrder")
  data_transactionvalidation <- subset(bigdata, bigdata$bigdata.V7 == "validationOrder")
  
  #subsetting dataframe by error and action
  partial_error <- data.frame(error$bigdata.V2, error$bigdata.V3)
  data_error <- subset(partial_error, error$bigdata.V3 == "[ERROR]")
  
  
  
  #List of dataframe
  #df_list <- list(data_getproduct, data_insertproduct, data_insertorder, data_transactionvalidation)
  df_list <- list(data_getproduct, data_insertproduct, data_insertorder, data_transactionvalidation, data_error)
  #rename daraframe columns
  df_list <- lapply(df_list, function(x){colnames(x) <- c("time", "action"); return(x)})
  #removing milisecond from timeseries 
  df_list <- lapply(df_list, function(x){x$time <- str_sub(x$time, 1, str_length(x$time)-4); return(x)})
  
  #Get unique timeseries
  interval <- unique(df_list[[1]]$time)
  
  #initialization of dataframe c 
  data_getproduct_c <- NULL
  data_insertproduct_c <- NULL
  data_insertorder_c <- NULL
  data_transactionvalidation_c <- NULL
  data_error_c <- NULL
  
  #Counting request per second
  a <- 1
  for(i in interval){
    data_getproduct_c[a] <- count(subset(df_list[[1]], df_list[[1]]$time == paste(i, sep = "")))
    data_insertproduct_c[a] <- count(subset(df_list[[2]], df_list[[2]]$time == paste(i, sep = "")))
    data_insertorder_c[a] <- count(subset(df_list[[3]], df_list[[3]]$time == paste(i, sep = "")))
    data_transactionvalidation_c[a] <- count(subset(df_list[[4]], df_list[[4]]$time == paste(i, sep = "")))
    data_error_c[a] <- count(subset(df_list[[5]], df_list[[5]]$time == paste(i, sep = "")))
    a <- a+1
  }
  
  #interval list to dataframe
  interval <- as.data.frame(interval)
  
  #List of dataframe
  df_list_c <- list(data_getproduct_c, data_insertproduct_c, data_insertorder_c, data_transactionvalidation_c, data_error_c)
  #List to dataframe
  df_list_c <- lapply(df_list_c, function(x){x <- as.data.frame(x); return(x)})
  
  #Building our dataframe (timeseries, count(request), index(timeseries))
  df_list_c <- lapply(df_list_c, function(x){x <- data.frame(interval, t(x), c(1:length(x))); return(x)})
  #rename list columns
  df_list_c <- lapply(df_list_c, function(x){colnames(x) <- c("timestamp", "res", "index"); return(x)})
  
  #dataframe list to dataframe 
  data_getproduct_c <- as.data.frame(df_list_c[1])
  data_insertproduct_c <- as.data.frame(df_list_c[2])
  data_insertorder_c <- as.data.frame(df_list_c[3])
  data_transactionvalidation_c <- as.data.frame(df_list_c[4])
  data_error_c <- as.data.frame(df_list_c[5])
  
  seq_len <- 2
  
  ggplot(NULL, aes(index, res)) + 
    #Four plots
    #geom_line(data = data_getproduct_c[seq(1,nrow(data_getproduct_c),seq_len), ], aes(colour = 'getProductByProductId')) + 
    #geom_line(data = data_insertproduct_c[seq(1,nrow(data_insertproduct_c),seq_len), ], aes(colour = 'insertProductOrder')) + 
    #geom_line(data = data_insertorder_c[seq(1,nrow(data_insertorder_c),seq_len), ], aes(colour = 'insertOrder')) + 
    #geom_line(data = data_transactionvalidation_c[seq(1,nrow(data_transactionvalidation_c),seq_len), ], aes(colour = 'validationOrder')) + 
    geom_line(data = data_error_c[seq(1,nrow(data_error_c),seq_len), ], aes(colour = 'error')) + 
    #smooth (http://faculty.chicagobooth.edu/richard.hahn/teaching/formulanotation.pdf)
    #geom_smooth(data = data_getproduct_c, aes(group = 1, colour = 'getProductByProductId')) +
    #geom_smooth(data = data_insertproduct_c, aes(group = 1, colour = 'insertProductOrder')) +
    #geom_smooth(data = data_insertorder_c, aes(group = 1, colour = 'insertOrder' )) +
    #geom_smooth(data = data_transactionvalidation_c, aes(group = 1, colour = 'validationOrder' )) +
    #Legend
    ggtitle(bench_name) + 
    expand_limits(x = 0, y = 0) +
    xlab("Time (second)") + 
    ylab("Request") +
    theme_light() 
  return(ggplotly())
}
#bench function call
bench(bench_name = "Spanner bench regional - request per second", 
      pod_vector = c(1:10), 
      path_file = "/Users/octo-luma/Desktop/logs/28.08.18-bench-regional/bench 8/log/application-pod")




#sandbox
#supperposition
#sum of data_getproduct_c + data_insertproduct_c + data_insertorder_c, data_transactionvalidation_c
supperposition <- data.frame(data_getproduct_c$timestamp, data_getproduct_c$res, data_insertproduct_c$res, data_insertorder_c$res, data_transactionvalidation_c$res)
colnames(supperposition) <- c("time", "A", "B", "C", "D")
total <- supperposition$A + supperposition$B + supperposition$C + supperposition$D
supperposition <- data_frame(supperposition$time, total, c(1:447))
colnames(supperposition) <- c("time", "total", "index")

seq_len <- 1

ggplot(supperposition[seq(1,nrow(supperposition),seq_len), ], aes(index, total)) + 
  #Four plots
  geom_point() +
  geom_smooth(aes(group = 1)) +
  #Legend
  ggtitle("cockroach bench - request per second") + 
  expand_limits(x = 0, y = 0) +
  xlab("Time (second)") + 
  ylab("Request") +
  theme_light() 

ggplotly()