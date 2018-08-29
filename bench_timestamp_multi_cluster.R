library(ggplot2)
library(plotly)
library(stringr)
library(tidyr)
library(dplyr)


bench_multicluster <- function(bench_name, pod_vector, path_file, cluster_vector){
  
  # parametres en durs pour les tests
  # bench_name <- "Spanner bench - request per second"
  # path_file <- "./data/spanner_test_timestamp/bench_2/application-cluster"
  # pod_vector <- c(1:10)

  application_pod <- NULL
  index_fichier <- pod_vector
  index_cluster <- cluster_vector
  
  #import main data files
  for(c_index in index_cluster){
    for(index in index_fichier){
      application_pod[index] <- c(paste(path_file, c_index,"-pod", index, ".log", sep = ""))
    
      assign(paste("application_pod", index, sep = ""), 
             as.data.frame(read.csv(application_pod[index], header=FALSE, sep=" ")))
    }
  }
  
  #bigdata <- rbind(application_pod1)
  bigdata <- rbind(application_pod1, application_pod2, application_pod3)#, application_pod4, application_pod5, application_pod6, application_pod7, application_pod8, application_pod9, application_pod10)
  #df reduction col1: timestamp, col2: action
  bigdata <- data.frame(bigdata$V2, bigdata$V7)
  
  #subseting dataframe by action (getProductByProductId, insertProductOrder, insertOrder, transactionValidation)
  data_getproduct <- subset(bigdata, bigdata$bigdata.V7 == "getProductByProductId")
  #####WARNING#####
  data_insertproduct <- subset(bigdata, bigdata$bigdata.V7 == "insert product order")
  #data_insertproduct <- subset(bigdata, bigdata$bigdata.V7 == "insert")
  #####WARNING#####
  data_insertorder <- subset(bigdata, bigdata$bigdata.V7 == "insertOrder")
  data_transactionvalidation <- subset(bigdata, bigdata$bigdata.V7 == "validationOrder")
  
  #List of dataframe
  df_list <- list(data_getproduct, data_insertproduct, data_insertorder, data_transactionvalidation)
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
  
  #Counting request per second
  a <- 1
  for(i in interval){
    data_getproduct_c[a] <- count(subset(df_list[[1]], df_list[[1]]$time == paste(i, sep = "")))
    data_insertproduct_c[a] <- count(subset(df_list[[2]], df_list[[2]]$time == paste(i, sep = "")))
    data_insertorder_c[a] <- count(subset(df_list[[3]], df_list[[3]]$time == paste(i, sep = "")))
    data_transactionvalidation_c[a] <- count(subset(df_list[[4]], df_list[[4]]$time == paste(i, sep = "")))
    a <- a+1
  }
  
  #interval list to dataframe
  interval <- as.data.frame(interval)
  
  #List of dataframe
  df_list_c <- list(data_getproduct_c, data_insertproduct_c, data_insertorder_c, data_transactionvalidation_c)
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
  
  seq_len <- 7
  
  ggplot(NULL, aes(index, res)) + 
    #Four plots
    geom_line(data = data_getproduct_c[seq(1,nrow(data_getproduct_c),seq_len), ], aes(colour = 'getProductByProductId')) + 
    geom_line(data = data_insertproduct_c[seq(1,nrow(data_insertproduct_c),seq_len), ], aes(colour = 'insertProductOrder')) + 
    geom_line(data = data_insertorder_c[seq(1,nrow(data_insertorder_c),seq_len), ], aes(colour = 'insertOrder')) + 
    geom_line(data = data_transactionvalidation_c[seq(1,nrow(data_transactionvalidation_c),seq_len), ], aes(colour = 'validationOrder')) + 
    #smooth (http://faculty.chicagobooth.edu/richard.hahn/teaching/formulanotation.pdf)
    geom_smooth(data = data_getproduct_c, aes(group = 1, colour = 'getProductByProductId')) +
    geom_smooth(data = data_insertproduct_c, aes(group = 1, colour = 'insertProductOrder')) +
    geom_smooth(data = data_insertorder_c, aes(group = 1, colour = 'insertOrder' )) +
    geom_smooth(data = data_transactionvalidation_c, aes(group = 1, colour = 'validationOrder' )) +
    #Legend
    ggtitle(bench_name) + 
    expand_limits(x = 0, y = 0) +
    xlab("Time (second)") + 
    ylab("Request") +
    theme_light() 
  
  return(ggplotly())

}
#bench function call
bench_multicluster(bench_name = "Spanner bench regional - request per second", 
      pod_vector = c(1:3), 
      path_file = "/Users/octo-luma/Desktop/logs/27.08.18-bench-avec-lolo/bench 3 copy/application-cluster",
      cluster_vector = c(1:3))

