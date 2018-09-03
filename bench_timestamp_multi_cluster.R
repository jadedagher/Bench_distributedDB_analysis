library(ggplot2)
library(plotly)
library(stringr)
library(tidyr)
library(dplyr)


bench_multicluster <- function(bench_name, pod_vector, path_file, cluster_vector){
  
  # parametres en durs pour les tests
  #bench_name = "Spanner bench regional - request per second" 
  #pod_vector = c(1:5)
  #path_file = "/Users/octo-luma/Desktop/logs/29.08.18-bench-multi-regions-3/log/application-cluster"
  #cluster_vector = c(1:3)
  
  

  application_pod <- NULL
  index_cluster <- cluster_vector
  
  #import main data files
  for(c_index in index_cluster){
    index_fichier <- pod_vector
    for(index in index_fichier){
      application_pod[index] <- c(paste(path_file, c_index, "-pod", index, ".log", sep = ""))
    
      assign(paste("application_cluster", c_index, "_pod", index, sep = ""), 
             as.data.frame(read.csv(application_pod[index], header=FALSE, sep=" ")))
    }
  }
  
  #bigdata <- rbind(application_pod1)
  bigdata <- rbind(application_cluster1_pod1, application_cluster1_pod2, application_cluster1_pod3, application_cluster1_pod4, application_cluster1_pod5, application_cluster2_pod1, application_cluster2_pod2, application_cluster2_pod3, application_cluster2_pod4, application_cluster2_pod5, application_cluster3_pod1, application_cluster3_pod2, application_cluster3_pod3, application_cluster3_pod4, application_cluster3_pod5)
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
    geom_line(data = data_getproduct_c[seq(1,nrow(data_getproduct_c),seq_len), ], aes(colour = 'getProductByProductId')) + 
    geom_line(data = data_insertproduct_c[seq(1,nrow(data_insertproduct_c),seq_len), ], aes(colour = 'insertProductOrder')) + 
    geom_line(data = data_insertorder_c[seq(1,nrow(data_insertorder_c),seq_len), ], aes(colour = 'insertOrder')) + 
    geom_line(data = data_transactionvalidation_c[seq(1,nrow(data_transactionvalidation_c),seq_len), ], aes(colour = 'validationOrder')) + 
    geom_line(data = data_error_c[seq(1,nrow(data_error_c),seq_len), ], aes(colour = 'error')) + 
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

bench_multicluster_error <- function(bench_name, pod_vector, path_file, cluster_vector){
  
  # parametres en durs pour les tests
  #bench_name = "Spanner bench regional - request per second" 
  #pod_vector = c(1:5)
  #path_file = "/Users/octo-luma/Desktop/logs/29.08.18-bench-multi-regions-3/log/application-cluster"
  #cluster_vector = c(1:3)
  
  
  
  application_pod <- NULL
  index_cluster <- cluster_vector
  
  #import main data files
  for(c_index in index_cluster){
    index_fichier <- pod_vector
    for(index in index_fichier){
      application_pod[index] <- c(paste(path_file, c_index, "-pod", index, ".log", sep = ""))
      
      assign(paste("application_cluster", c_index, "_pod", index, sep = ""), 
             as.data.frame(read.csv(application_pod[index], header=FALSE, sep=" ")))
    }
  }
  
  #bigdata <- rbind(application_pod1)
  errors <- rbind(application_cluster1_pod1, application_cluster1_pod2, application_cluster1_pod3, application_cluster1_pod4, application_cluster1_pod5, application_cluster2_pod1, application_cluster2_pod2, application_cluster2_pod3, application_cluster2_pod4, application_cluster2_pod5, application_cluster3_pod1, application_cluster3_pod2, application_cluster3_pod3, application_cluster3_pod4, application_cluster3_pod5)
  #df reduction col1: timestamp, col2: action
  #df error reduction: col1: timestamp, col2: log level ERROR, col3: action
  errors <- data.frame(errors$V2, errors$V3, errors$V7, errors$V31)
  
  errors <- subset(errors, errors$errors.V3 == "[ERROR]")
  errors <- data.frame(errors$errors.V2, errors$errors.V7, errors$errors.V31)
  
  
  data_error_insertPO <- subset(errors, errors$errors.errors.V7 == "insertProductOrder")
  data_error_handleRE <- subset(errors, errors$errors.errors.V7 == "handleRequestError")
  
  

  
  
  #List of dataframe
  #df_list <- list(data_getproduct, data_insertproduct, data_insertorder, data_transactionvalidation)
  df_list <- list(data_error_insertPO, data_error_handleRE)
  #rename daraframe columns
  df_list <- lapply(df_list, function(x){colnames(x) <- c("time", "error type", "more info"); return(x)})
  #removing milisecond from timeseries 
  df_list <- lapply(df_list, function(x){x$time <- str_sub(x$time, 1, str_length(x$time)-4); return(x)})
  
  #Get unique timeseries
  interval <- unique(df_list[[1]]$time)
  
  #initialization of dataframe c 
  data_error_insertPO_c <- NULL
  data_error_handleRE_c <- NULL
  
  #Counting request per second
  a <- 1
  for(i in interval){
    data_error_insertPO_c[a] <- count(subset(df_list[[1]], df_list[[1]]$time == paste(i, sep = "")))
    data_error_handleRE_c[a] <- count(subset(df_list[[2]], df_list[[2]]$time == paste(i, sep = "")))
    a <- a+1
  }
  
  #interval list to dataframe
  interval <- as.data.frame(interval)
  
  #List of dataframe
  df_list_c <- list(data_error_insertPO_c, data_error_handleRE_c)
  #List to dataframe
  df_list_c <- lapply(df_list_c, function(x){x <- as.data.frame(x); return(x)})
  
  #Building our dataframe (timeseries, count(request), index(timeseries))
  df_list_c <- lapply(df_list_c, function(x){x <- data.frame(interval, t(x), c(1:length(x))); return(x)})
  #rename list columns
  df_list_c <- lapply(df_list_c, function(x){colnames(x) <- c("timestamp", "res", "index"); return(x)})
  
  #dataframe list to dataframe 
  data_error_insertPO_c <- as.data.frame(df_list_c[1])
  data_error_handleRE_c <- as.data.frame(df_list_c[2])
  
  
  seq_len <- 2
  
  ggplot(NULL, aes(index, res)) + 
    #Four plots
    geom_line(data = data_error_insertPO_c[seq(1,nrow(data_error_insertPO_c),seq_len), ], aes(colour = 'insertProductOrder')) + 
    geom_line(data = data_error_handleRE_c[seq(1,nrow(data_error_handleRE_c),seq_len), ], aes(colour = 'handleRequestError')) + 
    #smooth (http://faculty.chicagobooth.edu/richard.hahn/teaching/formulanotation.pdf)
    #geom_smooth(data = data_getproduct_c, aes(group = 1, colour = 'getProductByProductId')) +
    #geom_smooth(data = data_insertproduct_c, aes(group = 1, colour = 'insertProductOrder')) +
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
      pod_vector = c(1:5), 
      path_file = "/Users/octo-luma/Desktop/logs/29.08.18-bench-multi-regions-3/log/application-cluster",
      cluster_vector = c(1:3))

bench_multicluster_error(bench_name = "Errors", 
                   pod_vector = c(1:5), 
                   path_file = "/Users/octo-luma/Desktop/logs/29.08.18-bench-multi-regions-3/log/application-cluster",
                   cluster_vector = c(1:3))

