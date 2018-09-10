library(ggplot2)
library(plotly)
library(stringr)
library(tidyr)
library(dplyr)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

bench_multicluster <- function(bench_name, pod_vector, path_file, cluster_vector, e, scale_max=0, scale_min=0){
  
  # parametres en durs pour les tests
  #bench_name = "Spanner bench regional - request per second" 
  #bench_name = "Spanner bench regional - errors" 
  #pod_vector <- c(1:6)
  #cluster_vector <- c(1:1)
  #path_file <- "/Users/octo-luma/Desktop/logs/5.9.18.spanner-150threads-to25-15kCustomer-6nodes/application-cluster"
  #e = TRUE


  application_pod <- NULL
  index_cluster <- cluster_vector
  
  #import main data files
  for(c_index in index_cluster){
    index_fichier <- pod_vector
    for(index in index_fichier){
      application_pod[index] <- c(paste(path_file, c_index, "-pod", index, ".log", sep = "", quote=""))
    
      assign(paste("application_cluster", c_index, "_pod", index, sep = ""), 
             as.data.frame(read.csv(application_pod[index], header=FALSE, sep=" ")))
    }
  }
  
  #bigdata <- rbind(application_cluster1_pod1)
  bigdata <- rbind(application_cluster1_pod1, application_cluster1_pod2, application_cluster1_pod3, application_cluster1_pod4, application_cluster1_pod5, application_cluster1_pod6)#, application_cluster1_pod4, application_cluster1_pod5, application_cluster2_pod1, application_cluster2_pod2, application_cluster2_pod3, application_cluster2_pod4, application_cluster2_pod5, application_cluster3_pod1, application_cluster3_pod2, application_cluster3_pod3, application_cluster3_pod4, application_cluster3_pod5)
  #df reduction col1: timestamp, col2: action
  errors <- data.frame(bigdata$V2, bigdata$V3, bigdata$V7, bigdata$V31)
  bigdata <- data.frame(bigdata$V2, bigdata$V7)
  
  
  # errors reduction
  errors <- subset(errors, errors$bigdata.V3 == "[ERROR]")
  errors <- data.frame(errors$bigdata.V2, errors$bigdata.V7, errors$bigdata.V31)
  
  # get the correct error
  data_error_insertPO <- subset(errors, errors$errors.bigdata.V7 == "insertProductOrder")
  data_error_retry    <- subset(errors, errors$errors.bigdata.V31 == "RETRY")
  data_error_handleRE <- subset(errors, errors$errors.bigdata.V7 == "handleRequestError")
  
  
  #subseting dataframe by action (getProductByProductId, insertProductOrder, insertOrder, transactionValidation)
  data_getproduct <- subset(bigdata, bigdata$bigdata.V7 == "getProductByProductId")
  #####WARNING#####
  data_insertproduct <- subset(bigdata, bigdata$bigdata.V7 == "insertProductOrder")
  #data_insertproduct <- subset(bigdata, bigdata$bigdata.V7 == "insert")
  #####WARNING#####
  data_insertorder <- subset(bigdata, bigdata$bigdata.V7 == "insertOrder")
  data_transactionvalidation <- subset(bigdata, bigdata$bigdata.V7 == "validationOrder")
  
  
  
  
  #List of dataframe
  #df_list <- list(data_getproduct, data_insertproduct, data_insertorder, data_transactionvalidation)
  df_list <- list(data_getproduct, data_insertproduct, data_insertorder, data_transactionvalidation)
  df_error_list <- list(data_error_handleRE, data_error_retry)
  #rename daraframe columns
  df_list <- lapply(df_list, function(x){colnames(x) <- c("time", "action"); return(x)})
  df_error_list <- lapply(df_error_list, function(x){colnames(x) <- c("time", "action", "error type"); return(x)})
  #removing milisecond from timeseries 
  df_list <- lapply(df_list, function(x){x$time <- str_sub(x$time, 1, str_length(x$time)-4); return(x)})
  df_error_list <- lapply(df_error_list, function(x){x$time <- str_sub(x$time, 1, str_length(x$time)-4); return(x)})
  
  #Get unique timeseries
  interval <- unique(df_list[[1]]$time)
  
  #initialization of dataframe c 
  data_getproduct_c <- NULL
  data_insertproduct_c <- NULL
  data_insertorder_c <- NULL
  data_transactionvalidation_c <- NULL
  
  data_error_handleRE_c <- NULL
  data_error_retry_c <- NULL

  
  #Counting request per second
  a <- 1
  for(i in interval){
    data_getproduct_c[a] <- count(subset(df_list[[1]], df_list[[1]]$time == paste(i, sep = "")))
    data_insertproduct_c[a] <- count(subset(df_list[[2]], df_list[[2]]$time == paste(i, sep = "")))
    data_insertorder_c[a] <- count(subset(df_list[[3]], df_list[[3]]$time == paste(i, sep = "")))
    data_transactionvalidation_c[a] <- count(subset(df_list[[4]], df_list[[4]]$time == paste(i, sep = "")))
    data_error_handleRE_c[a] <- count(subset(df_error_list[[1]], df_error_list[[1]]$time == paste(i, sep = "")))
    data_error_retry_c[a] <- count(subset(df_error_list[[2]], df_error_list[[2]]$time == paste(i, sep = "")))
    a <- a+1
  }
  
  #interval list to dataframe
  interval <- as.data.frame(interval)
  
  #List of dataframe
  df_list_c <- list(data_getproduct_c, data_insertproduct_c, data_insertorder_c, data_transactionvalidation_c)
  df_error_list_c <- list(data_error_handleRE_c, data_error_retry_c)
  #List to dataframe
  df_list_c <- lapply(df_list_c, function(x){x <- as.data.frame(x); return(x)})
  df_error_list_c <- lapply(df_error_list_c, function(x){x <- as.data.frame(x); return(x)})
  
  #Building our dataframe (timeseries, count(request), index(timeseries))
  df_list_c <- lapply(df_list_c, function(x){x <- data.frame(interval, t(x), c(1:length(x))); return(x)})
  df_error_list_c <- lapply(df_error_list_c, function(x){x <- data.frame(interval, t(x), c(1:length(x))); return(x)})
  #rename list columns
  df_list_c <- lapply(df_list_c, function(x){colnames(x) <- c("timestamp", "res", "index"); return(x)})
  df_error_list_c <- lapply(df_error_list_c, function(x){colnames(x) <- c("timestamp", "res", "index"); return(x)})
  
  #dataframe list to dataframe 
  data_getproduct_c <- as.data.frame(df_list_c[1])
  data_insertproduct_c <- as.data.frame(df_list_c[2])
  data_insertorder_c <- as.data.frame(df_list_c[3])
  data_transactionvalidation_c <- as.data.frame(df_list_c[4])
  
  data_error_handleRE_c <- as.data.frame(df_error_list_c[1])
  data_error_retry_c <- as.data.frame(df_error_list_c[2])
  
  if(scale_max == 0) {
    scale_max1 <- nrow(data_getproduct_c)
    scale_max2 <- nrow(data_insertproduct_c)
    scale_max3 <- nrow(data_insertorder_c)
    scale_max4 <- nrow(data_transactionvalidation_c)
    scale_error_max1 <- nrow(data_error_handleRE_c)
    scale_error_max2 <- nrow(data_error_retry_c)
    
  } else {
    scale_max1 <- scale_max
    scale_max2 <- scale_max
    scale_max3 <- scale_max
    scale_max4 <- scale_max
    scale_error_max1 <- scale_max
    scale_error_max2 <- scale_max
  }
  
  
  if(e == FALSE) {
    seq_len <- 2
    ggplot(NULL, aes(index, res)) + 
      #Four plots
      geom_line(data = data_getproduct_c[seq(scale_min, scale_max1, seq_len), ], aes(colour = 'getProductByProductId')) + 
      geom_line(data = data_insertproduct_c[seq(scale_min, scale_max2, seq_len), ], aes(colour = 'insertProductOrder')) + 
      geom_line(data = data_insertorder_c[seq(scale_min, scale_max3, seq_len), ], aes(colour = 'insertOrder')) + 
      geom_line(data = data_transactionvalidation_c[seq(scale_min, scale_max4, seq_len), ], aes(colour = 'validationOrder')) + 
      #smooth (http://faculty.chicagobooth.edu/richard.hahn/teaching/formulanotation.pdf)
      #geom_smooth(data = data_getproduct_c, aes(group = 1, colour = 'getProductByProductId')) +
      #geom_smooth(data = data_insertproduct_c, aes(group = 1, colour = 'insertProductOrder')) +
      #geom_smooth(data = data_insertorder_c, aes(group = 1, colour = 'insertOrder' )) +
      #geom_smooth(data = data_transactionvalidation_c, aes(group = 1, colour = 'validationOrder' )) +
      #Legend
      ggtitle(bench_name) + 
      expand_limits(x = scale_min, y = 0) +
      xlab("Time (second)") + 
      ylab("Request") +
      theme_light() 
    
  } else {
    seq_len <- 1
    ggplot(NULL, aes(index, res)) +
    geom_line(data = data_error_handleRE_c[seq(scale_min, scale_error_max1, seq_len), ], aes(colour = 'handleError')) + 
    geom_line(data = data_error_retry_c[seq(scale_min, scale_error_max2, seq_len), ], aes(colour = 'retry')) + 
    ggtitle(bench_name) + 
    expand_limits(x = scale_min, y = 0) +
    xlab("Time (second)") + 
    ylab("Request") +
    theme_light()
  }
  
  return(ggplotly())
  #returnList <- list("graph" = ggplotly(), "timeAbscisse" = interval)
  #return(returnList)
}


#bench_multicluster_error <- function(bench_name, pod_vector, path_file, cluster_vector, scale_min=0, scale_max=0, interval){
  
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
  
  #errors <- rbind(application_cluster1_pod1)
  errors <- rbind(application_cluster1_pod1, application_cluster1_pod2) #, application_cluster1_pod3, application_cluster1_pod4, application_cluster1_pod5, application_cluster2_pod1, application_cluster2_pod2, application_cluster2_pod3, application_cluster2_pod4, application_cluster2_pod5, application_cluster3_pod1, application_cluster3_pod2, application_cluster3_pod3, application_cluster3_pod4, application_cluster3_pod5)
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
  #interval <- unique(df_list[[1]]$time)
  
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
  #interval <- as.data.frame(interval)
  
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
  
  if(scale_max == 0) {
    scale_max1 <- nrow(data_error_insertPO_c)
    scale_max2 <- nrow(data_error_handleRE_c)
  } else {
    scale_max1 <- scale_max
    scale_max2 <- scale_max
  }
  
  ggplot(NULL, aes(index, res)) + 
    #Two plots
    geom_line(data = data_error_insertPO_c[seq(1, scale_max1, seq_len), ], aes(colour = 'insertProductOrder')) + 
    geom_line(data = data_error_handleRE_c[seq(1, scale_max2, seq_len), ], aes(colour = 'handleRequestError')) + 
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

bench_multicluster_histograms <- function(pod_vector, path_file, cluster_vector, scale_min, scale_max){
  
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
  
  #bigdata <- rbind(application_cluster1_pod1)
  bigdata <- rbind(application_cluster1_pod1, application_cluster1_pod2, application_cluster1_pod3, application_cluster1_pod4, application_cluster1_pod5, application_cluster1_pod6)#, application_cluster1_pod4, application_cluster1_pod5, application_cluster2_pod1, application_cluster2_pod2, application_cluster2_pod3, application_cluster2_pod4, application_cluster2_pod5, application_cluster3_pod1, application_cluster3_pod2, application_cluster3_pod3, application_cluster3_pod4, application_cluster3_pod5)
  #df reduction col1: timestamp, col2: action
  errors <- data.frame(bigdata$V2, bigdata$V3, bigdata$V7, bigdata$V31)
  bigdata <- data.frame(bigdata$V2, bigdata$V7)
  
  errors <- subset(errors, errors$bigdata.V3 == "[ERROR]")
  errors <- data.frame(errors$bigdata.V2, errors$bigdata.V7, errors$bigdata.V31)
  
  data_error_insertPO <- subset(errors, errors$errors.bigdata.V7 == "insertProductOrder")
  data_error_retry    <- subset(errors, errors$errors.bigdata.V31 == "RETRY")
  data_error_handleRE <- subset(errors, errors$errors.bigdata.V7 == "handleRequestError")
  
  
  
  #subseting dataframe by action (getProductByProductId, insertProductOrder, insertOrder, transactionValidation)
  data_getproduct <- subset(bigdata, bigdata$bigdata.V7 == "getProductByProductId")
  #####WARNING#####
  data_insertproduct <- subset(bigdata, bigdata$bigdata.V7 == "insertProductOrder")
  #data_insertproduct <- subset(bigdata, bigdata$bigdata.V7 == "insert")
  #####WARNING#####
  data_insertorder <- subset(bigdata, bigdata$bigdata.V7 == "insertOrder")
  data_transactionvalidation <- subset(bigdata, bigdata$bigdata.V7 == "validationOrder")
  
  
  
  
  #List of dataframe
  #df_list <- list(data_getproduct, data_insertproduct, data_insertorder, data_transactionvalidation)
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
  
  seq_len <- 2
  
  
  # Histogram
  
  
  ## get product
  percentiles_get_product <- quantile(data_getproduct_c$res, c(.50, .95, .99, .999, .9999))
  get_product_h <- ggplot(data_getproduct_c, aes(res)) + geom_histogram(binwidth = 100, color="black", fill="white")
  get_product_h <- get_product_h + geom_vline(aes(xintercept=percentiles_get_product[[1]]), color="blue", linetype="dashed", size=1)
  get_product_h <- get_product_h + geom_vline(aes(xintercept=percentiles_get_product[[2]]), color="green", linetype="dashed", size=1)
  get_product_h <- get_product_h + geom_vline(aes(xintercept=percentiles_get_product[[3]]), color="yellow", linetype="dashed", size=1)
  get_product_h <- get_product_h + geom_vline(aes(xintercept=percentiles_get_product[[4]]), color="purple", linetype="dashed", size=1)
  get_product_h <- get_product_h + geom_vline(aes(xintercept=percentiles_get_product[[5]]), color="red", linetype="dashed", size=1)
  get_product_h <- get_product_h + ggtitle("get product") + theme_light() + xlim(scale_min, scale_max)
  
  
  ## add product
  percentiles_add_product <- quantile(data_insertproduct_c$res, c(.50, .95, .99, .999, .9999))
  insert_product_h <- ggplot(data_insertproduct_c, aes(res)) + geom_histogram(binwidth = 100, color="black", fill="white")
  insert_product_h <- insert_product_h + geom_vline(aes(xintercept=percentiles_add_product[[1]]), color="blue", linetype="dashed", size=1)
  insert_product_h <- insert_product_h + geom_vline(aes(xintercept=percentiles_add_product[[2]]), color="green", linetype="dashed", size=1)
  insert_product_h <- insert_product_h + geom_vline(aes(xintercept=percentiles_add_product[[3]]), color="yellow", linetype="dashed", size=1)
  insert_product_h <- insert_product_h + geom_vline(aes(xintercept=percentiles_add_product[[4]]), color="purple", linetype="dashed", size=1)
  insert_product_h <- insert_product_h + geom_vline(aes(xintercept=percentiles_add_product[[5]]), color="red", linetype="dashed", size=1)
  insert_product_h <- insert_product_h + ggtitle("add product") + theme_light() + xlim(scale_min, scale_max)
  
  
  ## insert order
  percentiles_insert_order <- quantile(data_insertorder_c$res, c(.50, .95, .99, .999, .9999))
  insert_order_h <- ggplot(data_insertorder_c, aes(res)) + geom_histogram(binwidth = 100, color="black", fill="white")
  insert_order_h <- insert_order_h + geom_vline(aes(xintercept=percentiles_insert_order[[1]]), color="blue", linetype="dashed", size=1)
  insert_order_h <- insert_order_h + geom_vline(aes(xintercept=percentiles_insert_order[[2]]), color="green", linetype="dashed", size=1)
  insert_order_h <- insert_order_h + geom_vline(aes(xintercept=percentiles_insert_order[[3]]), color="yellow", linetype="dashed", size=1)
  insert_order_h <- insert_order_h + geom_vline(aes(xintercept=percentiles_insert_order[[4]]), color="purple", linetype="dashed", size=1)
  insert_order_h <- insert_order_h + geom_vline(aes(xintercept=percentiles_insert_order[[5]]), color="red", linetype="dashed", size=1)
  insert_order_h <- insert_order_h + ggtitle("open order") + theme_light() + xlim(scale_min, scale_max)
  
  ## validate order
  percentiles_validate_order <- quantile(data_transactionvalidation_c$res, c(.50, .95, .99, .999, .9999))
  transaction_validation_h <- ggplot(data_transactionvalidation_c, aes(res)) + geom_histogram(binwidth = 100, color="black", fill="white")
  transaction_validation_h <- transaction_validation_h + geom_vline(aes(xintercept=percentiles_validate_order[[1]]), color="blue", linetype="dashed", size=1)
  transaction_validation_h <- transaction_validation_h + geom_vline(aes(xintercept=percentiles_validate_order[[2]]), color="green", linetype="dashed", size=1)
  transaction_validation_h <- transaction_validation_h + geom_vline(aes(xintercept=percentiles_validate_order[[3]]), color="yellow", linetype="dashed", size=1)
  transaction_validation_h <- transaction_validation_h + geom_vline(aes(xintercept=percentiles_validate_order[[4]]), color="purple", linetype="dashed", size=1)
  transaction_validation_h <- transaction_validation_h + geom_vline(aes(xintercept=percentiles_validate_order[[5]]), color="red", linetype="dashed", size=1)
  transaction_validation_h <- transaction_validation_h + ggtitle("validate order") + theme_light() + xlim(scale_min, scale_max)
  
  histograms_full <- multiplot(get_product_h, insert_product_h, insert_order_h, transaction_validation_h, cols=2)
  return(ggplotly(histograms_full))
    
  
}

bench_multicluster_error_histograms <- function(pod_vector, path_file, cluster_vector, scale_min, scale_max){
  
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
  
  #errors <- rbind(application_cluster1_pod1)
  errors <- rbind(application_cluster1_pod1, application_cluster1_pod2) #, application_cluster1_pod3, application_cluster1_pod4, application_cluster1_pod5, application_cluster2_pod1, application_cluster2_pod2, application_cluster2_pod3, application_cluster2_pod4, application_cluster2_pod5, application_cluster3_pod1, application_cluster3_pod2, application_cluster3_pod3, application_cluster3_pod4, application_cluster3_pod5)
  #df reduction col1: timestamp, col2: action
  #df error reduction: col1: timestamp, col2: log level ERROR, col3: action
  errors <- data.frame(errors$V2, errors$V3, errors$V7, errors$V31)
  
  errors <- subset(errors, errors$errors.V3 == "[ERROR]")
  errors <- data.frame(errors$errors.V2, errors$errors.V7, errors$errors.V31)
  
  data_error_insertPO <- subset(errors, errors$errors.errors.V7 == "insertProductOrder")
  data_error_retry    <- subset(errors, errors$errors.errors.V31 == "RETRY")
  data_error_handleRE <- subset(errors, errors$errors.errors.V7 == "handleRequestError")
  
  
  
  
  
  #List of dataframe
  #df_list <- list(data_getproduct, data_insertproduct, data_insertorder, data_transactionvalidation)
  df_list <- list(data_error_retry, data_error_handleRE)
  #rename daraframe columns
  df_list <- lapply(df_list, function(x){colnames(x) <- c("time", "error type", "more info"); return(x)})
  #removing milisecond from timeseries 
  df_list <- lapply(df_list, function(x){x$time <- str_sub(x$time, 1, str_length(x$time)-4); return(x)})
  
  #Get unique timeseries
  interval <- unique(df_list[[1]]$time)
  
  #initialization of dataframe c 
  data_error_insertPO_c <- NULL
  data_error_handleRE_c <- NULL
  data_error_retry_c <- NULL
  
  #Counting request per second
  a <- 1
  for(i in interval){
    data_error_retry_c[a] <- count(subset(df_list[[1]], df_list[[1]]$time == paste(i, sep = "")))
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
  data_error_retry_c <- as.data.frame(df_list_c[1])
  data_error_handleRE_c <- as.data.frame(df_list_c[2])
  
  
  seq_len <- 2
  
  # Histogram
  
  
  ## error insert product order
  percentiles_error_PO <- quantile(data_error_retry_c$res, c(.50, .95, .99, .999, .9999))
  error_retry_h <- ggplot(data_error_retry_c, aes(res)) + geom_histogram(binwidth = 5, color="black", fill="white")
  #error_insertPO_h <- error_insertPO_h + geom_vline(aes(xintercept=percentiles_error_PO[[1]]), color="blue", linetype="dashed", size=1)
  #error_insertPO_h <- error_insertPO_h + geom_vline(aes(xintercept=percentiles_error_PO[[2]]), color="green", linetype="dashed", size=1)
  #error_insertPO_h <- error_insertPO_h + geom_vline(aes(xintercept=percentiles_error_PO[[3]]), color="yellow", linetype="dashed", size=1)
  #error_insertPO_h <- error_insertPO_h + geom_vline(aes(xintercept=percentiles_error_PO[[4]]), color="purple", linetype="dashed", size=1)
  #error_insertPO_h <- error_insertPO_h + geom_vline(aes(xintercept=percentiles_error_PO[[5]]), color="red", linetype="dashed", size=1)
  error_retry_h <- error_retry_h + ggtitle("retry") + theme_light() + xlim(scale_min, scale_max)
  
  
  ## error handle request
  percentiles_error_HR <- quantile(data_error_handleRE_c$res, c(.50, .95, .99, .999, .9999))
  error_handleRE_h <- ggplot(data_error_handleRE_c, aes(res)) + geom_histogram(binwidth = 5, color="black", fill="white")
  #error_handleRE_h <- error_handleRE_h + geom_vline(aes(xintercept=percentiles_error_HR[[1]]), color="blue", linetype="dashed", size=1)
  #error_handleRE_h <- error_handleRE_h + geom_vline(aes(xintercept=percentiles_error_HR[[2]]), color="green", linetype="dashed", size=1)
  #error_handleRE_h <- error_handleRE_h + geom_vline(aes(xintercept=percentiles_error_HR[[3]]), color="yellow", linetype="dashed", size=1)
  #error_handleRE_h <- error_handleRE_h + geom_vline(aes(xintercept=percentiles_error_HR[[4]]), color="purple", linetype="dashed", size=1)
  #error_handleRE_h <- error_handleRE_h + geom_vline(aes(xintercept=percentiles_error_HR[[5]]), color="red", linetype="dashed", size=1)
  error_handleRE_h <- error_handleRE_h + ggtitle("error handle request") + theme_light() + xlim(scale_min, scale_max)
  
  
  histograms_full <- multiplot(error_retry_h, error_handleRE_h, cols=1)
  return(ggplotly(histograms_full))
  
}

# parameters
pod <- c(1:6)
cluster <- c(1:1)
path <- "/Users/octo-luma/Desktop/logs/5.9.18.spanner-150threads-to25-15kCustomer-6nodes/application-cluster"


hist_scale_min= 0
hist_scale_max = 2500

#bench function call
bench_multicluster(bench_name = "Spanner bench regional - request per second", 
      pod_vector = pod, 
      path_file = path,
      e = FALSE,
      cluster_vector = cluster, 0, 0)

bench_multicluster(bench_name = "Spanner bench regional - errors", 
                   pod_vector = pod, 
                   path_file = path,
                   e = TRUE,
                   cluster_vector = cluster, 0, 0)

bench_multicluster_histograms(pod_vector = pod, 
                              path_file = path,
                              cluster_vector = cluster, 
                              hist_scale_min,
                              hist_scale_max)

bench_multicluster_error_histograms(pod_vector = pod, 
                                    path_file = path,
                                    cluster_vector = cluster,
                                    hist_scale_min,
                                    hist_scale_max)