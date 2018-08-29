library(ggplot2)

# ------------------------------------------------------------------------
# ATTENTION: la charge doit être calibrer
# ------------------------------------------------------------------------

bench_plot_10pt <- function(bench_name, action_line_name, vecteur_charge){
  
  # ------------------------------------------------------------------------
  # import main data file cockroach
  # ------------------------------------------------------------------------
  cockroachfile_pod0 <- NULL
  cockroachfile_pod1 <- NULL
  
  for(charge in vecteur_charge){
    cockroachfile_pod0[charge] <- c(paste("./data/cockroach_3N_100k/cockroach_3N_",charge,"K_0.txt", sep = ""))
    assign(paste("cockroach_", charge,"k_0", sep = ""), 
           as.data.frame(read.csv(cockroachfile_pod0[charge], header=FALSE, sep=" ")))
    
    cockroachfile_pod1[charge] <- c(paste("./data/cockroach_3N_100k/cockroach_3N_",charge,"K_1.txt", sep = ""))
    assign(paste("cockroach_", charge,"k_1", sep = ""), 
           as.data.frame(read.csv(cockroachfile_pod1[charge], header=FALSE, sep=" ")))
  }
  
  # ------------------------------------------------------------------------
  # bench cockroach, MEAN
  # ------------------------------------------------------------------------
  
  data_cockroach_0 <- data.frame(cockroach_10k_0$V1,
                                 cockroach_10k_0$V2,
                                 cockroach_20k_0$V2,
                                 cockroach_30k_0$V2,
                                 cockroach_40k_0$V2,
                                 cockroach_50k_0$V2,
                                 cockroach_60k_0$V2,
                                 cockroach_70k_0$V2,
                                 cockroach_80k_0$V2,
                                 cockroach_90k_0$V2,
                                 cockroach_100k_0$V2) 
  
  data_cockroach_1 <- data.frame(cockroach_10k_1$V1,
                                 cockroach_10k_1$V2,
                                 cockroach_20k_1$V2,
                                 cockroach_30k_1$V2,
                                 cockroach_40k_1$V2,
                                 cockroach_50k_1$V2,
                                 cockroach_60k_1$V2,
                                 cockroach_70k_1$V2,
                                 cockroach_80k_1$V2,
                                 cockroach_90k_1$V2,
                                 cockroach_100k_1$V2)
  
  
  action_line_name <- "validateordermedian"
  
  colnames(data_cockroach_0)[1] <- c("action")
  colnames(data_cockroach_1)[1] <- c("action")
  
  data_cockroach_0 <- subset(data_cockroach_0, data_cockroach_0$action == action_line_name)
  data_cockroach_1 <- subset(data_cockroach_1, data_cockroach_1$action == action_line_name)
  
  colnames(data_cockroach_1) <- colnames(data_cockroach_0)
  data_cockroach <- data.frame(t(data_cockroach_1), t(data_cockroach_0))
  data_cockroach <- data_cockroach[-1,]
  
  data_cockroach$X2 <- as.numeric(data_cockroach$X2)
  data_cockroach$X2.1 <- as.numeric(data_cockroach$X2.1)
  
  
  for(i in 1:nrow(data_cockroach)){
    if(data_cockroach$X2[i] == 0){
      data_cockroach$X2[i] <- data_cockroach$X2.1[i]
    } else if(data_cockroach$X2.1[i] == 0){
      data_cockroach$X2.1[i] <- data_cockroach$X2[i]
    } else {}
  }
  
  data_cockroach_moy <- data.frame(data_cockroach, (data_cockroach$X2+data_cockroach$X2.1)/2, vecteur_charge)
  colnames(data_cockroach_moy) <- c("pod0","pod1","res","charge")
  
  # ------------------------------------------------------------------------
  # import main data file spanner
  # ------------------------------------------------------------------------
  spannerfile_pod0 <- NULL
  spannerfile_pod1 <- NULL
  
  for(charge in vecteur_charge){
    spannerfile_pod0[charge] <- c(paste("./data/spanner_3N_100k/SPANNER_3N_",charge,"K_0.txt", sep = ""))
    assign(paste("spanner_", charge,"k_0", sep = ""), 
           as.data.frame(read.csv(spannerfile_pod0[charge], header=FALSE, sep=" ")))
    
    spannerfile_pod1[charge] <- c(paste("./data/spanner_3N_100k/SPANNER_3N_",charge,"K_1.txt", sep = ""))
    assign(paste("spanner_", charge,"k_1", sep = ""), 
           as.data.frame(read.csv(spannerfile_pod1[charge], header=FALSE, sep=" ")))
  }
  
  # ------------------------------------------------------------------------
  # bench spanner, MEAN
  # ------------------------------------------------------------------------
  
  data_spanner_0 <- data.frame(spanner_10k_0$V1,
                               spanner_10k_0$V2,
                               spanner_20k_0$V2,
                               spanner_30k_0$V2,
                               spanner_40k_0$V2,
                               spanner_50k_0$V2,
                               spanner_60k_0$V2,
                               spanner_70k_0$V2,
                               spanner_80k_0$V2,
                               spanner_90k_0$V2,
                               spanner_100k_0$V2) 
  
  data_spanner_1 <- data.frame(spanner_10k_1$V1,
                               spanner_10k_1$V2,
                               spanner_20k_1$V2,
                               spanner_30k_1$V2,
                               spanner_40k_1$V2,
                               spanner_50k_1$V2,
                               spanner_60k_1$V2,
                               spanner_70k_1$V2,
                               spanner_80k_1$V2,
                               spanner_90k_1$V2,
                               spanner_100k_1$V2)
  
  
  action_line_name <- "validateordermedian"
  
  colnames(data_spanner_0)[1] <- c("action")
  colnames(data_spanner_1)[1] <- c("action")
  
  data_spanner_0 <- subset(data_spanner_0, data_spanner_0$action == action_line_name)
  data_spanner_1 <- subset(data_spanner_1, data_spanner_1$action == action_line_name)
  
  colnames(data_spanner_1) <- colnames(data_spanner_0)
  data_spanner <- data.frame(t(data_spanner_1), t(data_spanner_0))
  data_spanner <- data_spanner[-1,]
  
  data_spanner$X2 <- as.numeric(data_spanner$X2)
  data_spanner$X2.1 <- as.numeric(data_spanner$X2.1)
  
  for(i in 1:nrow(data_spanner)){
    if(data_spanner$X2[i] == 0){
      data_spanner$X2[i] <- data_spanner$X2.1[i]
    } else if(data_spanner$X2.1[i] == 0){
      data_spanner$X2.1[i] <- data_spanner$X2[i]
    } else {}
  }
  
  data_spanner_moy <- data.frame(data_spanner, (data_spanner$X2+data_spanner$X2.1)/2, vecteur_charge)
  colnames(data_spanner_moy) <- c("pod0","pod1","res","charge")
  
  
  # ------------------------------------------------------------------------
  # plot RES / charge
  # ------------------------------------------------------------------------
  
  return(ggplot(NULL) + 
           geom_line(data = data_cockroach_moy, aes(charge, res, colour = 'cockroach')) + 
           geom_line(data = data_spanner_moy, aes(charge, res, colour = 'spanner')) + 
           ggtitle(paste(bench_name," ",action_type)) + 
           expand_limits(x = 0, y = 0) +
           xlab("charge") + 
           ylab("median time") +
           theme_light())
}

action_vector <- c("getproductmedian", "addproductordermedian", "createordermedian", "validateordermedian")

for(action_type in action_vector ){
  bench_plot_10pt(paste("Bench spanner_3N action:"), action_type, c((1:10)*10))
  ggsave(paste(action_type,".png"))
}



