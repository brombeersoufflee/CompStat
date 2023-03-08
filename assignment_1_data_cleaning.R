data <- read.csv("a1_data.csv")

for (col in colnames(data)){
  
  # First (probabilistic) approach is commented out
  
  # test for multimodality and get the distributions parameters 
  # p_value <- modetest(data[,col], method="SI")$p.value

  # # multimodal distribution
  # if (p_value <= 0.05){
  #   # locmodes(data[,col], mod0=2)
  #   print(glue("Var {col}, multimodal"))
  #   result <- normalmixEM(data[,col], k=2)
  #   mean <- result$mu
  #   sd <- result$sigma
  # }
  # 
  # # unimodal distribution
  # else{
  #   print(glue("Var {col}, unimodal"))
  #   mean <- mean(data[,col])
  #   sd <- sd(data[,col])
  # }

  
  # the cut points BUT not the bins
  cut_points <- seq(min(data[,col]), max(data[,col]), length.out=100)

  # fit the data into bins
  cuts <- cut(data[,col], cut_points, ordered_result = TRUE, dig.lab=7)
  
  # save the intervals
  levels <- levels(cuts)

  # create a temp. dataframe that stores the points of given variable and their respective interval
  bins <- data.frame(
    value = data[,col],
    interval = cuts
  )
  bins <-na.omit(bins)
  
  removed_points <- 0


  max_diff <- 0
  max_interval <- levels[0]
  bin_proportions <- prop.table(table(bins$interval))
  prob_total <-0
  
  # iterate over the intervals 
  for (i in 1:length(cut_points)){
  
    if (is.na(cut_points[i+1])){
      break
    }
    
  #   # calculate the probability for each interval 
  #   prob <- pnorm(cut_points[i+1], mean=mean, sd=sd, lower.tail = TRUE) -
  #     pnorm(cut_points[i], mean=mean, sd=sd, lower.tail = TRUE)
  #   
  #   # in case of multimodal just average the two probs.
  #   prob<-mean(prob)
  #   
  #   # check if it adds up to 1 
  #   prob_total <- prob_total + prob 
  #   
  #   # find the biggest difference 
  #   diff <- abs(bin_proportions[i]-prob)
  #   print(glue("Current diff: {diff} and current interval: {levels[i]}"))
  #   if (diff > max_diff){
  #     
  #     max_diff <- diff
  #     max_interval <- levels[i]
  #     max_id <- i
  #   }
  
  
  # amount of points to the left and to the right 
  if (i==1){
    freq_l <- 0
  }
    else{
      freq_l <- nrow(bins[bins$interval==levels[i-1],])
    }
  if (i==length(cut_points)){
    freq_r <- 0
  }
  else{
    freq_r <- nrow(bins[bins$interval==levels[i+1],])
  }
  
  
  # current amount of points in the max difference bin
  freq_current <- nrow(bins[bins$interval==levels[i],])
  
  # amount of points in the max difference bin after averagining 
  freq_new <- ceiling((freq_l+freq_r)/2)
  to_drop <- abs(freq_current-freq_new)
  
  
  # randomly select points to drop from the noisy bin 
  if (freq_current > freq_new*1.25){
    temp <- bins[bins$interval==levels[i],]
    selected <- temp[sample(nrow(temp), to_drop), ]
    removed_points = removed_points + to_drop
    bins <- bins[!(row.names(bins) %in% rownames(selected)),]
  }
}
  
  # remove the data until the proportion of the bin is statistically indifferent
  # from the probability given by the underlying distribution. It lacks the statistical
  # test. 
  # while(round(bin_proportions[max_id],digits=5) > (round(prob,digits=5))){
  #   temp <- bins[bins$interval==max_interval,]
  #   selected <- temp[sample(nrow(temp), 1), ]
  #   removed_points = removed_points + 1
  #   bins <- bins[!(row.names(bins) %in% rownames(selected)),]
  #   bin_proportions <- prop.table(table(bins$interval))
  # }
  
  # overwrite the data
  data[1:nrow(data),col] <- NA
  data[1:nrow(bins), col] <- bins$value
}

ggplot(pivot_longer(data, everything()), aes(value)) +
  geom_histogram(bins = 100) +
  facet_wrap(~name, nrow = 3)

write.csv(data, "data_reduced.csv", row.names=FALSE)
