bin1 = (1490+86)/10
bin2 = (1490+86)/100
bin3 = (1490+86)/1000

bin_10 <- replicate(5,system.time(bin_1d("ADS",1,bin1)))[3,]
bin_100 <- replicate(5,system.time(bin_1d("ADS",1,bin2)))[3,]
bin_1000 <- replicate(5,system.time(bin_1d("ADS",1,bin3)))[3,]

bin10 <- bin_1d("ADS",1,bin1)
bin100 <- bin_1d("ADS",1,bin2)
bin1000 <- bin_1d("ADS",1,bin3)


smooth_10 <- replicate(5,system.time(density_1d(bin10,50)))[3,]
smooth_100 <- replicate(5,system.time(density_1d(bin100,50)))[3,]
smooth_1000<- replicate(5,system.time(density_1d(bin1000,50)))[3,]


sample_data_bin <- rbind(bin_10,bin_100,bin_1000)
rownames(sample_data_bin) <- c("10 bins","100 bins","1000 bins")
colnames(sample_data_bin) <- c("1","2","3","4","5")
write.table(sample_data_bin, "Sample_500000_bin_time.csv", sep = ",", row = F)


sample_data_smooth<- rbind(smooth_10,smooth_100,smooth_1000)
rownames(sample_data_smooth) <- c("10 bins","100 bins","1000 bins")
colnames(sample_data_smooth) <- c("1","2","3","4","5")
write.table(sample_data_smooth, "Sample_500000_smooth_time.csv", sep = ",", row = F)


#write.table(subADS,"subADS.csv",sep = ",", row = F)
#rxTextToXdf(inFile = "subADS.csv", outFile = "subADS", stringsAsFactors = TRUE, #overwrite = TRUE)
#myTransform <- function(data){
 #     data$Late <- data$ArrDelay > 15
  #    data$DepHour <- as.integer(data$CRSDepTime)
   #   data$Night <- data$DepHour >= 20 | data$DepHour <= 5
    #  return(data)
	#}
#rxDataStepXdf(inFile = "ADS", outFile = "subADS",transformFunc = #myTransform,transformVars = c("ArrDelay", "CRSDepTime") overwrite = TRUE)

	


bin1 = (440+46)/10
bin2 = (440+46)/100
bin3 = (440+46)/1000

bin_10 <- replicate(5,system.time(bin_1d("subADS",1,bin1)))[3,]
bin_100 <- replicate(5,system.time(bin_1d("subADS",1,bin2)))[3,]
bin_1000 <- replicate(5,system.time(bin_1d("subADS",1,bin3)))[3,]

bin10 <- bin_1d("subADS",1,bin1)
bin100 <- bin_1d("subADS",1,bin2)
bin1000 <- bin_1d("subADS",1,bin3)

smooth_10 <- replicate(5,system.time(density_1d(bin10,50)))[3,]
smooth_100 <- replicate(5,system.time(density_1d(bin100,50)))[3,]
smooth_1000<- replicate(5,system.time(density_1d(bin1000,50)))[3,]

sample_data_bin <- rbind(bin_10,bin_100,bin_1000)
rownames(sample_data_bin) <- c("10 bins","100 bins","1000 bins")
colnames(sample_data_bin) <- c("1","2","3","4","5")
write.table(sample_data_bin, "Sample_50000_bin_time.csv", sep = ",", row = F)


sample_data_smooth <- rbind(smooth_10,smooth_100,smooth_1000)
rownames(sample_data_smooth) <- c("10 bins","100 bins","1000 bins")
colnames(sample_data_smooth) <- c("1","2","3","4","5")
write.table(sample_data_smooth, "Sample_50000_smooth_time.csv", sep = ",", row = F)

subADS <- rxReadXdf(file = "ADS", varsToKeep = c("ArrDelay","CRSDepTime","DayOfWeek"), startRow = 10000, numRows = 60000)

write.table(subADS,"subADS.csv",sep = ",", row = F)
rxTextToXdf(inFile = "subADS.csv", outFile = "subADS", stringsAsFactors = TRUE, overwrite = TRUE)
	
rxGetInfoXdf("subADS",getVarInfo = TRUE)
