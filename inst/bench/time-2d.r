info =  rxGetInfoXdf("ADS", getVarInfo = TRUE)
bin1 = c((info$varInfo[[1]]$high - info$varInfo[[1]]$low)/10, (info$varInfo[[2]]$high - info$varInfo[[2]]$low)/10)
bin2 = c((info$varInfo[[1]]$high - info$varInfo[[1]]$low)/100, (info$varInfo[[2]]$high - info$varInfo[[2]]$low)/100)
bin3 = c((info$varInfo[[1]]$high - info$varInfo[[1]]$low)/1000, (info$varInfo[[2]]$high - info$varInfo[[2]]$low)/1000)

bin_10 <- replicate(5,system.time(bin_2d("ADS",1,2,bin1)))[3,]
bin_100 <- replicate(5,system.time(bin_2d("ADS",1,2,bin2)))[3,]
bin_1000 <- replicate(5,system.time(bin_2d("ADS",1,2,bin3)))[3,]

bin10 <- bin_2d("ADS",1,2,bin1)
bin100 <- bin_2d("ADS",1,2,bin2)
bin1000 <- bin_2d("ADS",1,2,bin3)

smooth_10 <- replicate(5,system.time(density_2d(bin10,50)))[3,]
smooth_100 <- replicate(5,system.time(density_2d(bin100,50)))[3,]
smooth_1000<- replicate(5,system.time(density_2d(bin1000,50)))[3,]


sample_data_bin <- rbind(bin_10,bin_100,bin_1000)
rownames(sample_data_bin) <- c("10 bins","100 bins","1000 bins")
colnames(sample_data_bin) <- c("1","2","3","4","5")
write.table(sample_data_bin, "Sample_500000_bin_time.csv", sep = ",", row = F)


sample_data_smooth<- rbind(smooth_10,smooth_100,smooth_1000)
rownames(sample_data_smooth) <- c("10 bins","100 bins","1000 bins")
colnames(sample_data_smooth) <- c("1","2","3","4","5")
write.table(sample_data_smooth, "Sample_500000_smooth_time.csv", sep = ",", row = F)


info =  rxGetInfoXdf("subADS", getVarInfo = TRUE)
bin1 = c((info$varInfo[[1]]$high - info$varInfo[[1]]$low)/10, (info$varInfo[[2]]$high - info$varInfo[[2]]$low)/10)
bin2 = c((info$varInfo[[1]]$high - info$varInfo[[1]]$low)/100, (info$varInfo[[2]]$high - info$varInfo[[2]]$low)/100)
bin3 = c((info$varInfo[[1]]$high - info$varInfo[[1]]$low)/1000, (info$varInfo[[2]]$high - info$varInfo[[2]]$low)/1000)

bin_10 <- replicate(5,system.time(bin_2d("subADS",1,2,bin1)))[3,]
bin_100 <- replicate(5,system.time(bin_2d("subADS",1,2,bin2)))[3,]
bin_1000 <- replicate(5,system.time(bin_2d("subADS",1,2,bin3)))[3,]

bin10 <- bin_2d("subADS",1,2,bin1)
bin100 <- bin_2d("subADS",1,2,bin2)
bin1000 <- bin_2d("subADS",1,2,bin3)

smooth_10 <- replicate(5,system.time(density_2d(bin10,50)))[3,]
smooth_100 <- replicate(5,system.time(density_2d(bin100,50)))[3,]
smooth_1000<- replicate(5,system.time(density_2d(bin1000,50)))[3,]

sample_data_bin <- rbind(bin_10,bin_100,bin_1000)
rownames(sample_data_bin) <- c("10 bins","100 bins","1000 bins")
colnames(sample_data_bin) <- c("1","2","3","4","5")
write.table(sample_data_bin, "Sample_50000_bin_time.csv", sep = ",", row = F)


sample_data_smooth <- rbind(smooth_10,smooth_100,smooth_1000)
rownames(sample_data_smooth) <- c("10 bins","100 bins","1000 bins")
colnames(sample_data_smooth) <- c("1","2","3","4","5")
write.table(sample_data_smooth, "Sample_50000_smooth_time.csv", sep = ",", row = F)


