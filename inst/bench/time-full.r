############ 1e6
info =  rxGetInfoXdf("ADS_1e6", getVarInfo = TRUE)
bin1 = (info$varInfo[[1]]$high - info$varInfo[[1]]$low)/10
bin2 = (info$varInfo[[1]]$high - info$varInfo[[1]]$low)/100
bin3 = (info$varInfo[[1]]$high - info$varInfo[[1]]$low)/1000

bin_10 <- replicate(5,system.time(bin_1d("ADS_1e6",1,bin1)))[3,]
bin_100 <- replicate(5,system.time(bin_1d("ADS_1e6",1,bin2)))[3,]
bin_1000 <- replicate(5,system.time(bin_1d("ADS_1e6",1,bin3)))[3,]

bin10 <- bin_1d("ADS_1e6",1,bin1)
bin100 <- bin_1d("ADS_1e6",1,bin2)
bin1000 <- bin_1d("ADS_1e6",1,bin3)


smooth_10 <- replicate(5,system.time(density_1d(bin10,500)))[3,]
smooth_100 <- replicate(5,system.time(density_1d(bin100,500)))[3,]
smooth_1000<- replicate(5,system.time(density_1d(bin1000,500)))[3,]


sample_data_bin <- rbind(bin_10,bin_100,bin_1000)
rownames(sample_data_bin) <- c("10 bins","100 bins","1000 bins")
colnames(sample_data_bin) <- c("1","2","3","4","5")
write.table(sample_data_bin, "f1e6_bin_time_1d.csv", sep = ",", row = F)


sample_data_smooth<- rbind(smooth_10,smooth_100,smooth_1000)
rownames(sample_data_smooth) <- c("10 bins","100 bins","1000 bins")
colnames(sample_data_smooth) <- c("1","2","3","4","5")
write.table(sample_data_smooth, "f1e6_smooth_time_1d.csv", sep = ",", row = F)


info =  rxGetInfoXdf("ADS_1e6", getVarInfo = TRUE)
bin1 = c((info$varInfo[[1]]$high - info$varInfo[[1]]$low)/10, (info$varInfo[[2]]$high - info$varInfo[[2]]$low)/10)
bin2 = c((info$varInfo[[1]]$high - info$varInfo[[1]]$low)/100, (info$varInfo[[2]]$high - info$varInfo[[2]]$low)/100)
bin3 = c((info$varInfo[[1]]$high - info$varInfo[[1]]$low)/1000, (info$varInfo[[2]]$high - info$varInfo[[2]]$low)/1000)

bin_10 <- replicate(5,system.time(bin_2d("ADS_1e6",1,2,bin1)))[3,]
bin_100 <- replicate(5,system.time(bin_2d("ADS_1e6",1,2,bin2)))[3,]
bin_1000 <- replicate(5,system.time(bin_2d("ADS_1e6",1,2,bin3)))[3,]

bin10 <- bin_2d("ADS_1e6",1,2,bin1)
bin100 <- bin_2d("ADS_1e6",1,2,bin2)
bin1000 <- bin_2d("ADS_1e6",1,2,bin3)

smooth_10 <- replicate(5,system.time(density_2d(bin10,500)))[3,]
smooth_100 <- replicate(5,system.time(density_2d(bin100,500)))[3,]
smooth_1000<- replicate(5,system.time(density_2d(bin1000,500)))[3,]


sample_data_bin <- rbind(bin_10,bin_100,bin_1000)
rownames(sample_data_bin) <- c("10 bins","100 bins","1000 bins")
colnames(sample_data_bin) <- c("1","2","3","4","5")
write.table(sample_data_bin, "f1e6_smooth_time_2d.csv", sep = ",", row = F)


sample_data_smooth<- rbind(smooth_10,smooth_100,smooth_1000)
rownames(sample_data_smooth) <- c("10 bins","100 bins","1000 bins")
colnames(sample_data_smooth) <- c("1","2","3","4","5")
write.table(sample_data_smooth,"f1e6_smooth_time_2d.csv", sep = ",", row = F)

###########   5*1e6 ###########

info =  rxGetInfoXdf("ADS_5_1e6", getVarInfo = TRUE)
bin1 = (info$varInfo[[1]]$high - info$varInfo[[1]]$low)/10
bin2 = (info$varInfo[[1]]$high - info$varInfo[[1]]$low)/100
bin3 = (info$varInfo[[1]]$high - info$varInfo[[1]]$low)/1000

bin_10 <- replicate(5,system.time(bin_1d("ADS_5_1e6",1,bin1)))[3,]
bin_100 <- replicate(5,system.time(bin_1d("ADS_5_1e6",1,bin2)))[3,]
bin_1000 <- replicate(5,system.time(bin_1d("ADS_5_1e6",1,bin3)))[3,]

bin10 <- bin_1d("ADS_5_1e6",1,bin1)
bin100 <- bin_1d("ADS_5_1e6",1,bin2)
bin1000 <- bin_1d("ADS_5_1e6",1,bin3)


smooth_10 <- replicate(5,system.time(density_1d(bin10,500)))[3,]
smooth_100 <- replicate(5,system.time(density_1d(bin100,500)))[3,]
smooth_1000<- replicate(5,system.time(density_1d(bin1000,500)))[3,]


sample_data_bin <- rbind(bin_10,bin_100,bin_1000)
rownames(sample_data_bin) <- c("10 bins","100 bins","1000 bins")
colnames(sample_data_bin) <- c("1","2","3","4","5")
write.table(sample_data_bin, "f5_1e6_bin_time_1d.csv", sep = ",", row = F)


sample_data_smooth<- rbind(smooth_10,smooth_100,smooth_1000)
rownames(sample_data_smooth) <- c("10 bins","100 bins","1000 bins")
colnames(sample_data_smooth) <- c("1","2","3","4","5")
write.table(sample_data_smooth, "f5_1e6_smooth_time_1d.csv", sep = ",", row = F)


info =  rxGetInfoXdf("ADS_5_1e6", getVarInfo = TRUE)
bin1 = c((info$varInfo[[1]]$high - info$varInfo[[1]]$low)/10, (info$varInfo[[2]]$high - info$varInfo[[2]]$low)/10)
bin2 = c((info$varInfo[[1]]$high - info$varInfo[[1]]$low)/100, (info$varInfo[[2]]$high - info$varInfo[[2]]$low)/100)
bin3 = c((info$varInfo[[1]]$high - info$varInfo[[1]]$low)/1000, (info$varInfo[[2]]$high - info$varInfo[[2]]$low)/1000)

bin_10 <- replicate(5,system.time(bin_2d("ADS_5_1e6",1,2,bin1)))[3,]
bin_100 <- replicate(5,system.time(bin_2d("ADS_5_1e6",1,2,bin2)))[3,]
bin_1000 <- replicate(5,system.time(bin_2d("ADS_5_1e6",1,2,bin3)))[3,]

bin10 <- bin_2d("ADS_5_1e6",1,2,bin1)
bin100 <- bin_2d("ADS_5_1e6",1,2,bin2)
bin1000 <- bin_2d("ADS_5_1e6",1,2,bin3)

smooth_10 <- replicate(5,system.time(density_2d(bin10,500)))[3,]
smooth_100 <- replicate(5,system.time(density_2d(bin100,500)))[3,]
smooth_1000<- replicate(5,system.time(density_2d(bin1000,500)))[3,]


sample_data_bin <- rbind(bin_10,bin_100,bin_1000)
rownames(sample_data_bin) <- c("10 bins","100 bins","1000 bins")
colnames(sample_data_bin) <- c("1","2","3","4","5")
write.table(sample_data_bin, "f5_1e6_smooth_time_2d.csv", sep = ",", row = F)


sample_data_smooth<- rbind(smooth_10,smooth_100,smooth_1000)
rownames(sample_data_smooth) <- c("10 bins","100 bins","1000 bins")
colnames(sample_data_smooth) <- c("1","2","3","4","5")
write.table(sample_data_smooth,"f5_1e6_smooth_time_2d.csv", sep = ",", row = F)

###########   1e7 ###########

info =  rxGetInfoXdf("ADS_1e7", getVarInfo = TRUE)
bin1 = (info$varInfo[[1]]$high - info$varInfo[[1]]$low)/10
bin2 = (info$varInfo[[1]]$high - info$varInfo[[1]]$low)/100
bin3 = (info$varInfo[[1]]$high - info$varInfo[[1]]$low)/1000

bin_10 <- replicate(5,system.time(bin_1d("ADS_1e7",1,bin1)))[3,]
bin_100 <- replicate(5,system.time(bin_1d("ADS_1e7",1,bin2)))[3,]
bin_1000 <- replicate(5,system.time(bin_1d("ADS_1e7",1,bin3)))[3,]

bin10 <- bin_1d("ADS_5_1e7",1,bin1)
bin100 <- bin_1d("ADS_5_1e7",1,bin2)
bin1000 <- bin_1d("ADS_5_1e7",1,bin3)


smooth_10 <- replicate(5,system.time(density_1d(bin10,500)))[3,]
smooth_100 <- replicate(5,system.time(density_1d(bin100,500)))[3,]
smooth_1000<- replicate(5,system.time(density_1d(bin1000,500)))[3,]


sample_data_bin <- rbind(bin_10,bin_100,bin_1000)
rownames(sample_data_bin) <- c("10 bins","100 bins","1000 bins")
colnames(sample_data_bin) <- c("1","2","3","4","5")
write.table(sample_data_bin, "f1e7_bin_time_1d.csv", sep = ",", row = F)


sample_data_smooth<- rbind(smooth_10,smooth_100,smooth_1000)
rownames(sample_data_smooth) <- c("10 bins","100 bins","1000 bins")
colnames(sample_data_smooth) <- c("1","2","3","4","5")
write.table(sample_data_smooth, "f1e7_smooth_time_1d.csv", sep = ",", row = F)


info =  rxGetInfoXdf("ADS_1e7", getVarInfo = TRUE)
bin1 = c((info$varInfo[[1]]$high - info$varInfo[[1]]$low)/10, (info$varInfo[[2]]$high - info$varInfo[[2]]$low)/10)
bin2 = c((info$varInfo[[1]]$high - info$varInfo[[1]]$low)/100, (info$varInfo[[2]]$high - info$varInfo[[2]]$low)/100)
bin3 = c((info$varInfo[[1]]$high - info$varInfo[[1]]$low)/1000, (info$varInfo[[2]]$high - info$varInfo[[2]]$low)/1000)

bin_10 <- replicate(5,system.time(bin_2d("ADS_1e7",1,2,bin1)))[3,]
bin_100 <- replicate(5,system.time(bin_2d("ADS_1e7",1,2,bin2)))[3,]
bin_1000 <- replicate(5,system.time(bin_2d("ADS_1e7",1,2,bin3)))[3,]

bin10 <- bin_2d("ADS_1e7",1,2,bin1)
bin100 <- bin_2d("ADS_1e7",1,2,bin2)
bin1000 <- bin_2d("ADS_1e7",1,2,bin3)

smooth_10 <- replicate(5,system.time(density_2d(bin10,500)))[3,]
smooth_100 <- replicate(5,system.time(density_2d(bin100,500)))[3,]
smooth_1000<- replicate(5,system.time(density_2d(bin1000,500)))[3,]


sample_data_bin <- rbind(bin_10,bin_100,bin_1000)
rownames(sample_data_bin) <- c("10 bins","100 bins","1000 bins")
colnames(sample_data_bin) <- c("1","2","3","4","5")
write.table(sample_data_bin, "f1e7_smooth_time_2d.csv", sep = ",", row = F)


sample_data_smooth<- rbind(smooth_10,smooth_100,smooth_1000)
rownames(sample_data_smooth) <- c("10 bins","100 bins","1000 bins")
colnames(sample_data_smooth) <- c("1","2","3","4","5")
write.table(sample_data_smooth,"f1e7_smooth_time_2d.csv", sep = ",", row = F)
