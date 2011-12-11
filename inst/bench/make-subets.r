rxGetInfoXdf("AirlineData87to08",getVarInfo = TRUE)


ADS_1e6 <- rxReadXdf(file = "AirlineData87to08", varsToKeep = c("ArrDelay","CRSDepTime","DayOfWeek"), startRow = 10000, numRows = 1000000)
write.table(ADS_1e6,"ADS_1e6.csv",sep = ",", row = F)
rxTextToXdf(inFile = "ADS_1e6.csv", outFile = "ADS_1e6", stringsAsFactors = TRUE, overwrite = TRUE)	

ADS_5_1e6 <- rxReadXdf(file = "AirlineData87to08", varsToKeep = c("ArrDelay","CRSDepTime","DayOfWeek"), startRow = 10000, numRows = 5000000)
write.table(ADS_5_1e6,"ADS_5_1e6.csv",sep = ",", row = F)
rxTextToXdf(inFile = "ADS_5_1e6.csv", outFile = "ADS_5_1e6", stringsAsFactors = TRUE, overwrite = TRUE)	

ADS_1e7 <- rxReadXdf(file = "AirlineData87to08", varsToKeep = c("ArrDelay","CRSDepTime","DayOfWeek"), startRow = 10000, numRows = 10000000)
write.table(ADS_1e7,"ADS_1e7.csv",sep = ",", row = F)
rxTextToXdf(inFile = "ADS_1e.csv", outFile = "ADS_1e7", stringsAsFactors = TRUE, overwrite = TRUE)

ADS_5_1e7 <- rxReadXdf(file = "AirlineData87to08", varsToKeep = c("ArrDelay","CRSDepTime","DayOfWeek"), startRow = 10000, numRows = 50000000)
write.table(ADS_5_1e7,"ADS_5_1e7.csv",sep = ",", row = F)
rxTextToXdf(inFile = "ADS_5_1e7.csv", outFile = "ADS_5_1e7", stringsAsFactors = TRUE, overwrite = TRUE)	


ADS_1e8 <- rxReadXdf(file = "AirlineData87to08", varsToKeep = c("ArrDelay","CRSDepTime","DayOfWeek"), startRow = 10000, numRows = 100000000)
write.table(ADS_1e8,"ADS_1e8.csv",sep = ",", row = F)
rxTextToXdf(inFile = "ADS_1e8.csv", outFile = "ADS_1e8", stringsAsFactors = TRUE, overwrite = TRUE)

