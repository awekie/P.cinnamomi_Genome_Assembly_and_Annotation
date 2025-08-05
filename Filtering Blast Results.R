#use Blast_Filtering.R instead
myfiles <- list.files(pattern = "blastp$")
print(myfiles)


for (file in myfiles) {
  df<-read.delim(file, header = FALSE)
  listy<-c("QID","Qstart","Qend","DBID","DBstart","DBend","Evalue","bitscore","length","%ID","Qcovs")
  colnames(df) <- listy               
  
  uniqueID<-unique(df[1])
  uniqueDBID <- unique(df[4])
  
  output <- data.frame(matrix(data=NA, ncol = ncol(df), nrow = 0))
  colnames(output) <- listy
  
nrow(uniqueDBID)
  for(i in 1:nrow(uniqueDBID)) {
    test <- subset(df,DBID == uniqueDBID[i,])
    test2 <- test[which(test$Evalue == min(test$Evalue)),]
    test3 <- test2[which(test2$`%ID` == max(test2$`%ID`)),]
    output <- rbind(output,test3)
    output$DBID <- gsub(",",":",output$DBID)
  }
  
  output2 <- data.frame(matrix(data=NA, ncol = ncol(output), nrow = 0))

  uniqueID<-unique(output[1])
nrow(uniqueID)
  
for(j in 1:nrow(uniqueID)) {
  
    test4 <- subset(output,output$QID == uniqueID[j,])
    test5 <- test4[which(test4$Evalue == min(test4$Evalue)),]
    output2 <- rbind(output2,test5)
    final <- output2%>% select(QID,DBID)
}
  
  #-double ups
  test6 <- table(final$DBID)
  doubleups <- names(test6[test6>1])
  print(doubleups)
  test7 <- final[-which(final$DBID %in% doubleups),]
  newfilename <- paste0(file,"_filtered.csv")
  write.csv(test7, newfilename,quote = FALSE)
}
