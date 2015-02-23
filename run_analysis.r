
library(plyr)
library(dplyr)
library(data.table)
##Uncomment the following 6 lines if the data is not present in directory
##url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
##download.file(url, destfile="gcprojectdata.zip",  mode = "wb" );
##unzip(zipfile= "gcprojectdata.zip", files = NULL, list = FALSE, overwrite = TRUE,
##     junkpaths = FALSE,  unzip = "internal",
##     setTimes = FALSE)

## read in the features
cnames <-read.csv(file="UCI HAR Dataset//features.txt",sep =" ",header=FALSE);

## drop the number at the front and set a negative width if the column is not mean() or std()
cnames <- cnames %>%
    select(V2) %>%  
    mutate( wdth = ifelse(V2 %like% "*mean\\(\\)*" | V2 %like% "*std\\(\\)*",16, -16)) %>%
    ## make name r-legal
    mutate( colname= ifelse( wdth > 0, gsub("std\\(\\)", "Std", gsub("mean\\(\\)","Mean",gsub("-","", V2))), V2))

cols <- as.vector(select( filter(cnames, wdth==16), colname));

    
acts <- read.fwf(file="UCI HAR Dataset/activity_labels.txt", widths=c(2,20),header = FALSE, sep = "",
                 skip = 0, n = -1)
tstacts <- read.csv(file="UCI HAR Dataset/Test/y_Test.txt", header=FALSE)
tstacts <- tstacts  %>% dplyr::left_join(acts,by=c("V1" = "V1")) %>% select(V2)
names(tstacts) <- c("Activity")

trnacts <- read.csv(file="UCI HAR Dataset/Train/y_Train.txt", header=FALSE)
trnacts <- trnacts  %>% dplyr::left_join(acts,by=c("V1" = "V1")) %>% select(V2)
names(trnacts) <- c("activity")

tstsubj <- read.csv(file="UCI HAR Dataset/Test/subject_test.txt", col.names=c("subject"), header=FALSE)
trnsubj <- read.csv(file="UCI HAR Dataset/Train/subject_train.txt", col.names=c("subject"), header=FALSE)

tst <- read.fwf(file="UCI HAR Dataset/Test/X_Test.txt", widths=cnames$wdth, col.names=cols$colname,
              colClasses=rep("character",66) , header = FALSE, sep = "", skip = 0, n = -1)

trn <- read.fwf(file="UCI HAR Dataset/Train/X_Train.txt", widths=cnames$wdth, col.names=cols$colname, 
                colClasses=rep("character",66), header = FALSE, sep = "", skip = 0,n = -1)

## add column for activity
tst <- cbind(tstacts,tst);
trn <- cbind(trnacts,trn);

## add column for subject
tst <- cbind(tstsubj,tst);
trn <- cbind(trnacts,trn);

## join test and train datasets
t <- rbind(tst,trn)

grouped <- group_by(t, subject, activity)
tsum <- summarise(grouped, mean=mean(value) )

## write to a file
write.table( t, "gcdata.txt", row.name=FALSE)
write.table( tsum, "gcdatasummary.txt", row.name=FALSE)

