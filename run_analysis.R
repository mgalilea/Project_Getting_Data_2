analysis <- function(d_folder){
        
        # d_folder contains the folder in which the data 
        # is stored. Fotmat: folder/subfolder. This folder
        # should be in the working directory
        
        d_folder <- paste0(getwd(),"/",d_folder)
        # Take the data from the train folder
        dt <-tdata(d_folder,"X")
        
        # Change the names of the columns in the train measures
        f1 <- paste0(d_folder,"/features.txt")
        dt2 <- fread(f1)
        names(dt) <- dt2$V2
        
        # Take the values of the activity
        dt2 <-tdata(d_folder,"y")
        names(dt2) <- "activity"
        
        # Add the activity column to the main dataset
        dt <- cbind(dt2,dt)
        
        # Take the values from the subject
        dt2 <-tdata(d_folder,"subject")
        names(dt2) <- "subject"
        
        # Add the subject column to the main dataset.
        # STEP1. dt is the data set with the mergwd data from training and test
        dt <- cbind(dt2,dt)
        
        # STEP 2. Extract only the measurements on the mean and std
        subdatafeature <- grep("mean\\(\\)|std\\(\\)",names(dt))
        dt <- select(dt,c(1,2,subdatafeature))
        
        # STEP 3. Uses descriptive activity names to name the activities in the data set
        dt2 <- fread(paste0(d_folder,"/activity_labels.txt"))
        names(dt2) <- c("activity","activity_name")
        dt <- join(dt,dt2, by="activity")
        dt$activity <- dt$activity_name
        dt <- select(dt,1:(length(dt)-1))
        
        # STEP 4. Setting descriptive values for variables names
        names(dt) <- gsub("^t", "time",names(dt))
        names(dt) <- gsub("^f", "frequency",names(dt))
        names(dt) <- gsub("Acc", "Accelerometer",names(dt))
        names(dt) <- gsub("Gyro", "Gyroscope",names(dt))
        names(dt) <- gsub("Mag", "Magnitude",names(dt))
        names(dt) <- gsub("BodyBody", "Body",names(dt))
        
        # STEP 5. Create another tidy data set with the average of each variable 
        # for each activity and each subject
        
        dt$activity <- as.character(dt$activity)
        dt2 <- aggregate(. ~ subject + activity,data=dt,mean)
        
        # Writting the output file tidydata.txt
        write.table(dt2, file = "tidydata.txt",row.name=FALSE)
        
}

tdata <- function(d_folder,f) {
        # Combine the values from the train and test folder
        f1 <-paste0(d_folder,paste0("/train/",f,"_train.txt"))
        f2 <-paste0(d_folder,paste0("/test/",f,"_test.txt"))
        tdata <-rbind(fread(f1),fread(f2))
} 