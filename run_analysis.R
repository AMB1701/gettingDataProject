#load and merge the training and testing sets
xValues <- rbind(read.table("train/X_train.txt"), read.table("test/X_test.txt"))
yValues <- rbind(read.table("train/y_train.txt"), read.table("test/y_test.txt"))
subjValues <- rbind(read.table("train/subject_train.txt"), read.table("test/subject_test.txt"))
allFeatures <- read.table("features.txt")
colnames(xValues) <- tolower(gsub("\\(\\)","",allFeatures[,2]))
colnames(yValues) <- c("activity-name")
colnames(subjValues) <- c("subject")

#select only the mean and standard deviation entries (from features.txt)
meanIndices <- grep("-mean",allFeatures[,2])
stdIndices <- grep("-std",allFeatures[,2])
allIndices <- c(meanIndices,stdIndices)
xValues <- xValues[,allIndices]

#rename the activities using the descriptive labels (from activity_labels.txt)
allActivities <- read.table("activity_labels.txt")
allActivities[,2] <- tolower(allActivities[,2])
#reassign names in yValues using indicies in allActivities
yValues[,1] <- allActivities[yValues[,1],2]

#create original output set
tidySet <- cbind(subjValues,yValues,xValues)
write.table(tidySet,"tidy_set_original.txt")

#calculate averages for each variable for each activity and subject
uniqueSubjects <- unique(subjValues[,1])
finalSet <- tidySet[1:(length(uniqueSubjects)*length(allActivities[,1])),]

currentIndex <- 1

#for each subject...
for(subjectID in 1:length(uniqueSubjects))
{
  #for each activity...
  for(activityID in 1:length(allActivities[,1]))
  {
    finalSet[currentIndex,1] <- uniqueSubjects[subjectID]
    finalSet[currentIndex,2] <- allActivities[activityID,2]
    #find this subject's results for each activity
    subjResults <- tidySet[tidySet$subject == subjectID & tidySet$activity == allActivities[activityID,2],]
    finalSet[currentIndex, 3:ncol(tidySet)] <- colMeans(subjResults[,3:ncol(tidySet)])
    currentIndex <- currentIndex+1
  }
}

#create final output set with calculations
write.table(finalSet,"tidy_set_averages.txt", row.name=FALSE)