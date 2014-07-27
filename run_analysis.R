RunAnalysis <- function() {
  print("Open Files... ")
  print("Opening subject_test.txt")
  stest = read.table("UCI_HAR_Dataset/test/subject_test.txt", header=FALSE)
  print("Opening X_test.txt")
  xtest = read.table("UCI_HAR_Dataset/test/X_test.txt", header=FALSE)
  print("Opening y_text.txt")
  ytest = read.table("UCI_HAR_Dataset/test/y_test.txt", header=FALSE)
  print("Opening subject_train.txt")
  strain = read.table("UCI_HAR_Dataset/train/subject_train.txt", header=FALSE)
  print("Opening X_train.txt")
  xtrain = read.table("UCI_HAR_Dataset/train/X_train.txt", header=FALSE)
  print("Opening y_train.txt")
  ytrain = read.table("UCI_HAR_Dataset/train/y_train.txt", header=FALSE)
  print("Combine all three test data sets into a single data frame")
  testdf = data.frame(xtest, ytest, stest)
  print("Combine all three train data sets into a single data frame")
  traindf = data.frame(xtrain, ytrain, strain)
  print("Merge the combined training and the test data sets to create a joined data set")
  alldf = rbind(testdf, traindf)
  print("Open features file to get column names")
  features = read.table("UCI_HAR_Dataset/features.txt", header=FALSE)
  print("Rename column headers to names provided in features.txt")
  colnames(alldf) = features[,2]
  print("Rename last two columns to activity and subject")
  ncolumns = ncol(alldf)
  colnames(alldf)[c((ncolumns-1):ncolumns)] = c("activity","subject")
  meanstd = grep(pattern = "std|mean", x = names(alldf))
  ncolumns = ncol(alldf)
  meanstd = append(meanstd,c((ncolumns-1):ncolumns))
  print("Create df with only mean, std, subject, and activity columns")
  meanstddf = alldf[,meanstd]
  activities = read.table("UCI_HAR_Dataset/activity_labels.txt", header=FALSE, stringsAsFactors=FALSE)
  print("Change numbers in activity column to their respective activity name")
  ncolumns = ncol(meanstddf)
  for (i in activities[,1]) {
    meanstddf[meanstddf$activity==i, (ncolumns - 1)] = activities[i,2]
  }
  colnames(meanstddf) = gsub("[^[:alnum:]]", "", colnames(meanstddf))
  colnames(meanstddf) = gsub("mean", "Mean", colnames(meanstddf))
  colnames(meanstddf) = gsub("std", "Std", colnames(meanstddf))
  print("Building tidy data set with avg of each var for each activity of each subject...")
  col_averages = as.data.frame(matrix(, nrow=0,ncol=81))
  subjects = sort(unique(meanstddf$subject))
  ncolumns = ncol(meanstddf)
  cur_row = 0
  for(s in subjects) {
    acts = unique(meanstddf[meanstddf$subject==s, ]$activity)
    for(a in acts) {
      cur_row = cur_row + 1
      subact = meanstddf[ (meanstddf$subject == 1) & (meanstddf$activity == a), ]
      rownames(subact) = NULL
      means = unname(colMeans(subact[,c(1:(ncolumns-2))]))
      means = append(means, c(1,1))
      col_averages = rbind(col_averages, means)
      col_averages[cur_row,80] = a
      col_averages[cur_row,81] = s
    }
  }
  print("Renaming Column Headers")
  colnames(col_averages) = colnames(meanstddf)
  print("Saving to file")
  write.table(col_averages, file="subject_activity_variable_means.txt",row.names=FALSE)
}