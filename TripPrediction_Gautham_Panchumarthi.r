

# Predicting the "trip_label" using Random Forest model.

library(randomForest)

aa_train_subset = read.csv("trip_label_training_subset.csv")
aa_test_subset = read.csv("trip_label_score_subset.csv")

#Tuning parameters 

# 1. For finiding 'mtry'
train = sample(1:nrow(aa_train_subset),nrow(aa_train_subset)/10)
oob.error = double(19)
test.error=double(19)
for(mtry in 1:19){
    aa.rtree = randomForest(trip_label~.-trip_id,data=aa_train_subset,subset=train,mtry=mtry)
    oob.error[mtry] = aa.rtree$mse[400]
    pred= predict(aa.rtree,aa_train_subset[-train,])
    test.error[mtry] = with(aa_train_subset[-train,],mean((trip_label-pred)^2))
    cat(mtry,"")
}
matplot(1:mtry,cbind(test.error,oob.error),pch=19,col=c("red","blue"),type='b',ylab="MSE")
legend("topright",legend=c("OOB","Test"),pch=19,col=c("red","blue"))


# 2. For finiding 'ntree'
Best_Tree_Counts = randomForest(trip_label~.-trip_id,data=aa_train_subset,subset=train,mtry=4,ntree=1000)
plot(Best_Tree_Counts)

# Training the model
rtree = randomForest(as.factor(trip_label)~.-trip_id,data=aa_train_subset,mtry=4,ntree=200)
summary(rtree)
rtree # For missclassification error rate.


#Finding the important variables influencing the target variable
importance(rtree)
varImpPlot(rtree) 

#Predicting the final target labels

rtree.pred = predict(rtree,newdata=aa_test_subset)
responses = as.numeric(levels(rtree.pred)[rtree.pred])
final.scores = data.frame(cbind(aa_test_subset$trip_id,responses))


submission_file = write.csv(final.scores, file= "score")










