#install.packages("e1071")
library(e1071)
library(rpart)

uspsdata = read.table("uspsdata.txt")
uspscl = read.table("uspscl.txt")
index <- nrow(uspsdata)
testIndex <- sample(index, floor(index*0.2))
testSet = uspsdata[testIndex,]
testLabel = factor(uspscl[testIndex,])
trainSet = uspsdata[-testIndex,]
trainLabel = factor(uspscl[-testIndex,])

sequence <- 10^seq(-5,5,0.1)
linearAccuracy <- c()
#linear model
for (i in sequence){
  linearModel <- svm(x=trainSet, y=trainLabel,cross=5, kernel="linear",cost=i)
  linearAccuracy <- c(linearAccuracy,linearModel$tot.accuracy)
}

linearMiss <- 100 - linearAccuracy
library(ggplot2)
plot1 <- qplot(log(c(sequence)),linearMiss,xlab="log cost",ylab="mistake rate",main="Linear Mistake")+geom_line()

#train the optimal
index <- which.max(linearAccuracy)
#may be more than one; just choose the first
optimalcostLinear <- sequence[index[1]]
optimalLinear <- svm(x=trainSet,y=trainLabel,cross=5,kernel="linear",cost=optimalcostLinear)
linearPred <- predict(optimalLinear, testSet)
linearTestMiss <- 1-length(linearPred[linearPred==testLabel])/length(testLabel)

######REPORT
######test set misclassification rate:0.025
######parameter cost:0.0005011872

#RBF model
sequence2 <- 10^seq(-5,5,0.5)
RBFAccuracy <- matrix(0,length(sequence2),length(sequence2))
for (i in 1:length(sequence2)){
  a <- sequence2[i]
  for (j in 1:length(sequence2)){
    b <- sequence2[j]
    RBFModel <- svm(x=trainSet, y=trainLabel,cross=5,kernel="radial",cost=a,gamma=b)
    RBFPred <- predict(RBFModel, testSet)
    RBFAccuracy[i,j] <- RBFModel$tot.accuracy
  }

}
RBFMiss <- 100 - RBFAccuracy
library(scatterplot3d)
x <- rep(sequence2,each=length(sequence2))
y <- rep(sequence2,length(sequence2))
z <- as.vector(t(RBFMiss))
plot2 <- scatterplot3d(log(x),log(y),z, main="RBF Mistake Rate", xlab="cost",ylab="gamma",zlab="mistake rate")

#train the RBF optimal
index2 <- which(RBFAccuracy == max(RBFAccuracy), arr.ind = TRUE)
#may be more than one combinations, just choose the first combination here
optimalcost <- sequence2[index2[1,1]]
optimalgamma <- sequence2[index2[1,2]]
optimalRBF <- svm(x=trainSet,y=trainLabel,cross=5,cost=optimalcost,gamma=optimalgamma)
RBFPred <- predict(optimalRBF, testSet)
RBFTestMiss <- 1-length(RBFPred[RBFPred==testLabel])/length(testLabel)

######REPORT
######test set misclassification rate:0.05 
######parameter cost:100 gamma:10^(-5)


#####Comparison: These two methods are comparable in terms of misclassification rate (linear even slightly better)
#####So linear SVM is more optimal in that it is faster
#####Probably because the number of features is so large (256) that it does not do
#####much help to map the data to an even higher dimension space