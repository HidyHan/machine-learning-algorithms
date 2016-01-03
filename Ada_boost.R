uspsdata=read.table("uspsdata.txt")
uspscl=read.table("uspscl.txt")

rows <- nrow(uspsdata)
allIndex <- c(1:rows)
testIndex <- sample(rows, floor(rows*0.2))
trainIndex <- allIndex[-testIndex]
testSet <- uspsdata[testIndex,]
testLabel <- uspscl[testIndex,]
trainSet <- uspsdata[-testIndex,]
trainLabel <- uspscl[-testIndex,]


#X's columns are observations
#w weight vector
#y class labels (column vector)
#output a list of triplet(j,theta,m)
train <- function(X,w,y){
  dimensions<-nrow(X)
  error<-Inf
  m<-NA
  theta<-NA
  mChoice<-c(1,-1)
  for (i in 1:dimensions){
    v<-rbind((X[i,]),y)
    for (j in 1:ncol(v)){
      presentTheta <- v[1,j]
      greater <- v[1,]>=presentTheta
      smaller <- v[1,]<presentTheta
      pred <- rep(0,ncol(v))
      
      for (k in mChoice){
        pred[greater] <- k
        pred[smaller] <- -k
        indicator <- rep(1,ncol(v))
        indicator[pred==v[2,]] <- 0
        presentError <- sum(w*indicator)/sum(w)
        
        if (presentError<error){
          error <- presentError
          m <- k
          theta <- presentTheta
          dimension <- i
        }
      }
    }
  }
  return(list(axis=dimension,theta=theta,m=m))
}

#X's columns are observations
#pars=list(axis,theta,m)
#y is a row vector
classify <- function(X,pars){
  axis <- pars[["axis"]]
  theta <- pars[["theta"]]
  m <- pars[["m"]]
  v <- X[axis,]
  y <- rep(0,length(v))
  y[v>=theta] <- m
  y[v<theta] <- -m
  return(y)
}

#X's columns are observations
#alpha is a column vector of voting weights
#allPars is an iteration*#parameters matrix
#result is a column vector
agg_class <- function(X, alpha, allPars){
  ys <- matrix(,ncol(X),length(alpha))
  ##annoyingly, R treats vectors and matrices differently
  if (length(alpha)==1){
    ys <- t(classify(X, list(axis=allPars[1],theta=allPars[2],m=allPars[3])))
    result <- ys*alpha
  }else{
    for (i in 1:length(alpha)){
      ys[,i] <- t(classify(X, list(axis=allPars[i,1],theta=allPars[i,2],m=allPars[i,3])))
    }
    result <- ys %*% alpha
  }
  result[result>=0] <- 1
  result[result<0] <- -1
  return(result)
}

#ada_boost algorithm
#train n_iter times, each time with updated weight
ada_boost <- function(X,y,n_iter){
  w <- rep(1/length(y),length(y))
  alphas <- rep(0,n_iter)
  allPars <- matrix(,n_iter,3)
  for (i in 1:n_iter){
    pars <- train(X,w,y)
    label <- classify(X,pars)
    indicator <- rep(1,length(y))
    indicator[label==y] <- 0
    error <- sum(indicator*w)/sum(w)
    alpha <- log((1-error)/error)
    w <- w*exp(alpha*indicator)
    alphas[i] <- alpha
    allPars[i,] <- unlist(pars)[1:3]
  }
  return (list(allPars=allPars, alphas=alphas))
}


#k-fold implementation source: 
#http://stats.stackexchange.com/questions/61090/how-to-split-a-data-set-to-do-10-fold-cross-validation
require(caret)
flds <- createFolds(trainIndex, k = 5, list = TRUE, returnTrain = FALSE)

iteration <- 30
nparameters <- 3
errors <- matrix(0,iteration,2)


for (fold in 1:5){
  
  cvTrainIndex <- unlist(flds[c(-fold)])
  cvTestIndex <- unlist(flds[[fold]])
  cvTrainSet <- trainSet[cvTrainIndex,]
  cvTrainLabel <- trainLabel[cvTrainIndex]
  cvTestSet <- trainSet[cvTestIndex,]
  cvTestLabel <- trainLabel[cvTestIndex]
  
  result <- ada_boost(t(cvTrainSet),cvTrainLabel,iteration)
  alphas <- result[["alphas"]]
  allPars <- result[["allPars"]]
  
  for (b in 1:iteration){
    
    c_hat_train <- agg_class(t(cvTrainSet),alphas[1:b],allPars[1:b,])
    c_hat_test <- agg_class(t(cvTestSet),alphas[1:b],allPars[1:b,])
    
    errors[b,1] <- errors[b,1] + sum(c_hat_train!=cvTrainLabel)
    errors[b,2] <- errors[b,2] + sum(c_hat_test!=cvTestLabel)
  }
}

#average
errors <- errors/5

# cv average errors training/test
# [,1] [,2]
# [1,] 14.4  6.2
# [2,] 17.2  7.4
# [3,]  5.0  4.4
# [4,]  6.0  4.2
# [5,]  2.0  4.2
# [6,]  3.2  4.0
# [7,]  1.0  4.4
# [8,]  1.6  3.2
# [9,]  0.2  2.8
# [10,]  0.4  2.8
# [11,]  0.0  2.8
# [12,]  0.0  3.0
# [13,]  0.0  3.0
# [14,]  0.0  2.6
# [15,]  0.0  3.6
# [16,]  0.0  3.0
# [17,]  0.0  2.6
# [18,]  0.0  2.8
# [19,]  0.0  2.2
# [20,]  0.0  2.4
# [21,]  0.0  3.0
# [22,]  0.0  2.4
# [23,]  0.0  2.4
# [24,]  0.0  2.4
# [25,]  0.0  2.6
# [26,]  0.0  2.0
# [27,]  0.0  2.2
# [28,]  0.0  1.8
# [29,]  0.0  2.4
# [30,]  0.0  2.0


cvTrainingGraph<-qplot(1:iteration,errors[,1],xlab="iteration",ylab="average number of errors (out of
                       128)", main="cv Training Error",geom=c("point","line"))
cvTestGraph<-qplot(1:iteration,errors[,2],xlab="iteration",ylab="average number of errors (out of
                   32)", main="cv Test Error",geom=c("point","line"))


# for cv, 28 iterations gives the smallest test error
# however, it is not significantly smaller than other test errors
# and may be susceptible to randomness
# we need further evidence to choose a model
# below are the plots of overall training and test errors against b without cv

overallResult <- ada_boost(t(trainSet),trainLabel,iteration)
overallAlphas <- overallResult[["alphas"]]
overallAllPars <- overallResult[["allPars"]]
overallErrors <- matrix(,iteration,2)
for (b in 1:iteration){
  trainPredict <- agg_class(t(trainSet),overallAlphas[1:b],overallAllPars[1:b,])
  testPredict <- agg_class(t(testSet),overallAlphas[1:b],overallAllPars[1:b,])
  overallErrors[b,1] <- sum(trainPredict!=trainLabel)
  overallErrors[b,2] <- sum(testPredict!=testLabel)
}


# overallErrors
# [,1] [,2]
# [1,]   19    5
# [2,]   19    5
# [3,]   10    4
# [4,]    8    3
# [5,]    6    2
# [6,]    6    2
# [7,]    3    3
# [8,]    5    3
# [9,]    1    4
# [10,]    1    3
# [11,]    1    5
# [12,]    0    5
# [13,]    0    5
# [14,]    0    4
# [15,]    0    5
# [16,]    0    4
# [17,]    0    3
# [18,]    0    4
# [19,]    0    4
# [20,]    0    4
# [21,]    0    4
# [22,]    0    4
# [23,]    0    4
# [24,]    0    4
# [25,]    0    4
# [26,]    0    4
# [27,]    0    4
# [28,]    0    5
# [29,]    0    5
# [30,]    0    5


library("ggplot2")
trainingGraph <- qplot(1:iteration,overallErrors[,1],xlab="iteration",ylab="number of 
training error (out of 160)",main="training error against iteration",geom=c("line","point"))
testGraph <- qplot(1:iteration,overallErrors[,2],xlab="iteration",ylab="number of test error
(out of 40)",main="test error against iteration",geom=c("line","point"))