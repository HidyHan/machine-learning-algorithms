#install.packages("ggplot2")
#install.packages("reshape")
#S is a n by d+1 matrix
#z=(v1,...vd,-c) column vector
#return a n*1 column vector of class labels

classify <- function(S,z){
    sign <- S%*%z
    sign[sign>=0] <- 1
    sign[sign<0] <- -1
    return(sign)
}

#help function
#make the first part of z, i.e. vh, a unit vector
#leave offset c intact
makeUnit <- function(z){
  d <- length(z)
  vh <- z[1:d-1]
  vh <- vh/(sum(vh^2))^0.5
  z <- c(vh,z[d])
  return(z)
}

#S is a n by d+1 matrix
#y are true labels, n*1 column vector
perceptrain <- function(S, y){
    Z_history <- matrix(,nrow=0,ncol=ncol(S))
    i <- 0
    #randomly initialize z as a column vector
    z <- rep(1,times=ncol(S))
    z <- makeUnit(z)
    #infinite loop until 0 error
    while(TRUE){
      i <- i+1
      classMistake <- classify(S,z)-y
      classMistake[classMistake!=0] <- 1
      if (sum(classMistake)==0){
        break
      }
      adjust <- S*c(classMistake)*(-y)
      adjust <- apply(adjust,2,sum)
      #adjust is d+1 * 1
      z <- z-(1/i)*adjust
      z <- makeUnit(z)
      Z_history <- rbind(Z_history,t(z))

    }
    return(list(z=z,Z_history=Z_history))
  
}

#test
z <- makeUnit(runif(3,-1,1))
print(z)
list <- fakedata(z,100)
S=list[["S"]]
y=list[["y"]]
list2 <- perceptrain(S,y)

testList <- fakedata(z,100)
check <- classify(testList[["S"]],list2[["z"]])
print(check==testList[["y"]])
# print all "TRUE" (or mostly TRUE's, since this is
# a new dataset), correct implementation!

#convert data and vectors into their 2D representation
d <- length(z)
z2D <- z[1:d-1]
S2D <- S[,1:d-1]
Z_history <- list2[["Z_history"]]
test2D <-testList[["S"]][,1:d-1]
testResult <- testList[["y"]]
slope <- (-1)/(z2D[2]/z2D[1])


library("ggplot2")
library("reshape2")
g <- ggplot(melt(test2D),aes(test2D[,1],test2D[,2],color=factor(testResult)))+geom_point()
g <- g + geom_abline(intercept=z[d],slope=slope)
#test data set and classifier
g

trajectorySlope <- (-1)/(Z_history[,2]/Z_history[,1])
trajectoryOffset <- Z_history[,3]
trajectory <- melt(cbind(trajectorySlope,trajectoryOffset))
g2 <- ggplot(melt(S2D),aes(S2D[,1],S2D[,2],color=factor(y)))+geom_point()
g2 <- g2 + geom_abline(aes(intercept=trajectoryOffset,slope=trajectorySlope),data=trajectory)
