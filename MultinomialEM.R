H<-matrix(readBin("histograms.bin","double",640000),40000,16)
H<-(H+0.01)/121

MultinomialEM<-function(H,K,tau){
  centroidIndex<-sample.int(nrow(H),K)
  #each column represents one centroid
  centroids<-t(H[centroidIndex,])
  centroids<-normalize(centroids)
  c<-rep(1/K,K)
  firstIteration<-TRUE
  while(TRUE){
    a<-EStep(H,centroids,c)
    result<-MStep(H,a)
    if(firstIteration){
      firstIteration<-FALSE
      next
    }
    if(norm(a-aOld,"O")<tau){
      break;
    }
    aOld<-a
  }
  m<-apply(a,1,which.max)
  return(m)
}

EStep<-function(H,centroids,c){
  #40000*k
  phi<-exp(H%*%log(centroids))
  numerator<-t(t(phi)*c)
  denominator<-phi%*%c
  a<-numerator/as.vector(denominator)
  return(a)
}

MStep<-function(H,a){
  c<-apply(a,2,sum)/nrow(H)
  b<-t(H)%*%a
  bkjSum<-as.vector(apply(b,2,sum))
  centroids<-b/bkjSum
  return(list("weights"=c,"centroids"=centroids))
}

#normalize in columns
normalize<-function(mat){
  su<-apply(mat,2,sum)
  return(t(t(mat)/su))
}

#tau=1 looks fairly bad: the majority of
#the picture is red, little distinguishment
#tau=0.1,better
#tau=0.01, more scattered points shown
#tau=0.001, more scattered points shown (more complex boundary)
#tau=0.0001, hard to tell the difference from 0.001
#so we choose tau=0.001: smaller tau (higher "accuracy") does not make the algorithm
#run significantly slowlier, but 0.001 is sufficient 
K=5
tau=0.001
m<-MultinomialEM(H,K,tau)
png("visualizationK=5.png")
image(matrix(m,200,200)[,200:1])
dev.off()
