plotTeamCorrelations <- function(mlb.raw,mlb.vc)
  {
    X <- teamCorrelations(mlb.raw,mlb.vc)
    X[,5] <- -X[,5]

    size <- max(c(X[,1],X[,4]))
    r2 <- sum(X[,1]*X[,4])/sum(X[,4]*X[,4])
    plot(X[,1],X[,4],ylim=c(-size,size),xlim=c(-size,size),xlab="VC.Off",ylab="dRS",main=substitute(paste("r"^2," = ",r2)))
    lines(c(-size,size),c(-size,size))
    
    size <- max(c(X[,2],X[,5]))
    r2 <- sum(X[,2]*X[,5])/sum(X[,5]*X[,5])
    plot(X[,2],X[,5],ylim=c(-size,size),xlim=c(-size,size),xlab="VC.Def",ylab="-dRA",main=substitute(paste("r"^2," = ",r2)))
    lines(c(-size,size),c(-size,size))
    
    size <- max(c(X[,3],X[,6]))
    r2 <- sum(X[,3]*X[,6])/sum(X[,6]*X[,6])
    plot(X[,3],X[,6],ylim=c(-size,size),xlim=c(-size,size),xlab="WC",ylab="dW",main=substitute(paste("r"^2," = ",r2)))
    lines(c(-size,size),c(-size,size))
  }
