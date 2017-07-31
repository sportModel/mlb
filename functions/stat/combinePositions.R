combinePositions <- function(X)
  {
    identifier <- apply(X[,c("Name","Team")],1,paste,collapse="")
    unique.identifier <- unique(identifier)
    n <- length(unique.identifier)
    val.id <- matrix(NA,nrow=n,ncol=4)
    val.vc <- matrix(NA,nrow=n,ncol=ncol(X)-4)
    for (i in 1:n)
      {
        unit <- X[identifier==unique.identifier[i],]
        val.id[i,1] <- unit[1,1]
        val.id[i,2] <- unit[1,2]
        val.id[i,3] <- unit[1,3]
        val.id[i,4] <- unit[1,4]
        val.vc[i,] <- apply(unit[,-1:-4],2,sum)
      }
    val <- data.frame(val.id,val.vc,stringsAsFactors=F)
    names(val) <- names(X)
    val
  }
