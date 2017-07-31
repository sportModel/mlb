combinePos <- function(mlb.raw,mlb.vc,Pos)
  {
    id.f.raw <-mlb.raw@fielding$Name
    id.f <- mlb.vc@fielding$Name
    id.b <- mlb.vc@batting$Name
    id.unique <- unique(id.f.raw)

    id <- NULL
    n <- length(id.unique)
    for (i in 1:n)
      {
        unit <- mlb.raw@fielding[id.f.raw==id.unique[i],]
        dominant.pos <- unit$Pos[which.max(unit$Inn)]
        if (dominant.pos==Pos) id <- c(id,id.unique[i])
      }

    n <- length(id)
    batting <- matrix(NA,nrow=n,ncol=ncol(mlb.vc@batting)-4)
    fielding <- matrix(NA,nrow=n,ncol=ncol(mlb.vc@fielding)-4)
    rownames(batting) <- id
    rownames(fielding) <- id
    colnames(batting) <- colnames(mlb.vc@batting[-1:-4])
    colnames(fielding) <- colnames(mlb.vc@fielding[-1:-4])
    Teams <- character(n)
    for (i in 1:n)
      {
        unit <- mlb.vc@batting[id.b==id[i],]
        Teams[i] <- paste(unit[,"Team"],collapse="/")
        batting[i,] <- apply(unit[,-1:-4],2,sum)
      }
    for (i in 1:n)
      {
        unit <- mlb.vc@fielding[id.f==id[i],]
        fielding[i,] <- apply(unit[,-1:-4],2,sum,na.rm=TRUE)
      }
    val <- cbind(batting,fielding)
    VC.Tot <- val[,"VC.Off"] + val[,"VC.Def"]
    WC <- mlb.par@model["R"] * VC.Tot
    val <- data.frame(Teams,val,WC)
    val <- val[val$Teams!="",]
    val
  }
