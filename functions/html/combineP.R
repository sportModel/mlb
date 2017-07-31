combineP <- function(mlb.vc)
  {
    id.p <- mlb.vc@pitching$Name
    id <- unique(id.p)
    ind <- match(id,id.p)
    n <- length(id)
    
    pitching <- matrix(NA,nrow=n,ncol=(ncol(mlb.vc@pitching)-3))
    rownames(pitching) <- id
    colnames(pitching) <- colnames(mlb.vc@pitching[-1:-3])
    Teams <- character(n)
    for (i in 1:n)
      {
        unit <- mlb.vc@pitching[id.p==id[i],]
        Teams[i] <- paste(unit[,"Team"],collapse="/")
        pitching[i,] <- apply(unit[,-1:-3],2,sum)
      }
    WC <- mlb.par@model["R"] * pitching[,"VC.Def"]
    val <- data.frame(Teams,pitching,WC)
    val
  }
