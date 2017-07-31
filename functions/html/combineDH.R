combineDH <- function(mlb.raw, mlb.vc) {
  id.f.raw <- mlb.raw@fielding$Name
  id.b <- mlb.vc@batting$Name
  id.unique <- unique(id.b)
  id <- id.unique[!is.element(id.unique,id.f.raw)]
  
  n <- length(id)
  batting <- matrix(NA, nrow=n, ncol=ncol(mlb.vc@batting)-4)
  rownames(batting) <- id
  colnames(batting) <- colnames(mlb.vc@batting[-1:-4]) 
  Teams <- character(n)
  for (i in 1:n) {
    unit <- mlb.vc@batting[id.b==id[i],]
    Teams[i] <- paste(unit[,"Team"],collapse="/")
    batting[i,] <- apply(unit[,-1:-4],2,sum)
  }
  val <- batting
  VC.Tot <- val[,"VC.Off"]
  WC <- mlb.par@model["R"] * VC.Tot
  val <- data.frame(Teams,val,WC)
  val
}
