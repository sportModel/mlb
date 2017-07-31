calcVCfielding <- function(X,mlb.raw,team.BOZ,eZR,OOZR) {
  X <- X[!is.na(X$BIZ) & !is.na(X$RZR) & !is.na(X$OOZ),]
  W <- 7*X$IP/sum(X$IP)
  adj <- tapply(W,X$pos,sum)
  W <- W/adj[match(X$pos,names(adj))]
  dIZ <- X$BIZ * (eZR[X$pos] - X$RZR)
  dOZ <- team.BOZ*OOZR[X$pos]*W - X$OOZ
  team.dBIPH <<- sum(dIZ + dOZ)
  VC.IZ <- -1 * mlb.par@model["BIPH"] * dIZ
  VC.OZ <- -1 * mlb.par@model["BIPH"] * dOZ
  VC.SBA <- 0
  VC.CSA <- 0
  val <- cbind(X[,1:6],
               VC.IZ,
               VC.OZ,
               VC.SBA,
               VC.CSA)
  VC.Def <- apply(val[,-1:-6],1,sum)
  val <- cbind(val,VC.Def)
  val
}
