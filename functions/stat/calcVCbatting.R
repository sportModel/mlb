calcVCbatting <- function(X) {
  X <- X[X$PA!=0,]
  e1B <- X$PA * sum(X[,"1B"])/sum(X$PA)
  e2B <- X$PA * sum(X[,"2B"])/sum(X$PA)
  e3B <- X$PA * sum(X[,"3B"])/sum(X$PA)
  eHR <- X$PA * sum(X$HR)/sum(X$PA)
  eBB <- X$PA * sum(X$BB)/sum(X$PA)
  eSB <- X$PA * sum(X$SB)/sum(X$PA)
  eCS <- X$PA * sum(X$CS)/sum(X$PA)
  eWPA <- X$PA * sum(X$WPA)/sum(X$PA)
  d1B <- X[,"1B"] - e1B
  d2B <- X[,"2B"] - e2B
  d3B <- X[,"3B"] - e3B
  dHR <- X$HR - eHR
  dBB <- X$BB - eBB
  dSB <- X$SB - eSB
  dCS <- X$CS - eCS
  ##dWPA <- X$WPA-eWPA
  VC.1B <- mlb.par@model["1B"] * d1B
  VC.2B <- mlb.par@model["2B"] * d2B
  VC.3B <- mlb.par@model["3B"] * d3B
  VC.HR <- mlb.par@model["HR"] * dHR
  VC.H <- VC.1B+VC.2B+VC.3B+VC.HR
  VC.BB <- mlb.par@model["BB"] * dBB
  VC.SB <- mlb.par@model["SB"] * dSB
  VC.CS <- mlb.par@model["CS"] * dCS
  VC.SB <- VC.SB+VC.CS
  VC.Off <- VC.H+VC.BB+VC.SB
  val <- cbind(X[,1:4],
               VC.1B,
               VC.2B,
               VC.3B,
               VC.HR,
               VC.H,
               VC.BB,
               VC.SB,
               VC.Off)
  val
}
