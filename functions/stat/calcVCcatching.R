calcVCcatching <- function(mlb.raw)
  {
    X <- mlb.raw@fielding[mlb.raw@fielding$Pos=="c",]
    outs <- ip2outs(X$Inn)
    eSB <- outs * sum(X$SB)/sum(outs)
    eCS <- outs * sum(X$CS)/sum(outs)
    dSB <- X$SB - eSB
    dCS <- X$CS - eCS
    VC.SBA <- -1 * mlb.par@model["SB"] * dSB
    VC.CSA <- -1 * mlb.par@model["CS"] * dCS
    VC.SBA <- VC.SBA+VC.CSA
    val <- cbind(X[,c("Name","Team","Pos","Inn")],
                 VC.Arm=NA,
                 VC.DP=NA,
                 VC.Rng=NA,
                 VC.Err=NA,
                 VC.SBA,
                 VC.Def=VC.SBA)
    val
  }
