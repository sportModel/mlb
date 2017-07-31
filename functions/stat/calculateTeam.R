calculateTeam <- function(mlb.raw,mlb.vc) {
  val <- NULL
  for (i in mlb.par@team) {
    W <- sum(mlb.raw@pitching$W[mlb.raw@pitching$Team==i])
    L <- sum(mlb.raw@pitching$L[mlb.raw@pitching$Team==i])
    RS <- sum(mlb.raw@batting$R[mlb.raw@batting$Team==i])
    RA <- sum(mlb.raw@pitching$R[mlb.raw@pitching$Team==i])
    G <- W+L
    pct.pyth <- RS^2/(RS^2+RA^2)
    W.pyth <- G*pct.pyth
    L.pyth <- G*(1-pct.pyth)
    H <- sum(mlb.raw@batting$H[mlb.raw@batting$Team==i])
    BB <- sum(mlb.raw@batting$BB[mlb.raw@batting$Team==i])
    HBP <- sum(mlb.raw@batting$HBP[mlb.raw@batting$Team==i])
    AB <- sum(mlb.raw@batting$AB[mlb.raw@batting$Team==i])
    SH <- sum(mlb.raw@batting$SH[mlb.raw@batting$Team==i])
    OBP <- (H + BB + HBP)/(AB + BB + HBP + SH)
    H1 <- sum(mlb.raw@batting[,"1B"][mlb.raw@batting$Team==i])
    H2 <- sum(mlb.raw@batting[,"2B"][mlb.raw@batting$Team==i])
    H3 <- sum(mlb.raw@batting[,"3B"][mlb.raw@batting$Team==i])
    H4 <- sum(mlb.raw@batting$HR[mlb.raw@batting$Team==i])
    SLG <- (H1 + 2*H2 + 3*H3 + 4*H4)/AB
    OPS <- OBP+SLG
    VC.1B <- sum(mlb.vc@batting$VC.1B[mlb.vc@batting$Team==i])
    VC.2B <- sum(mlb.vc@batting$VC.2B[mlb.vc@batting$Team==i])
    VC.3B <- sum(mlb.vc@batting$VC.3B[mlb.vc@batting$Team==i])
    VC.HR <- sum(mlb.vc@batting$VC.HR[mlb.vc@batting$Team==i])
    VC.H <- sum(mlb.vc@batting$VC.H[mlb.vc@batting$Team==i])
    VC.BB <- sum(mlb.vc@batting$VC.BB[mlb.vc@batting$Team==i])
    VC.SB <- sum(mlb.vc@batting$VC.SB[mlb.vc@batting$Team==i])
    VC.Off <- sum(mlb.vc@batting$VC.Off[mlb.vc@batting$Team==i])
    ER <- sum(mlb.raw@pitching$ER[mlb.raw@pitching$Team==i])
    IP <- sum(mlb.raw@pitching$IP[mlb.raw@pitching$Team==i])
    ERA <- 9*ER/IP
    BB <- sum(mlb.raw@pitching$BB[mlb.raw@pitching$Team==i])
    H <- sum(mlb.raw@pitching$H[mlb.raw@pitching$Team==i])
    WHIP <- (BB+H)/IP
    VC.SOA <- sum(mlb.vc@pitching$VC.SOA[mlb.vc@pitching$Team==i])
    VC.BBA <- sum(mlb.vc@pitching$VC.BBA[mlb.vc@pitching$Team==i])
    VC.HRA <- sum(mlb.vc@pitching$VC.HRA[mlb.vc@pitching$Team==i])
    VC.BIP <- sum(mlb.vc@pitching$VC.BIP[mlb.vc@pitching$Team==i])
    VC.BABIP <- sum(mlb.vc@pitching$VC.BABIP[mlb.vc@pitching$Team==i])
    VC.DefPitch <- sum(mlb.vc@pitching$VC.Def[mlb.vc@pitching$Team==i])
    VC.Arm <- sum(mlb.vc@fielding$VC.Arm[mlb.vc@fielding$Team==i],na.rm=TRUE)
    VC.DP <- sum(mlb.vc@fielding$VC.DP[mlb.vc@fielding$Team==i],na.rm=TRUE)
    VC.Rng <- sum(mlb.vc@fielding$VC.Rng[mlb.vc@fielding$Team==i],na.rm=TRUE)
    VC.Err <- sum(mlb.vc@fielding$VC.Err[mlb.vc@fielding$Team==i],na.rm=TRUE)
    VC.SBA <- sum(mlb.vc@fielding$VC.SBA[mlb.vc@fielding$Team==i],na.rm=TRUE)
    VC.DefField <- sum(mlb.vc@fielding$VC.Def[mlb.vc@fielding$Team==i])
    VC.Def <- VC.DefField+VC.DefPitch
    WC <- mlb.par@model["R"] * (VC.Off+VC.Def)
    W.WC <- G/2 + WC
    L.WC <- G/2 - WC
    val.i <- data.frame(W,L,W.pyth,L.pyth,W.WC,L.WC,
                        RS,OBP,SLG,OPS,VC.Off,VC.1B,VC.2B,VC.3B,VC.HR,VC.H,VC.BB,VC.SB,
                        RA,ERA,WHIP,VC.Def,VC.DefPitch,VC.SOA,VC.BBA,VC.HRA,VC.BIP,VC.BABIP,VC.DefField,VC.Arm,VC.DP,VC.Rng,VC.Err,VC.SBA)
    val <- rbind(val,val.i)
  }
  rownames(val) <- mlb.par@team
  return(val)
}
