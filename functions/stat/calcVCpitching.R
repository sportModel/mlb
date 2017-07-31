calcVCpitching <- function(X, mlb.raw, team.UZR) {
    ## League
    BIPH <- mlb.raw@pitching$H - mlb.raw@pitching$HR
    BIPO <- ip2outs(mlb.raw@pitching$IP) - mlb.raw@pitching$SO
    BIP <- BIPH + BIPO
    BABIP.league <- sum(BIPH)/sum(BIP)
    BIPR.league <- sum(BIP)/sum(mlb.raw@pitching$TBF)

    ## Team
    BIPH <- X$H - X$HR
    BIPO <- ip2outs(X$IP) - X$SO
    BIP <- BIPH + BIPO
    #BABIP <- (BIPH + mlb.par@prior.weight*BABIP.league) / (BIP + mlb.par@prior.weight)
    BABIP <- BIPH/BIP
    BIPR <- BIP/X$TBF

    eSOA <- X$TBF * sum(mlb.raw@pitching$SO)/sum(mlb.raw@pitching$TBF)
    eBBA <- X$TBF * sum(mlb.raw@pitching$BB)/sum(mlb.raw@pitching$TBF)
    eHRA <- X$TBF * sum(mlb.raw@pitching$HR)/sum(mlb.raw@pitching$TBF)
    dSOA <- X$SO - eSOA
    dBBA <- X$BB - eBBA
    dHRA <- X$HR - eHRA
    dBIPR <- X$TBF*(BIPR - BIPR.league)*BABIP.league
    dBABIP <- BIP*(BABIP - BABIP.league)
    ##dBIPH <- BIP*BABIP - X$TBF*BIPR.league*BABIP.league

    VC.SOA <- -1 * mlb.par@model["SOA"] * dSOA
    VC.BBA <- -1 * mlb.par@model["BBA"] * dBBA
    VC.HRA <- -1 * mlb.par@model["HRA"] * dHRA
    VC.BIP <- -1 * mlb.par@model["BIPH"] * dBIPR
    VC.BABIP.unadj <- -1 * mlb.par@model["BIPH"] * dBABIP

    W <- BIP/sum(BIP)
    VC.BABIP <- VC.BABIP.unadj - W*team.UZR
    VC.BABIP[BIP==0] <- 0

    val <- cbind(X[,1:3],
                 VC.SOA,
                 VC.BBA,
                 VC.HRA,
                 VC.BIP,
                 VC.BABIP)
    VC.Def <- apply(val[,-1:-3],1,sum)
    val <- cbind(val,VC.Def)
    val
  }
