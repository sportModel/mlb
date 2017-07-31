teamCorrelations <- function(mlb.raw,mlb.vc)
  {
    n <- length(mlb.par@team)
    val <- matrix(NA,nrow=n,ncol=6)
    colnames(val) <- c("VC.Off","VC.Def","WC","dRS","dRA","dW")
    rownames(val) <- mlb.par@team
    pitchers.AL <- mlb.raw@pitching[which(mlb.raw@pitching$Lg=="AL"),]
    pitchers.NL <- mlb.raw@pitching[which(mlb.raw@pitching$Lg=="NL"),]
    batters.AL <- mlb.raw@batting[which(mlb.raw@batting$Lg=="AL"),]
    batters.NL <- mlb.raw@batting[which(mlb.raw@batting$Lg=="NL"),]
    RAG.AL <- sum(pitchers.AL$R)/(sum(pitchers.AL$W) + sum(pitchers.AL$L))
    RAG.NL <- sum(pitchers.NL$R)/(sum(pitchers.NL$W) + sum(pitchers.NL$L))
    RSG.AL <- sum(batters.AL$R)/(sum(pitchers.AL$W) + sum(pitchers.AL$L))
    RSG.NL <- sum(batters.NL$R)/(sum(pitchers.NL$W) + sum(pitchers.NL$L))

    for (i in 1:n)
      {
        batting.vc <- mlb.vc@batting[which(mlb.vc@batting$team==mlb.par@team[i]),]
        pitching.vc <- mlb.vc@pitching[which(mlb.vc@pitching$team==mlb.par@team[i]),]
        fielding.vc <- mlb.vc@fielding[which(mlb.vc@fielding$team==mlb.par@team[i]),]
        batting.raw <- mlb.raw@batting[which(mlb.raw@batting$team==mlb.par@team[i]),]
        pitching.raw <- mlb.raw@pitching[which(mlb.raw@pitching$team==mlb.par@team[i]),]
        fielding.raw <- mlb.raw@fielding[which(mlb.raw@fielding$team==mlb.par@team[i]),]
        G <- sum(pitching.raw$W) + sum(pitching.raw$L)
        
        val[i,1] <- sum(batting.vc$VC.Off)
        val[i,2] <- sum(pitching.vc$VC.Def) + sum(fielding.vc$VC.Def)
        val[i,3] <- mlb.par@model["R"] * (val[i,1] + val[i,2])
        if (pitching.vc$Lg[1]=="AL")
          {
            val[i,4] <- sum(batting.raw$R) - G*RSG.AL
            val[i,5] <- sum(pitching.raw$R) - G*RAG.AL
          }
        if (pitching.vc$Lg[1]=="NL")
          {
            val[i,4] <- sum(batting.raw$R) - G*RSG.NL
            val[i,5] <- sum(pitching.raw$R) - G*RAG.NL
          }
        val[i,6] <- sum(pitching.raw$W) - G/2
      }
    val
  }
