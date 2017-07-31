calcVCteamDefense <- function(mlb.raw, team, catching) {
  tp <- mlb.raw@pitching[mlb.raw@pitching$Team==team,]
  tf <- mlb.raw@fielding[mlb.raw@fielding$Team==team & !is.na(mlb.raw@fielding$UZR),]
  tc <- catching[catching$Team==team,]

  fielding <- data.frame(tf[,c("Name","Team","Pos","Inn")],VC.Arm=tf$ARM,VC.DP=tf$DPR,VC.Rng=tf$RngR,VC.Err=tf$ErrR,VC.SBA=NA,VC.Def=tf$UZR)
  fielding <- rbind(fielding, tc)
  pitching <- calcVCpitching(tp, mlb.raw, sum(fielding$VC.Def))

  val <- list(pitching=pitching,
              fielding=fielding)
  val
}
