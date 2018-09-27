updateWebsite <- function(mlb.raw,mlb.vc,mlb.team) {
  makeTeams(mlb.team)
  makeTeamComparison(mlb.team)
  pos <- c("c","1b","2b","3b","ss","lf","cf","rf","p", "dh")
  mlb.pos <- vector("list", 2)
  names(mlb.pos) <- c("pos", "pit")
  for (p in pos) {
    if (p=="p") {
      X <- combineP(mlb.vc)
      mlb.pos$pit <- X
    } else {
      if (p=="dh") {
        X.tmp <- combineDH(mlb.raw, mlb.vc)
        X <- matrix(0, nrow(X.tmp), ncol(mlb.pos$pos)-2, dimnames=list(NULL, names(mlb.pos$pos)[-(1:2)]))
        for (j in names(X.tmp)[-(1:2)]) X[,j] <- X.tmp[,j]
        X <- cbind(Teams=X.tmp$Teams, Pos="dh", as.data.frame(X))
        rownames(X) <- rownames(X.tmp)
      } else {
        X <- combinePos(mlb.raw, mlb.vc, p)
        X <- cbind(Pos=p, X)
      }
      mlb.pos$pos <- rbind(mlb.pos$pos, X)
    }
  }  
  for (i in c(pos, "")) makePos(mlb.pos, i)
  for (i in mlb.par@team) {
    makeTeamMain(mlb.raw,mlb.vc,mlb.team,i)
    makeTeamBatting(mlb.raw,mlb.vc,i)
    makeTeamPitching(mlb.raw,mlb.vc,i)
    makeTeamFielding(mlb.raw,mlb.vc,i)
  }
}
