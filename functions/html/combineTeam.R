combineTeam <- function(mlb.vc,team) {
  batting <- mlb.vc@batting[which(mlb.vc@batting$Team==team),]
  pitching <- mlb.vc@pitching[which(mlb.vc@pitching$Team==team),]
  fielding <- mlb.vc@fielding[which(mlb.vc@fielding$Team==team),]
  
  id.b <- batting$Name
  id.p <- pitching$Name
  id.f <- fielding$Name
  id <- unique(c(id.b,id.p,id.f))
  n <- length(id)
  
  vc <- matrix(NA,nrow=n,ncol=3)
  rownames(vc) <- id
  colnames(vc) <- c("VC.Off","VC.Def","WC")
  Pos <- character(n)
  for (i in 1:n) {
    unit1 <- batting[id.b==id[i],"VC.Off"]
    unit2 <- pitching[id.p==id[i],"VC.Def"]
    unit3 <- fielding[id.f==id[i],"VC.Def"]
    if (length(unit1)==0) Pos[i] <- "p"
    else Pos[i] <- batting[id.b==id[i],"Pos"]
    vc[i,1] <- sum(unit1)
    vc[i,2] <- sum(unit2) + sum(unit3)
  }
  vc[,3] <- mlb.par@model["R"] * (vc[,1]+vc[,2])
  val <- data.frame(Pos,vc)
  val
}
