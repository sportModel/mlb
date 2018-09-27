makeTeamComparison <- function(mlb.team) {
  display.categories.ovr <- c("W","L","W.pyth","L.pyth","W.WC","L.WC","RS","RA")
  display.digits.ovr <- rep(0,length(display.categories.ovr)+1)
  ind.ovr <- sort(mlb.team$W,ind=T,decreasing=TRUE)$ix
  
  display.categories.off <- c("RS","OBP","SLG","OPS","VC.1B","VC.2B","VC.3B","VC.HR","VC.H","VC.BB","VC.SB","VC.Off")
  display.digits.off <- c(0,0,3,3,3,rep(1,8))
  ind.off <- sort(mlb.team$VC.Off,ind=T,decreasing=TRUE)$ix
  
  display.categories.def <- c("RA","ERA","WHIP","VC.SOA","VC.BBA","VC.HRA","VC.BIP","VC.BABIP","VC.Arm","VC.DP","VC.Rng","VC.Err","VC.SB","VC.DefPitch","VC.DefField","VC.Def")
  display.digits.def <- c(0,0,2,2,rep(1,13))
  ind.def <- sort(mlb.team$VC.Def,ind=T,decreasing=TRUE)$ix
  
  display.ovr <- xtable(mlb.team[ind.ovr,display.categories.ovr],digits=display.digits.ovr)
  align(display.ovr) <- rep("r",length(align(display.ovr)))
  align(display.ovr)[1] <- "l"
  display.off <- xtable(mlb.team[ind.off,display.categories.off],digits=display.digits.off)
  align(display.off) <- rep("r",length(align(display.off)))
  align(display.off)[1] <- "l"
  display.def <- xtable(mlb.team[ind.def,display.categories.def],digits=display.digits.def)
  align(display.def) <- rep("r",length(align(display.def)))
  align(display.def)[1] <- "l"
  
  
  f <- paste(mlb.par@loc, "/", mlb.par@year, "/team_comparison.html", sep="")
  cat('---\nyear: ', mlb.par@year, '\nrel: ../../\n---\n', file=f)
  print(display.ovr,type="html",html.table.attributes="class=\"sortable ctable\"", file=f, append=TRUE)
  print(display.off,type="html",html.table.attributes="class=\"sortable ctable\"", file=f, append=TRUE)
  print(display.def,type="html",html.table.attributes="class=\"sortable ctable\"", file=f, append=TRUE)
}
