makeTeamPitching <- function(mlb.raw, mlb.vc, team) {
  raw <- mlb.raw@pitching[which(mlb.raw@pitching$Team==team),]
  rownames(raw) <- raw$Name
  vc <- mlb.vc@pitching[which(mlb.vc@pitching$Team==team),]
  rownames(vc) <- raw$Name
  
  display.categories.raw <- c("G","GS","W","L","SV","IP","H","HR","BB","SO","WHIP","ERA")
  display.digits.raw <- c(rep(0,6),1,rep(0,4),rep(2,2))
  display.categories.vc <- c("VC.SOA","VC.BBA","VC.HRA","VC.BIP","VC.BABIP","VC.Def")
  display.digits.vc <- c(0,rep(1,length(display.categories.vc)))
  
  ind.raw <- sort(raw[,"IP"], ind=T, dec=T)$ix
  ind.vc <- sort(vc[,"VC.Def"], ind=T, dec=T)$ix
  
  display.raw <- xtable(raw[ind.raw,display.categories.raw],digits=display.digits.raw)
  align(display.raw) <- rep("r",length(align(display.raw)))
  align(display.raw)[1] <- "l"
  display.vc <- xtable(vc[ind.vc,display.categories.vc],digits=display.digits.vc)
  align(display.vc) <- rep("r",length(align(display.vc)))
  align(display.vc)[1] <- "l"
  
  
  filename <- paste(mlb.par@loc,"/",mlb.par@year,"_",team,"_pitching",".html",sep="")
  sink(filename)
  print(display.raw,type="html",html.table.attributes="class=\"sortable ctable\"")
  print(display.vc,type="html",html.table.attributes="class=\"sortable ctable\"")
  sink()
  cleanTable(filename)
}
