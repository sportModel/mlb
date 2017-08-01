makeTeamBatting <- function(mlb.raw,mlb.vc,team) {
  raw <- mlb.raw@batting[which(mlb.raw@batting$Team==team),]
  vc <- mlb.vc@batting[which(mlb.vc@batting$Team==team),]
  rownames(raw) <- raw$Name
  rownames(vc) <- vc$Name
  
  ## Calculate OBP, SLG, OPS
  OBP <- (raw$H+raw$BB+raw$HBP)/(raw$AB+raw$BB+raw$HBP+raw$SF)
  SLG <- (raw[,"1B"]+2*raw[,"2B"]+3*raw[,"3B"]+4*raw[,"HR"])/raw$AB
  OPS <- OBP+SLG
  raw <- cbind(raw,OBP,SLG,OPS)
  
  display.categories.raw <- c("Pos","G","PA","R","H","2B","3B","HR","RBI","BB","SB","CS","AVG","OBP","SLG","OPS")
  display.digits.raw <- c(rep(0,13),rep(3,4))
  display.categories.vc <- c("Pos","VC.1B","VC.2B","VC.3B","VC.HR","VC.H","VC.BB","VC.SB","VC.Off")
  display.digits.vc <- c(0,0,rep(1,length(display.categories.vc)-1))
  
  ind.raw <- sort(as.numeric(raw[,"PA"]),ind=T,dec=T)$ix
  ind.vc <- sort(vc[,"VC.Off"],ind=T,dec=T)$ix
  
  display.raw <- xtable(raw[ind.raw,display.categories.raw],digits=display.digits.raw)
  align(display.raw) <- rep("r",length(align(display.raw)))
  align(display.raw)[1] <- "l"
  display.vc <- xtable(vc[ind.vc,display.categories.vc],digits=display.digits.vc)
  align(display.vc) <- rep("r",length(align(display.vc)))
  align(display.vc)[1] <- "l"
  
  
  filename <- paste(mlb.par@loc,"/",mlb.par@year,"_",team,"_batting",".html",sep="")
  sink(filename)
  cat('---\n---\n')
  print(display.raw,type="html",html.table.attributes="class=\"sortable ctable\"")
  print(display.vc,type="html",html.table.attributes="class=\"sortable ctable\"")
  sink()
  cleanTable(filename)
}
