makeTeamBatting <- function(mlb.raw, mlb.vc, team) {
  raw <- mlb.raw@batting[which(mlb.raw@batting$Team==team),]
  vc <- mlb.vc@batting[which(mlb.vc@batting$Team==team),]
  rownames(raw) <- raw$Name
  rownames(vc) <- vc$Name
  
  ## Calculate OBP, SLG, OPS
  OBP <- (raw$H+raw$BB+raw$HBP)/(raw$AB+raw$BB+raw$HBP+raw$SF)
  SLG <- (raw[,"1B"]+2*raw[,"2B"]+3*raw[,"3B"]+4*raw[,"HR"])/raw$AB
  OPS <- OBP+SLG
  raw <- cbind(raw,OBP,SLG,OPS)
  
  ctg_raw <- c("Pos","G","PA","R","H","2B","3B","HR","RBI","BB","SB","CS","AVG","OBP","SLG","OPS")
  dig_raw <- c(rep(0,13),rep(3,4))
  ind_raw <- sort(as.numeric(raw[,"PA"]),ind=T,dec=T)$ix
  aln_raw <- rep('r', length(ctg_raw))
  raw_tab <- knitr::kable(raw[ind_raw,ctg_raw], digits=dig_raw, format='html', align=aln_raw, table.attr='class="sortable ctable"')
  
  ctg_vc <- c("Pos", "VC.1B", "VC.2B", "VC.3B", "VC.HR", "VC.H", "VC.BB", "VC.SB", "VC.Off")
  dig_vc <- c(0,0,rep(1,length(ctg_vc)-1))
  ind_vc <- sort(vc[,"VC.Off"],ind=T,dec=T)$ix
  aln_vc <- rep('r', length(ctg_vc))
  vc_tab <- knitr::kable(vc[ind_vc,ctg_vc], digits=dig_vc, format='html', align=aln_vc, table.attr='class="sortable ctable"')
  
  f <- paste(mlb.par@loc, "/", mlb.par@year, "/", team, "_batting.html", sep="")
  cat('---\nyear: ', mlb.par@year, '\nrel: ../\n---\n', file=f)
  cat(raw_tab, type="html", file=f, append=TRUE)
  cat(vc_tab, type="html", file=f, append=TRUE)
}
