makeTeamMain <- function(mlb.raw, mlb.vc, mlb.team, team) {
  i <- row.names(mlb.team)==team
  tv <- mlb.team
  tv$WC.Off <- tv$VC.Off * mlb.par@model["R"]
  tv$WC.Def <- tv$VC.Def * mlb.par@model["R"]
  tv$WC.DefPitch <- tv$VC.DefPitch * mlb.par@model["R"]
  tv$WC.DefField <- tv$VC.DefField * mlb.par@model["R"]
  tr <- apply(-tv,2,rank)
  tr[,c("RA","ERA","WHIP")] <- apply(tv[,c("RA","ERA","WHIP")],2,rank)
  tr <- round(tr)
  tv[,c("W.pyth","W.WC")] <- round(tv[,c("W.pyth","W.WC")])
  tv[,c("ERA","WHIP")] <- round(tv[,c("ERA","WHIP")],digits=2)
  tv[,c("OBP","SLG","OPS")] <- round(tv[,c("OBP","SLG","OPS")],digits=3)
  tv[,grep("VC",colnames(tv))] <- round(tv[,grep("VC",colnames(tv))],digits=1)
  tv[,grep("WC",colnames(tv))] <- round(tv[,grep("WC",colnames(tv))],digits=1)
  
  cat1 <- c("W","W.pyth","W.WC","RS","RA","WC.Off","WC.Def","VC.Off","VC.Def")
  X1 <- cbind(as.character(tv[i,cat1]), as.character(tr[i,cat1]))
  rownames(X1) <- cat1
  
  cat2 <- c("OBP","SLG","OPS","VC.1B","VC.2B","VC.3B","VC.HR","VC.H","VC.BB","VC.SB")
  X2 <- cbind(as.numeric(tv[i,cat2]), as.character(tr[i,cat2]))
  rownames(X2) <- cat2
  
  cat3 <- c("ERA","WHIP","VC.SOA","VC.BBA","VC.HRA","VC.BIP","VC.BABIP","VC.DefPitch","WC.DefPitch")
  X3 <- cbind(as.numeric(tv[i,cat3]), as.character(tr[i,cat3]))
  rownames(X3) <- cat3
  
  cat4 <- c("VC.Arm","VC.DP","VC.Rng","VC.Err","VC.SBA","VC.DefField","WC.DefField")
  X4 <- cbind(as.numeric(tv[i,cat4]), as.character(tr[i,cat4]))
  rownames(X4) <- cat4
  
  colnames(X1) <- colnames(X2) <- colnames(X3) <- colnames(X4) <- c("Value","Rank")
  aln <- c("r","r")
  display1 <- knitr::kable(X1, format='html', align=aln, table.attr='class="sortable ctable"')
  display2 <- knitr::kable(X2, format='html', align=aln, table.attr='class="sortable ctable"')
  display3 <- knitr::kable(X3, format='html', align=aln, table.attr='class="sortable ctable"')
  display4 <- knitr::kable(X4, format='html', align=aln, table.attr='class="sortable ctable"')
  
  Y <- combineTeam(mlb.vc,team)
  dig <- c(0, rep(1,3))
  ind <- sort(Y[,"WC"],ind=T,dec=T)$ix
  aln <- rep('r', ncol(Y))
  display5 <- knitr::kable(Y[ind,], digits=dig, format='html', align=aln, table.attr='class="sortable ctable"')

  f <- paste(mlb.par@loc, "/", mlb.par@year, "/", team, ".html", sep="")
  cat(paste0("---\nyear: ", mlb.par@year, "\nrel: ../\n---\n"), file=f)
  cat("<a href=", team,"_batting.html> Batting </a> <br><br>\n",sep="", file=f, append=TRUE)
  cat("<a href=", team,"_pitching.html> Pitching </a> <br><br>\n",sep="", file=f, append=TRUE)
  cat("<a href=", team,"_fielding.html> Fielding </a> <br><br>\n",sep="", file=f, append=TRUE)
  cat("<TABLE class=\"container\">\n<TR><TD>", file=f, append=TRUE)
  cat(display1, file=f, append=TRUE)
  cat("</TD>\n<TD>", file=f, append=TRUE)
  cat(display2, file=f, append=TRUE)
  cat("</TD>\n<TD>", file=f, append=TRUE)
  cat(display3, file=f, append=TRUE)
  cat("</TD>\n<TD>", file=f, append=TRUE)
  cat(display4, file=f, append=TRUE)
  cat("</TD></TR></TABLE>\n", file=f, append=TRUE)
  cat(display5, file=f, append=TRUE)
}
