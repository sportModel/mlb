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
  X1 <- cbind(cat1,
              as.character(tv[i,cat1]),
              as.character(tr[i,cat1]))
  
  cat2 <- c("OBP","SLG","OPS","VC.1B","VC.2B","VC.3B","VC.HR","VC.H","VC.BB","VC.SB")
  X2 <- cbind(cat2,
              as.numeric(tv[i,cat2]),
              as.numeric(tr[i,cat2]))
  
  cat3 <- c("ERA","WHIP","VC.SOA","VC.BBA","VC.HRA","VC.BIP","VC.BABIP","VC.DefPitch","WC.DefPitch")
  X3 <- cbind(cat3,
              as.numeric(tv[i,cat3]),
              as.numeric(tr[i,cat3]))
  
  cat4 <- c("VC.Arm","VC.DP","VC.Rng","VC.Err","VC.SBA","VC.DefField","WC.DefField")
  X4 <- cbind(cat4,
              as.numeric(tv[i,cat4]),
              as.numeric(tr[i,cat4]))
  
  ##nr <- c(nrow(X1),nrow(X2),nrow(X3),nrow(X4))
  ##max.row <- max(nr)
  ##if (nr[1]!=max.row) for (i in 1:(max.row-nr[1])) X1 <- rbind(X1,"")
  ##if (nr[2]!=max.row) for (i in 1:(max.row-nr[2])) X2 <- rbind(X2,"")
  ##if (nr[3]!=max.row) for (i in 1:(max.row-nr[3])) X3 <- rbind(X3,"")
  ##if (nr[4]!=max.row) for (i in 1:(max.row-nr[4])) X4 <- rbind(X4,"")
  ##X <- cbind(X1,X2,X3,X4)
  
  Y <- combineTeam(mlb.vc,team)
  display.digits <- c(0,0,rep(1,3))
  ind <- sort(Y[,"WC"],ind=T,dec=T)$ix
  
  display1 <- xtable(X1)
  display2 <- xtable(X2)
  display3 <- xtable(X3)
  display4 <- xtable(X4)
  names(display1) <- names(display2) <- names(display3) <- names(display4) <- c("","Value","Rank")
  align(display1) <- align(display2) <- align(display3) <- align(display4) <- c("l","l","r","r")
  display5 <- xtable(Y[ind,],digits=display.digits)
  align(display5) <- c("l",rep("r",length(align(display5))-1))
  
  f <- paste(mlb.par@loc, "/", mlb.par@year, "/", team, ".html", sep="")
  cat(paste0("---\nyear: ", mlb.par@year, "\nrel: ../../\n---\n"), file=f)
  cat("<a href=", team,"_batting.html> Batting </a> <br><br>\n",sep="", file=f, append=TRUE)
  cat("<a href=", team,"_pitching.html> Pitching </a> <br><br>\n",sep="", file=f, append=TRUE)
  cat("<a href=", team,"_fielding.html> Fielding </a> <br><br>\n",sep="", file=f, append=TRUE)
  cat("<TABLE class=\"container\">\n<TR><TD>", file=f, append=TRUE)
  print(display1,type="html",include.rownames=FALSE,html.table.attributes="class=\"sortable ctable\" width=100%", file=f, append=TRUE)
  cat("</TD>\n<TD>", file=f, append=TRUE)
  print(display2,type="html",include.rownames=FALSE,html.table.attributes="class=\"sortable ctable\" width=100%", file=f, append=TRUE)
  cat("</TD>\n<TD>", file=f, append=TRUE)
  print(display3,type="html",include.rownames=FALSE,html.table.attributes="class=\"sortable ctable\" width=100%", file=f, append=TRUE)
  cat("</TD>\n<TD>", file=f, append=TRUE)
  print(display4,type="html",include.rownames=FALSE,html.table.attributes="class=\"sortable ctable\" width=100%", file=f, append=TRUE)
  cat("</TD></TR></TABLE>\n", file=f, append=TRUE)
  print(display5,type="html",html.table.attributes="class=\"sortable ctable\"", file=f, append=TRUE)
}
