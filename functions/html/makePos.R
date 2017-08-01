makePos <- function(mlb.pos, Pos) {
#   if (Pos=="p") X <- combineP(mlb.vc)
#   if (Pos=="dh") X <- combineDH(mlb.raw, mlb.vc)
#   if (!is.element(Pos,c("p","dh"))) X <- combinePos(mlb.raw,mlb.vc,Pos)
  if (Pos=="p") {
    X <- mlb.pos$pit
  } else if (Pos=="") {
    X <- mlb.pos$pos
  } else {
    x <- Pos
    X <- subset(mlb.pos$pos, Pos==x)
  }
  
  if (Pos=="p") display.categories <- c("Teams","VC.SOA","VC.BBA","VC.HRA","VC.BIP","VC.BABIP","VC.Def","WC")
  if (Pos=="dh") display.categories <- c("Teams","VC.1B","VC.2B","VC.3B","VC.HR","VC.H","VC.BB","VC.SB","VC.Off","WC")
  if (Pos=="c") display.categories <- c("Teams","VC.1B","VC.2B","VC.3B","VC.HR","VC.H","VC.SB","VC.SBA","VC.Off","VC.Def","WC")
  if (is.element(Pos,c("1b","2b","3b","ss"))) display.categories <- c("Teams","VC.1B","VC.2B","VC.3B","VC.HR","VC.H","VC.BB","VC.SB","VC.DP","VC.Err","VC.Rng","VC.Off","VC.Def","WC")
  if (is.element(Pos,c("lf","cf","rf"))) display.categories <- c("Teams","VC.1B","VC.2B","VC.3B","VC.HR","VC.H","VC.BB","VC.SB","VC.Arm","VC.Err","VC.Rng","VC.Off","VC.Def","WC")
  if (Pos=="") {
    display.categories <- names(X)
    Pos <- "allpos"
  }
  display.digits <- c(0,0,rep(1,length(display.categories)-1))
  ind <- sort(X[,"WC"],ind=T,dec=T)$ix
  
  display <- xtable(X[ind,display.categories],digits=display.digits)
  align(display) <- rep("r",length(align(display)))
  align(display)[1] <- "l"
  
  filename <- paste(mlb.par@loc,"/",mlb.par@year,"_",Pos,".html",sep="")
  sink(filename)
  cat("---\n---\n")
  print(display,type="html",html.table.attributes="class=\"sortable ctable\"")
  sink()
  cleanTable(filename)
}
