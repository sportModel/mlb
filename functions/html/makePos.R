makePos <- function(mlb.pos, Pos) {
  if (Pos=="p") {
    X <- mlb.pos$pit
  } else if (Pos=="") {
    X <- mlb.pos$pos
  } else {
    x <- Pos
    X <- subset(mlb.pos$pos, Pos==x)
  }
  
  if (Pos=="p") ctg <- c("Teams","VC.SOA","VC.BBA","VC.HRA","VC.BIP","VC.BABIP","VC.Def","WC")
  if (Pos=="dh") ctg <- c("Teams","VC.1B","VC.2B","VC.3B","VC.HR","VC.H","VC.BB","VC.SB","VC.Off","WC")
  if (Pos=="c") ctg <- c("Teams","VC.1B","VC.2B","VC.3B","VC.HR","VC.H","VC.SB","VC.SBA","VC.Off","VC.Def","WC")
  if (is.element(Pos,c("1b","2b","3b","ss"))) ctg <- c("Teams","VC.1B","VC.2B","VC.3B","VC.HR","VC.H","VC.BB","VC.SB","VC.DP","VC.Err","VC.Rng","VC.Off","VC.Def","WC")
  if (is.element(Pos,c("lf","cf","rf"))) ctg <- c("Teams","VC.1B","VC.2B","VC.3B","VC.HR","VC.H","VC.BB","VC.SB","VC.Arm","VC.Err","VC.Rng","VC.Off","VC.Def","WC")
  if (Pos=="") {
    ctg <- names(X)
    Pos <- "allpos"
  }
  dig <- c(0,0,rep(1,length(ctg)-1))
  ind <- sort(X[,"WC"], ind=T, dec=T)$ix
  aln <- rep('r', length(ctg))
  display <- knitr::kable(X[ind,ctg], digits=dig, format='html', align=aln, table.attr='class="sortable ctable"')

  f <- paste(mlb.par@loc, "/", mlb.par@year, "/", Pos, ".html", sep="")
  cat('---\nyear: ', mlb.par@year, '\nrel: ../\n---\n', file=f)
  cat(display, file=f, append=TRUE)
}
