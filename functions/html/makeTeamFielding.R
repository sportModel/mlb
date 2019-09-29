makeTeamFielding <- function(mlb.raw, mlb.vc, team) {
  raw <- mlb.raw@fielding[which(mlb.raw@fielding$Team==team),]
  attr(raw,"row.names") <- raw$Name    
  vc <- mlb.vc@fielding[which(mlb.vc@fielding$Team==team),]
  attr(vc,"row.names") <- vc$Name
  
  ctg_raw <- c("Pos","Inn","PO","A","E","FP","SB","CS")
  dig_raw <- c(0, 1, rep(0,3), 3, 0, 0)
  ind_raw <- order(as.numeric(raw[,"Inn"]), decreasing = TRUE)
  aln_raw <- rep('r', length(ctg_raw))
  raw_tab <- knitr::kable(raw[ind_raw,ctg_raw], digits=dig_raw, format='html', align=aln_raw, table.attr='class="sortable ctable"')
  
  ctg_vc <- c("Pos","VC.Arm","VC.DP","VC.Rng","VC.Err","VC.SBA","VC.Def")
  dig_vc <- c(0, rep(1,6))
  ind_vc <- order(vc[,'VC.Def'], decreasing = TRUE)
  aln_vc <- rep('r', length(ctg_vc))
  vc_tab <- knitr::kable(vc[ind_vc,ctg_vc], digits=dig_vc, format='html', align=aln_vc, table.attr='class="sortable ctable"')
  
  f <- paste(mlb.par@loc, "/", mlb.par@year, "/",team, "_fielding.html", sep="")
  cat('---\nyear: ', mlb.par@year, '\nrel: ../\n---\n', file=f)
  cat(raw_tab, file=f, append=TRUE)
  cat(vc_tab, file=f, append=TRUE)
}
