makeTeamComparison <- function(mlb.team) {
  cat_ovr <- c("W","L","W.pyth","L.pyth","W.WC","L.WC","RS","RA")
  dig_ovr <- rep(0, length(cat_ovr)+1)
  ind_ovr <- order(mlb.team$W, decreasing=TRUE)
  aln_ovr <- rep("r", length(cat_ovr))
  ovr <- knitr::kable(mlb.team[ind_ovr, cat_ovr], digits = dig_ovr, format = 'html', align=aln_ovr, table.attr='class="sortable ctable"')

  cat_off <- c("RS","OBP","SLG","OPS","VC.1B","VC.2B","VC.3B","VC.HR","VC.H","VC.BB","VC.SB","VC.Off")
  dig_off <- c(0, 0, 3, 3, 3, rep(1,8))
  ind_off <- order(mlb.team$VC.Off, decreasing=TRUE)
  aln_off <- rep("r", length(cat_off))
  off <- knitr::kable(mlb.team[ind_off, cat_off], digits = dig_off, format = 'html', align=aln_off, table.attr='class="sortable ctable"')

  cat_def <- c("RA","ERA","WHIP","VC.SOA","VC.BBA","VC.HRA","VC.BIP","VC.BABIP","VC.Arm","VC.DP","VC.Rng","VC.Err","VC.SB","VC.DefPitch","VC.DefField","VC.Def")
  dig_def <- c(0, 0, 2, 2, rep(1,13))
  ind_def <- order(mlb.team$VC.Def, decreasing=TRUE)
  aln_def <- rep("r", length(cat_def))
  def <- knitr::kable(mlb.team[ind_def, cat_def], digits = dig_def, format = 'html', align=aln_def, table.attr='class="sortable ctable"')
  
  f <- paste(mlb.par@loc, "/", mlb.par@year, "/team_comparison.html", sep="")
  cat('---\nyear: ', mlb.par@year, '\nrel: ../\n---\n', file=f)
  cat(ovr, file=f, append=TRUE)
  cat(off, file=f, append=TRUE)
  cat(def, file=f, append=TRUE)
}
