makeTeamPitching <- function(mlb.raw, mlb.vc, team) {
  raw <- mlb.raw@pitching[which(mlb.raw@pitching$Team==team),]
  rownames(raw) <- raw$Name
  vc <- mlb.vc@pitching[which(mlb.vc@pitching$Team==team),]
  rownames(vc) <- raw$Name
  
  ctg_raw <- c("G","GS","W","L","SV","IP","H","HR","BB","SO","WHIP","ERA")
  dig_raw <- c(rep(0,5), 1, rep(0,4), rep(2,2))
  ind_raw <- order(raw[,'IP'], decreasing = TRUE)
  aln_raw <- rep('r', length(ctg_raw))
  raw_tab <- knitr::kable(raw[ind_raw,ctg_raw], digits=dig_raw, format='html', align=aln_raw, table.attr='class="sortable ctable"')
  
  ctg_vc <- c("VC.SOA","VC.BBA","VC.HRA","VC.BIP","VC.BABIP","VC.Def")
  dig_vc <- c(rep(1,length(ctg_vc)))
  ind_vc <- order(vc[,"VC.Def"], decreasing = TRUE)
  aln_vc <- rep('r', length(ctg_vc))
  vc_tab <- knitr::kable(vc[ind_vc,ctg_vc], digits=dig_vc, format='html', align=aln_vc, table.attr='class="sortable ctable"')
  
  f <- paste(mlb.par@loc, "/", mlb.par@year, "/",team, "_pitching.html", sep="")
  cat('---\nyear: ', mlb.par@year, '\nrel: ../\n---\n', file=f)
  cat(raw_tab, type="html", file=f, append=TRUE)
  cat(vc_tab, type="html", file=f, append=TRUE)
}
