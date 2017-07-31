calcVCdefense <- function(mlb.raw) {
  pitching <- NULL
  fielding <- NULL
  catching <- calcVCcatching(mlb.raw)
  for (i in mlb.par@team) {
    team.defense <- calcVCteamDefense(mlb.raw, i, catching)
    pitching <- rbind(pitching, team.defense$pitching)
    fielding <- rbind(fielding, team.defense$fielding)
  }
  val <- list(pitching=pitching,
              fielding=fielding)
  val
}
