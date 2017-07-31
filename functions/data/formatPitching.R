formatPitching <- function() {
  val <- NULL
  for (i in mlb.par@team) {
    team.pitching <- formatTeamPitching(mlb.par@year,i)
    val <- rbind(val,team.pitching)
  }
  return(val)
}
