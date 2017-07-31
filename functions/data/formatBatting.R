formatBatting <- function() {
  val <- NULL
  for (i in mlb.par@team) {
    team.batting <- formatTeamBatting(mlb.par@year,i)
    val <- rbind(val,team.batting)
  }
val
}
