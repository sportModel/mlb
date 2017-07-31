formatFielding <- function() {
  val <- NULL
  for (i in mlb.par@team) {
    team.fielding <- formatTeamFielding(mlb.par@year,i)
    val <- rbind(val,team.fielding)
  }
  val$Pos <- tolower(val$Pos)
  return(val)
}
