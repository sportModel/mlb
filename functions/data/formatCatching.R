formatCatching <- function()
  {
    val <- NULL
    for (i in mlb.par@team)
      {
        team.catching <- formatTeamCatching(mlb.par@year,i)
        val <- rbind(val,team.catching)
      }
    return(val)
  }
