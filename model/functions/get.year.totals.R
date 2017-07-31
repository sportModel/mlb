## This function accepts a data frame of team-level data and calculates yearly totals
get.year.totals <- function(team.data)
  {
    year <- as.numeric(names(table(team.data$yearID)))
    n.year <- length(year)
    stats <- c("G","R","AB","H","2B","3B","HR","BB","SO","SB","CS","RA","ER","Outs","HA","HRA","BBA","SOA","E","DP")
    val <- matrix(NA,nrow=n.year,ncol=length(stats))
    colnames(val) <- stats
    for (i in 1:n.year)
      {
        teams.i <- team.data[which(team.data$yearID==year[i]),stats]
        val[i,] <- apply(teams.i,2,sum)
      }
    val <- cbind(year,val)
    val
  }
