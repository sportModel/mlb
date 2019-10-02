formatData <- function() {
  batting <- formatBatting()
  pitching <- formatPitching()
  fielding <- formatFielding()

  ## Apply any special fixes here
  fielding[Name=="Delino Deshields", Name := "Delino DeShields"]
  fielding[Name=="Peter Alonso", Name := "Pete Alonso"]
  batting[Name=="Hyun-jin Ryu", Name := "Hyun-Jin Ryu"]
  pitching[Name=="Hyun-jin Ryu", Name := "Hyun-Jin Ryu"]

  batting$Pos <- assignPos(batting,fielding)

  val <- new("mlbData",
             batting=batting,
             pitching=pitching,
             fielding=fielding)
  val
}
