formatData <- function() {
  batting <- formatBatting()
  pitching <- formatPitching()
  fielding <- formatFielding()

  ## Apply any special fixes here
  fielding$Name[fielding$Name=="Delino Deshields"] <- "Delino DeShields"
  batting$Name[batting$Name=="Hyun-jin Ryu"] <- "Hyun-Jin Ryu"
  pitching$Name[pitching$Name=="Hyun-jin Ryu"] <- "Hyun-Jin Ryu"
  batting$Name <- gsub(" \\(.*", "", batting$Name)
  pitching$Name <- gsub(" \\(.*", "", pitching$Name)

  batting$Pos <- assignPos(batting,fielding)

  val <- new("mlbData",
             batting=batting,
             pitching=pitching,
             fielding=fielding)
  val
}
