formatTeamFielding <- function(year,team) {
  AllPos <- c("p", "c", "1b", "2b", "3b", "ss", "lf", "cf", "rf")
  val <- data.table()
  for (Pos in AllPos) {
    filename <- paste("data/", year, "/raw/", team, "_fld_std_", Pos, ".html", sep="")
    allRaw <- XML::readHTMLTable(filename, stringsAsFactors=FALSE)
    name <- grep("LeaderBoard", names(allRaw), value=TRUE)
    STD <- as.data.table(allRaw[[name]][,-1])

    filename <- paste("data/", year, "/raw/", team, "_fld_adv_", Pos, ".html", sep="")
    allRaw <- XML::readHTMLTable(filename, stringsAsFactors=FALSE)
    name <- grep("LeaderBoard", names(allRaw), value=TRUE)
    ADV <- as.data.table(allRaw[[name]][,-1])
    ADV[, Pos := NULL]
    ADV[, Inn := NULL]
    DT <- merge(STD, ADV, by='Name')
    DT[, Team := team]
    setcolorder(DT, c('Name', 'Pos', 'Team'))
    val <- rbind(val, DT)
  }

  for (i in 4:ncol(val)) {
    suppressWarnings(val[, names(val)[i] := as.numeric(val[[i]])])
  }
  val$Inn <- floor(val$Inn) + 10*(val$Inn-floor(val$Inn))/3
  val
}
