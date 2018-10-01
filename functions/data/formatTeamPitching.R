formatTeamPitching <- function(year,team) {
  filename <- paste("data/",year,"/raw/",team,"_br.html",sep="")
  raw <- XML::readHTMLTable(filename, stringsAsFactors=FALSE)[["team_pitching"]]
  raw <- raw[raw$Age != "Age", -1]
  names(raw)[2] <- "Name"
  raw$Name <- gsub("*", "", raw$Name, fixed=TRUE)
  raw$Name <- gsub("#", "", raw$Name, fixed=TRUE)
  raw <- cbind(raw[,1:2], Team=team, raw[,-(1:2)])
  for (i in 4:ncol(raw)) {
    raw[,i] <- as.numeric(raw[,i])
  }
  names(raw)[names(raw)=="BF"] <- "TBF"
  names(raw)[names(raw)=="SO/9"] <- "K9"
  names(raw)[names(raw)=="BB/9"] <- "BB9"
  names(raw)[names(raw)=="SO/BB"] <- "KBB"
  raw
}
