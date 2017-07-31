formatTeamBatting <- function(year,team) {
  filename <- paste("data/",year,"/raw/",team,"_br.html",sep="")
  require(XML)
  raw <- readHTMLTable(filename)[["team_batting"]]
  raw <- raw[raw$PA != "PA",-1]
  names(raw)[2] <- "Name"
  raw$Name <- gsub("*", "", raw$Name, fixed=TRUE)
  raw$Name <- gsub("#", "", raw$Name, fixed=TRUE)
  raw <- cbind(raw[,1:2], Team=team, raw[,-(1:2)])
  for (i in 4:ncol(raw)) {
    raw[,i] <- as.numeric(raw[,i])
  }
  raw[,"1B"] <- raw$H - raw[,"2B"] - raw[,"3B"] - raw[,"HR"]
  names(raw)[names(raw)=="BA"] <- "AVG"
  raw[raw$PA!=0,]
}
