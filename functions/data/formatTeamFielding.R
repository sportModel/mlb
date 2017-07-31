formatTeamFielding <- function(year,team) {
  require(XML)
  AllPos <- c("p", "c", "1b", "2b", "3b", "ss", "lf", "cf", "rf")
  val <- NULL
  for (Pos in AllPos) {
    filename <- paste("data/", year, "/raw/", team, "_fld_std_", Pos, ".html", sep="")
    allRaw <- readHTMLTable(filename)
    name <- grep("LeaderBoard", names(allRaw), value=TRUE)
    raw1 <- allRaw[[name]]
    raw1 <- raw1[,-1]

    filename <- paste("data/", year, "/raw/", team, "_fld_adv_", Pos, ".html", sep="")
    allRaw <- readHTMLTable(filename)
    name <- grep("LeaderBoard", names(allRaw), value=TRUE)
    raw2 <- allRaw[[name]]
    raw2 <- raw2[,-1]
    raw2 <- raw2[match(raw1$Name, raw2$Name),]
    
    val.i <- cbind(raw1[, 1, drop=FALSE], Team=team, raw1[,-1], raw2[,-1:-3])
    val <- rbind(val, val.i)
  }
  
  for (j in 4:ncol(val)) {
    val[,j] <- suppressWarnings(as.numeric(val[,j]))
  }
  val$Inn <- floor(val$Inn) + 10*(val$Inn-floor(val$Inn))/3
  val
}
