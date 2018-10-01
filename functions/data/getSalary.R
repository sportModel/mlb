getSalary <- function() {
  year <- mlb.par@year
  val <- NULL
  for (team in mlb.par@team) {
    filename <- paste("data/",year,"/raw/",team,"_br.html",sep="")
    raw <- XML::readHTMLTable(filename, stringAsFactors=FALSE)
    s <- c(raw[["players_value_batting"]]$Salary, raw[["players_value_pitching"]]$Salary)
    s <- gsub("$", "", s, fixed=TRUE)
    s <- gsub(",", "", s, fixed=TRUE)
    s <- as.numeric(s)/1e6
    Name <- c(raw[["players_value_batting"]][,1], raw[["players_value_pitching"]][,1])
    Name <- gsub("*", "", Name, fixed=TRUE)
    Name <- gsub("#", "", Name, fixed=TRUE)
    names(s) <- Name
    val <- c(val, s[unique(Name)])
  }
  val
}
