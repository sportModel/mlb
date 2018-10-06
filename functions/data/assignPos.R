assignPos <- function(batting, fielding) {
  n <- nrow(batting)
  Pos <- character(n)
  for (i in 1:n) {
    unit <- fielding[Name == batting$Name[i] & Team == batting$Team[i]]
    pct <- unit$Inn/sum(unit$Inn)
    names(pct) <- unit$Pos
    Pos[i] <- paste(names(sort(pct[pct > .25],decreasing=TRUE)),collapse="/")
  }
  Pos
}
