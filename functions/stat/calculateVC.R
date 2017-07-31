calculateVC <- function(mlb.raw) {
  batting <- calcVCoffense(mlb.raw)
  defense <- calcVCdefense(mlb.raw)
  pitching <- defense$pitching
  fielding <- defense$fielding
  val <- new("mlbVC",
             batting=batting,
             pitching=pitching,
             fielding=fielding)
  val
}
