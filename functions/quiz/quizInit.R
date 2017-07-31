quizInit <- function(year)
  {
    mlb.par <<- setDefaultPar(year)
    mlb.raw <<- formatData()
    mlb.vc <<- calculateVC(mlb.raw)
    mlb.team <<- calculateTeam(mlb.raw,mlb.vc)
  }
