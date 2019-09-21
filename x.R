year <- as.numeric(commandArgs(TRUE)[[1]])
mlb.par <- setDefaultPar(year)
mlb.raw <- formatData()
mlb.vc <- calculateVC(mlb.raw)
mlb.team <- calculateTeam(mlb.raw,mlb.vc)
save(mlb.par, mlb.raw, mlb.vc, mlb.team, file=paste("data/",year,"/mlb.rda",sep=""))
updateWebsite(mlb.raw, mlb.vc, mlb.team)

# year <- 2019
# load('data/2019/mlb.rda')
