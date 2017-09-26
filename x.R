year <- as.numeric(commandArgs(TRUE)[[1]])
mlb.par <- setDefaultPar(year)
mlb.raw <- formatData()
mlb.vc <- calculateVC(mlb.raw)
mlb.team <- calculateTeam(mlb.raw,mlb.vc)
save(mlb.par, mlb.raw, mlb.vc, mlb.team, file=paste("data/",year,"/mlb.RData",sep=""))
updateWebsite(mlb.raw, mlb.vc, mlb.team)

# source('~/dev/html/R/buildHTML.R')
# .html <- NULL
# .html$dir <- "../web/baseball/"
# buildHTML(TRUE)

##X <- rbind(combineP(mlb.vc)[,"WC",drop=FALSE],combineDH(mlb.raw,mlb.vc)[,"WC",drop=FALSE],combinePos(mlb.raw,mlb.vc,"c")[,"WC",drop=FALSE],combinePos(mlb.raw,mlb.vc,"1b")[,"WC",drop=FALSE],combinePos(mlb.raw,mlb.vc,"2b")[,"WC",drop=FALSE],combinePos(mlb.raw,mlb.vc,"3b")[,"WC",drop=FALSE],combinePos(mlb.raw,mlb.vc,"ss")[,"WC",drop=FALSE],combinePos(mlb.raw,mlb.vc,"lf")[,"WC",drop=FALSE],combinePos(mlb.raw,mlb.vc,"cf")[,"WC",drop=FALSE],combinePos(mlb.raw,mlb.vc,"rf")[,"WC",drop=FALSE])
##X[order(X$WC,decreasing=TRUE)[1:20],,drop=FALSE]
