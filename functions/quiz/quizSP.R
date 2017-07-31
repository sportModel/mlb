quizSP <- function(team,year=2010)
  {
    load(paste("data/",year,"/mlb.RData",sep=""))
    .qX <- mlb.raw@pitching[mlb.raw@pitching$Team==team,]
    .qX <- .qX[.qX$GS!=0,]
    .qX <- .qX[order(.qX$GS,decreasing=TRUE),c("Name","GS")]
    row.names(.qX) <- NULL
    .qX <<- data.frame(.qX,Got=rep(FALSE,nrow(.qX)))
  }

