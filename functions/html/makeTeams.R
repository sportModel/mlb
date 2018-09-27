makeTeams <- function(mlb.team) {
  ## create list of matrices
  n.d <- length(mlb.par@divisions)
  X <- D <- vector("list",n.d)
  for (d in 1:length(mlb.par@divisions)) {
    teams <- mlb.par@divisions[[d]]
    ind <- match(teams,mlb.par@team)
    X[[d]] <- matrix("",nrow=length(teams),ncol=5)
    X[[d]][,1] <- teams
    X[[d]][,2] <- mlb.par@team.long[ind]
    X[[d]][,3] <- w <- mlb.team$W[ind]
    X[[d]][,4] <- l <- mlb.team$L[ind]
    
    ## Calculate GB, sort
    over500 <- w - (w+l)/2
    ord <- order(-over500)
    gb <- over500[ord][1]-over500[ord]
    X[[d]] <- X[[d]][ord,]
    X[[d]][,5] <- gb
    
    ## Format for printing
    link <- character(length(teams))
    for (i in 1:length(teams)) {
      link[i] <- paste0("<a href=\"", X[[d]][i,1], ".html\"> ", X[[d]][i,2], "</a>")
    }
    X[[d]] <- cbind(link,X[[d]][,3:5])
    colnames(X[[d]]) <- c("Team","W","L","GB")
    #D[[d]] <- xtable(X[[d]])
    #align(D[[d]]) <- c("l","l","r","r","r")
  }
  
  ## display
  f <- paste(mlb.par@loc, "/", mlb.par@year, "/teams.html", sep="")
  cat('---\nyear: ', mlb.par@year, '\nrel: ../../\n---\n', file=f)
  cat("<TABLE class=\"container\">\n", file=f, append=TRUE)
  for (i in 1:3) {
    cat("<TR><TD align=\"center\">",names(mlb.par@divisions)[i],"</TD><TD align=\"center\">",names(mlb.par@divisions)[i+3],"</TD></TR>\n<TR><TD>", file=f, append=TRUE)
    cat(knitr::kable(X[[i]], 'html', escape=FALSE, table.attr = "class=\"sortable ctable\" width=100%"), file=f, append=TRUE)
    #print(D[[i]], type="html", include.rownames=FALSE,html.table.attributes="class=\"sortable ctable\" width=100%", file=f, append=TRUE)
    cat("</TD>\n<TD>", file=f, append=TRUE)
    cat(knitr::kable(X[[i+3]], 'html', escape=FALSE, table.attr = "class=\"sortable ctable\" width=100%"), file=f, append=TRUE)
    #print(D[[i+3]], type="html", include.rownames=FALSE,html.table.attributes="class=\"sortable ctable\" width=100%", file=f, append=TRUE)
    cat("</TD></TR>\n", file=f, append=TRUE)
  }
  cat("</TABLE>\n", file=f, append=TRUE)
}
