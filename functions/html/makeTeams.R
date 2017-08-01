makeTeams <- function(mlb.team) {
  ## create list of matrices
  n.d <- length(mlb.par@divisions)
  X <- D <- vector("list",n.d)
  for (d in 1:length(mlb.par@divisions))
  {
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
      link[i] <- paste("@@lt@@A href=@@quote@@",mlb.par@year,"_",X[[d]][i,1],".html@@quote@@ @@gt@@ ",X[[d]][i,2],"@@lt@@/a@@gt@@",sep="")
    }
    X[[d]] <- cbind(link,X[[d]][,3:5])
    colnames(X[[d]]) <- c("Team","W","L","GB")
    D[[d]] <- xtable(X[[d]])
    align(D[[d]]) <- c("l","l","r","r","r")
  }
  
  ## display
  filename <- paste(mlb.par@loc,"/",mlb.par@year,"_teams.html",sep="")
  sink(filename)
  cat("---\n---\n<TABLE class=\"container\">\n")
  for (i in 1:3) {
    cat("<TR><TD align=\"center\">",names(mlb.par@divisions)[i],"</TD><TD align=\"center\">",names(mlb.par@divisions)[i+3],"</TD></TR>\n<TR><TD>")
    print(D[[i]],type="html",include.rownames=FALSE,html.table.attributes="class=\"sortable ctable\" width=100%")
    cat("</TD>\n<TD>")
    print(D[[i+3]],type="html",include.rownames=FALSE,html.table.attributes="class=\"sortable ctable\" width=100%")
    cat("</TD></TR>\n")
  }
  cat("</TABLE>\n")
  sink()
  cleanTable(filename)
}
