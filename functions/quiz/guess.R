guess <- function(x)
  {
    GotCol <- which(names(.qX)=="Got")
    ind <- agrep(x,.qX$Name)
    if (length(ind)!=0) .qX[ind,GotCol] <- TRUE
    Y <- .qX[,-GotCol]
    Y$Name[!.qX$Got] <- "???"
    print(Y)
    .qX <<- .qX
  }
