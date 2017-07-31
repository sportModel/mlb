formatName <- function(x)
  {
  browser()
    x <- gsub(" \\(.*\\)","",x,perl=T)
    n <- length(x)
    val <- matrix(NA,nrow=n,ncol=2)
    for (i in 1:n)
      {
        name <- strsplit(x[i]," ")[[1]]
        first.name <- NULL
        last.name <- NULL
        if (length(name)==2)
          {
            first.name <- name[1]
            last.name <- name[2]
          }
        if (length(name)==3)
          {
            first.name <- paste(name[1],name[2])
            last.name <- name[3]
          }
        if (length(name)==4)
          {
            first.name <- paste(name[1],name[2],name[3])
            last.name <- name[4]
          }
        val[i,] <- c(first.name,last.name)
      }
    val <- as.data.frame(val,stringsAsFactors=F)
    names(val) <- c("first.name","last.name")
    val
  }
