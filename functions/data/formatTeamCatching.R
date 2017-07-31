formatTeamCatching <- function(year,team)
  {
    raw <- readLines(paste("data/",year,"/raw/",team,"_catching.html",sep=""),warn=F)
    tbody.lines <- grep("tbody",raw)
    n.tbody <- length(tbody.lines)
    raw <- raw[(tbody.lines[n.tbody-1]+1):(tbody.lines[n.tbody]-1)]
    raw <- gsub("\t","",raw,fixed=F)    
    raw <- gsub("\\<a[^\\>]*\\>","",raw,perl=T)
    raw <- gsub("\\</a\\>","",raw,perl=T)
    raw <- gsub("\\</td\\>","",raw,perl=T)
    raw <- gsub("\\</tr\\>","",raw,perl=T)
    raw <- gsub(" align[^\\>]*","",raw,perl=T)
    raw <- gsub(" class[^\\>]*","",raw,perl=T)
    raw <- gsub(" bgcolor[^\\>]*","",raw,perl=T)
    raw <- raw[raw!=""]
    raw <- raw[raw!="<tr>"]
    raw <- gsub("<td>","",raw,fixed=T)
    raw <- gsub("N/A","",raw,fixed=T) ## To ensure proper NA conversion
    val <- matrix(raw,ncol=15,byrow=T)
    val <- val[,-1]
    val <- as.data.frame(val,stringsAsFactors=F)
    names(val) <- c("Year","last.name","first.name","Team","Lg","IP","SBAG","CSP","ERA","WPPBG","PO","A","TE","FE")
    val[,"Team"] <- team
    val[,"CSP"] <- gsub("%","",val[,"CSP"],fixed=T)
    val <- cbind(Team=val$Team,Year=val$Year,Name=apply(val[,c("first.name","last.name")],1,paste,collapse=" "),val[,-1:-5])
    for (i in 4:ncol(val))
      {
        val[,i] <- as.numeric(val[,i])
      }
    val
  }
