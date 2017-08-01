makeTeamFielding <- function(mlb.raw,mlb.vc,team) {
  raw <- mlb.raw@fielding[which(mlb.raw@fielding$Team==team),]
  attr(raw,"row.names") <- raw$Name    
  ##vc <- mlb.vc@fielding[which(mlb.vc@fielding$Team==team & mlb.vc@fielding$Pos!="c"),]
  vc <- mlb.vc@fielding[which(mlb.vc@fielding$Team==team),]
  attr(vc,"row.names") <- vc$Name
  
  display.categories.raw <- c("Pos","Inn","PO","A","E","FP","SB","CS")
  display.digits.raw <- c(rep(0,2),1,rep(0,3),3,0,0)
  display.categories.vc <- c("Pos","VC.Arm","VC.DP","VC.Rng","VC.Err","VC.SBA","VC.Def")
  display.digits.vc <- c(0,0,rep(1,6))
  
  ind.raw <- sort(as.numeric(raw[,"Inn"]),ind=T,dec=T)$ix
  ind.vc <- sort(vc[,"VC.Def"],ind=T,dec=T)$ix
  
  display.raw <- xtable(raw[ind.raw,display.categories.raw],digits=display.digits.raw)
  align(display.raw) <- rep("r",length(align(display.raw)))
  align(display.raw)[1] <- "l"
  display.vc <- xtable(vc[ind.vc,display.categories.vc],digits=display.digits.vc)
  align(display.vc) <- rep("r",length(align(display.vc)))
  align(display.vc)[1] <- "l"
  
  
  filename <- paste(mlb.par@loc,"/",mlb.par@year,"_",team,"_fielding",".html",sep="")
  sink(filename)
  cat('---\n---\n')
  print(display.raw,type="html",html.table.attributes="class=\"sortable ctable\"")
  print(display.vc,type="html",html.table.attributes="class=\"sortable ctable\"")
  sink()
  cleanTable(filename)
  buffer <- readLines(filename)
  buffer <- gsub("([^0123456789])\\.[123456789]","\\1",buffer)
  f <- file(filename,"w")
  writeLines(buffer,con=f)
  close(f)    
}
