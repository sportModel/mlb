makeBattingPos <- function(mlb.raw,pos)
{
  ## Determine fielding subset
  ## Must have batting data (no 0 PA) and play pos
  batting <- mlb.raw@batting[mlb.raw@batting$PA!=0,]
  fielding <- mlb.raw@fielding[mlb.raw@fielding$Inn!=0,]    
  identifier <- apply(batting[,c("Name","Team")],1,paste,collapse=" ")
  pos.identifier <- apply(fielding[,c("Name","Team")],1,paste,collapse=" ")
  pos.ind <- which(fielding$Pos==pos & is.element(pos.identifier,identifier))
  
  ## For each identifier, determine multiplier
  n <- length(pos.ind)
  multiplier <- numeric(n)
  for (i in 1:n) {
    unit <- fielding[pos.identifier==pos.identifier[pos.ind[i]],]
    total.ip <- sum(unit$Inn)
    pos.ip <- unit$Inn[which(unit$Pos==pos)]
    multiplier[i] <- pos.ip/total.ip
  }
  
  ## Subset and apply multiplier
  ind <- match(pos.identifier[pos.ind],identifier)
  val <- batting[ind,]
  val[,-1:-4] <- val[,-1:-4]*multiplier
  val
}
