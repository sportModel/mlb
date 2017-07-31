makeBattingDH <- function(mlb.raw)
  {
    ## Determine subset
    ## Must have batting data (no 0 PA) and play no position
    batting <- mlb.raw@batting[mlb.raw@batting$PA!=0,]
    fielding <- mlb.raw@fielding[mlb.raw@fielding$Inn!=0,]    
    identifier <- apply(batting[,c("Name","Team")],1,paste,collapse=" ")
    pos.identifier <- apply(fielding[,c("Name","Team")],1,paste,collapse=" ")
    ind <- which(is.na(match(identifier,pos.identifier)))

    ## Subset and apply multiplier
    val <- batting[ind,]
    val    
  }
