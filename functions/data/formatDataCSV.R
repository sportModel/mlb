formatDataCSV <- function()
{
  batting <- pitching <- fielding <- NULL
  for (i in mlb.par@team) {
    ## Batting
    std <- read.csv(paste("data/", mlb.par@year, "/raw/", i, "_batting.csv", sep=""))
    wpa <- read.csv(paste("data/", mlb.par@year, "/raw/", i, "_batting_wpa.csv", sep=""))
    id <- match(std[,1], wpa[,1])
    team.batting <- cbind(Name=std[,1], Team=i, Year=mlb.par@year, Pos="", std[, -1], wpa[id, -1])
    batting <- rbind(batting, team.batting)
    
    ## Pitching
    std <- read.csv(paste("data/", mlb.par@year, "/raw/", i, "_pitching_standard.csv", sep=""))
    adv <- read.csv(paste("data/", mlb.par@year, "/raw/", i, "_pitching_advanced.csv", sep=""))
    wpa <- read.csv(paste("data/", mlb.par@year, "/raw/", i, "_pitching_wpa.csv", sep=""))
    id.adv <- match(std[,1], adv[,1])
    id.wpa <- match(std[,1], wpa[,1])
    team.pitching <- cbind(Name=std[,1], Team=i, Year=mlb.par@year, std[, -1], adv[id.adv, -1], wpa[id.wpa, -1])
    pitching <- rbind(pitching, team.pitching)
    
    ## Fielding
    std <- read.csv(paste("data/", mlb.par@year, "/raw/", i, "_fielding_standard.csv", sep=""))
    adv <- read.csv(paste("data/", mlb.par@year, "/raw/", i, "_fielding_advanced.csv", sep=""))
    Pos <- tolower(std$Pos)
    id <- match(apply(std[,1:2], 1, paste, collapse=" "), apply(adv[,1:2], 1, paste, collapse=" "))
    team.fielding <- cbind(Name=std[,1], Team=i, Year=mlb.par@year, Pos, std[, -(1:2)], adv[id, -(1:2)])
    fielding <- rbind(fielding, team.fielding)    
  }
  batting$Pos <- assignPos(batting,fielding)
  
  new("mlbData",
      batting=batting,
      pitching=pitching,
      fielding=fielding)
}
