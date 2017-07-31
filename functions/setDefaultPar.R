setClass("MLBpar",representation(team="character",
                                 team.long="character",
                                 divisions="list",
                                 year="numeric",
                                 model="numeric",
                                 loc="character",
                                 prior.weight="numeric",
                                 iz.weight="numeric"))

setDefaultPar <- function(year)
  {
    team <- c("nyy","tor","bos","tam","bal","chw","det","min","kan","cle","oak","tex","laa","sea","atl","phi","was","nym","fla","pit","mil","stl","chc","cin","hou","sfo","sdg","col","lad","ari")
    team.long <- c("New York Yankees","Toronto Blue Jays","Boston Red Sox","Tampa Bay Rays","Baltimore Orioles","Chicago White Sox","Detroit Tigers","Minnesota Twins","Kansas City Royals","Cleveland Indians","Oakland Athletics","Texas Rangers","Los Angeles Angels","Seattle Mariners","Atlanta Braves","Philadelphia Phillies","Washington Nationals","New York Mets","Miami Marlins","Pittsburgh Pirates","Milwaukee Brewers","St. Louis Cardinals","Chicago Cubs","Cincinnati Reds","Houston Astros","San Francisco Giants","San Diego Padres","Colorado Rockies","Los Angeles Dodgers","Arizona Diamondbacks")
    divisions <- list(c("nyy","tor","bos","tam","bal"),c("chw","det","min","kan","cle"),c("oak","tex","laa","sea","hou"),c("atl","phi","was","nym","fla"),c("pit","mil","stl","chc","cin"),c("sfo","sdg","col","lad","ari"))
    names(divisions) <- c("AL East","AL Central","AL West","NL East","NL Central","NL West")
    load(file="model/model.RData")
    prior.weight <- 150
    iz.weight <- .565
    loc <- "../web/baseball"
    val <- new("MLBpar",
               team=team,
               team.long=team.long,
               divisions=divisions,
               year=year,
               model=model,
               loc=loc,
               prior.weight=prior.weight,
               iz.weight=iz.weight)
    val
  }
