makeIndex <- function()
  {
    filename <- paste(mlb.par@loc,"/index.html",sep="")
    sink(filename)
    cat("<p>\n",
        "Statistics for each season are available in two formats: by team and by position.  The team pages tell a story about which players on the team have contributed the most to the team's win/loss record.  The position pages inform us about which players have been the best and worst in MLB at their respective positions.\n",
        "</p>\n",
        "<p>\n",
        "If players are traded during a season, the team pages contain only their stats for that team.  However, the position pages contain all of a player's stats for the year, combining across teams.\n",
        "</p>\n",
        "<p>\n",
        "Players appear only once in the position pages.  If a player spent time at more than one position, then they are listed at the position at which they spent the most time.  The information on how many defensive runs they saved, however, is combined from the multiple positions.  For instance, if a player saved 10 runs at 2B and 5 runs at SS, but spent more innings at SS, they will be found under SS as saving 15 runs.  This has the potential to be somewhat misleading, so be aware of it.  If you are interested in a breakdown of a player's fielding contributions by position, you will have to visit his team page.\n",
        "</p>\n",
        "<p>\n",
        "Updated: ",as.character(Sys.time()),"\n",
        "</p>\n",
        "<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>\n\n\n",
        sep="")
    sink()
  }
