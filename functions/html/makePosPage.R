makePosPage <- function() {
  f <- paste(mlb.par@loc, "/", mlb.par@year, "/", "pos.md", sep="")
  cat('---\nyear: ', mlb.par@year, '\nrel: ../\n---\n', file=f)
  cat('
[Pitcher](p.html)  
[Catcher](c.html)  
[First base](1b.html)  
[Second base](2b.html)  
[Shortstop](ss.html)  
[Third base](3b.html)  
[Left field](lf.html)  
[Center field](cf.html)  
[Right field](rf.html)  
[Designated hitter](dh.html)  
[All position players](allpos.html)',
      file=f, append=TRUE)
}
