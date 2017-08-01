makePosPage <- function() {
  filename <- paste(mlb.par@loc, "/", mlb.par@year, "_", "pos.html", sep="")
  cat('---\n---\n&nbsp &nbsp &nbsp &nbsp &nbsp&nbsp &nbsp &nbsp <a                        
href="', mlb.par@year, '_cf.html">CF</a><br>                                            
  &nbsp &nbsp <a href="', mlb.par@year, '_lf.html">LF</a>                                
  &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp <a                 
href="', mlb.par@year, '_rf.html">RF</a><br><br><br>                                    
  &nbsp &nbsp &nbsp &nbsp &nbsp <a href="', mlb.par@year, '_ss.html">SS</a>              
  &nbsp &nbsp &nbsp &nbsp <a href="', mlb.par@year, '_2b.html">2B</a><br><br>            
  &nbsp &nbsp &nbsp <a href="', mlb.par@year, '_3b.html">3B</a>                          
  &nbsp &nbsp &nbsp <a href="', mlb.par@year, '_p.html">P</a>                            
  &nbsp &nbsp &nbsp <a href="', mlb.par@year, '_1b.html">1B</a><br><br><br>              
  &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp <a                       
href="', mlb.par@year, '_c.html">C</a><br><br>                                          
  &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp  
&nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp   
&nbsp <a href="', mlb.par@year, '_dh.html">DH</a><br><br>                               
  <br>                                                                      
  <br>                                                                      
  <a href="', mlb.par@year, '_allpos.html">All Positions</a>                              
  <br>                                                                      
  <br>                                                                      
  <br>                                                                      
  <br>                                                                      
  <br>                                                                      
  <br>                                                                      
  <br>          
  <br>', sep="", file=filename)
}
  
  