setClass("mlbVC",representation(batting="data.frame",
                                pitching="data.frame",
                                fielding="data.frame"))

show.mlbVC <- function(object)
  {
    print("show not enabled yet")
  }

setMethod("show","mlbVC",function(object) show.mlbVC(object))
