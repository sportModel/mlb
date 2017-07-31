ip2outs <- function(ip)
  {
    whole <- floor(ip)
    remainder <- ceiling(2*(ip-whole))
    val <- 3*whole + remainder
    val
  }
