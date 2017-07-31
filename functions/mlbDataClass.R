setClass("mlbData",representation(batting="data.frame",
                                  pitching="data.frame",
                                  fielding="data.frame"))

show.mlbData <- function(object)
  {
    cat("@batting : ",nrow(object@batting)," rows, ",ncol(object@batting)," columns\n",sep="")
    cat("@pitching: ",nrow(object@pitching)," rows, ",ncol(object@pitching)," columns\n",sep="")
    cat("@fielding: ",nrow(object@fielding)," rows, ",ncol(object@fielding)," columns\n",sep="")
  }

setMethod("show","mlbData",function(object) show.mlbData(object))
