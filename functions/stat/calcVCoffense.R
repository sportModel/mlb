calcVCoffense <- function(mlb.raw) {
  batting.p <- makeBattingPos(mlb.raw,"p")
  batting.c <- makeBattingPos(mlb.raw,"c")
  batting.1b <- makeBattingPos(mlb.raw,"1b")
  batting.2b <- makeBattingPos(mlb.raw,"2b")
  batting.3b <- makeBattingPos(mlb.raw,"3b")
  batting.ss <- makeBattingPos(mlb.raw,"ss")
  batting.lf <- makeBattingPos(mlb.raw,"lf")
  batting.cf <- makeBattingPos(mlb.raw,"cf")
  batting.rf <- makeBattingPos(mlb.raw,"rf")
  batting.dh <- makeBattingDH(mlb.raw)
  vc.p <- calcVCbatting(batting.p)
  vc.c <- calcVCbatting(batting.c)
  vc.1b <- calcVCbatting(rbind(batting.1b,batting.dh))
  vc.2b <- calcVCbatting(batting.2b)
  vc.3b <- calcVCbatting(batting.3b)
  vc.ss <- calcVCbatting(batting.ss)
  vc.lf <- calcVCbatting(batting.lf)
  vc.cf <- calcVCbatting(batting.cf)
  vc.rf <- calcVCbatting(batting.rf)
  val <- rbind(vc.p,
               vc.c,
               vc.1b,
               vc.2b,
               vc.3b,
               vc.ss,
               vc.lf,
               vc.cf,
               vc.rf)
  val <- combinePositions(val)
  val
}
