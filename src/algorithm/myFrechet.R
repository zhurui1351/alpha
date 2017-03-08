frechet_diss = function (x, y, ...) 
{
  abscissex = 1:length(x)
  abscissey = 1:length(y)
  distFrechet_bak(abscissex, x, abscissey, y, ...)
}

distFrechet_bak = function (Px, Py, Qx, Qy, timeScale = 0.1, FrechetSumOrMax = "max") 
{
  missingsP <- is.na(Px) | is.na(Py)
  Px <- Px[!missingsP]
  Py <- Py[!missingsP]
  missingsQ <- is.na(Qx) | is.na(Qy)
  Qx <- Qx[!missingsQ]
  Qy <- Qy[!missingsQ]
  Px <- Px * timeScale
  Qx <- Qx * timeScale
  maxP <- length(Px)
  maxQ <- length(Qx)
  Mdist <- Mfret <- matrix(0, maxP, maxQ, dimnames = c(list(paste("P", 
                                                                  1:maxP, sep = ""), paste("Q", 1:maxQ, sep = ""))))
  for (i in 1:maxP) {
    for (j in 1:maxQ) {
      Mdist[i, j] <- dist(rbind(c(Px[i], Py[i]), c(Qx[j], 
                                                   Qy[j])))
      if (i == 1 && j == 1) {
        Mfret[1, 1] = Mdist[1, 1]
      }
      if (i > 1 && j == 1) {
        Mfret[i, 1] = do.call(FrechetSumOrMax, list(Mfret[i - 
                                                            1, 1], Mdist[i, 1]))
      }
      if (i == 1 && j > 1) {
        Mfret[1, j] = do.call(FrechetSumOrMax, list(Mfret[1, 
                                                          j - 1], Mdist[1, j]))
      }
      if (i > 1 && j > 1) {
        Mfret[i, j] = do.call(FrechetSumOrMax, list(min(Mfret[i - 
                                                                1, j], Mfret[i - 1, j - 1], Mfret[i, j - 1]), 
                                                    Mdist[i, j]))
      }
    }
  }
  return(Mfret[maxP, maxQ])
}