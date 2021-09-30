sigma_ewma <- function(pvProductDf, pLogReturns, plambda) {
  Np <-  ncol(pvProductDf)
  EWMA_NumCol <-  sum(1:ncol(pLogReturns))
  EWMA_NumRow <- nrow(pvProductDf)
  EWMA <- matrix(rep(0, EWMA_NumCol * EWMA_NumRow), nrow = EWMA_NumRow, ncol = EWMA_NumCol)
  for (i in 2:EWMA_NumRow){
    for (k in 1:Np){
      EWMA[i,k] <- plambda * EWMA[i - 1,k] + (1 - plambda) * (pLogReturns[i - 1,k] ^ 2)
    }
    if (Np > 1) {
      k <- Np + 1
      for (x in 2:Np - 1) {
        for (y in (x + 1):Np) {
          if (y > x) {
            EWMA[i,k] <- plambda * EWMA[i - 1, k] + (1 - plambda) * pLogReturns[i - 1,x] * pLogReturns[i - 1,y]
            k <- k + 1
          }
        }
      }
    }
  }
  
  EWMALastRow <- EWMA[nrow(EWMA),]
  
  Sigma <- matrix(rep(0, Np * Np), nrow = Np, ncol = Np, byrow = TRUE)
  
  for(i in 1:Np){
    for(j in 1:Np){
      if(i==j){
        Sigma[i,j] <- EWMALastRow[i]
      }
    }
  }

  if(Np>1){
    k <- Np+1
    for(i in 1:(Np-1)){
      for(j in (i+1):Np){
        Sigma[i,j] <- EWMALastRow[k]
        Sigma[j,i] <- EWMALastRow[k]
        k <- k + 1
      }
    }
  }
  
  return(Sigma)
}