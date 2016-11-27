NearNeigh = function(pid, NumN, df){
  
  Row = which(df$playerID == pid)
  Object = df[Row,]
  Data = df[-c(Row),]
  C = ncol(Data)
  
  R = nrow(Data)
  Data$dist = rep(0,R)
  
  pb <- txtProgressBar(min = 0, max = R, style = 3)
  for (i in 1:R){
    
    Distance = dist(rbind(Data[i,2:C],Object[,2:C]))
    Data$dist[i] = Distance[1]
    setTxtProgressBar(pb, i)
    
  }
  
  close(pb) 
  TopNeighbors = Data[order(Data$dist),]
  TopNeighbors[1:NumN,]
}