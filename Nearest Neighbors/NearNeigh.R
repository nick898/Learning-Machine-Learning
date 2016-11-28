NearNeigh = function(pid, NumN, df){
# This function takes as input a player ID, the number of neighbors, and a data frame consisting of 
# player season statistics. Using this data it calculates the similarity of each player in the data set
# to the given player specified by the player ID. 
#
# The output is a data frame of players who are most similar to the given player. The number of neighbors 
# input 'NumN' specifies exactly how many similar players to find
#
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