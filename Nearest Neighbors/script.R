library(Lahman)

Age = 30
pid = "ortizda01"
NumNeighbors = 10

M = Master
M2 = data.frame(M$playerID,M$nameFirst,M$nameLast)
colnames(M2) = c("playerID", "nameFirst", "nameLast")



BatAge = AgeEqPlayer(Age,Bat)
NearestNeighbors = NearNeigh(pid, NumNeighbors, BatAge)

NearestNeighbors = merge(M2,NearestNeighbors)
