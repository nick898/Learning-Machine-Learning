YearsWithRedSox <- function(df,MinYear,MinAB){
  # This function will find what players have stayed with a single team for a minimum number of years (MinYear)
  # and have a minimum number of at bats (MinAB)
  #
  #
  # INPUTS:
  #         1. 'df' - data frame of players from a single team 
  #         2. 'MinYear' - the minimum number of years the player is with the team
  #         3. 'MinAB' - minimum number of at bats (used to remove pitchers)
  #
  # OUTPUTS:
  #         1. character vector of player IDs have been on team for 'MinYear' years who have accumulated
  #            'MinAB' at bats
  #
  
  R = nrow(df)
  C = ncol(df)
  P = c()
  
  
  for (i in 1:R){
    
    Player = df[i,1]
    if (sum(df$playerID == Player) >= MinYear){
      P[i] = Player
    }
    else{
      P = P
    }
    
  }
  P = P[!is.na(P)]
  Rp = length(P)
  PlayerBat = c()
  
  for (i in 1:Rp){
    Player = P[i]
    if (sum(df[df$playerID == Player,]$AB, na.rm = TRUE) > MinAB){
      PlayerBat[i] = Player
    }
    else{
      PlayerBat = PlayerBat
    }
    
  }
 
  PlayerBat = PlayerBat[!is.na(PlayerBat)]
  
  PlayerBat = PlayerBat[which(!duplicated(PlayerBat))]
  
  
}