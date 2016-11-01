ComputeOPS = function(PlayerIDs,df){
  # This function takes a character vector of player IDs and a data frame of players from a single team
  #
  #
  # INPUTS:
  #         1. 'PlayerIDs' - character vector of player IDs
  #         2. 'df' - data frame of players from one team
  #
  # OUTPUTS:
  #         1. data frame consisting of the OPS of all the players in the PlayerIDs vector
  #
  #
  
  

  c.OPS = 0;
  pID = "RemoveMe"
  FinalDF = data.frame(playerID = pID, OPS = c.OPS)
  
  for (i in 1:length(PlayerIDs)){

    d = df[df$playerID == PlayerIDs[i],]
    c.S = sum(d$H - d$X2B - d$X3B - d$HR, na.rm = TRUE)
    c.X2B = sum(d$X2B, na.rm = TRUE)
    c.X3B = sum(d$X3B, na.rm = TRUE)
    c.HR = sum(d$HR, na.rm = TRUE)
    c.BB = sum(d$BB, na.rm = TRUE)
    c.HBP = sum(d$HBP, na.rm = TRUE)
    c.SF = sum(d$SF, na.rm = TRUE)
    c.AB = sum(d$AB, na.rm = TRUE)
    c.OBP = (c.S + c.X2B + c.X3B + c.HR + c.BB + c.HBP)/(c.AB + c.BB + c.SF + c.HBP)
    c.TB = c.S + 2*c.X2B + 3*c.X3B + 4*c.HR
    c.SLG = c.TB/c.AB
    c.OPS = c.SLG + c.OBP;
    
    TempDF = data.frame(playerID = PlayerIDs[i], OPS = c.OPS)
    FinalDF = rbind(FinalDF, TempDF)
    
  }

  FinalDF = FinalDF[2:nrow(FinalDF),]
}