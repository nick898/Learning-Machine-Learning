TeamNames = function(df, TeamIDN){
  
  N1 = length(df)
  N2 = nrow(TeamIDN)
  Output = rep(0,30)
  for (i in 1:N1){
    for (j in 1:N2){
      if (TeamIDN$TeamID[j] == df[i]){
        
        Output[i] = TeamIDN$TeamName[j]
        
      }
    }
    
    
  }
  Output
  
}