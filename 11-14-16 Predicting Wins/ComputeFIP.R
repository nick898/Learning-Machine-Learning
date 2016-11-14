ComputeFIP = function(df){
  
  HR = sum(df$HR, na.rm = TRUE)
  BB = sum(df$BB, na.rm = TRUE)
  SO = sum(df$SO, na.rm = TRUE)
  IP = sum(df$IP, na.rm = TRUE)
  HBP = sum(df$HBP, na.rm = TRUE)
  
  
  
    
  FIP = (13*HR + 3*(BB + HBP) - 2*SO)/(IP) + df$FIPC[1]
  FIP  
}