ComputeFIPC = function(df){
  
  lgERA = sum(df$ER, na.rm = TRUE)/sum(df$IP, na.rm = TRUE)*9
  df$lgERA = lgERA
  
  lgHR = sum(df$HR,na.rm = TRUE)
  lgBB = sum(df$BB,na.rm = TRUE)
  lgHBP = sum(df$HBP,na.rm = TRUE)
  lgSO = sum(df$SO,na.rm = TRUE)
  lgIP = sum(df$IP,na.rm = TRUE)
  
  FIPC = lgERA - (13*lgHR + 3*(lgBB + lgHBP) - 2*lgSO)/(lgIP)
  
  df$FIPC = FIPC
  
  df
  
}