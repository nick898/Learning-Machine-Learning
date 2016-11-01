TeamAVGOPS = function(d){
# This function takes a data frame consisting of players and computes the average, OPS, and runs scored. Can
# be combined with 'ddply' function in 'plyr' package to compute AVG, OPS, and R by factors such as playerID
# and/or yearID
#
# INPUTS: 
#       1. 'd' - data frame
#
# OUTPUTS:
#       1. data frame consisting of batting average, OPS, and runs scored
#
#
  
    
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
  c.AVG = (c.S + c.X2B + c.X3B + c.HR)/sum(d$AB, na.rm = TRUE)
  c.R = sum(d$R, na.rm = TRUE)
  
  data.frame(AVG = c.AVG, OPS = c.OPS, R = c.R)
}