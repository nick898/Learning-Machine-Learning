#LINEAR MODEL WITH ONE VARIABLE

# par(mfrow= c(2,3))
# plot(W~FIP,data = Data) + abline(Coeff9, col = "red") + title(main = "FIP vs. W", sub = "Adj. R-squared: 0.11", col.sub = "red", cex.sub = 1.5, cex.main = 1.5) 
# plot(W~OPS,data = Data) + abline(Coeff10, col = "red") + title(main = "OPS vs. W", sub = "Adj. R-squared: 0.18", col.sub = "red", cex.sub = 1.5, cex.main = 1.5) 
# plot(W~DER,data = Data) + abline(Coeff11, col = "red") + title(main = "DE vs. W", sub = "Adj. R-squared: 0.08", col.sub = "red", cex.sub = 1.5, cex.main = 1.5) 
# 
# plot(W~ERA,data = Databasic) + abline(CoeffB9, col = "red") + title(main = "ERA vs. W", sub = "Adj. R-squared: 0.23", col.sub = "red", cex.sub = 1.5, cex.main = 1.5) 
# plot(W~AVG,data = Databasic) + abline(CoeffB10, col = "red") + title(main = "OPS vs. W", sub = "Adj. R-squared: 0.13", col.sub = "red", cex.sub = 1.5, cex.main = 1.5) 
# plot(W~FP,data = Databasic) + abline(CoeffB11, col = "red") + title(main = "FP vs. W", sub = "Adj. R-squared: 0.05", col.sub = "red", cex.sub = 1.5, cex.main = 1.5) 

#Residuals of FIP vs W Linear Regression

# plot(fitted(Coeff9), rstandard(Coeff9), xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted Values (in FIP vs. W Regression)", col = ifelse(rstandard(Coeff9) > 2 | rstandard(Coeff9) < -2 ,"red", "black"), sub = "Points in red lie above/below the blue lines", col.sub = "red") + 
#   abline(a = 2, b = 0, col = "blue") + 
#   abline(a = 0,b = 0, col = "orange") +
#   abline(a = -2, b = 0, col = "blue") + 
#   legend('topright', legend = c("y = 2","y = 0","y = -2"), lty = 1,col = c('blue', 'orange', 'blue'))

