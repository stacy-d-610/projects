#importing libraries and data
test <- read.csv("CBDtestNoY.csv")
train <- read.csv("CBDtrain.csv")
library(ggplot2)
library(corrplot)
library(GGally)
library(readr)
library(car)
library(gridExtra) 
library(dplyr)
library(leaps)


#Checking the data
head(train)


#DIAGPLOT CODE
theme(plot.title = element_text(hjust = 0.5)) 
diagPlot<-function(model){  
  p1<-ggplot(model, aes(model$fitted, model$residuals),label=rownames(bonds))+geom_point() 
  p1<-p1+stat_smooth(method="loess")+geom_hline(yintercept=0, col="red", linetype="dashed") 
  p1<-p1+xlab("Fitted values")+ylab("Residuals") 
  p1<-p1+ggtitle("Residual vs Fitted Plot")+theme_bw() 
  
  p2<-ggplot(model,aes(sample=rstandard(model))) + stat_qq() + stat_qq_line() 
  p2<-p2+xlab("Theoretical Quantiles")+ylab("Standardized Residuals") 
  p2<-p2+ggtitle("Normal Q-Q") 
  
  p3<-ggplot(model, aes(model$fitted, sqrt(abs(rstandard(model)))))+geom_point(na.rm=TRUE) 
  p3<-p3+stat_smooth(method="loess", na.rm = TRUE)+xlab("Fitted Value") 
  p3<-p3+ylab(expression(sqrt("|Standardized residuals|"))) 
  p3<-p3+ggtitle("Scale-Location")+theme_bw()+geom_hline(yintercept=sqrt(2), col="red", linetype="dashed") 
  
  p4<-ggplot(model, aes(seq_along(cooks.distance(model)), cooks.distance(model)))+geom_bar(stat="identity", position="identity") 
  p4<-p4+xlab("Obs. Number")+ylab("Cook's distance") 
  p4<-p4+ggtitle("Cook's distance")+theme_bw()+geom_hline(yintercept=4/(length(model$residuals-2)), col="red", linetype="dashed") 
  p5<-ggplot(model, aes(hatvalues(model), rstandard(model)))+geom_point(aes(size=cooks.distance(model)), na.rm=TRUE) 
  p5<-p5+stat_smooth(method="loess", na.rm=TRUE) 
  p5<-p5+xlab("Leverage")+ylab("Standardized Residuals") 
  p5<-p5+ggtitle("Residual vs Leverage Plot") 
  p5<-p5+scale_size_continuous("Cook's Distance", range=c(1,5)) 
  p5<-p5+theme_bw()+theme(legend.position="bottom")+geom_hline(yintercept=c(-2,2), col="red",linetype="dashed")+geom_vline(xintercept=22/(length(model$residuals)),col="blue",linetype="dashed")+ylim(-4,4) 
  
  p6<-ggplot(model, aes(hatvalues(model), cooks.distance(model)))+geom_point(na.rm=TRUE)+stat_smooth(method="loess", na.rm=TRUE) 
  p6<-p6+xlab("Leverage hii")+ylab("Cook's Distance") 
  p6<-p6+ggtitle("Cook's dist vs Leverage") 
  p6<-p6+geom_abline(slope=seq(0,3,0.5), color="gray", linetype="dashed") 
  p6<-p6+theme_bw() 
  return(grid.arrange(p1,p2,p3,p4,p5,p6,ncol=3)) 
}


#All variable model
initial <- lm(W.P ~ X500.Level + ADJOE + ADJDE + EFG_O + EFG_D + TOR+ TORD + ORB + DRB + FTR +FTRD + X2P_O + X2P_D + X3P_O
              + X3P_D + WAB + YEAR + NCAA +Adjusted.Tempo + Power.Rating, data = train)



#Checking the response variable
#Table 2
summary(train$W.P)

hist(train$W.P, main = "Histogram of Winning Proportions", xlab = "Winning Proportions")


#Check if y needs to be transformed
#Table 3
powerTransform(cbind(W.P)~1, data = train)



#Confirm degree of regression
mmp(initial, main = "Marginal Model Plot of the Data")



#Backwards stepwise model (AIC)
step(initial, direction = "backward", data = train, k = 2) #table 4



#Model after stepwise
model <- lm(formula = W.P ~ X500.Level + ADJOE + ADJDE + EFG_O + EFG_D + TOR + TORD + ORB + DRB + FTR + FTRD + X2P_O + X3P_O + X3P_D + WAB + Adjusted.Tempo + Power.Rating, data = train)

#Table 5
summary(model)



#Model after eliminating non-statistically significant
model <- lm(W.P ~ X500.Level + ADJOE + ADJDE + EFG_O + EFG_D + TOR + TORD + ORB + DRB + FTR + FTRD + X2P_O + WAB + Adjusted.Tempo, data = train)

#Table 6
summary(model)


#---------------------------------- 
#Unused model 

#Table 7
#anova(model1) 

#anova(model1, model)
#---------------------------------- 

#Table 8
#Regsubset (BIC)
out <- regsubsets(W.P ~ X500.Level + ADJOE + ADJDE + EFG_O + EFG_D + TOR + TORD + ORB + DRB + FTR + FTRD + X2P_O + WAB + Adjusted.Tempo, data = train, method = "exhaustive")

plot(out, main = "BIC Graph of Model 2")



#After regsubset
model2 <- lm(W.P ~ X500.Level + ADJDE + EFG_O + EFG_D + TORD + DRB + FTRD + WAB, data = train)

#Table 9
summary(model2)

diagPlot(model2)



#Check if transformations needed on predictors
#Table 10
summary(powerTransform(cbind(ADJDE, EFG_O, EFG_D, TORD, DRB, FTRD)~1, data = train)) 



#Applying transformations
traintf <- train %>%
  mutate(ADJDE2 = ADJDE^1.25) %>%
  mutate(EFG_O2 = EFG_O^(1/2)) %>%
  mutate(EFG_D2 = EFG_D^(1/2)) %>%
  mutate(TORD2 = TOR^(1/3))

model2_tf <- lm(W.P ~ X500.Level + ADJDE2 + EFG_O2 + EFG_D2 + TORD2 + DRB + FTRD + WAB, data = traintf)

#Table 11
summary(model2_tf)

inverseResponsePlot(model2)



#Check VIF
#Table 12
vif(model2) 



#Fixing model by taking out high VIF
#Taking out WAB
model2_1 <- lm(W.P ~ X500.Level + ADJDE + EFG_O + EFG_D + TORD + DRB + FTRD, data = train)

#table 13.1
summary(model2_1)
#13.2
anova(model2_1, model2)

#Taking out ADJDE
model2_2 <- lm(W.P ~ X500.Level + EFG_O + EFG_D + TORD + DRB + FTRD + WAB, data = train)

#14.1
summary(model2_2)
#14.2
anova(model2_2, model2)



#Correlation matrix for interaction terms
x <- cor(train[c(4, 5, 6, 8, 10, 12, 17)], use = "pairwise.complete.obs")
corrplot(x, method = "number")



#Combining variables with dplyr
#Combining by adding
train2 <- train %>%
  mutate(efficiency = ADJDE+WAB)

model2_comb <- lm(W.P ~ X500.Level + efficiency + EFG_O + EFG_D + TORD + DRB + FTRD, data = train2)

#Table 15.1
summary(model2_comb) 

#Combining by subtracting
train2 <- train %>%
  mutate(efficiency = ADJDE-WAB)

model2_comb <- lm(W.P ~ X500.Level + efficiency + EFG_O + EFG_D + TORD + DRB + FTRD, data = train2)

#15.2
summary(model2_comb)

#Combining by multiplying
train2 <- train %>%
  mutate(efficiency = ADJDE*WAB)

model2_comb <- lm(W.P ~ X500.Level + efficiency + EFG_O + EFG_D + TORD + DRB + FTRD, data = train2)

#15.3
summary(model2_comb) 



#Leverage plot
leveragePlots(model2)



#Interaction plot
interaction.plot(train$X500.Level, train$Power.Rating, train$W.P)

#model_added <- lm(W.P ~ X500.Level + ADJDE + EFG_O + EFG_D + TORD + DRB + FTRD + WAB + Power.Rating, data = train)

#train$X500.Level <- as.factor(train$X500.Level)
#train$Power.Rating <- as.factor(train$Power.Rating)
#interaction.plot(model_added)



#ggpairs plot
ggpairs(train[,c(4, 5, 6, 8, 10, 12, 17)], aes(color= train$X500.Level, alpha =0.5))



#Interaction terms

model_inter1 <- lm(W.P ~ X500.Level * (ADJDE + EFG_O + EFG_D + TORD + DRB + FTRD + WAB), data = train)

#Table 16.1
summary(model_inter1)

model_inter2 <- lm(W.P ~ X500.Level * (ADJDE + EFG_D + TORD + DRB) + EFG_O + FTRD + WAB, data = train)

#model_inter <- lm(W.P ~ X500.Level * (ADJDE * WAB + DRB) + TORD + EFG_O + EFG_D + FTRD, data = train)

#16.2
summary(model_inter2)



#REAL MODEL
model_final <- lm(W.P ~ X500.Level * (ADJDE + DRB) + TORD + EFG_O + EFG_D + FTRD + WAB, data = train)

summary(model_final)





# Why we didn't choose the other model
x <- lm(W.P ~ X500.Level + ADJDE + ADJOE + EFG_O + EFG_D + TOR + TORD + ORB + DRB + FTRD + WAB, data = train)
extractAIC(hello, k = log(n))
extractAIC(x, k = log(n))
