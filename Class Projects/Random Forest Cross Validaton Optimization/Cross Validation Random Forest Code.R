#Import libraries
library(FrF2)
library(AlgDesign)
library(corrplot)
library(car)


#Load Data
source("CrossValidation_RF.R")
load("diabetes.RData")



#Factorial Design, Randomized
ff1 <- FrF2(nruns = 32, nfactors = 7,
            factor.names = c("ntree", "mtry", "replace", "nodesize", "classwt",
                             "cutoff", "maxnodes"),randomize = T)
print(desnum(ff1))



#Check aliases
D <- desnum(ff1)
S <- model.matrix(~(ntree + replace + mtry + nodesize + maxnodes +
                      classwt + cutoff)^2-1, data.frame(D))

contrast.vectors.correlations.two <- cor(S)
corrplot(contrast.vectors.correlations.two, type = "full", 
         tl.col = "black", tl.srt = 90, method = "color",
         addgrid.col = "gray")



#Factors turn into numeric

ff1$ntree <- as.numeric(as.character(ff1$ntree))
ff1$mtry <- as.numeric(as.character(ff1$mtry))
ff1$replace <- as.numeric(as.character(ff1$replace))
ff1$nodesize <- as.numeric(as.character(ff1$nodesize))
ff1$classwt <- as.numeric(as.character(ff1$classwt))
ff1$cutoff <- as.numeric(as.character(ff1$cutoff))
ff1$maxnodes <- as.numeric(as.character(ff1$maxnodes))

ff1



#Replace factors as original

#ntree
ff1[,1][ff1[,1] == -1] <- 100
ff1[,1][ff1[,1] == 1] <- 1000

#mtry
ff1[,2][ff1[,2] == -1] <- 2
ff1[,2][ff1[,2] == 1] <- 6

#replace
ff1[,3][ff1[,3] == -1] <- 0
ff1[,3][ff1[,3] == 1] <- 1

#nodesize
ff1[,4][ff1[,4] == 1] <- 11
ff1[,4][ff1[,4] == -1] <- 1

#classwt
ff1[,5][ff1[,5] == -1] <- 0.5
ff1[,5][ff1[,5] == 1] <- 0.9

#cutoff
ff1[,6][ff1[,6] == -1] <- 0.2
ff1[,6][ff1[,6] == 1] <- 0.8

#maxnodes
ff1[,7][ff1[,7] == -1] <- 10
ff1[,7][ff1[,7] == 1] <- 1000



#Cross Validation
results <- cv.rf(as.data.frame(ff1), y, X)
print(results)


#Re-code the variables
results_coded <- results
results_coded

#ntree
results_coded[,1][results_coded[,1] == 100] <- -1
results_coded[,1][results_coded[,1] == 1000] <- 1

#mtry
results_coded[,2][results_coded[,2] == 2] <- -1
results_coded[,2][results_coded[,2] == 6] <- 1

#replace
results_coded[,3][results_coded[,3] == 0] <- -1
results_coded[,3][results_coded[,3] == 1] <- 1

#nodesize
results_coded[,4][results_coded[,4] == 11] <- 1
results_coded[,4][results_coded[,4] == 1] <- -1

#classwt
results_coded[,5][results_coded[,5] == 0.5] <- -1
results_coded[,5][results_coded[,5] == 0.9] <- 1

#cutoff
results_coded[,6][results_coded[,6] == 0.2] <- -1
results_coded[,6][results_coded[,6] == 0.8] <- 1

#maxnodes
results_coded[,7][results_coded[,7] == 10] <- -1
results_coded[,7][results_coded[,7] == 1000] <- 1

results_coded



#DanielPlot (un-coded results)
model <- lm(CV~(ntree+mtry+replace+nodesize+classwt+cutoff+maxnodes)^2, data = results)

DanielPlot(model)



#Model with only relevant main effects and interaction effects

model2 <- lm(CV~cutoff+maxnodes+classwt+classwt*maxnodes+cutoff*maxnodes+classwt*cutoff+replace:nodesize, data = results_coded)

anova(model2)



#first correlation plot

B <- model.matrix(~(cutoff+maxnodes+classwt+classwt*maxnodes+cutoff*maxnodes+classwt*cutoff+replace:nodesize)-1, data = results_coded)


contrast.vectors.correlations.two <- cor(B)
corrplot(contrast.vectors.correlations.two, type = "full", 
         tl.col = "black", tl.srt = 90, method = "color",
         addgrid.col = "gray")



#removed replace:nodesize because insignificant, new model

model3 <- lm(CV~cutoff+maxnodes+classwt+classwt*maxnodes+cutoff*maxnodes+classwt*cutoff, data = results)

anova(model3)



#second corrplot

C <- model.matrix(~(cutoff+maxnodes+classwt+classwt*maxnodes+cutoff*maxnodes+classwt*cutoff)-1, data = results)


contrast.vectors.correlations.two <- cor(C)
corrplot(contrast.vectors.correlations.two, type = "full", 
         tl.col = "black", tl.srt = 90, method = "color",
         addgrid.col = "gray")



vif(model3)



#check residuals + normal

plot(model3, 1:2)



#removed replace:nodesize because insignificant, new model

model4 <- lm(CV~cutoff+maxnodes+classwt+cutoff*maxnodes, data = results)

anova(model4)



#third corrplot

C <- model.matrix(~(cutoff+maxnodes+classwt+cutoff*maxnodes)-1, data = results)


contrast.vectors.correlations.two <- cor(C)
corrplot(contrast.vectors.correlations.two, type = "full", 
         tl.col = "black", tl.srt = 90, method = "color",
         addgrid.col = "gray")
vif(model4)



#check residuals + normal

plot(model4, 1:2)




tuning_par <- data.frame("ntree"=factor(c(100,1000)),"mtry"=factor(c(2,6)),"replace"=factor(c(0,1)),"nodesize"=factor(c(1,11)),"classwt"=factor(c(.5,.9)),"cutoff"=factor(c(.2,.8)),"maxnodes"=factor(c(10,1000)))

coded_tuning_par <- data.frame("ntree"=c(-1,1),"mtry"=c(-1,1),"replace"=c(-1,1),"nodesize"=c(-1,1),"classwt"=c(-1,1),"cutoff"=c(-1,1),"maxnodes"=c(-1,1))



set.seed(0)
des_matrix <- BHH2::ffDesMatrix(7)
des_matrix <- data.frame(des_matrix)
names(des_matrix) <- c("ntree","mtry","replace","nodesize","classwt","cutoff","maxnodes")
des_matrix[[1]][des_matrix[[1]] == -1] <- 100
des_matrix[[1]][des_matrix[[1]] == 1] <- 1000
des_matrix[[2]][des_matrix[[2]] == -1] <- 2
des_matrix[[2]][des_matrix[[2]] == 1] <- 6
des_matrix[[3]][des_matrix[[3]] == -1] <- 0
des_matrix[[3]][des_matrix[[3]] == 1] <- 1
des_matrix[[4]][des_matrix[[4]] == 1] <- 11
des_matrix[[4]][des_matrix[[4]] == -1] <- 1
des_matrix[[5]][des_matrix[[5]] == -1] <- .5
des_matrix[[5]][des_matrix[[5]] == 1] <- .9
des_matrix[[6]][des_matrix[[6]] == -1] <- .2
des_matrix[[6]][des_matrix[[6]] == 1] <- .8
des_matrix[[7]][des_matrix[[7]] == -1] <- 10
des_matrix[[7]][des_matrix[[7]] == 1] <- 1000



opt_design <- AlgDesign::optFederov(~ntree+mtry+replace+nodesize+classwt+cutoff+maxnodes,des_matrix,nTrials=35,nRepeats=1000)
opt_design$design

cv_opt_design <- cv.rf(opt_design$design, y, X)

opt_mat <- model.matrix(~(ntree+mtry+replace+nodesize+classwt+cutoff+maxnodes)^2-1,data.frame(opt_design$design))
corrplot::corrplot(cor(opt_mat),type="full",addgrid.col="gray",tl.col="black",tl.srt=90,method="color")

coded_cv_opt_design <- cbind(cv_opt_design)
for (i in 1:7) {
  coded_cv_opt_design[[i]][cv_opt_design[[i]]>mean(cv_opt_design[[i]])] <- 1
  coded_cv_opt_design[[i]][cv_opt_design[[i]]<mean(cv_opt_design[[i]])] <- -1
}

cv_opt_lm_1 <- lm(CV~(ntree+mtry+replace+nodesize+classwt+cutoff+maxnodes)^2,data=coded_cv_opt_design)
anova(cv_opt_lm_1)
plot(cv_opt_lm_1, 1:2)
cv_opt_lm_2 <- lm(CV~classwt+cutoff+maxnodes+ntree:nodesize+mtry:nodesize+replace:nodesize+nodesize:classwt+classwt:cutoff+classwt:maxnodes,data=coded_cv_opt_design)
anova(cv_opt_lm_2)
plot(cv_opt_lm_2, 1:2)
cv_opt_lm_3 <- lm(CV~classwt+cutoff+maxnodes+ntree:nodesize+mtry:nodesize+nodesize:classwt+classwt:cutoff+classwt:maxnodes,data=coded_cv_opt_design)
anova(cv_opt_lm_3)
plot(cv_opt_lm_3, 1:2)

opt_mat_2 <- model.matrix(~(classwt+cutoff+maxnodes+ntree:nodesize+mtry:nodesize+nodesize:classwt+classwt:cutoff+classwt:maxnodes)-1,data.frame(opt_design$design))
corrplot::corrplot(cor(opt_mat_2),type="full",addgrid.col="gray",tl.col="black",tl.srt=90,method="color")

cv_opt_lm_4 <- lm(CV~classwt+cutoff+maxnodes+ntree:nodesize+nodesize:mtry+classwt:nodesize,data=coded_cv_opt_design)
anova(cv_opt_lm_4)
plot(cv_opt_lm_4, 1:2)
summary(cv_opt_lm_4)



