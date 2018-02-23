setwd("/Users/michaelbostwick/Documents/Galapagos")

library(tidyverse)
library(ggplot2)
library(readxl)
library(glmnet)
library(mpath)

load("BASES_CENSO_UPA_spss/clean_data.RData")

vars <-  read_excel("Variables.xlsx", sheet = "vars", range = "C2:I242")

production_include <- vars[which(is.na(vars$`UPA Production`)),1]$`Variable Name`
production_x <- subset(reduced_data, select = production_include)
#production_x_num <- as.matrix(production_x[sapply(production_x, function(x) !is.factor(x))])
production_x <- model.matrix(~., production_x)[,-1]

fit <- lm(productivity ~ . , data = subset(reduced_data, select = production_include))

fit = glmnet(production_x, reduced_data$productivity)
plot(fit)  

reduced_data$log_productivity <- log(reduced_data$productivity+1)

foldid=sample(1:10,size=length(reduced_data$log_productivity),replace=TRUE)
cv1=cv.glmnet(production_x,reduced_data$log_productivity,foldid=foldid,alpha=1)
cv.5=cv.glmnet(production_x,reduced_data$log_productivity,foldid=foldid,alpha=.5)
cv0=cv.glmnet(production_x,reduced_data$log_productivity,foldid=foldid,alpha=0)


par(mfrow=c(2,2))
plot(cv1,main="Alpha = 1");plot(cv.5,main="Alpha = 0.5");plot(cv0,main="Alpha = 0");plot()
#plot(log(cv1$lambda),cv1$cvm,pch=19,col="red",xlab="log(Lambda)",ylab=cv1$name,  main="Alpha = 1")
#points(log(cv.5$lambda),cv.5$cvm,pch=19,col="grey", main="Alpha = 0.5")
#points(log(cv0$lambda),cv0$cvm,pch=19,col="blue", main="Alpha = 0")
#legend("bottomleft",legend=c("alpha= 1","alpha= .5","alpha 0"),pch=19,col=c("red","grey","blue"))
par(mfrow=c(1,1))

lasso.coef=predict(cv1,type="coefficients",s=cv1$lambda.1se)[1:240,]
lasso.coef[lasso.coef!=0]

#model fulltime workers as poisson? overdispersion? negative binomial?
hist(reduced_data$fulltimework)
summary(reduced_data$fulltimework[reduced_data$fulltimework>0])
mean(reduced_data$fulltimework[reduced_data$fulltimework>0])


workers_include <- vars[which(is.na(vars$`Number workers supported`)),1]$`Variable Name`
workers_x <- subset(reduced_data, select =workers_include)
workers_x <- model.matrix(~., workers_x)[,2:20]

reduced_data$fulltimework <- as.integer(reduced_data$fulltimework)

workers.poisson <-  cv.glmnet(workers_x, reduced_data$fulltimework, family = "poisson", alpha = 0.5)
plot(workers.poisson)

workers <- subset(reduced_data, select =c(workers_include,"fulltimework"))
hist(workers$fulltimework)
workers$fulltimework <- as.integer(workers$fulltimework)

NB.fit <- cv.glmregNB(fulltimework ~ ., alpha=1, lambda=seq(0.01, 1, by=0.1), data = workers)
