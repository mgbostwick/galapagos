setwd("/Users/michaelbostwick/Documents/Galapagos")

library(readxl)

# Load clean data and subset to appropriate variables for this analysis
load("BASES_CENSO_UPA_spss/clean_data.RData")
vars <-  read_excel("Variables.xlsx", sheet = "vars", range = "C2:I241")
production_include <- vars[which(is.na(vars$`UPA Production`)),1]$`Variable Name`
production_x.df <- subset(reduced_data[reduced_data$productivity > 0,], select = production_include)

# Perform log transformation
log_productivity <- log10(reduced_data[reduced_data$productivity > 0, 'productivity'])

elastic5 <-  production_x.df[,c("pc4", "percpasture", "percperm","v30_a","pc6")]
elastic5.mat <- model.matrix(~.,data=elastic5)[,-1]
cor(elastic5.mat[,-1])  

forward5 <- production_x.df[,c("canton", "percinv", "percperm2","percbrush","CPermanentes_PAPAYA")]
forward5.mat <- model.matrix(~.,data=forward5)[,-1]
cor(forward5.mat[,-c(2,3)])  


var.names <- c("PastureNo","PercPasture","PercPerm","GrassFeed","PastureSurface",
               "SanCristobal","PercInv","PercPerm2","PercBrush","Papaya")
keep_vars1 <- c("pc4None","percperm","pc6", "cantonSan Cristobal",  
                "percinv","percperm2","percbrush","CPermanentes_PAPAYA")
var.names1 <- c("PastureNo","PercPerm","PastureSurface",
                "SanCristobal","PercInv","PercPerm2","PercBrush","Papaya")
keep_vars2 <- c("percpasture", "v30_a")
var.names2 <- c("PercPasture","GrassFeed")

all5 <- production_x.df[,c("pc4", "percpasture", "percperm","v30_a","pc6",
                           "canton", "percinv", "percperm2","percbrush","CPermanentes_PAPAYA")]
all5.mat <- model.matrix(~.,data=all5)[,-1]
remove_vars <- c("pc4Solo","cantonSanta Cruz", "cantonFloreana")
keep_vars <- c("pc4None","percpasture", "percperm", "v30_a","pc6", "cantonSan Cristobal",  
               "percinv","percperm2","percbrush","CPermanentes_PAPAYA")
all5.mat.reduced <- all5.mat[,!colnames(all5.mat) %in% remove_vars]
colnames(all5.mat.reduced) <- var.names
cors <- cor(all5.mat.reduced)  




library(corrplot)
pdf("Paper/images/corplot.pdf",width=10,height=10)
corrplot(cors, method = "number", type="upper",
         tl.col = c(rep("darkgreen",5),rep("burlywood4",5)), tl.srt =45, bg=NA)
dev.off()

var.names <- c("PastureNo","PercPasture","PercPerm","GrassFeed","PastureSurface",
               "SanCristobal","PercInv","PercPerm2","PercBrush","Papaya")
keep_vars1 <- c("pc4None","percperm","pc6", "cantonSan Cristobal",  
               "percinv","percperm2","percbrush","CPermanentes_PAPAYA")
var.names1 <- c("PastureNo","PercPerm","PastureSurface",
               "SanCristobal","PercInv","PercPerm2","PercBrush","Papaya")
keep_vars2 <- c("percpasture", "v30_a")
var.names2 <- c("PercPasture","GrassFeed")

cor(forward5.mat, log_productivity)

library(pls)
set.seed(123)
out <- plsr(log_productivity~., data = all5,scale=TRUE,validation="CV")
summary(out)
pdf("Paper/images/plsplot1.pdf",width=10,height=6)
validationplot(out,val.type="MSEP", ylab = "MSE", main = "Partial Least Squares")
dev.off()
plot(out, "loadings", comps = 1:2, legendpos = "topleft")
pdf("Paper/images/plsplot2.pdf",width=10,height=6)
plot(out$projection,type="n")
text(out$projection[keep_vars1,], pos = 2, labels = var.names1, col=c(rep("darkgreen",3),rep("burlywood4",5)), cex=1.1)
text(out$projection[keep_vars2,], pos = 4, labels = var.names2, col="darkgreen",cex=1.1)
dev.off()

library(glmnet)
library(plotmo)
set.seed(1)
foldid=sample(1:10,size=length(log_productivity),replace=TRUE)
cv1=cv.glmnet(elastic5.mat,log_productivity,family="gaussian",foldid=foldid,alpha=1)
scaled.dat <- scale(elastic5.mat)[,-1]
colnames(scaled.dat) <- c("PastureNo","PercPasture","PercPerm","GrassFeed","PastureSurface")
elasticfit.1 <- glmnet(scaled.dat,log_productivity,family="gaussian",alpha=0.1)
elasticfit1 <- glmnet(scaled.dat,log_productivity,family="gaussian",alpha=1)
#plot(elasticfit,xvar = "lambda", label = TRUE)
pdf("Paper/images/elasticplot1.pdf",width=10,height=6)
plot_glmnet(elasticfit1)
dev.off()
pdf("Paper/images/elasticplot2.pdf",width=10,height=6)
plot_glmnet(elasticfit.1)
dev.off()

library(leaps)
best.fwd <- regsubsets(log_productivity~., data=production_x.df,nvmax=min(100,nrow(production_x.df)),method = "forward")
best.fwd.summary <- summary(best.fwd)
best.fwd$vorder
names(production_x.df[, best.fwd$vorder[1:5]])

x.matrix <- model.matrix(~., production_x.df)[,-1]                          

fwd.coefs5 <- colnames(x.matrix)[best.fwd$vorder[1:5]]
plot(best.fwd.summary$bic ,xlab="Number of Variables ",ylab="BIC", type='l',
     xlim=c(length(best.fwd.summary$bic),0), cex.axis = 1.5, cex.lab = 2)
