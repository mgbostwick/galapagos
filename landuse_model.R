setwd("/Users/michaelbostwick/Documents/Galapagos")

library(tidyverse)
library(ggplot2)
library(readxl)
library(glmnet)
library(mpath)
library(reshape2)
library(gridExtra)
library(leaps)
library(bestsubset)

source("modeling.function.R")

load("BASES_CENSO_UPA_spss/clean_data.RData")
vars <-  read_excel("Variables.xlsx", sheet = "vars", range = "C2:I241")

landuse_include <- vars[which(is.na(vars$`Land use`)),1]$`Variable Name`
landuse_x.df <- subset(reduced_data, select = landuse_include)

perc_vars <- c(landuse_x.df$percperm, landuse_x.df$perctemp)

survey_vars <- c('percperm', 'perctemp', 'percfallow', 'perctill', 'percpasture', 'percbrush')
reclass_vars <- c('ReclassAGRICOLA','ReclassCONSERVACION', 'ReclassFORESTAL', 'ReclassHABITACIONAL',
                  'ReclassPECUARIO', 'ReclassSIN APROVECHAMIENTO')


perc_long1 <- melt(landuse_x.df[,survey_vars])
pdf("Paper/images/landuse_boxplots.pdf",width=12,height=8)
ggplot(perc_long1, aes(x=factor(variable),y=value)) + geom_boxplot() + xlab("Land Use Category") + ylab("Percent")
dev.off()

perc_long2 <- melt(landuse_x.df[,reclass_vars])
ggplot(perc_long2, aes(x=factor(variable),y=value)) + geom_boxplot()

primary_use <- factor(apply(landuse_x.df[,c('percperm','perctemp','perctill','percpasture','percbrush')], 1, which.max))
table(primary_use)

landuse.models <- fit.models(model.name = "landuse", x.data = landuse_x.df, 
                                     y.response = primary_use, response.family = "multinomial")
