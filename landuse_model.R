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

survey_vars <- c('percperm', 'perctemp', 'percfallow', 'perctill', 'percpasture', 'percbrush',"percinv")
reclass_vars <- c('ReclassAGRICOLA','ReclassCONSERVACION', 'ReclassFORESTAL', 'ReclassHABITACIONAL',
                  'ReclassPECUARIO', 'ReclassSIN APROVECHAMIENTO')


perc_long1 <- melt(landuse_x.df[,survey_vars])
pdf("Paper/images/landuse_boxplots.pdf",width=12,height=8)
ggplot(perc_long1, aes(x=factor(variable),y=value)) + geom_boxplot() + xlab("Land Use Category") + ylab("Percent")
dev.off()

use_data <- reduced_data[,survey_vars]
use_data <- use_data[order(use_data[,7],use_data[,6],use_data[,5],use_data[,4],use_data[,3],use_data[,2],use_data[,1]),][,-7]
use_data$rownum <- 1:nrow(use_data)
use_data_melt <- melt(use_data, id="rownum")
pdf("Paper/images/landuse_barplot.pdf",width=12,height=8)
g <- ggplot(use_data_melt, aes(x = rownum, y= value))
g + geom_bar(aes(fill=variable), stat="identity")+xlab("Farms")+ylab("Percent")
dev.off()

perc_long2 <- melt(landuse_x.df[,reclass_vars])
ggplot(perc_long2, aes(x=factor(variable),y=value)) + geom_boxplot()

primary_use <- factor(apply(reduced_data[,c('percperm','perctemp','perctill','percpasture','percbrush')], 1, which.max))
#primary_use <- factor(apply(landuse_x.df[,survey_vars], 1, which.max))
table(primary_use)

perc_vars <- as.matrix(reduced_data[,survey_vars])

landuse.models <- fit.models(model.name = "landuse", x.data = landuse_x.df, 
                                     y.response = primary_use, response.family = "multinomial")

summary(landuse_x.df$ReclassPECUARIO)
summary(landuse_x.df$`ReclassSIN APROVECHAMIENTO`)
summary(landuse_x.df$percpasture)
