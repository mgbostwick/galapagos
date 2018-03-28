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

# Load modeling function
source("modeling.function.R")

# Load clean data and subset to appropriate variables for this analysis
load("BASES_CENSO_UPA_spss/clean_data.RData")
vars <-  read_excel("Variables.xlsx", sheet = "vars", range = "C2:I241")
landuse_include <- vars[which(is.na(vars$`Land use`)),1]$`Variable Name`
landuse_x.df <- subset(reduced_data, select = landuse_include)

# Create parallel coordinate plot and output to PDF
df_melt <- melt(reduced_data[,c('percperm', 'perctemp', 'percfallow', 'perctill', 'percpasture', 'percbrush',"UPA")], id.vars = "UPA")
pdf("Paper/images/landuse_parplot.pdf",width=12,height=8)
ggplot(df_melt, aes(x = variable, y = value, group = UPA)) +
  geom_path(alpha = 0.1, col = 'blue') + ylab("Percent") + xlab("Land Use")
dev.off()

# Determine primary use for each farm
primary_use <- factor(apply(reduced_data[,c('percperm','perctemp','perctill','percpasture','percbrush')], 1, which.max))
table(primary_use)

# Call modeling function (This computation is much slower than for other models)
landuse.models <- fit.models(model.name = "landuse", x.data = landuse_x.df, 
                                     y.response = primary_use, response.family = "multinomial")

# To find top 5 variable order
betas[abs(betas$s1)>0,'variable']
