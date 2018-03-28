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
netincome_include <- vars[which(is.na(vars$`Net Income`)),1]$`Variable Name`
netincome_x.df <- subset(reduced_data[abs(reduced_data$netincome) < sd(reduced_data$netincome),], select = netincome_include)
netincome <- reduced_data[abs(reduced_data$netincome) < sd(reduced_data$netincome), 'netincome']

# Plot histograms and output to PDF
full_hist <- ggplot(data = reduced_data) + geom_histogram(mapping = aes(x = netincome)) + theme(text = element_text(size=14))
reduced_hist <- ggplot(data = reduced_data) + geom_histogram(mapping = aes(x = netincome)) + 
  scale_x_continuous(limits = c(quantile(reduced_data$netincome, .025), quantile(reduced_data$netincome, .975))) + 
  theme(text = element_text(size=14))
pdf("Paper/images/netincome_histograms.pdf",width=12,height=8)
grid.arrange(full_hist, reduced_hist, nrow = 1)
dev.off()


# Check for zero variance columns and remove
netincome.matrix <- model.matrix(~., netincome_x.df)[,-1]
col_vars <- apply(netincome.matrix, 2, var)
zero_variance <- names(col_vars[col_vars == 0])
netincome_x.nonzero <- netincome.matrix[,!colnames(netincome.matrix) %in% zero_variance]


### Diagnostics to find linear dependencies and correlations between variables
your.matrix <- netincome_x.nonzero
rankifremoved <- sapply(1:ncol(your.matrix), function (x) qr(your.matrix[,-x])$rank)
which(rankifremoved == max(rankifremoved))



# Call modeling function
netincome.models <- fit.models(model.name = "netincome", x.data = as.data.frame(netincome_x.nonzero), 
                                y.response = netincome, response.family = "gaussian")

# To find top 5 variable order
betas <- netincome.models$betas  
betas[abs(betas$s0)>0,'variable']

netincome.models$fwdorder

