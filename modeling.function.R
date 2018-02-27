fit.models <- function(model.name, x.data, y.response, response.family){
  
  # For testing
  
  model.name <- "landuse"
  x.data <- landuse_x.df
  y.response <- primary_use
  response.family <- "multinomial"
  
  x.matrix <- model.matrix(~., x.data)[,-1]
  
  # Elastic net modeling
  set.seed(1)
  foldid=sample(1:10,size=length(y.response),replace=TRUE)
  if (response.family == "gaussian"){
    cv1=cv.glmnet(x.matrix,y.response,family=response.family,foldid=foldid,alpha=1)
    cv.75=cv.glmnet(x.matrix,y.response,family=response.family,foldid=foldid,alpha=0.75)
    cv.5=cv.glmnet(x.matrix,y.response,family=response.family,foldid=foldid,alpha=.5)
    cv.25=cv.glmnet(x.matrix,y.response,family=response.family,foldid=foldid,alpha=0.25)
  }
  if (response.family == "binomial") {
    cv1=cv.glmnet(x.matrix,y.response,family=response.family,type.measure = "class",foldid=foldid,alpha=1)
    cv.75=cv.glmnet(x.matrix,y.response,family=response.family,type.measure = "class",foldid=foldid,alpha=0.75)
    cv.5=cv.glmnet(x.matrix,y.response,family=response.family,type.measure = "class",foldid=foldid,alpha=.5)
    cv.25=cv.glmnet(x.matrix,y.response,family=response.family,type.measure = "class",foldid=foldid,alpha=0.25)
  } 
  if (response.family == "multinomial") {
    cv1=cv.glmnet(x.matrix,y.response,family=response.family,type.measure = "class",foldid=foldid,alpha=1)
    cv.75=cv.glmnet(x.matrix,y.response,family=response.family,type.measure = "class",foldid=foldid,alpha=0.75)
    cv.5=cv.glmnet(x.matrix,y.response,family=response.family,type.measure = "class",foldid=foldid,alpha=.5)
    cv.25=cv.glmnet(x.matrix,y.response,family=response.family,type.measure = "class",foldid=foldid,alpha=0.25)
  }
  
 
  models <- vector(mode="list", length=4)
  
  models[[1]] <- cv.25
  models[[2]] <- cv.5 
  models[[3]] <- cv.75 
  models[[4]] <- cv1

  alphas <- c(0.25,0.5,0.75,1)

  cv1.mse <- cv1$cvm[cv1$lambda == cv1$lambda.1se]
  cv.75.mse <- cv.75$cvm[cv.75$lambda == cv.75$lambda.1se]
  cv.5.mse <- cv.5$cvm[cv.5$lambda == cv.5$lambda.1se]
  cv.25.mse <- cv.25$cvm[cv.25$lambda == cv.25$lambda.1se]
  
  mses <- c(cv.25.mse, cv.5.mse, cv.75.mse, cv1.mse)
  best_model <- models[[which.min(mses)]]
  best_alpha <- alphas[which.min(mses)]
  
  elastic.full <- glmnet(x.matrix,y.response,family=response.family,alpha=best_alpha)
  #lambda.seq <- c(seq(1, 0.64, by=-0.05), elastic.nvars$lambda)
  #elastic.nvars <- glmnet(x.matrix,y.response,family=response.family,alpha=best_alpha, lambda = lambda.seq)

  found_df = FALSE
  df = 25
  while (found_df == FALSE){
    if (length(elastic.full$lambda[elastic.full$df == df]) > 0){
      lambda.5var <- elastic.full$lambda[elastic.full$df == df]  
      elastic5.error <- best_model$cvm[best_model$glmnet.fit$df == df][1]
      found_df = TRUE
    } else {
      df = df + 1
    }
  }
  
  
  elastic.coef5 <- coef(elastic.full,s=lambda.5var[1])[1:ncol(x.matrix),] 
  elastic.coef5.nonzero <- elastic.coef5[elastic.coef5!=0][2:6]
  elastic5.names <- names(elastic.coef5.nonzero)

  
  
  ############ Figure out what to do with matrix of coefficients #############
  if (response.family == "multinomial"){
    elastic.coef5 <- coef(elastic.full,s=lambda.5var[1])
    elastic.coef5.nonzero1 <- as.data.frame(as.matrix(elastic.coef5$`1`))
    elastic5.names1 <- names(elastic.coef5.nonzero1[elastic.coef5.nonzero != 0,])
    elastic.coef5.nonzero2 <- as.data.frame(as.matrix(elastic.coef5$`2`))
    elastic5.names2 <- names(elastic.coef5.nonzero2[elastic.coef5.nonzero2 != 0,])
    elastic.coef5.nonzero3 <- as.data.frame(as.matrix(elastic.coef5$`3`))
    elastic5.names3 <- names(elastic.coef5.nonzero3[elastic.coef5.nonzero3 != 0,])
    elastic.coef5.nonzero4 <- as.data.frame(as.matrix(elastic.coef5$`4`))
    elastic5.names4 <- names(elastic.coef5.nonzero4[elastic.coef5.nonzero4 != 0,])
    elastic.coef5.nonzero5 <- as.data.frame(as.matrix(elastic.coef5$`5`))
    elastic5.names5 <- names(elastic.coef5.nonzero5[elastic.coef5.nonzero5 != 0,])
    
  }
  path <- sprintf("Paper/images/elastic_cv_%s.pdf", model.name)
  pdf(path,width=12,height=8)
  plot(best_model);title(main=sprintf("Alpha = %s", best_alpha), line = 2.1);
  # par(mfrow=c(2,2))
  # plot(cv1);title(main="Alpha = 1", line = 2.1);
  # plot(cv.75);title(main="Alpha = .75", line = 2.1);
  # plot(cv.5);title(main="Alpha = .5", line = 2.1);
  # plot(cv.25);title(main="Alpha = .25", line = 2.1);
  # par(mfrow=c(1,1))
  dev.off()
  #plot(log(cv1$lambda),cv1$cvm,pch=19,col="red",xlab="log(Lambda)",ylab=cv1$name,  main="Alpha = 1")
  #points(log(cv.5$lambda),cv.5$cvm,pch=19,col="grey", main="Alpha = 0.5")
  #points(log(cv0$lambda),cv0$cvm,pch=19,col="blue", main="Alpha = 0")
  
  
  elastic.coef.best <- coef(elastic.full,s=best_model$lambda.1se)[1:ncol(x.matrix),] 
  elastic.coef.best.nonzero <- elastic.coef.best[elastic.coef.best!=0]
  elastic.best.names <- names(elastic.coef.best.nonzero)

  if (best_model$lambda.1se == best_model$lambda[1]){
    best.lambda = best_model$lambda.min
  } else {
    best.lambda = best_model$lambda.1se
  }
  
  elastic.predicts <- predict(elastic.full, newx = x.matrix,  type = "response", s = best.lambda)
  #elastic.predict_class <- predict(elastic.full, newx = x.matrix,  type = "class", s = best.lambda)
  elastic.resids <- (y.response - elastic.predicts)
  
  #mean(y.response == elastic.predict_class)
  

  
  # Forward/Backward modeling
  if (response.family == "gaussian"){
    best.fwd <- regsubsets(y.response~., data=x.data ,nvmax=min(100,nrow(x.data)),method = "forward")
    best.fwd.summary <- summary(best.fwd)
    
    fwd.coefs <- coef(best.fwd, which.min(best.fwd.summary$bic))
    incl_vars <- names(fwd.coefs)[-1]
    incl_x <- x.matrix[,incl_vars]
    fwd.predicts <- incl_x %*% fwd.coefs[-1] + fwd.coefs[1]
    fwd.resids <- (y.response - fwd.predicts)
    
    fwd.coefs5 <- colnames(x.matrix)[best.fwd$vorder[1:5]]
    fwd5.r2 <- best.fwd.summary$rsq[5]
    
    path <- sprintf("Paper/images/forward_nvars_%s.pdf", model.name)
    pdf(path,width=12,height=8)
    par(mfrow=c(2,1))
    plot(best.fwd.summary$cp ,xlab="Number of Variables ",ylab="Cp", type='l')
    min.cp <- which.min(best.fwd.summary$cp)
    points(min.cp,best.fwd.summary$cp[min.cp],col="red",cex=2,pch=20)
    plot(best.fwd.summary$bic ,xlab="Number of Variables ",ylab="BIC", type='l')
    min.bic <- which.min(best.fwd.summary$bic)
    points(min.bic,best.fwd.summary$bic[min.bic],col="red",cex=2,pch=20)
    dev.off()
    
    par(mfrow=c(1,1))
    
    
  } 
  if (response.family == "binomial") {
    fwd.data <- cbind(x.data,y.response)
    model.null = glm(y.response ~ 1, data=fwd.data, family = binomial(link="logit"))
    
    model.full = glm(y.response ~ ., data=fwd.data, family = binomial(link="logit"))
    
    fwd.steps <- step(model.null,scope = list(upper=model.full), direction="forward", data=fwd.data,
                    k = log(nrow(fwd.data)), trace = 0)
    
    fwd.coefs5 <- fwd.steps$anova$Step[2:6]
    class(fwd.coefs5) <- class(fwd.coefs5)[-match("AsIs", class(fwd.coefs5))]
    fwd.coefs5 <- trimws(gsub("[^[:alnum:] ]", "", fwd.coefs5))
    
    top5coef <- cbind(sort(elastic5.names), sort(fwd.coefs5))
    top5coef <- gsub("_", " ", top5coef)
    colnames(top5coef) <- c("elasticnet", "forward")
    
    fwd.coefs <- fwd.steps$coefficients
    
    fwd.predicted <- 1*(fwd.steps$fitted.values > 0.5)
    fwd.correct <- (fwd.predicted == y.response)
    fwd.plot <- data.frame(cbind(fwd.steps$fitted.values, fwd.predicted, fwd.correct))
    colnames(fwd.plot) <- c("Predicted Probability", "Predicted", "Correct")
    path <- sprintf("Paper/images/binarypreds_%s.pdf", model.name)
    pdf(path,width=12,height=8)
    ggplot(fwd.plot, aes(`Predicted Probability`, factor(Predicted))) + geom_point(aes(colour = factor(fwd.correct)))
    dev.off()
  }
  if (response.family == "gaussian"){
    path <- sprintf("Paper/images/resids_%s.pdf", model.name)
    pdf(path,width=12,height=8)
    par(mfrow=c(2,2))
    plot(elastic.predicts, elastic.resids)
    qqnorm(elastic.resids)
    plot(fwd.predicts, fwd.resids)
    qqnorm(fwd.resids)
    dev.off()
    par(mfrow=c(1,1))
  
  
    best.subset <- bs(x.matrix, y.response, k = 5, time.limit = 120)
    
    bs.beta = coef(best.subset)
    bs.coef5 = apply(bs.beta != 0, 2, which)[1:5]
    bs.coef5.names <- colnames(x.matrix)[bs.coef5]
    print("Best subset solution:")
    print(best.subset$status)
    
    bs.fit <- lm(y.response ~ x.matrix[,colnames(x.matrix) %in% bs.coef5.names])
    fwd.fit <- lm(y.response ~ x.matrix[,colnames(x.matrix) %in% fwd.coefs5 ])
    elastic.fit <- lm(y.response ~ x.matrix[,colnames(x.matrix) %in% elastic5.names])
    
    top5coef <- cbind(sort(elastic5.names), sort(fwd.coefs5), sort(bs.coef5.names))
    top5coef <- trimws(gsub("[^[:alnum:] ]", "", top5coef))
    colnames(top5coef) <- c("elasticnet", "forward", "subset")
  }
  

  path <- sprintf("Paper/%s_top5names.csv", model.name)
  write.csv(top5coef, path, row.names = FALSE)

  elastic.coef.best.nonzero <- as.data.frame(elastic.coef.best.nonzero)
  elastic.coef.best.nonzero <- rownames_to_column(elastic.coef.best.nonzero)
  fwd.coefs <- as.data.frame(fwd.coefs)
  fwd.coefs <- rownames_to_column(fwd.coefs)
  merged_best_coefs <- full_join(elastic.coef.best.nonzero, fwd.coefs, by="rowname")
  colnames(merged_best_coefs) <- c("variable", "elastic", "forward")
  merged_best_coefs$variable <- gsub("_", " ", merged_best_coefs$variable)
  path <- sprintf("Paper/fullcoeflist_%s.csv", model.name)
  write.csv(merged_best_coefs, path, row.names = FALSE)
  
  results <- list("best.elastic" = best_model, "best.alpha" = best_alpha, "elastic.coef" = elastic.coef.best.nonzero,
                  "elastic.top5" = elastic5.names, "fwd.coefs" = fwd.coefs)

  return(results)
}

