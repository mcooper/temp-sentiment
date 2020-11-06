
runEvalGlmnet <- function(formula, data, cut_qs, 
                          control_vars=c('fips', 'dow', 'doy', 'tod', 'year', 'daynum'),
                          interact_var=NULL,
                          outcome_var=c('vader'),
                          save=NULL,
                          plot=TRUE){
  #Function to run a glmnet model, with segements at cut_qs
  #Then make a predictive dataframe
  #And optionally:
  #   plot the results 
  #   save both the dataframe and the predicted results
 
  ##############################
  # Setup data and run model
  ################################
  for (n in names(cut_qs)){
    data[ , paste0(n, '_q')] <- cut(data[ , n], cut_qs[[n]])
  }

  mm <- sparse.model.matrix(formula, data=data)

  mod <- glmnet(mm, data$vader, family="gaussian", alpha=0, lambda=0)

  #################################
  # Make a predictive data frame 
  ###############################
  # Get every combination of vars we are interested in
  eg <- list()
  for (v in names(cut_qs)){
    eg[[v]] = seq(min(data[ , v]), max(data[ , v]), length.out=500) 
  }
  for (i in interact_var){
    eg[[i]] = unique(data[ , i])
  }

  preddat <- expand.grid(eg)
 
  # Make segements
  for (n in names(cut_qs)){
    preddat[ , paste0(n, '_q')] <- cut(preddat[ , n], cut_qs[[n]])
  }
  
  #Add control vars
  for (v in control_vars){
    preddat[ , v] <- data[1 , v]
  }

  preddat[ , outcome_var] <- 1

  ########################################
  # Make Prediction
  ############################
  
  pmm <- sparse.model.matrix(formula, data=preddat)
  preddat[ , outcome_var] <- predict(mod, pmm)[ , 1]

  ################################
  # Plot and/or save results
  ################################

  if (!is.null(save)){
    save('preddat', 'mod', file=save)
  }

  plotdat = list()
  if (plot){
    for (v in names(cut_qs)){
      if (!is.null(interact_var)){
        aes <- aes_string(x=v, y=outcome_var, color=interact_var)
      } else{
        aes <- aes_string(x=v, y=outcome_var)
      }

      plt <- ggplot(preddat) + 
              geom_line(aes)

      plotdat[[v]] <- plt
    }
  }
  
  return(plotdat)
}


