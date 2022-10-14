# Global Settings------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
set.seed(seed = seed)

# Packages------

library(dplyr)
library(ggplot2)
library(readxl)
library(rstan)

library(rstanarm)
library(shinystan)
library(loo)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Global Functions-------

# Merge by subject name
MergeByName <- function(df_pre,df_post){
  df_merge <- full_join(df_pre,df_post,
                        by = "Name",
                        suffix = c(".pre",".post"))
  return(df_merge)
}

# Keep PA related variables in merged data
KeepPAData <- function(df_merge){
  df_merge <- df_merge %>% select(
    Name,
    Course, PostCourse,
    Gender,Age,
    ClassStat,Major,SchoolCombined,
    VigMET.pre,VigMET.post,ModMET.pre,ModMET.post,WalkMET.pre,WalkMET.post,
    TotalMET.pre, TotalMET.post
  )  %>% mutate(
    TotalMET.diff = TotalMET.post - TotalMET.pre
  )
  return(df_merge)
}

# Get Last Name
GetLastName <- function(Name){  # Name is a string or a vector of strings
  LastName <- sub("^\\S+\\s+","",Name)
  return(LastName)
}

GroupSpecifciStandarize <- function(df_input,yname,groupname){
  df_summarize <- df_input %>% group_by(Group = eval(parse(text = groupname))) %>%
    dplyr::summarise(Mean = mean(eval(parse(text = yname))),
                     SD = sd(eval(parse(text = yname))))
  Ninput <- nrow(df_input)
  for(i in 1:Ninput){
    Coach <- df_input$CoachID[i]
    GroupMean <- as.numeric(df_summarize[i,"Mean"])
    GroupSD <- as.numeric(df_summarize[i,"SD"])
    df_temp <- df_input %>% filter(CoachID == Coach)
  }
}

ExtractRawData <- function(df_input){
  df_temp <- df_input
  # df_temp <- df_temp %>% select(TotalMET.diff,Term,CoachID,Gender,Age,School) %>%
  #   na.omit()
  
  RawData <- list(
    N = nrow(df_temp),
    C = length(unique(df_temp$CoachID)),
    Coach = as.integer(df_temp$CoachID),
    Te = length(unique(df_temp$Term)),
    Term = as.integer(df_temp$Term),
    mtx_CoachID = model.matrix( ~ 0 +  CoachID, data = df_temp), 
    mtx_Term = model.matrix(~ 0 + Term, data = df_temp),
    y = as.numeric(df_temp$TotalMET.diff)
  )
  
  return(RawData)
}

# Hierarchical model-----
getstanFit <- function(df_input, modelname,inits = NULL){
  # # Load stan script
  # source("I119 Panel data model scripts.R")
  
  # Extract Raw Data
  RawData <- ExtractRawData(df_input = df_input)
  
  # Priors by model
  prior_onepool_nocov <- list(
    mtx_X <- model.matrix( ~ 0 + CoachID, data = df_input),
    K = 1,
    prior_dsigma = c(0,5),
    prior_dbeta1 = c(0,10),
    nu = 8
  )
  
  prior_onepool_cov <- list(
    mtx_X = model.matrix( ~ 1 + Term + Gender + School + CoachID + Age,
                         data = df_input),
    K = ncol(model.matrix( ~ 1 + Term + Gender + School + CoachID + Age,
                           data = df_input)),
    prior_dsigma = c(0,5),
    prior_dbeta1 = c(0,10),
    nu = 8
  )
  
  prior_hier_mean_cov <- list(
    mtx_X = model.matrix( ~ 1 + Gender + School + Age,
                         data = df_input),
    K = ncol(model.matrix( ~ 1 + Gender + School + Age,
                          data = df_input)),
    prior_dsigma = c(0,10),
    prior_dbeta1 = c(0,100),
    prior_dbeta2 = c(0,100),
    prior_dbeta3 = c(0,100),
    nu = 8
  )
  
  prior_hier_meanvar_cov <- list(
    mtx_X = model.matrix( ~ 1 + Gender + School + Age,
                          data = df_input),
    K = ncol(model.matrix( ~ 1 + Gender + School + Age,
                           data = df_input)),
    prior_sigma = c(0,10),
    prior_beta1 = c(0,100),
    prior_beta3 = c(0,100),
    prior_beta2_mu = c(0,100),
    prior_beta2_sigma = c(0,10),
    nu = 8
  )
  
  prior_hier_mean_cov_alt <- list(
    mtx_X = model.matrix( ~ 1 + Gender + School + Age,
                         data = df_input),
    K = ncol(model.matrix(~ 1 + Gender + School + Age,
                          data = df_input)),
    prior_dsigma = c(0,5),
    prior_dbeta1 = c(0,10),
    prior_dbeta2 = c(0,10),
    prior_dbeta3 = c(0,10),
    nu = 8
  )
  
  # Assemble raw data and priors
  DataList <- append(
    RawData,
    eval(parse(text = paste0("prior_",
                             modelname)))
  )
  
  # set initial values
  inits <- ifelse(is.null(inits),"random",inits)
  
  # Compile stanmodels
  assign(paste0("model_",modelname), 
         # stan_model(model_code = eval(parse(text = paste0("modelstring_",modelname))))
         stan_model(file = paste0("stanmodel_",modelname,".stan"))
  )
  
  # # write rstan file
  # writeLines(
  #   text = eval(parse(text = paste0("modelstring_",modelname))),
  #   con = paste0("stanmodel_",modelname,".stan")
  # )
  
  stanfit <- stan(
    # object =  eval(parse(text = paste0("model_",
    #                                                             modelname))),
    file = paste0("stanmodel_",modelname,".stan"),
                             data = DataList,
                             iter = iterChain,
                             chains = Nchain,
                             warmup = warmup * iterChain,
                             thin = thin,
                             seed = seed,
                             init = inits,
                             control = list(
                               # adapt_delta  = 0.99,
                               # max_treedepth = 20
                             )
  )
  
  save(stanfit,file = paste0("stanfit_",modelname,".RData"))
  
  return(stanfit)
  
}

pairs_stan <- function(chain, stan_model, pars) {
  energy <- as.matrix(sapply(get_sampler_params(stan_model, inc_warmup = F), 
                             function(x) x[,"energy__"]))
  pars <- extract(stan_model, pars = pars, permuted = F)
  df <- data.frame(energy[,chain], pars[,chain,])
  names(df)[1] <- "energy"
  GGally::ggpairs(df, title = paste0("Chain", chain), 
                  lower = list(continuous = GGally::wrap("points", alpha = 0.2)))                    
}

# ExtractPosterior <- function(stanfit){
#   prior <- eval(parse(text = paste0("prior_",
#                                     modelname)))
#   CovNames <- colnames(prior$mtx_X)
#   ParsList <- c(
#     "sigma",
#     paste0("beta1[",1:length(CovNames),"]"),
#     "lp__"
#   )
#   
#   df_posterior <- as.matrix(stanfit,
#                             par = ParsList)
#   colnames(df_posterior) <- ParsList
#   
#   write.csv(df_posterior,
#             paste0("df_posterior_",modelname,".csv"),
#             row.names = F)
#   
#   
# }