# Settings--------
seed <- 47405
iterChain <- 1E4
Nchain <- 4
warmup <- 0.5
thin <- 1

source("I119 panel data utility functions.R")

# Input---------
load("I119 Panel Data Clean.Rdata")

df_input <- df_I119Merged

# Preliminary-ANOVA by Term--------
summary(aov(TotalMET.diff ~ as.factor(CoachID), data = S19FA.masked))

summary(aov(TotalMET.diff ~ as.factor(CoachID), data = S20SP.masked))

summary(aov(TotalMET.diff ~ as.factor(CoachID), data = S20FA.masked))

summary(aov(TotalMET.diff ~ as.factor(CoachID), data = S21SP.masked))

# Visualize ANOVA
plotbase <- ggplot(data = data.frame())+geom_blank()+
  theme_classic(base_size = 21)

plotbase + geom_boxplot(data = S19FA.masked,
                        aes(x = as.factor(CoachID), y = TotalMET.diff))

plotbase + geom_boxplot(data = S20SP.masked,
                        aes(x = as.factor(CoachID), y = TotalMET.diff))

plotbase + geom_boxplot(data = S20FA.masked,
                        aes(x = as.factor(CoachID), y = TotalMET.diff))

plotbase + geom_boxplot(data = S21SP.masked,
                        aes(x = as.factor(CoachID), y = TotalMET.diff))

# Panel ANOVA
AOV_i119 <- summary(aov(TotalMET.diff ~ as.factor(CoachID), data = df_I119Merged))
plot_aov_i119 <- plotbase + 
  geom_boxplot(data = df_I119Merged,
               aes(x = as.factor(CoachID),
                   y = TotalMET.diff))
print(plot_aov_i119)

# One-pooled without covariates -------

I119_onepool_nocov_stanglm <- stan_glm(TotalMET.diff ~ CoachID,
                                       data = df_input,
                                       na.action = "na.omit")
summary(I119_onepool_nocov_stanglm)

# stanfit
modelname <- "onepool_nocov"
assign(
  paste0("stanfit_",modelname),
  value = getstanFit(modelname = modelname,
                     df_input = df_input)
)
print(
  eval(parse(text = paste0("stanfit_",modelname)))
)


# One-pooled with covariates ------
I119_onepool_cov_stanglm <- stan_glm(TotalMET.diff ~ Term + CoachID +
                                   Gender + Age + School,
                                data = df_input,
                                 x = T,
                                QR = T,
                                na.action = "na.omit")
summary(I119_onepool_cov_stanglm)

modelname <- "onepool_cov"
assign(
  paste0("stanfit_",modelname),
  value = getstanFit(modelname = modelname,
                     df_input = df_input)
)
print(
  eval(parse(text = paste0("stanfit_",modelname)))
)

# Mixed effect random intercept on Coach and Term----------

I119_lme_cov_stanlmer <- stan_lmer(data = df_input,
                                TotalMET.diff ~ (1|Term)  + Gender + Age + School + (1|CoachID),
                                # random = ~1|CoachID,
                                # na.action = "na.omit",
                               control = list(
                                 adapt_delta  = 0.99,
                                 max_treedepth = 20
                               ),
                               QR = T)
# Examine model fittings
print(I119_lme_cov_stanlmer)
summary(I119_lme_cov_stanlmer)

# Extract posteriors
df_posterior_lme_cov_stanlmer <- as.matrix(I119_lme_cov_stanlmer)

# Specify Coach parameters to extract
ParsList <- c(
  paste0("b[(Intercept) CoachID:",1:9,"]")
)

df_posterior_lme_cov_stanlmer_Coach <- as.data.frame(df_posterior_lme_cov_stanlmer[,ParsList])

colnames(df_posterior_lme_cov_stanlmer_Coach ) <- c(paste0("Coach",1:9))
write.csv(df_posterior_lme_cov_stanlmer_Coach,
          "Posteriors of Predicted Coach Specific Effects.csv",
          row.names = F)



# Mixed effect Hierarchical model Bayesian mean only----
modelname <- "hier_mean_cov"
assign(
  paste0("stanfit_",modelname),
  value = getstanFit(modelname = modelname,
                     df_input = df_input)
)
print(
  eval(parse(text = paste0("stanfit_",modelname)))
)

df_posterior_lme_cov_stan <- as.data.frame(stanfit_hier_mean_cov)
colnames(df_posterior_lme_cov_stan) <- c(
  "sigma",
  "GenderFemale","GenderMale","GenderOther",
  paste0("School",1:11),
  "Age",
  paste0("Coach",1:9),
  paste0("Term",1:4),
  "lp___"
)
write.csv(df_posterior_lme_cov_stan,
          file = "Posterior lme with cov stan.csv",
          row.names = F)
df_posterior_lme_cov_stan_Coach <- df_posterior_lme_cov_stan[,paste0("Coach",1:9)]
write.csv(df_posterior_lme_cov_stan_Coach,
          file = "Posteriors of Predicted Coach Specific Effects.csv",
          row.names = F)

parsList <- c(
  paste0("dbeta2[",1:9,"]")
        
)

pairs_stan(chain = 1,stan_model = stanfit_hier_mean_cov,pars = parsList)
stan_diag(stanfit_hier_mean_cov,
          information = "stepsize",
          chain = 0)
stan_par(object = stanfit_hier_mean_cov,
         par = parsList,
         chain = 0)
traceplot(stanfit_hier_mean_cov,pars = parsList)
print(stanfit_hier_mean_cov,pars = parsList)

# Mixed effect Hierarchical model Bayesian mean and variance----
modelname <- "hier_meanvar_cov"
assign(
  paste0("stanfit_",modelname),
  value = getstanFit(modelname = modelname,
                     df_input = df_input)
)
print(
  eval(parse(text = paste0("stanfit_",modelname)))
)

df_posterior_lme_cov_stan <- as.data.frame(stanfit_hier_meanvar_cov)
colnames(df_posterior_lme_cov_stan) <- c(
  "sigma",
  "GenderFemale","GenderMale","GenderOther",
  paste0("School",1:11),
  "Age",
  paste0("Coach",1:9),
  paste0("Coach",1:9,"_mean"),
  paste0("Coach",1:9,"_sd"),
  paste0("Term",1:4),
  "lp___"
)
write.csv(df_posterior_lme_cov_stan,
          file = "Posterior lme with cov stan.csv",
          row.names = F)
df_posterior_lme_cov_stan_Coach <- df_posterior_lme_cov_stan[,paste0("Coach",1:9)]
write.csv(df_posterior_lme_cov_stan_Coach,
          file = "Posteriors of Predicted Coach Specific Effects.csv",
          row.names = F)

parsList <- c(
  paste0("beta2[",1:9,"]")
  
)

pairs_stan(chain = 1,stan_model = stanfit_hier_meanvar_cov,pars = parsList)
stan_diag(stanfit_hier_meanvar_cov,
          information = "stepsize",
          chain = 0)
stan_par(object = stanfit_hier_meanvar_cov,
         par = parsList,
         chain = 0)
traceplot(stanfit_hier_meanvar_cov,pars = parsList)
print(stanfit_hier_meanvar_cov,pars = parsList)


# Visualization------
load("I119 Panel Modeling Results.Rdata")

# Decide which posterior to use
df_posterior_coach <- df_posterior_lme_cov_stan_Coach

# Unmask Coach (optional)
colnames(df_posterior_coach) <- c("Anna","Chris","Elena","Lanie","Matt",
                                  "Shellie","Zach","Dani","Kyle")

# Rank by Median Performance

df_posterior_coach <- df_posterior_coach %>%
  relocate(
    # order((df_posterior_coach %>% colMeans()))
    order(apply(df_posterior_coach,2,median))
  )

# Transform the posterior to long format
df_posterior_coach_long <- reshape2::melt(df_posterior_coach,
                                          value.name = "Beta3")
colnames(df_posterior_coach_long)[1] <- "Coach"

df_coach_median <- df_posterior_coach_long %>% group_by(Coach) %>% summarise(CoachMedian = median(Beta3))

plot_caterpillar_coach_1 <- ggplot(df_posterior_coach_long,
                                   aes(y = Beta3))+
  geom_histogram(aes(x = ..density..,  group = Coach),
                 binwidth = 20,
                 fill = "white", col = "grey",alpha = 0.5)+
  geom_density(aes(col = Coach),
               size = 1,show.legend = F)+
  geom_hline(data = df_coach_median,
             aes(yintercept = CoachMedian,
                 col = Coach),
             size= 1.5,lty = "dashed",
             show.legend = F)+
  coord_cartesian(ylim = c(-300,300))+
  scale_x_continuous(breaks = NULL)+
  # scale_y_continuous(breaks = seq(from = -300,to=300,by=50))+
  scale_y_continuous(breaks = c(-300,0,300),
                     labels = c("Poor","Neutral","Good"))+
  geom_hline(aes(yintercept = 0),size = 2, col = "blue")+
  facet_wrap(~ Coach,
             # scales = "free_x",
             ncol = 9)+
  theme_classic(base_size = 21)+
  labs(title = "Histogram and Density Ranked by Median Performance",
       x = "Density",
       y = "Delivery Performance")


# plot_caterpillar_coach_2 <- ggplot(df_posterior_coach_long)+
#   geom_point(aes(y = Beta3, x = Coach))+
#   coord_cartesian(xlim = c(-300,300))+
#   facet_wrap(~Coach, ncol = 9, scales = "free_y")

svg(filename = "Program Delivery by Median Performance.svg",
    height = 14, width = 14)
print(plot_caterpillar_coach_1)
dev.off()

# Output Model Results-----
save(
  # I119_onepool_cov_stanglm,
  I119_lme_cov_stanlmer,
  stanfit_hier_mean_cov,
  stanfit_hier_meanvar_cov,
  df_posterior_lme_cov_stan,
  df_posterior_lme_cov_stan_Coach,
  # stanfit_onepool_cov,
  file = "I119 Panel Modeling Results.Rdata"
)
