#------------------------------------------------------------------------------
# Script purpose: fit models and create plots
#------------------------------------------------------------------------------




# packages---------------------------------------------------------------------

library(MASS)
library(dplyr)
library(ggplot2)
library(sandwich)
library(reshape2)
library(jtools)
library(lmtest)
library(wesanderson)


# import data------------------------------------------------------------------
df_visit <- read.csv(file = 'clean_data/df_visit.csv',
                     colClasses = c(home_county = "character")
                     )

df_ortega <- read.csv(file = "clean_data/df_ortega.csv",
                      colClasses = c(GEOID = "character"))

df_census <- read.csv(file = 'clean_data/df_census.csv',
                      colClasses = c(GEOID = "character",
                                     state = "factor"))

df_chetty <- read.csv(file = 'clean_data/df_chetty_sc.csv',
                      colClasses = c(home_county = "character"))
df_jec <- read.csv('clean_data/df_jec.csv',
                   colClasses = c(home_county = "character"))

## merge data 
df <- merge(df_visit, 
            df_census,
            by.x="home_county",
            by.y="GEOID")

df <- merge(df,df_ortega,
            by.x='home_county',
            by.y="GEOID")

df <- merge(df,
            df_chetty, 
            by='home_county')


df_all <- merge(df,
                df_jec, 
                by="home_county")



# define model fitting functions------------------------------------------------

scale_vector <- function(x) {
  as.vector(scale(x))
}


## scale predictors

##############################################################
#remove code that creates counties and states
##############################################################

get_data <- function(df, tier_name=NULL){
  if (!is.null(tier_name)){
    df <- subset(df, tier == tier_name)
  }
  df$home_county <- sprintf("%05s", as.character(df$home_county))
  df$home_state <- substr(df$home_county, 1, 2)
  df$home_state <- factor(df$home_state)
  
  numeric_vars <- names(df)[sapply(df, is.numeric)]
  no_scale_vars <- c('tier_visitors', 'all_visitors')
  to_scale_vars <- setdiff(numeric_vars, no_scale_vars)
  df[to_scale_vars] <- lapply(df[to_scale_vars], scale)
  
  return(df)
}


# function for model fitting that allows various model specifications

   
model_fitting <- function(df, 
                          tier=NULL,
                          inequality_var=NULL,
                          income_var='log_income',
                          other_var=NULL){
  
  df_scaled <- get_data(df,tier)
  
  
  if (!is.null(inequality_var)) {
    if (inequality_var == "ortega") {
      inequality_var <- c("alpha", "gamma")
    }else if (inequality_var == "quintile") {
      inequality_var <- c("ratio3.1", "ratio5.3")
    }
  }
  
  
  formula_string <- reformulate(c(inequality_var,
                                  income_var,
                                  other_var, 
                                  "pop_density", 
                                  "age", 
                                  "foreign",
                                  "store_ratio", 
                                  "female", 
                                  "offset(log(all_visitors))"),
                                response = "tier_visitors"
  )
  
  model <- glm.nb(formula_string,
                  data = df_scaled,
                  control = glm.control(maxit = 50))
  
  
  ## cluster standard errors at US state level
  model_cse <- coeftest(model, 
                          vcov = vcovCL(model, cluster = ~home_state),
                          save = TRUE)
    
  return(model_cse)
}


# models------------------------------------------------------------------------

tiers <- c("tier1","tier2","tier3","tier4")

sc_vars <- c("family_unity", 
             "community_health",
             "institutional_health",
             'sk2014', 
             'civic_organizations', 
             'volunteering_rate', 
             'ec',
             'support_ratio', 
             'clustering')

## visit rate ~ Gini
models_gini <- list()
for (tier in tiers){
    model <- model_fitting(df = df_all,
                           tier=tier,
                           inequality_var = "gini") 
    
    models_gini[[tier]] <- model}


## visit rate ~ Ortega parameters
models_ortega <- list()
for (tier in tiers){
  model <- model_fitting(df = df_all,
                         tier=tier,
                         inequality_var = "ortega") 
  
  models_ortega[[tier]] <- model}


## visit rate ~ social capital
models_sc <- list()

for (tier in tiers){
  for (sc in sc_vars) {
    #fit multivariate regressions
    model <- model_fitting(df = df_all,
                           tier=tier,
                           other_var = sc) 
    key <- paste(tier, sc, sep = "_")
    
    models_sc[[key]] <- model}}



## visit rate ~ Gini + social capital

models_sc_gini_main <- list()
for (tier in tiers){
  for (sc in sc_vars) {
    #fit multivariate regressions
    model <- model_fitting(df = df_all,
                              tier=tier,
                              inequality_var = "gini",
                              other_var = sc) 
    key <- paste(tier, sc, sep = "_")
    
    models_sc_gini_main[[key]] <- model}}



## visit rate ~ Ortega + social capital

models_sc_ortega_main <- list()
for (tier in tiers){
  for (sc in sc_vars) {
    #fit multivariate regressions
    model <- model_fitting(df = df_all,
                              tier=tier,
                              inequality_var = "ortega",
                              other_var = sc) 
    key <- paste(tier, sc, sep = "_")
    
    models_sc_ortega_main[[key]] <- model}}

## visit rate ~ Gini * social capital

models_sc_gini_interactions <- list()
for (tier in tiers){
  for (sc in sc_vars) {
    interact_term <- paste0("gini*",sc)
    model <- model_fitting(df=df_all,
                              tier=tier,
                              other_var = interact_term)
    key <- paste(tier, sc, sep = "_")
    models_sc_gini_interactions[[key]] <- model}}



## visit rate ~ alpha * social capital
models_sc_alpha_interactions <- list()

for (tier in tiers){
  for (sc in sc_vars) {
    interact_term <- paste0("alpha*",sc)
    #fit multivariate regressions
    model <- model_fitting(df=df_all,
                              tier=tier,
                              inequality_var = "ortega",
                              other_var = interact_term)
    key <- paste(tier, sc, sep = "_")
    models_sc_alpha_interactions[[key]] <- model}}


## visit rate ~ gamma * social capital
models_sc_gamma_interactions <- list()

for (tier in tiers){
  for (sc in sc_vars) {
    interact_term <- paste0("gamma*",sc)
    #fit multivariate regressions
    model <- model_fitting(df=df_all,
                           tier=tier,
                           inequality_var = "ortega",
                           other_var = interact_term)
    key <- paste(tier, sc, sep = "_")
    models_sc_gamma_interactions[[key]] <- model}}







# plots-------------------------------------------------------------------------

## Fig.1 main effect of inequality measures

# Fit the models for each sample
coefficients <- list()
for (tier in tiers) {
  # Store coefficients and their confidence intervals for each tier
  coefficients[[tier]] <- data.frame(
    tier = tier,
    measure = c("gini","alpha","gamma"),
    coefficient = c(models_gini[[tier]]["gini", "Estimate"],
                    models_ortega[[tier]]["alpha", "Estimate"],
                    models_ortega[[tier]]["gamma", "Estimate"]),
    
    
    conf.low = c(confint(models_gini[[tier]])["gini", 1],
                 confint(models_ortega[[tier]])["alpha", 1],
                 confint(models_ortega[[tier]])["gamma", 1]),
    
    conf.high = c(confint(models_gini[[tier]])["gini", 2],
                  confint(models_ortega[[tier]])["alpha", 2],
                  confint(models_ortega[[tier]])["gamma", 2])
  )
}

coefficients <- do.call("rbind", coefficients)
#coefficients$tier <- paste0("Tier",coefficients$tier)

gini <- subset(coefficients,measure=="gini")
gini$measure <- "Gini"
ortega <- subset(coefficients,measure %in% c("alpha",'gamma'))
gini$tier <- factor(gini$tier, levels = rev(levels(factor(gini$tier))))
ortega$tier <- factor(ortega$tier, levels = rev(levels(factor(ortega$tier))))

plot_gini <-
  ggplot(gini, aes(y = tier, x = coefficient, color = measure, group = measure)) +
  geom_point() +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
  geom_vline(aes(xintercept = 0), color = "grey", linetype = "dashed")+
  labs(y = "brands tiers", 
       x = "coefficients",
       color="measure") +
  scale_color_manual(values = wes_palette("Darjeeling1", 5, type = "discrete"))+
  theme_classic()+
  ggtitle("a")+
  theme(legend.position.inside = c(0.93, 0.5),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(face = "bold", hjust = -0.2))

plot_ortega <-
  ggplot(ortega, aes(y = tier, x = coefficient, color = measure, group = measure)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2,
                position = position_dodge(width = 0.5)) +

  geom_vline(aes(xintercept = 0), color = "grey", linetype = "dashed")+
  labs(y = "brands tiers", 
       x = "coefficients",
       color="measure") +
  scale_color_manual(values = c(wesanderson::wes_palette(n=5,"Darjeeling1")[2],
                                wesanderson::wes_palette(n=5,"Darjeeling1")[3]),
                     labels = c(expression(paste("Ortega ",alpha)), expression(paste("Ortega ",gamma))))+
  theme_classic()+
  ggtitle("b")+
  theme(legend.position = c(0.9, 0.5),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(face = "bold", hjust = -0.2))

fig1 <- grid.arrange(plot_gini,plot_ortega,ncol=2,widths=c(0.95,1))


## Fig.2 coefficients of social capital

models <- list(
               models_sc[["tier1_family_unity"]],
               models_sc[["tier1_community_health"]],
               models_sc[["tier1_institutional_health"]],
               models_sc[["tier1_sk2014"]],
               models_sc[["tier1_civic_organizations"]],
               models_sc[["tier1_volunteering_rate"]],
               models_sc[["tier1_ec"]],
               models_sc[["tier1_support_ratio"]],
               models_sc[["tier1_clustering"]],
               
               #models_pca[["tier2_pc1"]],
               models_sc[["tier2_family_unity"]],
               models_sc[["tier2_community_health"]],
               models_sc[["tier2_institutional_health"]],
               models_sc[["tier2_sk2014"]],
               models_sc[["tier2_civic_organizations"]],
               models_sc[["tier2_volunteering_rate"]],
               models_sc[["tier2_ec"]],
               models_sc[["tier2_support_ratio"]],
               models_sc[["tier2_clustering"]],
               
               #models_pca[["tier3_pc1"]],
               models_sc[["tier3_family_unity"]],
               models_sc[["tier3_community_health"]],
               models_sc[["tier3_institutional_health"]],
               models_sc[["tier3_sk2014"]],
               models_sc[["tier3_civic_organizations"]],
               models_sc[["tier3_volunteering_rate"]],
               models_sc[["tier3_ec"]],
               models_sc[["tier3_support_ratio"]],
               models_sc[["tier3_clustering"]],
               
               #models_pca[["tier4_pc1"]],
               models_sc[["tier4_family_unity"]],
               models_sc[["tier4_community_health"]],
               models_sc[["tier4_institutional_health"]],
               models_sc[["tier4_sk2014"]],
               models_sc[["tier4_civic_organizations"]],
               models_sc[["tier4_volunteering_rate"]],
               models_sc[["tier4_ec"]],
               models_sc[["tier4_support_ratio"]],
               models_sc[["tier4_clustering"]])  


clean_sc_vars <- c("family unity", 
                   "community health", 
                   "institutional health",
                    "Penn State Index",
                   "civic organizations",
                   "volunteering rate",
                    "economic connectiveness",
                   "support ratio",
                   "clustering")

### Define significance levels
signif_codes <- function(p) {
  ifelse(p < 0.001, '***', ifelse(p < 0.01, '**', ifelse(p < 0.05, '*', '')))
}

### Extract coefficients and p-values
results <- lapply(models, function(ct) {
  # Assuming ct is a matrix with row names as variable names
  sc_rows <- rownames(ct) %in% sc_vars  # Filter for social capital variable rows
  data.frame(
    Variable = rownames(ct)[sc_rows],
    Coefficient = ct[sc_rows, "Estimate"],
    PValue = ct[sc_rows, "Pr(>|z|)"],
    Significance = sapply(ct[sc_rows, "Pr(>|z|)"], signif_codes)
  )
})

results <- do.call(rbind, results)
results$tier  <- rep(c('Tier1', 'Tier2','Tier3','Tier4'),each=9)

results$Variable <- rep(clean_sc_vars,time=4)

### Labels are coefficients with asterisks
results$Label <- paste0(round(results$Coefficient, 3), results$Significance)

results$Variable <- factor(results$Variable,
                                levels = rev(unique(results$Variable)))

# set the color for the heatmap
color_low <- wesanderson::wes_palette(n=5,"Zissou1")[1]
color_high <- wesanderson::wes_palette(n=5,"Zissou1")[5]

fig2 <- 
  ggplot(results, aes(x = tier, y = Variable, fill = Coefficient)) +
  geom_tile() +  # Use geom_tile for heatmap squares
  geom_text(aes(label = Label), color = "black", size = 3) + 
  scale_fill_gradient2(low = color_low, high = color_high) +
  theme_minimal()+
  scale_x_discrete(position = "top") +  # Moves the x-axis labels to the top
  theme(
    axis.title.x = element_blank(),  
    axis.title.y = element_blank())  


## Fig.3

models <- list( 
  models_sc_gini_main[["tier1_family_unity"]],
  models_sc_ortega_main[["tier1_family_unity"]],
  
  models_sc_gini_main[["tier1_community_health"]],
  models_sc_ortega_main[["tier1_community_health"]],
  
  models_sc_gini_main[["tier1_institutional_health"]],
  models_sc_ortega_main[["tier1_institutional_health"]],
  
  models_sc_gini_main[["tier1_sk2014"]],
  models_sc_ortega_main[["tier1_sk2014"]],
  
  models_sc_gini_main[["tier1_civic_organizations"]],
  models_sc_ortega_main[["tier1_civic_organizations"]],
  
  models_sc_gini_main[["tier1_volunteering_rate"]],
  models_sc_ortega_main[["tier1_volunteering_rate"]],
  
  models_sc_gini_main[["tier1_ec"]],
  models_sc_ortega_main[["tier1_ec"]],
  
  models_sc_gini_main[["tier1_support_ratio"]],
  models_sc_ortega_main[["tier1_support_ratio"]],
  
  models_sc_gini_main[["tier1_clustering"]],
  models_sc_ortega_main[["tier1_clustering"]])



coefficients_list <- lapply(1:18, function(i) {
  model_coefs <- models[[i]][, "Estimate"]
  p_values <- models[[i]][, "Pr(>|z|)"]  # Assuming p-values are in this column
  
  sc <- names(model_coefs)[names(model_coefs) %in% sc_vars]
  
  ### Rename the social capital component to "social capital"
  names(model_coefs)[names(model_coefs) %in% sc_vars] <- "Social Capital"
  names(p_values)[names(p_values) %in% sc_vars] <- "Social Capital"
  
  ### extract coefficients and p values from each model
  expected_vars <- c("gini", "alpha", "gamma", "Social Capital")
  coef_vals <- sapply(expected_vars, function(v) 
    if(v %in% names(model_coefs)) model_coefs[v] else NA, 
    simplify = FALSE)
  pval_vals <- sapply(expected_vars, function(v) 
    if(v %in% names(p_values)) p_values[v] else NA, 
    simplify = FALSE)
  
  # Create the data frame with safe handling for missing variables
  data.frame(Coefficient = unlist(coef_vals), p_values = unlist(pval_vals))
  
})

### Combine all models into a data frame
coefficients <- do.call(cbind, coefficients_list)
names(coefficients) <- make.unique(names(coefficients), sep = "_")


### get coefficients and p values separately

coefficients_df <- coefficients %>%
  select(starts_with("Coefficient"))
p_df <- coefficients %>%
  select(starts_with("p_values"))


col_names <- c()
for(sc in clean_sc_vars){
  
  for (inequality in c("Gini","Ortega")){
    
    name <- paste(inequality, "+", sc)
    col_names <- c(col_names,name)
  }
}


colnames(coefficients_df) <- col_names
rownames(coefficients_df) <- c("Gini", "Ortega α", "Ortega γ","social capital")



coefficients_df$id <- rownames(coefficients_df)

colnames(p_df) <- col_names
rownames(p_df) <- c("Gini", "Ortega α", "Ortega γ","social capital")
p_df$id <- rownames(p_df)


coefficients_melted <- melt(coefficients_df, id.vars = "id", 
                            variable.name = "Models", value.name = "coefficients")
p_melted <- melt(p_df, id.vars = "id", 
                 variable.name = "Models", value.name = "p")

 
coefficients_melted$p <- p_melted[,c("p")]
coefficients_melted$significance = sapply(coefficients_melted[, "p"], signif_codes)
coefficients_melted$label <- paste0(round(coefficients_melted$coefficient, 3), coefficients_melted$significance)
coefficients_melted$label[coefficients_melted$label == "NANA"] <- NA

inequality_order <- c("Gini", "Ortega α", "Ortega γ","social capital")  

### set variable orders
coefficients_melted$id <- factor(coefficients_melted$id, levels = inequality_order)
coefficients_melted$Models <- factor(coefficients_melted$Models,
                                     levels = rev(unique(coefficients_melted$Models)))



fig3 <- 
  ggplot(coefficients_melted, aes(x = id, y = Models, fill = coefficients)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = color_low, high = color_high, na.value = "grey") +
  geom_text(aes(label = label), color = "black", size = 3) +
  theme_minimal() +
  labs(fill = "Coefficient")+
  scale_x_discrete(position = "top") +  # Moves the x-axis labels to the top
  theme(
    axis.title.x = element_blank(),  # Hides the x-axis title if not needed
    axis.title.y = element_blank())  # Centers the plot title


## Fig.4 coefficients for models with interactions 
models <- list( 
  models_sc_gini_interactions[["tier1_family_unity"]],
  models_sc_alpha_interactions[["tier1_family_unity"]],
  models_sc_gamma_interactions[["tier1_family_unity"]],
  
  models_sc_gini_interactions[["tier1_community_health"]],
  models_sc_alpha_interactions[["tier1_community_health"]],
  models_sc_gamma_interactions[["tier1_community_health"]],
  
  models_sc_gini_interactions[["tier1_institutional_health"]],
  models_sc_alpha_interactions[["tier1_institutional_health"]],
  models_sc_gamma_interactions[["tier1_institutional_health"]],
  
  models_sc_gini_interactions[["tier1_sk2014"]],
  models_sc_alpha_interactions[["tier1_sk2014"]],
  models_sc_gamma_interactions[["tier1_sk2014"]],
  
  models_sc_gini_interactions[["tier1_civic_organizations"]],
  models_sc_alpha_interactions[["tier1_civic_organizations"]],
  models_sc_gamma_interactions[["tier1_civic_organizations"]],
  
  models_sc_gini_interactions[["tier1_volunteering_rate"]],
  models_sc_alpha_interactions[["tier1_volunteering_rate"]],
  models_sc_gamma_interactions[["tier1_volunteering_rate"]],
  
  
  models_sc_gini_interactions[["tier1_ec"]],
  models_sc_alpha_interactions[["tier1_ec"]],
  models_sc_gamma_interactions[["tier1_ec"]],
  
  models_sc_gini_interactions[["tier1_support_ratio"]],
  models_sc_alpha_interactions[["tier1_support_ratio"]],
  models_sc_gamma_interactions[["tier1_support_ratio"]],
  
  models_sc_gini_interactions[["tier1_clustering"]],
  models_sc_alpha_interactions[["tier1_clustering"]],
  models_sc_gamma_interactions[["tier1_clustering"]]
)


coefficients_list <- lapply(1:27, function(i) {
  model_coefs <- models[[i]][, "Estimate"]
  p_values <- models[[i]][, "Pr(>|z|)"]  # Assuming p-values are in this column
  
  sc <- names(model_coefs)[names(model_coefs) %in% sc_vars]
  
  # Rename the coefficient of social capital variable
  names(model_coefs)[names(model_coefs) %in% sc_vars] <- "Social Capital"
  names(p_values)[names(p_values) %in% sc_vars] <- "Social Capital"
  
  interactions <- c(paste0("gini:",sc), paste0("alpha:",sc), paste0("gamma:",sc))
  names(model_coefs)[names(model_coefs) %in% interactions] <- "interaction"
  names(p_values)[names(p_values) %in% interactions] <- "interaction"
  
  
  expected_vars <- c("gini", "alpha", "gamma", "Social Capital", "interaction")
  coef_vals <- sapply(expected_vars, function(v) if(v %in% names(model_coefs)) model_coefs[v] else NA, simplify = FALSE)
  pval_vals <- sapply(expected_vars, function(v) if(v %in% names(p_values)) p_values[v] else NA, simplify = FALSE)
  
  # Create the data frame with safe handling for missing variables
  data.frame(Coefficient = unlist(coef_vals), p_values = unlist(pval_vals))
  
})

# Combine all coefficients into a data frame
results_df <- do.call(cbind, coefficients_list)
names(results_df) <- make.unique(names(results_df), sep = "_")


coefficients_df <- results_df %>%
  select(starts_with("Coefficient"))
p_df <- results_df %>%
  select(starts_with("p_values"))

col_names <- c()
for(sc in clean_sc_vars){
  
  for (inequality in c("gini","Ortega α", "Ortega γ")){
    if (inequality=="gini"){
      inequality <- "Gini"
    }
    name <- paste(sc, "\u00D7", inequality)
    col_names <- c(col_names,name)
  }
}



colnames(coefficients_df) <- col_names
rownames(coefficients_df) <- c("Gini", "Ortega α", "Ortega γ","social\ncapital", "interaction")
coefficients_df$variables <- rownames(coefficients_df)

colnames(p_df) <- col_names
rownames(p_df) <- c("Gini", "Ortega α", "Ortega γ","social\ncapital", "interaction")
p_df$variables <- rownames(p_df)


coefficients_melted <- melt(coefficients_df, id.vars = "variables", 
                            variable.name = "Models", value.name = "coefficients")
p_melted <- melt(p_df, id.vars = "variables", 
                 variable.name = "Models", value.name = "p")


coefficients_melted$p <- p_melted[,c("p")] #add p values to coefficients data
coefficients_melted$significance = sapply(coefficients_melted[, "p"], signif_codes)
coefficients_melted$label <- paste0(round(coefficients_melted$coefficient, 3), coefficients_melted$significance)
coefficients_melted$label[coefficients_melted$label == "NANA"] <- NA

variable_order <- c("Gini", "Ortega α", "Ortega γ","social\ncapital", "interaction")  

coefficients_melted$variables <- factor(coefficients_melted$variables, levels = variable_order)
coefficients_melted$Models <- factor(coefficients_melted$Models,
                                     levels = rev(unique(coefficients_melted$Models)))


fig4 <- 
  ggplot(coefficients_melted, aes(x = variables, y = Models, fill = coefficients)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = color_low, high = color_high, na.value = "grey") +
  geom_text(aes(label = label), color = "black", size = 3) +
  theme_minimal() +
  labs(fill = "Coefficient")+
  scale_x_discrete(position = "top") +  # Moves the x-axis labels to the top
  theme(
    axis.title.x = element_blank(),  # Hides the x-axis title if not needed
    axis.title.y = element_blank())  # Centers the plot title




