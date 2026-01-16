path = "C:/Users/cmpyl/Documents/M1 2025-2026/QS/LCA/"
#File path variable

library(poLCA) #Latent Class Analysis
library(haven) #Load .dta file
library(dplyr) #Data manipulation
library(nnet) #Multinomial logit regression function
library(gt) #Export tables as HTML (to be pasted into written report doc)

#Load data
data <- read_dta(paste0(path, "ExEvalWGICOL.dta"))

#Select governance indicators
gov_vars <- data %>%
  dplyr::select(country, voicacc, polstab, goveff, regqual, lawrule, ctrlcrp)

#poLCA requires categorical indicators
#Discretize governance indices into terciles (low / medium / high)
gov_cat <- gov_vars %>%
  mutate(
    across(
      -country,   # exclude country from discretization
      ~ cut(.,
            breaks = quantile(., probs = c(0, .33, .66, 1),
                              na.rm = TRUE),
            include.lowest = TRUE,
            labels = c(1, 2, 3))
    )
  )

#Combine with original data
data_lca <- merge.data.frame(data, gov_cat, by = "country")

#Define LCA formula (indicators only)
f_lca <- cbind(voicacc.y, polstab.y, goveff.y, regqual.y, lawrule.y, ctrlcrp.y) ~ 1

#Estimate models with 2 to 6 classes
set.seed(123)

lca_models <- list()
fit_stats <- data.frame()

for (k in 2:6) {
  
  model <- try(
    poLCA(f_lca,
          data = data_lca,
          nclass = k,
          nrep = 50,
          verbose = FALSE),
    silent = TRUE
  )
  
  # Skip if poLCA failed outright
  if (inherits(model, "try-error")) {
    message(paste("Model with", k, "classes failed"))
    next
  }
  
  #Skip if fit statistics are missing
  if (length(model$llik) == 0 ||
      length(model$aic)  == 0 ||
      length(model$bic)  == 0) {
    message(paste("Model with", k, "classes returned no fit statistics"))
    next
  }
  
  #Store model
  lca_models[[paste0("Class_", k)]] <- model
  
  #Store fit statistics
  fit_stats <- rbind(
    fit_stats,
    data.frame(
      Classes = k,
      LogLik  = model$llik,
      AIC     = model$aic,
      BIC     = model$bic,
      Entropy = ifelse(length(model$entropy) == 0,
                       NA, model$entropy)
    )
  )
}

#Display results -- arrange by BIC to identify preferred model

fit_stats %>% arrange(BIC) 

#Export table as .html
fit_stats_gt = gt(fit_stats)
gtsave(fit_stats_gt, "fit_stats.html", path = path)

#PART B

#Display conditional probabilities of being in terciles given class membership
lca_models$Class_3$probs

#Extract probabilities list
probs_list <- lca_models$Class_3$probs

#Convert to one long table combining raw output of multiple tables (one for each governance variable)
marginal_probs_clean <- do.call(
  rbind,
  lapply(names(probs_list), function(var) {
    p <- as.data.frame(probs_list[[var]])
    p$Class <- rownames(p)
    p$Indicator <- var
    p
  })
)

# Reorder columns
marginal_probs_clean <- marginal_probs_clean[, c("Indicator", "Class", "1", "2", "3")] %>%
  rename("p(1st Tercile)" = "1", "p(2nd Tercile)" = "2", "p(3rd Tercile)" = "3")

# Reset row names
rownames(marginal_probs_clean) <- NULL

# View table
marginal_probs_clean
#Export cleaned table as .html
marginal_probs_gt <- gt(marginal_probs_clean)
gtsave(marginal_probs_gt, "marginal_probs.html", path = path)

#PART C

# ---------------------------------------------------------
#Stepwise approach
# ---------------------------------------------------------

#Modal class assignment
data_lca$class_m1 <- lca_models$Class_3$predclass

#Set reference categories 
data_lca$freesince <- relevel(factor(data_lca$freesince), ref = 4)#Before 1880
data_lca$lastcol   <- relevel(factor(data_lca$lastcol), ref = 1)#Britain

#Perform multinomial logit regression (reference class = class 1 -- medium governance regime)
data_lca$class_m1 <- factor(data_lca$class_m1)

step_model <- multinom(
  class_m1 ~ coldur + freesince + lastcol,
  data = data_lca
)

summary(step_model) #Display results

#Export as .html
step_coeff <- as.data.frame(summary(step_model)$coefficients)
step_gt = gt (step_coeff)
gtsave(step_gt, "stepwise_coeff.html", path = path)

# ---------------------------------------------------------
#Simultaneous approach
# ---------------------------------------------------------

#Ensure covariates are correctly coded
data_lca$freesince <- factor(data_lca$freesince)
data_lca$lastcol   <- factor(data_lca$lastcol)

#Set reference categories explicitly (just to be sure!)
data_lca$freesince <- relevel(data_lca$freesince, ref = 4)#Before 1880
data_lca$lastcol   <- relevel(data_lca$lastcol, ref = 1)#Britain

# ---------------------------------------------------------
#LCA formula with covariates
# ---------------------------------------------------------

#Left-hand side: categorical governance indicators (terciles)
#Right-hand side: colonial predictors of class membership

f_lca_cov <- cbind(
  voicacc.y,
  polstab.y,
  goveff.y,
  regqual.y,
  lawrule.y,
  ctrlcrp.y
) ~ coldur + freesince + lastcol

#Estimate model
set.seed(123)

lca_cov_model <- poLCA(
  f_lca_cov,
  data = data_lca,
  nclass = 3,
  nrep = 50,
  maxiter = 5000,
  verbose = FALSE
)

#Inspect model output
print(lca_cov_model)

#Extract class-membership regression coefficients
lca_cov_model$coeff

#Export as .html
simul_coeff <- as.data.frame(lca_cov_model$coeff)
simul_coeff_gt = gt(simul_coeff)
gtsave(simul_coeff_gt, "simultaneous_coeff.html", path = path)

# PART D 

# ---------------------------------------------------------
# Baseline LCA model (no direct effects)
# ---------------------------------------------------------

# Latent class model with governance indicators
# Colonial variables affect class membership only
f_base <- cbind(
  voicacc.y,
  polstab.y,
  goveff.y,
  regqual.y,
  lawrule.y,
  ctrlcrp.y
) ~ coldur + freesince + lastcol

# Estimate baseline model
set.seed(123)
lca_base <- poLCA(
  f_base,
  data = data_lca,
  nclass = 3,
  nrep = 50,
  maxiter = 5000,
  verbose = FALSE
)

# ---------------------------------------------------------
# Function to test direct effects for one indicator at a time
# ---------------------------------------------------------

test_direct_effect <- function(indicator) {
  
  # Build model formula allowing direct effects of covariates
  # on the selected governance indicator only
  f_direct <- as.formula(
    paste0(
      "cbind(voicacc.y, polstab.y, goveff.y, regqual.y, lawrule.y, ctrlcrp.y) ~ ",
      "coldur + freesince + lastcol + ",
      indicator, ":(coldur + freesince + lastcol)"
    )
  )
  
  # Estimate model with direct effects
  set.seed(123)
  model <- poLCA(
    f_direct,
    data = data_lca,
    nclass = 3,
    nrep = 50,
    maxiter = 5000,
    verbose = FALSE
  )
  
  # Likelihood-ratio test comparing to baseline model
  LR_stat <- 2 * (model$llik - lca_base$llik)
  
  # Difference in number of estimated parameters
  df <- length(model$coeff) - length(lca_base$coeff)
  
  # Return fit statistics and test results
  list(
    indicator = indicator,
    model = model,
    AIC = model$aic,
    BIC = model$bic,
    LR = LR_stat,
    df = df,
    p_value = pchisq(LR_stat, df = df, lower.tail = FALSE)
  )
}

# ---------------------------------------------------------
# Run direct-effect tests for all governance indicators
# ---------------------------------------------------------

indicators <- c(
  "voicacc.y",
  "polstab.y",
  "goveff.y",
  "regqual.y",
  "lawrule.y",
  "ctrlcrp.y"
)

# Apply function to each indicator
direct_tests <- lapply(indicators, test_direct_effect)

# ---------------------------------------------------------
# Create summary table of direct-effect tests
# ---------------------------------------------------------

direct_effect_summary <- do.call(
  rbind,
  lapply(direct_tests, function(x)
    data.frame(
      Indicator = x$indicator,
      AIC = x$AIC,
      BIC = x$BIC,
      LR_stat = x$LR,
      df = x$df,
      p_value = x$p_value
    )
  )
)

# Display results
direct_effect_summary

# Only include significant effects (e.g., p < 0.05)
direct_effects_sig <- direct_effect_summary[direct_effect_summary$p_value < 0.05, ]

#Export as .html
direct_effects_gt = gt(direct_effects_sig)
gtsave(direct_effects_gt, "direct_effects.html", path = path)