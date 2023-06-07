library(tidyverse)
library(ggplot2)
library(dplyr)
library(rsample)
library(caret)
library(rsample)
library(recipes)
library(cowplot)


daily_fluxes_davos <- read_csv("data/FLX_Davos.csv")
daily_fluxes_lae <- read_csv("data/FLX_Laegern.csv")

both <- merge(daily_fluxes_davos,daily_fluxes_lae, by = "TIMESTAMP",suffixes = c("", ".y"))

set.seed(1982)  # for reproducibility

split <- rsample::initial_split(daily_fluxes_davos, prop = 0.8, strata = "VPD_F")
daily_fluxes_davos_train <- rsample::training(split)
daily_fluxes_davos_test <- rsample::testing(split)

split <- rsample::initial_split(daily_fluxes_lae, prop = 0.8, strata = "VPD_F")
daily_fluxes_lae_train <- rsample::training(split)
daily_fluxes_lae_test <- rsample::testing(split)

split <- rsample::initial_split(both, prop = 0.8, strata = "VPD_F")
both_train <- rsample::training(split)
both_test <- rsample::testing(split)

# Model and pre-processing formulation, use all variables but LW_IN_F
pp_davos <- recipes::recipe(GPP_NT_VUT_REF ~ SW_IN_F + VPD_F + TA_F, 
                            data = daily_fluxes_davos_train |> drop_na()) |> 
  recipes::step_BoxCox(all_predictors()) |> 
  recipes::step_center(all_numeric(), -all_outcomes()) |>
  recipes::step_scale(all_numeric(), -all_outcomes())

pp_lae <- recipes::recipe(GPP_NT_VUT_REF ~ SW_IN_F + VPD_F + TA_F, 
                          data = daily_fluxes_lae_train |> drop_na()) |> 
  recipes::step_BoxCox(all_predictors()) |> 
  recipes::step_center(all_numeric(), -all_outcomes()) |>
  recipes::step_scale(all_numeric(), -all_outcomes())

pp_both <- recipes::recipe(GPP_NT_VUT_REF ~ SW_IN_F + VPD_F + TA_F, 
                           data = both_train |> drop_na()) |> 
  recipes::step_BoxCox(all_predictors()) |> 
  recipes::step_center(all_numeric(), -all_outcomes()) |>
  recipes::step_scale(all_numeric(), -all_outcomes())

#Get the optimal for Davos

davos_mod_cv <- caret::train(pp_davos, 
                             data = daily_fluxes_davos_train |> drop_na(), 
                             method = "knn",
                             trControl = caret::trainControl(method = "cv", number =                                    10),
                             tuneGrid = data.frame(k = c(2,5,10,15,25,30, 35, 40, 60,                                   100)),
                             metric = "MAE")


ggplot(davos_mod_cv)

print(davos_mod_cv)

#Get the optimal k for Laegern

lae_mod_cv <- caret::train(pp_lae, 
                           data = daily_fluxes_lae_train |> drop_na(), 
                           method = "knn",
                           trControl = caret::trainControl(method = "cv", number =                                    10),
                           tuneGrid = data.frame(k = c(2,5,10,15,25,30, 35, 40, 60,                                   100)),
                           metric = "MAE")


ggplot(lae_mod_cv)

print(lae_mod_cv)

#Get the optimal k for both

both_mod_cv <- caret::train(pp_both, 
                            data = both_train |> drop_na(), 
                            method = "knn",
                            trControl = caret::trainControl(method = "cv", number =                                    10),
                            tuneGrid = data.frame(k = c(2,5,10,15,25,30, 35, 40, 60,                                   100)),
                            metric = "MAE")

ggplot(both_mod_cv)

print(both_mod_cv)

# make model evaluation into a function to reuse code
eval_model <- function(mod, df_train, df_test){
  
  # add predictions to the data frames
  df_train <- df_train |> 
    drop_na()
  df_train$fitted <- predict(mod, newdata = df_train)
  
  df_test <- df_test |> 
    drop_na()
  df_test$fitted <- predict(mod, newdata = df_test)
  
  # get metrics tables
  metrics_train <- df_train |> 
    yardstick::metrics(GPP_NT_VUT_REF, fitted)
  
  metrics_test <- df_test |> 
    yardstick::metrics(GPP_NT_VUT_REF, fitted)
  
  # extract values from metrics tables
  rmse_train <- metrics_train |> 
    filter(.metric == "rmse") |> 
    pull(.estimate)
  rsq_train <- metrics_train |> 
    filter(.metric == "rsq") |> 
    pull(.estimate)
  
  rmse_test <- metrics_test |> 
    filter(.metric == "rmse") |> 
    pull(.estimate)
  rsq_test <- metrics_test |> 
    filter(.metric == "rsq") |> 
    pull(.estimate)
  
  # visualise as a scatterplot
  # adding information of metrics as sub-titles
  plot_1 <- ggplot(data = df_train, aes(GPP_NT_VUT_REF, fitted)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
    labs(subtitle = bquote( italic(R)^2 == .(format(rsq_train, digits = 2)) ~~
                              RMSE == .(format(rmse_train, digits = 3))),
         title = "Training set") +
    theme_classic()
  
  plot_2 <- ggplot(data = df_test, aes(GPP_NT_VUT_REF, fitted)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
    labs(subtitle = bquote( italic(R)^2 == .(format(rsq_test, digits = 2)) ~~
                              RMSE == .(format(rmse_test, digits = 3))),
         title = "Test set") +
    theme_classic()
  
  out <- cowplot::plot_grid(plot_1, plot_2)
  
  return(out)
}

##within-site davos
davos_within_site <- eval_model(mod = davos_mod_cv,
                                df_train = daily_fluxes_davos_train,
                                df_test = daily_fluxes_davos_test)
print(davos_within_site)

#davos across-site
davos_across_site <- eval_model(mod = lae_mod_cv,
                                df_train = daily_fluxes_lae_train,
                                df_test = daily_fluxes_davos_test)
print(davos_across_site)

#within-site laegern
laegern_within_site <- eval_model(mod = lae_mod_cv,
                                df_train = daily_fluxes_lae_train,
                                df_test = daily_fluxes_lae_test)
print(laegern_within_site)

#laegern across-site
laegern_across_site <- eval_model(mod = davos_mod_cv,
                                df_train = daily_fluxes_davos_train,
                                df_test = daily_fluxes_lae_test)
print(laegern_across_site)

#davos across both
davos_across_both <- eval_model(mod = both_mod_cv,
                                df_train = both_train,
                                df_test = daily_fluxes_davos_test)
print(davos_across_both)

#laegern across both
laegern_across_both <- eval_model(mod = both_mod_cv,
                                df_train = both_train,
                                df_test = daily_fluxes_lae_test)
print(laegern_across_both)
