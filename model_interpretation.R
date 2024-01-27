# 1. Setup ----

# Load Libraries 
library(tidyverse)
library(tidyquant)
library(lime)

stacked_ensemble <- h2o.loadModel("C:/work/opim5770/Modelling/h2o_models/StackedEnsemble_AllModels_AutoML_20191007_012837")




predictions_tbl <- stacked_ensemble %>% 
  h2o.predict(newdata = as.h2o(test_tbl)) %>%
  as.tibble() %>%
  bind_cols(
    test_tbl %>%
      select(XLABEL, POLICYNO)
  )

predictions_tbl

train_tbl %>% 
    write_rds('../data/train_tbl.rds')

test_tbl %>%
  slice(5) %>%
  glimpse()

# 5 1       0.0189 0.981  1      10008 

explainer <- train_tbl %>%
  select(-XLABEL) %>%
  lime(
    model           = stacked_ensemble,
    bin_continuous  = TRUE,
    n_bins          = 4,
    quantile_bins   = TRUE
  )

explanation <- test_tbl %>%
  slice(5) %>%
  select(-XLABEL) %>%
  lime::explain(
    explainer = explainer,
    n_labels   = 1,
    n_features = 8,
    n_permutations = 5000,
    kernel_width   = 1
  )


explanation %>%
  as.tibble() %>%
  select(feature:prediction) 

plot_features(explanation = explanation, ncol = 1)


explanation <- test_tbl %>%
  slice(1:20) %>%
  select(-XLABEL) %>%
  lime::explain(
    explainer = explainer,
    n_labels   = 1,
    n_features = 8,
    n_permutations = 5000,
    kernel_width   = 1
  )

explanation %>%
  as.tibble()


plot_features(explanation, ncol = 4)


plot_explanations(explanation)


plot_explanations(explanation)