## Setup ---- 

pkgs <- c(
    "h2o",        # High performance machine learning
    "lime",       # Explaining black-box models
    "recipes",    # Creating ML preprocessing recipes
    "tidyverse",  # Set of pkgs for data science: dplyr, ggplot2, purrr, tidyr, ...
    "tidyquant",  # Financial time series pkg - Used for theme_tq ggplot2 theme
    "glue",       # Pasting text
    "cowplot",    # Handling multiple ggplots
    "GGally",     # Data understanding - visualizations
    "skimr",      # Data understanding - summary information
    "fs",         # Working with the file system - directory structure
    "readxl",     # Reading excel files
    "writexl"     # Writing to excel files
)

install.packages(pkgs)


# Bake the data - complete all the preprocessing Steps ----

recipe_obj <- recipe(XSTATUSCODE ~ ., data = dataFModel)%>%
    step_zv(all_predictors()) %>%
    prep()

train_tbl <- bake(recipe_obj, new_data = train)
test_tbl <- bake(recipe_obj, new_data = test)

glimpse(train_tbl)

## Modeling ----
remove.packages("h2o")
install.packages("h2o")
library(h2o)
# h2o cluster initialization...
h2o.shutdown(prompt = FALSE)
h2o.init(max_mem_size = "20g")

split_h2o  <- h2o.splitFrame(as.h2o(train_tbl),ratios = c(.85),seed=1234)

train_h2o <- split_h2o[[1]]
valid_h2o <- split_h2o[[2]]
test_h2o  <- as.h2o(test_tbl)

####################### Saving and reading as RDS for easy read #####################
saveRDS(train_h2o,file='train_h2o.rds')
saveRDS(valid_h2o,file='valid_h2o.rds')
saveRDS(test_h2o,file='test_h2o.rds')

train_h2o <- readRDS('train_h2o.rds')
valid_h2o <-  readRDS('valid_h2o.rds')
test_h2o  <-  readRDS('test_h2o.rds')
####################### Saving as RDS for easy read #####################


y <- "XSTATUSCODE"
x <- setdiff(names(train_h2o),y)

#h2o.automl - http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/algo-params/max_runtime_secs.html#max-runtime-secs
automl_models <- h2o.automl(
    x = x,
    y = y,
    training_frame = train_h2o,
    validation_frame = valid_h2o,
    balance_classes = TRUE
)
# balance_classes - Since our data is heavily imbalanced, we When enabled, 
#H2O will either undersample the majority classes or oversample the minority classes. 
#Note that the resulting model will also correct the final probabilities (“undo the sampling”) 
#using a monotonic transform, so the predicted probabilities of the first model will differ from a 
#second model. However, because AUC only cares about ordering, it won’t be affected
#https://gking.harvard.edu/files/0s.pdf


automl_models@leaderboard

library(lime)

# Save the model for future use.
h2o.getModel("StackedEnsemble_BestOfFamily_0_AutoML_20191003_035824") %>%
    h2o.saveModel(path = "/h2o_models/")



