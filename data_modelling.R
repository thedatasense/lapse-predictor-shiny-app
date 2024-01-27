# Setup ----
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
  "writexl",    # Writing to excel files,
  "shiny",
  "h2o",
  "recipes",
  "caret"
)


ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak(pkgs)

data_preprocess <- function(tbl) {
  col_factors_model <- c('XLABEL','XSECPOLICYHOLDER','DISTCHANNEL','PRODTYPE')
  tbl[col_factors_model] <- lapply(tbl[col_factors_model], factor) 
  
  recipe_obj <- recipe(XLABEL ~ ., data = tbl)%>%
    step_zv(all_predictors()) %>%
    prep()
  tbl <- tbl %>%
    select(-c(POLICYNO)) %>%
    select (-c(TERMDATE,ISSUEDATE,X,X1PREMPERYEAR,ISSUEDATE))
  tbl <- bake(recipe_obj, new_data = tbl)
  return(tbl)
}

glimpse(uniquePolicy)
data_preprocess(uniquePolicy %>%
  slice(1L))

tbl <- uniquePolicy %>%
  slice(1L) %>%
  select(-c(POLICYNO)) %>%
  select (-c(TERMDATE,ISSUEDATE,X,X1PREMPERYEAR,ISSUEDATE))
tbl <- bake(recipe_obj, new_data = tbl)


# Reading unique policies with no missing data
uniquePolicy <- as_tibble(read_rds('./data/data_modelling_V6_no_missing.rds'))

#Dropping columns that are not needed for Binary classification
dataFModel <- uniquePolicy %>%
  subset(select = -c(TERMDATE,ISSUEDATE,X,X1PREMPERYEAR,ISSUEDATE))

glimpse(dataFModel)

col_factors_model <- c('XLABEL','XSECPOLICYHOLDER','DISTCHANNEL','PRODTYPE')
dataFModel[col_factors_model] <- lapply(dataFModel[col_factors_model], factor)  ## as.factor() could also be used

#Anyone born between 1981 and 1996 (ages 23 to 38 in 2019) is considered a Millennial
dataFModel <- dataFModel %>%
  select(-c(POLICYNO)) %>%
  filter(XPRIMAGE1 < 39 & XPRIMAGE1 > 22)

dataFModel %>%
  group_by(XLABEL) %>%
  summarise(Total=n()) %>%
  mutate(Percent =Total/sum(Total))

# XLABEL  Total Percent
#   0      137082   0.623
#   1       82952   0.377

#createDataPartition from caret package. 
#Its document states: By default, createDataPartition does a stratified random split of the data.

train.index <- createDataPartition(dataFModel$XLABEL, p = .8, list = FALSE)
train <- dataFModel[ train.index,]
test  <- dataFModel[-train.index,]

# Bake the data - complete all the preprocessing Steps ----

recipe_obj <- recipe(XLABEL ~ ., data = dataFModel)%>%
  step_zv(all_predictors()) %>%
  prep()

train_tbl <- bake(recipe_obj, new_data = train)
test_tbl <- bake(recipe_obj, new_data = test)


# H2o machine learning model initialization ----
# h2o cluster initialization...
h2o.shutdown(prompt = FALSE)
h2o.init(max_mem_size='10G')

localh2o <- h2o.init(max_mem_size = "48g")

split_h2o  <- h2o.splitFrame(as.h2o(train_tbl),ratios = c(.85),seed=1234)

train_h2o <- split_h2o[[1]]
valid_h2o <- split_h2o[[2]]
test_h2o  <- as.h2o(test_tbl)

y <- "XLABEL"
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
#Note that the resulting model will also correct the final probabilities ("undo the sampling") 
#using a monotonic transform, so the predicted probabilities of the first model will differ from a 
#second model. However, because AUC only cares about ordering, it won't be affected
#https://gking.harvard.edu/files/0s.pdf



automl_models@leaderboard



# Saving & Loading ----

h2o.getModel("XGBoost_2_AutoML_20191117_182234") %>%
  h2o.saveModel(path = "./h2o_models/millenial")

h2o.getModel("XGBoost_1_AutoML_20191117_182234") %>%
  h2o.saveModel(path = "./h2o_models/millenial")

h2o.getModel("GBM_1_AutoML_20191117_182234") %>%
  h2o.saveModel(path = "./h2o_models/millenial")



xgboost1 <- h2o.loadModel("./h2o_models/millenial/XGBoost_2_AutoML_20191117_182234")
xgboost2 <- h2o.loadModel("./h2o_models/millenial/XGBoost_1_AutoML_20191117_182234")
GBM <- h2o.loadModel("./h2o_models/millenial/GBM_1_AutoML_20191117_182234")


# for example I have 4 H2OModels
roc_plot <- list(xgboost1,xgboost2,xgboost3) %>% 
  # map a function to each element in the list
  map(function(x) x %>% h2o.performance(valid=T) %>% 
        # from all these 'paths' in the object
        .@metrics %>% .$thresholds_and_metric_scores %>% 
        # extracting true positive rate and false positive rate
        .[c('tpr','fpr')] %>% 
        # add (0,0) and (1,1) for the start and end point of ROC curve
        add_row(tpr=0,fpr=0,.before=T) %>% 
        add_row(tpr=0,fpr=0,.before=F)) %>% 
  # add a column of model name for future grouping in ggplot2
  map2(c('xgboost1','xgboost2','xgboost3'),
       function(x,y) x %>% add_column(model=y)) %>% 
  # reduce four data.frame to one
  reduce(rbind) %>% 
  # plot fpr and tpr, map model to color as grouping
  ggplot(aes(fpr,tpr,col=model))+
  geom_line()+
  geom_segment(aes(x=0,y=0,xend = 1, yend = 1),linetype = 2,col='grey')+
  xlab('False Positive Rate')+
  ylab('True Positive Rate')+
  ggtitle('ROC Curve for Four Models')
library(plotly)
ggplotly(roc_plot)


predictions <- h2o.predict(stackedEnsemble, newdata = as.h2o(test_h2o))





predictions_tbl <- predictions %>% as_tibble()

predictions_tbl


#Visualizing The Leaderboard ----
  
data_transformed <- automl_models@leaderboard %>% 
  as_tibble() %>%
  mutate(model_type = str_split(model_id, "_", simplify = T)[,1]) %>%
  slice(1:10) %>%
  rownames_to_column() %>%
  mutate(
    model_id   = as_factor(model_id) %>% reorder(auc),
    model_type = as.factor(model_type)
  ) %>%
  gather(key = key, value = value, -c(model_id, model_type, rowname), factor_key = T) %>%
  mutate(model_id = paste0(rowname, ". ", model_id) %>% as_factor() %>% fct_rev())


data_transformed %>%
  ggplot(aes(value, model_id, color = model_type)) +
  geom_point(size = 3) +
  geom_label(aes(label = round(value, 2), hjust = "inward")) +
  facet_wrap(~ key, scales = "free_x") +
  theme_tq() +
  scale_color_tq() +
  labs(title = "H2O Leaderboard Metrics",
       subtitle = paste0("Ordered by: auc"),
       y = "Model Postion, Model ID", x = "")


h2o_leaderboard <- automl_models@leaderboard



plot_h2o_leaderboard <- function(h2o_leaderboard, order_by = c("auc", "logloss"), 
                                 n_max = 20, size = 4, include_lbl = TRUE) {
  
  # Setup inputs
  order_by <- tolower(order_by[[1]])
  
  leaderboard_tbl <- h2o_leaderboard %>%
    as.tibble() %>%
    mutate(model_type = str_split(model_id, "_", simplify = T)[,1]) %>%
    rownames_to_column(var = "rowname") %>%
    mutate(model_id = paste0(rowname, ". ", as.character(model_id)) %>% as.factor())
  
  # Transformation
  if (order_by == "auc") {
    
    data_transformed_tbl <- leaderboard_tbl %>%
      slice(1:n_max) %>%
      mutate(
        model_id   = as_factor(model_id) %>% reorder(auc),
        model_type = as.factor(model_type)
      ) %>%
      gather(key = key, value = value, 
             -c(model_id, model_type, rowname), factor_key = T) 
    
  } else if (order_by == "logloss") {
    
    data_transformed_tbl <- leaderboard_tbl %>%
      slice(1:n_max) %>%
      mutate(
        model_id   = as_factor(model_id) %>% reorder(logloss) %>% fct_rev(),
        model_type = as.factor(model_type)
      ) %>%
      gather(key = key, value = value, -c(model_id, model_type, rowname), factor_key = T)
    
  } else {
    stop(paste0("order_by = '", order_by, "' is not a permitted option."))
  }
  
  # Visualization
  g <- data_transformed_tbl %>%
    ggplot(aes(value, model_id, color = model_type)) +
    geom_point(size = size) +
    facet_wrap(~ key, scales = "free_x") +
    theme_tq() +
    scale_color_tq() +
    labs(title = "Leaderboard Metrics",
         subtitle = paste0("Ordered by: ", toupper(order_by)),
         y = "Model Postion, Model ID", x = "")
  
  if (include_lbl) g <- g + geom_label(aes(label = round(value, 2), hjust = "inward"))
  
  return(g)
  
}


automl_models@leaderboard %>%
  plot_h2o_leaderboard(order_by = "logloss")

# Analyze model performance -----
mdl_performance <- h2o.performance(xgboost2, newdata = as.h2o(test_h2o))

mdl_performance@metrics


# Classifier Summary Metrics

h2o.auc(mdl_performance, train = T, valid = T, xval = T)
h2o.giniCoef(mdl_performance)
h2o.logloss(mdl_performance)

h2o.confusionMatrix(xgboost2)

h2o.confusionMatrix(mdl_performance)


# Precision vs Recall Plot

performance_tbl <- mdl_performance %>%
  h2o.metric() %>%
  as_tibble() 
performance_tbl

performance_tbl %>%
  filter(f1 == max(f1))

performance_tbl %>%
  ggplot(aes(x = threshold)) +
  geom_line(aes(y = precision), color = "blue", size = 1) +
  geom_line(aes(y = recall), color = "red", size = 1) +
  geom_vline(xintercept = h2o.find_threshold_by_max_metric(mdl_performance, "f1")) +
  theme_tq() +
  labs(title = "Precision vs Recall", y = "value")


# ROC Plot

path <- xgboost2

load_model_performance_metrics <- function(path, test_tbl) {
  
  model_h2o <- h2o.loadModel(path)
  perf_h2o  <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl)) 
  
  perf_h2o %>%
    h2o.metric() %>%
    as.tibble() %>%
    mutate(auc = h2o.auc(perf_h2o)) %>%
    select(tpr, fpr, auc)
  
}

model_metrics_tbl <- fs::dir_info(path = "./h2o_models/millenial") %>%
  select(path) %>%
  mutate(metrics = map(path, load_model_performance_metrics, test_tbl)) %>%
  unnest()

model_metrics_tbl %>%
  mutate(
    path = str_split(path, pattern = "/", simplify = T)[,3] %>% as_factor(),
    auc  = auc %>% round(3) %>% as.character() %>% as_factor()
  ) %>%
  ggplot(aes(fpr, tpr, color = path, linetype = auc)) +
  geom_line(size = 1) +
  theme_tq() +
  scale_color_tq() +
  theme(legend.direction = "vertical") +
  labs(
    title = "ROC Plot",
    subtitle = "Performance of 3 Top Performing Models"
  )


# Precision vs Recall

load_model_performance_metrics <- function(path, test_tbl) {
  
  model_h2o <- h2o.loadModel(path)
  perf_h2o  <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl)) 
  
  perf_h2o %>%
    h2o.metric() %>%
    as.tibble() %>%
    mutate(auc = h2o.auc(perf_h2o)) %>%
    select(tpr, fpr, auc, precision, recall)
  
}

model_metrics_tbl <- fs::dir_info(path = "./h2o_models/millenial") %>%
  select(path) %>%
  mutate(metrics = map(path, load_model_performance_metrics, test_tbl)) %>%
  unnest()

model_metrics_tbl %>%
  mutate(
    path = str_split(path, pattern = "/", simplify = T)[,3] %>% as_factor(),
    auc  = auc %>% round(3) %>% as.character() %>% as_factor()
  ) %>%
  ggplot(aes(recall, precision, color = path, linetype = auc)) +
  geom_line(size = 1) +
  theme_tq() +
  scale_color_tq() +
  theme(legend.direction = "vertical") +
  labs(
    title = "Precision vs Recall Plot",
    subtitle = "Performance of 3 Top Performing Models"
  )



# LIME ----

automl_leader <- xgboost2
predictions_tbl <- automl_leader %>% 
  h2o.predict(newdata = as.h2o(test_tbl)) %>%
  as.tibble() %>%
  bind_cols(
    test_tbl %>%
      select(XLABEL)
  )

predictions_tbl
#Single Explanation ----
explainer <- train_tbl %>%
  select(-XLABEL) %>%
  lime(
    model           = automl_leader,
    #bin_continuous  = TRUE,
    n_bins          = 3,
    quantile_bins   = TRUE
  )

test_tbl %>%
  filter(XLABEL==0)

explanation <- test_tbl %>%
  slice(34) %>%
  select(-XLABEL) %>%
  lime::explain(
    explainer = explainer,
    n_labels   = 1,
    n_features = 8,
    n_permutations = 5000,
    kernel_width   = 3
  )

plot_features(explanation = explanation, ncol = 1)

explanation %>%
  as.tibble() %>%
  select(feature:prediction) 

plot_features(explanation = explanation, ncol = 1)


explanation %>%
  as.tibble()

case_1 <- explanation %>%
  filter(case == 1)

case_1 %>%
  plot_features()


library(glue)

# Transformation
data_transformed <- case_1 %>%
  as.tibble() %>%
  mutate(
    feature_desc = as_factor(feature_desc) %>% 
      fct_reorder(abs(feature_weight), .desc = FALSE),
    key     = ifelse(feature_weight > 0, "Supports", "Contradicts") %>% 
      fct_relevel("Supports"),
    case_text    = glue("Case: {case}"),
    label_text   = glue("Label: {label}"),
    prob_text    = glue("Probability: {round(label_prob, 2)}"),
    r2_text      = glue("Explanation Fit: {model_r2 %>% round(2)}")
  ) %>%
  select(feature_desc, feature_weight, key, case_text:r2_text)

data_transformed


data_transformed %>%
  ggplot(aes(feature_desc, feature_weight, fill = key)) +
  geom_col() +
  coord_flip() +
  theme_tq() +
  scale_fill_tq() +
  labs(y = "Weight", x = "Feature") +
  facet_wrap(~ case_text + label_text + prob_text + r2_text,
             ncol = 1, scales = "free")


plot_features_tq <- function(explanation, ncol) {
  
  data_transformed <- explanation %>%
    as.tibble() %>%
    mutate(
      feature_desc = as_factor(feature_desc) %>% 
        fct_reorder(abs(feature_weight), .desc = FALSE),
      key     = ifelse(feature_weight > 0, "Supports", "Contradicts") %>% 
        fct_relevel("Supports"),
      case_text    = glue("Case: {case}"),
      label_text   = glue("Label: {label}"),
      prob_text    = glue("Probability: {round(label_prob, 2)}"),
      r2_text      = glue("Explanation Fit: {model_r2 %>% round(2)}")
    ) %>%
    select(feature_desc, feature_weight, key, case_text:r2_text)
  
  
  data_transformed %>%
    ggplot(aes(feature_desc, feature_weight, fill = key)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_tq() +
    scale_fill_tq() +
    labs(y = "Weight", x = "Feature") +
    theme(title = element_text(size = 9)) +
    facet_wrap(~ case_text + label_text + prob_text + r2_text,
               ncol = ncol, scales = "free")
  
}

explanation %>%
  filter(case %in% 1) %>%
  plot_features_tq(ncol = 2)

explanation %>%
  filter(case %in% 1:6) %>%
  plot_features(ncol = 2)



# 4.2 Recreating plot_explanations ----



explanation %>%
  as.tibble()

plot_explanations(explanation)


data_transformed <- explanation %>%
  as.tibble() %>%
  mutate(
    case    = as_factor(case),
    order_1 = rank(feature) 
  ) %>%
  # select(case, feature, feature_value, order_1) %>%
  # arrange(order_1)
  group_by(feature) %>%
  mutate(
    order_2 = rank(feature_value)
  ) %>%
  ungroup() %>%
  # select(case, feature, feature_value, order_1, order_2) %>%
  # arrange(order_1, order_2)
  mutate(
    order = order_1 * 1000 + order_2
  ) %>%
  # select(case, feature, feature_value, order_1, order_2, order) %>%
  # arrange(order)
  mutate(
    feature_desc = as.factor(feature_desc) %>% 
      fct_reorder(order, .desc =  T) 
  ) %>%
  select(case, feature_desc, feature_weight, label)

data_transformed %>%
  ggplot(aes(case, feature_desc)) +
  geom_tile(aes(fill = feature_weight)) +
  facet_wrap(~ label) +
  theme_tq() +
  scale_fill_gradient2(low = palette_light()[[2]], 
                       mid = "white",
                       high = palette_light()[[1]]) +
  theme(
    panel.grid = element_blank(),
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  ) +
  labs(y = "Feature", x = "Case", 
       fill = glue("Feature
                    Weight"))

plot_explanations_tq <- function(explanation) {
  
  data_transformed <- explanation %>%
    as.tibble() %>%
    mutate(
      case    = as_factor(case),
      order_1 = rank(feature) 
    ) %>%
    group_by(feature) %>%
    mutate(
      order_2 = rank(feature_value)
    ) %>%
    ungroup() %>%
    mutate(
      order = order_1 * 1000 + order_2
    ) %>%
    mutate(
      feature_desc = as.factor(feature_desc) %>% 
        fct_reorder(order, .desc =  T) 
    ) %>%
    select(case, feature_desc, feature_weight, label)
  
  data_transformed %>%
    ggplot(aes(case, feature_desc)) +
    geom_tile(aes(fill = feature_weight)) +
    facet_wrap(~ label) +
    theme_tq() +
    scale_fill_gradient2(low = palette_light()[[2]], mid = "white",
                         high = palette_light()[[1]]) +
    theme(
      panel.grid = element_blank(),
      legend.position = "right",
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    ) +
    labs(y = "Feature", x = "Case", 
         fill = glue("Feature
                         Weight"))
  
}

plot_explanations(explanation)

plot_explanations_tq(explanation)

