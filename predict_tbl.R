

topredict <- tibble(
    PRODTYPE = 4,
    PRIMGENDER = "F",
    ISSUESTATE = "NC",
    DISTCHANNEL = 2,
    XPRIMRISKSTDCLASS = "Unknown",
    XPRIMAGE1 = 35,
    XANNPLANNEDPREM = 	467.6,
    XCREDITEDRATEBOY = -0.01725,
    XFAMTBOY = 	590000,
    XUMEMPRT = 	9.75,
    XMEDHINCOME = 47.0,
    #XDURATION  = 3,
    XLABEL = 0
)

str(topredict)
str(train_tbl)
ptble <- topredict
plotfeature <- function (ptble) {
    ptble <- bake(recipe_obj, new_data = ptble)
    explanation <- ptble %>%
        select(-XLABEL) %>%
        as.data.frame() %>%
        lime::explain(
            check.rows = FALSE,
            explainer = explainer,
            n_labels   = 1,
            n_features = 8,
            n_permutations = 5000,
            kernel_width   = 3
        )
    return (plot_features(explanation = explanation, ncol = 1))
}

plotfeature(topredict)

h2o.predict(xgboost2, as.h2o(topredict))


str(train_tbl)
train_tbl <- readRDS('../data/train_tbl.rds')

train_tbl <- train_tbl %>%
    select(-c(X, POLICYNO, X1PREMPERYEAR))


explainer <- train_tbl %>%
    select(-c(XLABEL)) %>%
    lime(
        model           = xgboost2,
        bin_continuous  = TRUE,
        n_bins          = 4,
        quantile_bins   = TRUE
    )

topredict <- tibble(
    PRODTYPE = 4,
    PRIMGENDER = "F",
    ISSUESTATE = "NC",
    DISTCHANNEL = 2,
    XPRIMRISKSTDCLASS = "Unknown",
    XPRIMAGE1 = 35,
    XANNPLANNEDPREM = 	467.6,
    XCREDITEDRATEBOY = -0.01725,
    XFAMTBOY = 	590000,
    XUMEMPRT = 	9.75,
    XMEDHINCOME = 47.0,
    XDURATION  = 3
)

col_factors_model <- c('XLABEL', 'DISTCHANNEL', 'PRODTYPE')
topredict[col_factors_model] <-
    lapply(topredict[col_factors_model], factor)


plotfeature <- function (ptble) {
    ptble <- bake(recipe_obj, new_data = ptble)
    explanation <- ptble %>%
        select(-XLABEL) %>%
        as.data.frame() %>%
        lime::explain(
            check.rows = FALSE,
            explainer = explainer,
            n_labels   = 1,
            n_features = 8,
            n_permutations = 5000,
            kernel_width   = 3
        )
    return (plot_features(explanation = explanation, ncol = 1))
}

plotfeature(topredict)

topredict <- topredict %>%
    select(-c(XDURATION))

h2o.init(max_mem_size = "24g")
predicLapse <- function(topredict) {
    rettbl <- tibble(field = numeric(), value = double())
    pred <- as.data.frame(h2o.predict(xgboost2, as.h2o(topredict)))
    p1 <- pull(pred, p1)
    p0 <- pull(pred, p0)
    if (p1 > p0) {
        return (p1)
    } else {
        return (p0)
    }
    
}


xgboost2 <-
    h2o.loadModel('./mllab/5770/h2o_models/millenial/XGBoost_2_AutoML_20191028_210616')
print(predicLapse(topredict))
as.data.frame(topredict)
