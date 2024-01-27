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
    "writexl",     # Writing to excel files
    "shinydashboard", 
    "shinythemes", 
    "shinyjs", 
    "shinyBS", 
    "shinyWidgets", 
    "shinycssloaders"
)

install.packages(pkgs)


# Libraries
library(recipes)
library(tidyverse)
library(tidyquant)



#Read the data as a tibble ----
rdata <- as_tibble(read.csv("./data/x_pp_limra_ds_v2.csv"))
saveRDS(rdata,file='./data/x_pp_limra_ds_v2.rds')
rdata <- readRDS("./data/x_pp_limra_ds_v2.rds")
rdatanSec <-  readRDS("x_pp_limra_ds_noSec.rds")

glimp
#(BK) Columns to drop, rdatanSec is the frame without secondary policy holder information.
rdatanSec=subset(rdata, select = - c(PRODFORM,GUARFAMTBOY,GUARCREDITEDRATE,CREDITEDRATEBOY,SECGUAR,SECGUARIND,CURRENTPREM,TERMDATE,LOANBOY,SECGENDER,SECDOB,SECRISKCLASS,LOANBOY,XMONTHSACTIVE,XSECAGEINYEARS,XYEARSACTIVE,PRIMDOB,ZIP,XSECRISKSTDCLASS))
#(BK) Save the tbl as RDS where it will be faster to retrieve later, much faster. 
saveRDS(rdatanSec, file = "x_pp_limra_ds_noSec.rds")
saveRDS(rdata, file = "x_pp_limra_ds_v2.rds")

#(BK)If you look at the data, status code, product type,distchannel is read as integer where it should be a factor. 
col_factors <- c('PRODTYPE','XSTATUSCODE','DISTCHANNEL')

rdata[col_factors] <- lapply(rdata[col_factors], factor)  ## as.factor() could also be used
rdatanSec[col_factors] <- lapply(rdatanSec[col_factors], factor)  ## as.factor() could also be used


#Drop the columns that are not relevant for modelling, I know its a bad practice to perform variable selection, however I believe these are optional 
dataFModel <- rdatanSec %>% 
    subset(select = - c(COCODE,CUMPREM,ISSUEDATE,PRIMRISKCLASS,STATUSCODE,AVBOY,FAMTBOY,SGUARPERIOD,TARGETPREM,CVENRIDER,LTCRIDER,XAVFMTMISS,XTERMDATE,OBSYR,EXTMATOPT,DURATION)) %>% 
    group_by(PRODTYPE,POLICYNO,PRIMGENDER,ISSUESTATE,DISTCHANNEL,FUNDINGPATTERN,ANNPLANNEDPREM,XSECPOLICYHOLDER,XPRIMRISKSTDCLASS,LABEL) %>%
    ## For the purpose of modelling, I am taking the mean of XPREMPERYEAR and the maximum value of duration
    summarize(avgyrpremium = mean(XPREMPERYEAR),maxdaysactive=max(XDAYSACTIVE),xprimeage=max(XPRIMAGEINYEARS),XLABEL=max(LABEL)) %>%
    ungroup()

dataFModel <- dataFModel %>% 
    subset(select = - c(maxdaysactive,FUNDINGPATTERN,avgyrpremium,LABEL))



col_factors_model <- c('XSECPOLICYHOLDER','PRODTYPE','DISTCHANNEL','XLABEL')
dataFModel[col_factors_model] <- lapply(dataFModel[col_factors_model], factor)  ## as.factor() could also be used

str(dataFModel)

saveRDS(dataFModel, file = "./data/data_modelling_v3.rds")

# This is a function to plot the histogram of all factor variables. 
plot_hist_facet <- function(data, bins = 10, ncol = 5,
                            fct_reorder = FALSE, fct_rev = FALSE, 
                            fill = palette_light()[[3]], 
                            color = "white", scale = "free") {
    
    data_factored <- data %>%
        mutate_if(is.character, as.factor) %>%
        mutate_if(is.factor, as.numeric) %>%
        gather(key = key, value = value, factor_key = TRUE) 
    
    if (fct_reorder) {
        data_factored <- data_factored %>%
            mutate(key = as.character(key) %>% as.factor())
    }
    
    if (fct_rev) {
        data_factored <- data_factored %>%
            mutate(key = fct_rev(key))
    }
    
    g <- data_factored %>%
        ggplot(aes(x = value, group = key)) +
        geom_histogram(bins = bins, fill = fill, color = color) +
        facet_wrap(~ key, ncol = ncol, scale = scale) + 
        theme_tq()
    
    return(g)
    
}

dataFModel <- dataFModel %>%
    group_by(PRODTYPE,POLICYNO,PRIMGENDER,ISSUESTATE,DISTCHANNEL,XSECPOLICYHOLDER,XPRIMRISKSTDCLASS,XLABEL) %>%
    summarise(XPRIMAGE1=max(xprimeage), XANNPLANNEDPREM=max(ANNPLANNEDPREM)) %>%
    distinct() %>%
    ungroup()


dataFModel <- dataFModel %>%
    group_by(PRODTYPE,POLICYNO,PRIMGENDER,ISSUESTATE,DISTCHANNEL,XSECPOLICYHOLDER,XPRIMRISKSTDCLASS,XLABEL) %>%
    summarise(XPRIMAGE2=max(XPRIMAGE1), XANNPLANNEDPREM=max(ANNPLANNEDPREM)) %>%
    distinct() %>%
    ungroup()


dataFModel %>%
    select(POLICYNO) %>%
    distinct()

dataFModel <- dataFModel %>%
    group_by(POLICYNO) %>%
    filter(n() > 1) %>%
    ungroup()

dataFModel <- dataFModel %>%
    distinct(POLICYNO, .keep_all= TRUE)

dataFModel %>%
    filter(POLICYNO =='2380040002') %>%
    View()

dataFModel %>%
    select(XSTATUSCODE, everything()) %>%
    plot_hist_facet(bins = 10, ncol = 5, fct_rev = F)


dataForTest <- dataFModel %>%
    filter(POLICYNO == 10000)


unempincome <- as_tibble(read.csv('./data/unempincome.csv'))




rdata <- rdata %>%
    group_by(POLICYNO) %>%
    filter(n_distinct(OBSYR) > 3) %>%
    ungroup()

rdata3 <- rdata %>%
    select(POLICYNO,GUARCREDITEDRATE,CUMPREM,CREDITEDRATEBOY,CURRENTPREM,TARGETPREM,FAMTBOY,AVBOY,XPREMPERYEAR,XSTATUSCODE,LABEL,OBSYR) %>%
    distinct()

rdata3 <- rdata3 %>% 
    group_by(POLICYNO) %>%
    mutate(XRANK=dense_rank(OBSYR)) %>%
    ungroup()

rdata3 <- rdata3 %>%
    #filter(POLICYNO=='5061229840') %>%
    group_by(POLICYNO) %>%
    mutate(LAG.CREDITEDRATEBOY = as.double(CREDITEDRATEBOY - lag(CREDITEDRATEBOY, n = 1, default = NA))) %>%
    mutate(LAG.XPREMPERYEAR = as.double(XPREMPERYEAR - lag(XPREMPERYEAR, n = 1, default = NA))) %>%
    mutate(LAG.AVBOY = as.double(AVBOY - lag(AVBOY, n = 1, default = NA)) )%>%
    select(POLICYNO,OBSYR,CREDITEDRATEBOY,LAG.CREDITEDRATEBOY,XPREMPERYEAR,LAG.XPREMPERYEAR,XSTATUSCODE,LABEL,XRANK,FAMTBOY,AVBOY,LAG.AVBOY) %>%
    ungroup()

saveRDS(rdata3,file='./data/yearlyvarying.rds')

str(unempincome)

unempincome <- unempincome %>% 
    group_by(Issue_State) %>%
    summarise(XUMEMPRT=median(UnemploymentRate),XMEDHINCOME=median(as.double(MedianHouseHoldIncome)))
dataFModel1 <- dataFModel %>% inner_join(rdata3,by="POLICYNO")

dataFModel1 <- dataFModel1 %>% left_join(unempincome,by = c("ISSUESTATE" = "Issue_State"))

saveRDS(dataFModel1,file='./data/data_modelling_V4.rds')
write_csv(dataFModel1,'./data/data_modelling_V4.csv',col_names = TRUE)
skim(dataFModel1)

## Join datasets

test = readRDS('./data/yearlyvarying.rds')
test2 = readRDS('./data/x_pp_limra_ds_v2.rds')


test2 %>%
    filter(POLICYNO==10005)  %>%
    View()




# 1. Zero Variance Features ---- 
# these are features that do not have a variance, and doesnt hold any value to the model
recipe_obj <- recipe(XSTATUSCODE ~ ., data = dataFModel) %>%
    step_zv(all_predictors())

recipe_obj



# 2. Transformations ---- 

skewed_feature_names <- dataFModel %>%
    select_if(is.numeric) %>%
    map_df(skewness) %>%
    gather(factor_key = T) %>%
    arrange(desc(value)) %>%
    filter(value >= 0.8) %>%
    pull(key) %>%
    as.character()


dataFModel %>%
    select(skewed_feature_names) %>%
    plot_hist_facet()

recipe_obj <- recipe(XSTATUSCODE ~ ., data = dataFModel) %>%
    step_zv(all_predictors()) %>%
    step_YeoJohnson(skewed_feature_names)

recipe_obj %>% 
    prep() %>% 
    bake(dataFModel) %>%
    select(skewed_feature_names) %>%
    plot_hist_facet()

# 3. Center/Scaling -----

dataFModel %>%
    select_if(is.numeric) %>%
    plot_hist_facet()

dataFModel <- dataFModel %>%
    select(-c ('ISSUESTATE'))
factor_names <- c('XPRIMRISKSTDCLASS','PRIMGENDER')

recipe_obj <- recipe(XSTATUSCODE ~ ., data = dataFModel)%>%
    step_zv(all_predictors()) %>%
    step_YeoJohnson(skewed_feature_names) %>%
    step_center(all_numeric()) %>%
    step_scale(all_numeric())
glimpse(dataFModel)

prepared_recipe <- recipe_obj %>% prep()

prepared_recipe$steps[[4]]


prepared_recipe %>%
    bake(newdata = dataFModel) %>%
    select_if(is.numeric) %>%
    plot_hist_facet()


# 4. Dummy Variables ----

dummied_recipe_obj <- recipe(XSTATUSCODE ~ ., data = dataFModel)%>%
    step_zv(all_predictors()) %>%
    step_YeoJohnson(skewed_feature_names) %>%
    step_center(all_numeric()) %>%
    step_scale(all_numeric()) %>%
    step_dummy(all_nominal())

# Final Recipe -----

recipe_obj <- recipe(XSTATUSCODE ~ ., data = dataFModel)%>%
    step_zv(all_predictors()) %>%
    step_YeoJohnson(skewed_feature_names) %>%
    step_center(all_numeric()) %>%
    step_scale(all_numeric()) %>%
    step_dummy(XPRIMRISKSTDCLASS,PRIMGENDER,XSTATUSCODE) %>%
    prep()





## Add unemployment and income to the variable set. Check the covariance of this with status code.
## Perform the analysis for 3 different age groups. young, middle, old


## New data set with unique entries.

uniquePolicy <- read.csv("UniquePolicyNumber.csv")

glimpse(uniquePolicy)

dataFModel <- uniquePolicy %>%
    subset(select = -c(XTERMDATE,XYEARSACTIVE,PRIMDOB,XMONTHSACTIVE,XDAYSACTIVE,XLASTOBSYEAR,XFIRSTOBSYEAR,XLOANFLAG,XSTATUSCODE,ISSUEDATE))

library(caret)
train.index <- createDataPartition(dataFModel$XLABEL, p = .7, list = FALSE)
train <- dataFModel[ train.index,]
test  <- dataFModel[-train.index,]

write.csv(train,file='training_split.csv',col.names = TRUE)
write.csv(test,file='test_split.csv',col.names = TRUE)
















