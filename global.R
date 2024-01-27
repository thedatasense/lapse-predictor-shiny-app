# CRAN Packages ----
pkgs_cran <- c(
    # File System
    "fs",         # working with the file system
    
    # Import
    "readxl",     # reading excel files
    "writexl",    # saving data as excel files
    "odbc",       # connecting to databases
    "RSQLite",    # connecting to SQLite databases
    
    # Tidy, Transform, & Visualize
    "tidyverse",  # dplyr, ggplot2, tibble, tidyr, readr, purrr, stringr, forcats
    "lubridate",  # working with dates and times
    "tidyquant",  # used mainly for the ggplot plotting theme
    
    # Model
    "tidymodels", # installs broom, infer, recipes, rsample, & yardstick
    "umap",       # used for visualizing clusters
    
    # Other
    "devtools" ,   # used to install non-CRAN packages,
    
    "data.table", #data table is faster than data frame
    
    "VIM",
    "naniar",
    "missMDA",
    "Amelia",
    "mice",
    "missForest",
    "FactoMineR",
    "ggplot2"
)


install.packages("dataPreparation",dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages("DataExplorer",dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages("leaflet",dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages("ggplot2",dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages("tidyverse",dependencies=TRUE, repos='http://cran.rstudio.com/')
library(DataExplorer)
library(tidyverse)
library(ggplot2)
library()
install.packages("VIM",dependencies=TRUE, repos='http://cran.rstudio.com/') 


install.packages("naniar",dependencies=TRUE, repos='http://cran.rstudio.com/')  
library(VIM)
library(naniar)
install.packages(pkgs_cran,dependencies=TRUE, repos='http://cran.rstudio.com/')  # Install many packages

require(pkgs_cran)
lapply(pkgs_cran, require,character.only = TRUE)
do.call("require", pkgs_cran)

library(tibble) # loading data frame as a tibble, https://tibble.tidyverse.org/ 
rdata <- as_tibble(read.csv("x_pp_limra_ds_v2.csv"))
standardRC <- as_tibble(read.csv("standardRC.csv"))
glimpse(standardRC)

#Preview the data
glimpse(rdata)





#join the tbl using left join.
rdatawRC <- left_join(rdata,standardRC,by("PrimRiskClass","PrimRiskClass"))

#(BK)If you look at the data, status code, product type,distchannel is read as integer where it should be a factor. 
col_factors <- c('PRODTYPE','STATUSCODE','DISTCHANNEL')
rdata[col_factors] <- lapply(rdata[col_factors], factor)  ## as.factor() could also be used

#(BK) Columns to drop, rdatanSec is the frame without secondary policy holder information.
rdatanSec=subset(rdata, select = - c(PRODFORM,GUARFAMTBOY,GUARCREDITEDRATE,CREDITEDRATEBOY,SECGUAR,SECGUARIND,CURRENTPREM,TERMDATE,LOANBOY,SECGENDER,SECDOB,SECRISKCLASS,LOANBOY,XMONTHSACTIVE,XSECAGEINYEARS,XYEARSACTIVE,PRIMDOB,ZIP,XSECRISKSTDCLASS))
#(BK) Save the tbl as RDS where it will be faster to retrieve later, much faster. 
saveRDS(rdatanSec, file = "x_pp_limra_ds_noSec.rds")
saveRDS(rdata, file = "x_pp_limra_ds_v2.rds")

rdata <- readRDS("x_pp_limra_ds_v2.rds")
rdatanSec <-  readRDS("x_pp_limra_ds_noSec.rds")

glimpse(rdata)

#(BK) Plot missing data
plot_intro(rdatanSec)
b_size = 15
plot_missing(rdatanSec,ggtheme = theme_minimal(base_size = b_size, base_family = "", base_line_size = b_size/22, base_rect_size = b_size/22))

library(tidyverse)
library(lubridate)
library(tidyquant)
install.packages("glue")

devtools::install_github("tidyverse/glue")

rdatanSec %>%
    select(POLICYNO) %>%
    summarise(TotalPolicies = n_distinct(POLICYNO)) 

count_by_class_tbl <- rdatanSec %>%
    select( POLICYNO,LABEL) %>%
    group_by(LABEL) %>%
    summarize(Total = n_distinct(POLICYNO)) %>%
    ungroup()

rdatanSec %>%
    select( POLICYNO,XSTATUSCODE) %>%
    group_by(XSTATUSCODE) %>%
    summarize(Total = n_distinct(POLICYNO)) %>%
    ungroup()


# Bar Plot
count_by_class_tbl %>%
    ggplot(aes(LABEL, Total)) +
    geom_col(fill = "#2c3e50") + 
    coord_flip()

?lag

rdatanSec %>%
    select(POLICYNO,OBSYR,CUMPREM) %>%
    mutate(PremiumPerYear = CUMPREM - lag(CUMPREM,n=1,by=POLICYNO)) %>%
    View()

# install.packages("DBI",dependencies=TRUE, repos='http://cran.rstudio.com/')
# install.packages("odbc",dependencies=TRUE, repos='http://cran.rstudio.com/')
# library(DBI)
# library(odbc)
# 
# 
# con <- dbConnect(odbc::odbc(), "cpdata", UID="ironman", PWD= rstudioapi::askForPassword("jarvis"))



rdatanSec 
    select()

require(reshape2)
ggplot(data = melt(rdatanSec), aes(x=variable, y=value)) + geom_boxplot(aes(fill=variable))


## Cleaning up secondary data

library(tidyr)
library(dplyr)

unempr=read.csv("UnemploymentRate.csv")

unemp <- unempr %>% gather(Year,UnemploymentRate,Unemployment_Rate_2009:Unemployment_Rate_2014)
unemp <- unemp %>% separate(Year,into=c('TBR1','TBR2','ObsYear'),sep = '_' )
unemp <- unemp %>%  select(ï..State,Issue_State,ObsYear,UnemploymentRate)
unemp <- unemp %>% rename(State=ï..State)
View(unemp)

hinc <-read.csv("median_house_hold_income.csv")
hinc <- hinc %>% gather(Year,MedianHouseHoldIncome,year_2009:year_2014)
hinc <- hinc %>% separate(Year,into=c('TBR1','ObsYear'),sep = '_' )
hinc <- hinc %>%  select(ï..State,Issue_State,ObsYear,MedianHouseHoldIncome)
hinc <- hinc %>% rename(State=ï..State)
View(hinc)

unemphinc <- inner_join(unemp,hinc)
glimpse(unemphinc)

unemphinc %>% write.csv(file="unempincome.csv")

glimpse(rdatanSec)


#=## Fields to Drop for modelling

fieldsToDrop <- c(COCODE,ISSUEDATE,PRIMRISKCLASS,STATUSCODE,AVBOY,FAMTBOY,SGUARPERIOD,EXTMATOP,TARGETPREM,CVENRIDER,LTCRIDER,LABEL,XAVFMTMISS,XTERMDATE)

dataFModel <- rdatanSec %>% 
    subset(select = - c(COCODE,ISSUEDATE,PRIMRISKCLASS,STATUSCODE,AVBOY,FAMTBOY,SGUARPERIOD,TARGETPREM,CVENRIDER,LTCRIDER,LABEL,XAVFMTMISS,XTERMDATE,OBSYR,EXTMATOPT))


glimpse(dataFModel)  


dataForTest <- dataFModel %>%
    filter(POLICYNO == 10000)
    

dataFModel <- dataFModel %>%
    group_by(PRODTYPE,POLICYNO,PRIMGENDER,ISSUESTATE,DISTCHANNEL,FUNDINGPATTERN,CUMPREM,ANNPLANNEDPREM,DURATION,XSECPOLICYHOLDER,XPRIMRISKSTDCLASS,XSTATUSCODE) %>%
    summarize(avg_premium = avg(CUMPREM),max_duration=max(DURACTION))

dataForTest %>%
    group_by(PRODTYPE,POLICYNO,PRIMGENDER,ISSUESTATE,DISTCHANNEL,FUNDINGPATTERN,ANNPLANNEDPREM,XSECPOLICYHOLDER,XPRIMRISKSTDCLASS) %>%
    summarize(avg_premium = mean(CUMPREM),max_duration=max(DURATION)) %>% View()

modelL1 <- dataFModel %>%
    group_by(PRODTYPE,POLICYNO,PRIMGENDER,ISSUESTATE,DISTCHANNEL,FUNDINGPATTERN,ANNPLANNEDPREM,XSECPOLICYHOLDER,XPRIMRISKSTDCLASS) %>%
    summarize(avg_premium = mean(CUMPREM),max_duration=max(DURATION))


