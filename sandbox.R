rdata2 <- rdata %>%
    group_by(POLICYNO) %>%
    filter(n_distinct(OBSYR) > 3) %>%
    ungroup()

library(skimr)

skim(rdata2)

str(rdata2)

rdata %>%
    filter(POLICYNO=='1010') %>%
    View()

rdatanSec=subset(rdata, select = - c(PRODFORM,GUARFAMTBOY,GUARCREDITEDRATE,CREDITEDRATEBOY,SECGUAR,SECGUARIND,CURRENTPREM,TERMDATE,LOANBOY,SECGENDER,SECDOB,SECRISKCLASS,LOANBOY,XMONTHSACTIVE,XSECAGEINYEARS,XYEARSACTIVE,PRIMDOB,ZIP,XSECRISKSTDCLASS))


#2106359

rdata3 <- rdata2 %>%
    select(POLICYNO,GUARFAMTBOY,GUARCREDITEDRATE,CREDITEDRATEBOY,CURRENTPREM,TARGETPREM,FAMTBOY,AVBOY) %>%
    distinct()


str(rdata3)

skimr()

install.packages("plotly")
library(plotly)


install.packages("listviewer")
library(listviewer)
