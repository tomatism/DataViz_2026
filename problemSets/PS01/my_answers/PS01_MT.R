######################
# Data Viz  
# Problem Set 1:  
# Tidyverse and ggplot        
######################

rm(list=ls())

detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

lapply(c("tidyverse", "ggplot2", "kableExtra"),  pkgTest)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

####### DATA MANIPULATION #####
### Loading the data ###

mep_info_26Jul11 <- read_csv2("mep_info_26Jul11.csv")
rcv_ep1 <- read_csv("rcv_ep1.txt")


### Pivot longer ###
rcv_ep1 <- rcv_ep1 |>
  pivot_longer(
    cols = starts_with("V"),
    names_to = "vote_number",
    values_to = "vote_decision"
  )
rcv_ep1$vote_decision <- recode_factor(rcv_ep1$vote_decision, 
                              "0"= "Absent",
                             "1"="Yes",
                             "2"="No",
                             "3"="Abstain",
                             "4"="Present_no_vote",
                             "5"="Absent_no_MEP")
### Summary table ###

summary_vote <- rcv_ep1 |>
  group_by(vote_decision) |>
  summarise(
    total_votes = n()
  )|>
  mutate(
    vote_share = total_votes / sum(total_votes)
  )
summary_vote |>
  kbl(caption="Summary count and share of MEPs vote decisions",
      format= "latex")
