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
lapply(c("tidyverse", "ggplot2", "kableExtra", "readxl", "extrafont", "scales", "viridis"),
       pkgTest)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

####### DATA MANIPULATION #####
### Loading the data ###


rcv_ep1 <- read_csv("rcv_ep1.txt")|>
  mutate(MEPID = as.character(MEPID))

mep_info_26Jul11 <- read_excel("mep_info_26Jul11.xls", sheet = "EP1") |>
  rename(MEPID = `MEP id`)|>
  mutate(MEPID = as.character(MEPID))


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
                             "5"="Not_MEP")
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
      format= "latex",
    booktabs = TRUE,
    digits = 3
  ) |>
  kable_styling(latex_options = "HOLD_position")

### Merging datasets ###

mep_merged <- rcv_ep1 |>
  left_join(mep_info_26Jul11, by = "MEPID") |>
  select(!c(8:11))|>
  mutate( `NOM-D1` = as.numeric(`NOM-D1`),
         `NOM-D2` = as.numeric(`NOM-D2`))

any(is.na(mep_merged))
colSums(is.na(mep_merged))

mep_missing <- mep_merged |> 
  group_by(MEPID) |> 
  summarise(has_missing_coordinates = any(is.na(`NOM-D1`))) 

MEPs <- length(unique(mep_merged$MEPID)) 
MEP_not_coord <- sum(mep_missing$has_missing_coordinates) 

paste("Out of a total of ", MEPs,", ", MEP_not_coord, " have some missing information" )

### EP groups info ###

mep_merged <- mep_merged |>
  mutate(EPG = na_if(EPG, "0"))

vote_rate_epg <- mep_merged |> 
  filter(vote_decision %in% c("Yes", "No", "Abstain")) |> 
  group_by(EPG) |> 
  summarise( 
    yes = sum(vote_decision == "Yes"), 
    no = sum(vote_decision == "No"), 
    abstain = sum(vote_decision == "Abstain"), 
    yes_rate = yes / (yes + no + abstain),
    abstain_rate = abstain / (yes + no + abstain))

 yes_share <- vote_rate_epg |>
   select(EPG, yes_rate)
 
abstain_share <- vote_rate_epg |>
   select(EPG, abstain_rate)
 
mep_merged <- mep_merged |>
  mutate(
    `NOM-D1` = as.numeric(gsub(",", ".", `NOM-D1`)),
    `NOM-D2` = as.numeric(gsub(",", ".", `NOM-D2`))
  )

info_EPG_mean_coord <- mep_merged |>
  group_by(EPG) |>
  summarise(
    mean_coord1 = mean(`NOM-D1`, na.rm = TRUE),
    mean_coord2 = mean(`NOM-D2`, na.rm = TRUE),
  )

yes_share |>
  kbl(
    caption = "The mean rate of Yes votes - by EP group",
    format = "latex",
    booktabs = TRUE,
    digits = 3
  ) |>
  kable_styling(latex_options = "HOLD_position")

abstain_share |>
  kbl(caption="The mean abstention rate - by EP group",
      format= "latex",
      booktabs = TRUE,
      digits = 3
  ) |>
  kable_styling(latex_options = "HOLD_position")

info_EPG_mean_coord |>  
  kbl(caption="The mean of each coordinate dimension - by EP group",
                   format= "latex",
      booktabs = TRUE,
      digits = 3
  ) |>
  kable_styling(latex_options = "HOLD_position")
  

####### DATA MANIPULATION #####

### Data preparation ###
mep_merged_viz <- mep_merged |>
  select(MEPID, EPG, `NOM-D1`, `NOM-D2`) |> 
  unique() |> 
  na.omit()

### Visualisation 1 ###

windows()
# font_import(pattern = "Times", prompt = FALSE)
loadfonts(device = "pdf")

pdf("viz1.pdf", width = 8, height = 6, family = "Times")  

mep_merged_viz|>
ggplot( aes(x = `NOM-D1`, fill = EPG)) +
  geom_density(alpha = 0.7) +
  facet_wrap(vars(EPG), scales = "free_y") +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_fill_viridis_d(name = "EP group") +
  labs(
    title = "Coordinate 1 density distribution - by European Parliament Group",
    x = "\nNominate Coordinate 1",
    y = "\nDensity"
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "Times"), 
    plot.title = element_text(hjust = 0.5))

dev.off()

### Visualisation 2 ###

pdf("viz2.pdf", width = 8, height = 6, family = "Times")

mep_merged_viz |>
  ggplot(aes(x = `NOM-D1`, y = `NOM-D2`, color = EPG)) +
  geom_jitter(position = position_jitter(width = 0.2, height = 0.2), alpha = 0.5) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_color_viridis_d(name = "EP group") +
  labs(
    title = "Nominate coordinates for each Member of the European Parliament",
    x = "\nNominate Coordinate 1", 
    y = "\nNominate Coordinate 2"
  ) + 
  theme_bw() +
  theme(
    text = element_text(family = "Times"),
    plot.title = element_text(hjust = 0.5)
  )
dev.off()

### Visualisation 3 ###

yes_rate_viz <- mep_merged |> 
  filter(vote_decision %in% c("Yes", "No", "Abstain")) |> 
  group_by(EPG, vote_number) |> 
  summarise( 
    yes = sum(vote_decision == "Yes"), 
    no = sum(vote_decision == "No"), 
    abstain = sum(vote_decision == "Abstain"), 
    yes_rate = yes / (yes + no + abstain)) |>
  ungroup()

pdf("viz3.pdf", width = 8, height = 6, family = "Times")

yes_rate_viz |>
  ggplot(aes(x = yes_rate, y = EPG, fill = EPG))+
  geom_boxplot(alpha = 0.7) +
  coord_flip() +
  scale_x_continuous(labels = percent) +
  scale_fill_viridis_d(name = "EP group") +
  labs(
    title = "Proportion of Yes as vote decision - by European Parliament Group",
    x = "\nProportion of Yes per roll-call", 
    y = "\nEPG"
  ) + 
  theme_bw() +
  theme(
    text = element_text(family = "Times"),
    plot.title = element_text(hjust = 0.5)
  )
  
dev.off()

### Visualisation 4 ###

yes_rate_viz_ng <- mep_merged |> 
  filter(vote_decision %in% c("Yes", "No", "Abstain")) |> 
  mutate( NP = as.factor(NP)) |>
  group_by(NP) |> 
  summarise( 
    yes = sum(vote_decision == "Yes"), 
    no = sum(vote_decision == "No"), 
    abstain = sum(vote_decision == "Abstain"), 
    yes_rate_ng = yes / (yes + no + abstain)) |>
  ungroup()

pdf("viz4.pdf", width = 8, height = 6, family = "Times")

yes_rate_viz_ng |>
ggplot(aes(x = NP, y = yes_rate_ng, fill = yes_rate_ng)) +
  geom_col(alpha = 0.5, width = 0.7) +
  coord_flip() +
  scale_fill_viridis_c(name = "Yes vote share",
                       labels = percent) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Proportion of Yes as vote decision, over all roll-calls - by National Party",
    y= "\nProportion of Yes", 
    x = "\nNP"
  ) + 
  theme_bw() +
  theme(
    text = element_text(family = "Times"),
    plot.title = element_text(hjust = 0.5)
  )
dev.off()

