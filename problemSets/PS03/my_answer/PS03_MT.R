############## MATILDA TOMATIS - DATA VIZ - PROBLEM SET 2 ##########

# Remove objects
rm(list=ls())

# Detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# Load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# Load any necessary packages
lapply(c("tidyverse", "ggridges", "ggplot2", "kableExtra", "scales", 
         "viridis", "patchwork", "extrafont", "ggrepel"),
       pkgTest)

# Set working directory for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

### DATA MANIPULATION ###

# Loading and filtering data 

ces2015_raw <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/DataViz_2026/refs/heads/main/datasets/CES2015.csv")

ces2015 <- ces2015_raw|> 
  filter(discard == "Good quality") 

ces2015 <- ces2015 |>
  mutate(
    p_voted = case_when(
      p_voted == "Yes" ~ "Yes",
      p_voted == "No" ~ "No",
      p_voted %in% c("Don't know", "Refused") ~ NA_character_
    ))

# Preparing the age binned 

ces2015 <- ces2015 |> 
  mutate( age = as.numeric(age), 
          age_years = 2015 - age )|> 
  #removing wrongly coded observation
  filter(age_years != 1015) 

unique(ces2015$age_years)


ces2015$age_cat <- cut(
  ces2015$age_years,
  breaks = c(-1, 29, 44, 64, 115),
  labels = c("<30", "30-44", "45-64", "65+"),
  right = TRUE)

#### DATA VISUALISATION ####

windowsFonts(Times = windowsFont("TT Times New Roman"))
windowsFonts()

update_geom_defaults("text", list(family = "Times"))
update_geom_defaults("label", list(family = "Times"))
update_geom_defaults("label_repel", list(family = "Times"))


## Viz 1 ##

total_valid <- ces2015 |>
  filter(p_voted %in% c("Yes", "No")) |>
  nrow()

ces2015_viz1 <- ces2015 |>
  filter(p_voted %in% c("Yes", "No")) |>
  group_by(age_cat) |>
  summarise(
    yes_count = sum(p_voted == "Yes", na.rm = TRUE),
    turnout_rate = yes_count / total_valid
  )

pdf("viz1.pdf", family = "Times")

ggplot(data = ces2015_viz1, aes(x = age_cat, y = turnout_rate, fill = age_cat)) +
  geom_col(alpha = 0.6) +
  geom_text(aes(label = scales::percent(turnout_rate, accuracy = 1)),
            position = position_stack(vjust = 0.5),
            size = 3) +
  scale_fill_viridis_d(name = "age_cat") +
  labs(title = "Turnout rate by age group",
       subtitle = "Canadian Election Study - 2015",
       x = "\nAge category",
       y = "Turnout rate\n") +
  theme_bw() +
  theme(
    text = element_text(family = "Times"),   
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none"
  )

dev.off()

## Viz 2 ## 

unique(ces2015$p_selfplace)
str(ces2015$p_selfplace)
ces2015_viz2 <- ces2015 |>
  mutate(p_selfplace = as.numeric(p_selfplace))|> 
  filter(vote_for %in% c(
    "Liberal",
    "Conservatives",
    "ndp",
    "Bloc Quebecois",
    "Green Party"),
  p_selfplace %in% 0:10)
  
  
unique(ces2015_viz2$p_selfplace)
unique(ces2015_viz2$vote_for)

pdf("viz2.pdf", family = "Times")
ggplot(data = ces2015_viz2, aes(x = p_selfplace, fill = vote_for))+
  geom_density(alpha = 0.6) +
  facet_wrap(vars(vote_for)) +
  scale_fill_viridis_d(name = "vote_for")+
labs(title = "Left-right density distribution - Main parties",
     subtitle = "Canadian Election Study - 2015",
     x = "\nLeft-Right placement",
     y = "Probability density\n") +
  theme_bw() +
  theme(
    text = element_text(family = "Times"),  
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(size = 8 , hjust = 0),
    panel.border = element_rect(color = "darkgrey"),
    legend.position = "none"
  )
dev.off()

## Viz 3 ##

unique(ces2015$province)

ces2015 <- ces2015 |>
  #removing wrongly coded observation
  filter(province != 1000) |>
  mutate(
    province_long = recode(
      province,
      "bc" = "British Columbia",
      "Alberta" = "Alberta",
      "Sask" = "Saskatchewan",
      "Manitoba" = "Manitoba",
      "Ontario" = "Ontario",
      "Quebec" = "Quebec",
      "nb" = "New Brunswick",
      "ns" = "Nova Scotia",
      "pei" = "Prince Edward Island",
      "Nfld" = "Newfoundland & Labrador",
      "nwt" = "Northwest Territories",
      "Yukon" = "Yukon",
      "nunavut" = "Nunavut"
    )
  ) |>
  filter(!province_long %in% c("Yukon", "Nunavut"))

ces2015_viz3 <- ces2015 |>
  #removing non-income level valuesp
  filter(!income_full %in% c("", ".d", ".r")) |>
  group_by(province_long, income_full) |>
  summarise(
    yes_count = sum(p_voted == "Yes", na.rm = TRUE),
    .groups = "drop"
  )


pdf("viz3.pdf", family = "Times")
ggplot(data = ces2015_viz3, aes(x = income_full, y = yes_count, fill = province_long)) +
  geom_col(alpha = 0.6) +
  scale_fill_viridis_d(name = "province_long") +
  facet_wrap(vars(province_long)) +
  coord_flip()+
  labs(title = "Turnout count by income level",
       subtitle = "Canadian Election Study - 2015",
       x = "Income level\n",
       y = "\nTurnout count") +
  theme_bw() +
  theme(
    text = element_text(family = "Times"), 
    strip.text = element_text(size = 7 , hjust = 0),
    panel.border = element_rect(color = "darkgrey"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "none"
  )
dev.off()

  
## Task 4 ##

PS03_theme <- theme_bw(base_family = "Times", base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = rel(1.5), hjust = 0.5),
    plot.subtitle = element_text(face = "plain", size = rel(1.2), color = "grey20", hjust = 0.5),
    plot.caption = element_text(face = "italic", size = rel(0.8), color = "grey30", hjust = 0.5),
    strip.text = element_text(face = "bold", size = 9, hjust = 0.5),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    strip.background = element_rect(fill = "grey90", color = NA),
    axis.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.border = element_rect(color = "darkgrey", fill = NA),
    panel.background = element_rect(fill = "#FFF8E7", color = NA),
    panel.grid.major = element_line(color = "grey85"),
    legend.position = "none"
  )

low_count <- ces2015_viz3 |>
  group_by(province_long)|>
  slice_min(yes_count, n = 1) |>
  ungroup()

pdf("viz4.pdf", width = 10, height = 7, family = "Times")
ggplot(data = ces2015_viz3, aes(x = income_full, y = yes_count, fill = province_long)) +
  geom_col(alpha = 0.6) +
  scale_fill_viridis_d(name = "province_long") +
  facet_wrap(vars(province_long)) +
  coord_flip()+
  geom_text_repel(data = low_count,
                  aes(x = income_full, y = yes_count, label = "Lower turnout"),
                  nudge_y = 300,    
                  size = 2.5,
                  color = "darkred",
                  fontface = "bold",
                  family = "Times",
                  arrow = arrow(length = unit(0.07, "in"), type = "closed"), 
                    segment.color = "darkred",                
                    segment.size = 0.2)+
  labs(
    title = "Turnout count shows a U-shaped pattern across income levels in each region",
    subtitle = "Number of people who voted in each income bracket, for each of the Canadian regions (2015 CES)",
    caption = "Data source: Canadian Election Study 2015. \nCounts are unweighted. Only respondents reporting Yes for voting were included in the count. Only respondents reporting income were kept.
    The provinces of Yukon and Nunavut were removed for a lack of data.",
    x = "Income level\n",
    y = "\nTurnout count") + 
  PS03_theme
  
dev.off()


  
