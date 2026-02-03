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
lapply(c("tidyverse", "ggridges", "ggplot2", "kableExtra", "scales", "viridis", "patchwork"),
       pkgTest)

# Set working directory for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

####### DATA MANIPULATION ######

### Loading the data ###

NCSS_raw <- read.csv("NCSS_v1.csv")
NCSS <- NCSS_raw |>
  select(CASEID, YEAR, GDREGION, NUMOFFMBR, TRAD6,
         TRAD12, INCOME)

#It is useful to check unique values, ù
#   to avoid any obvious error in the variables entries
NCSS_unique <- lapply(
  NCSS[c("YEAR", "GDREGION", "TRAD6", "TRAD12", "NUMOFFMBR")],
  unique
)
print(NCSS_unique)

### Filtering for religions ###

NCSS <- NCSS |>
  filter(TRAD6 %in% c("Chrétiennes", "Juives", "Musulmanes"))


### Computing count by religious affiliation ###

NCSS_count <- NCSS |>
  group_by(YEAR, TRAD6) |>
  summarise(
    count = n(),
    mean_income   = mean(INCOME, na.rm = TRUE),
    median_income = median(INCOME, na.rm = TRUE),
    .groups = "drop"
  )

NCSS_count |>
  kbl(
    caption = "Count & summary statistics - by year and religious classification",
    format = "latex",
    booktabs = TRUE,
    digits = 3
  ) |>
  kable_styling(latex_options = "HOLD_position")


### Creating the new variable ###


NCSS <- NCSS |>
  group_by(YEAR, TRAD6) |>                           
  mutate(
    mean_income = mean(INCOME, na.rm = TRUE),     
    AVG_INCOME = case_when(
      is.na(INCOME) ~ NA_integer_,
      INCOME >= mean_income~ 1L,                  
      TRUE ~ 0L                                        
    )
  ) |>
  ungroup() |>
  select(-mean_income) 


######### DATA VISUALIZATION #########

## DATA VIZ 1 ##

NCSS_viz1 <- NCSS |>
  group_by(YEAR, TRAD12) |>
  summarise(prop_above = mean(AVG_INCOME, na.rm = TRUE), .groups = "drop") |>
  mutate(prop_below = 1 - prop_above) |>
  pivot_longer(cols = c(prop_above, prop_below), 
               names_to = "IncomeLevel", values_to = "Proportion") |>
  mutate(IncomeLevel = recode(IncomeLevel,
                              prop_above = "Above Average",
                              prop_below = "Below Average"))


my_colors <- viridis(5, option = "D")[c(3, 4)]  

pdf("viz1.pdf", width = 10, height = 8, family = "Times")

ggplot(NCSS_viz1, aes(x = TRAD12, y = Proportion, fill = IncomeLevel)) +
  geom_col(position = "stack") +
  geom_text(aes(label = scales::percent(Proportion, accuracy = 1)),
            position = position_stack(vjust = 0.5),
            size = 3) +
  facet_wrap(~ YEAR) +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = my_colors, name = "Income Level") +
  labs(
    title = "Proportion of congregations above and below average income by TRAD12",
    x = "\nTRAD12",
    y = "Proportion of congregations\n"
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "Times"),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

dev.off()

## DATA  VIZ 2 ##

# Calculating number of official members #

NCSS_viz2 <- NCSS |>
  filter(YEAR == 2022) |>
  group_by(TRAD6, TRAD12) |>
  summarise(
    total_members = sum(NUMOFFMBR, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(total_members > 0)

#Visualization 2 #

pdf("viz2.pdf", width = 10, height = 8, family = "Times")

NCSS_viz2 |>
  ggplot(aes(x = TRAD6, y = total_members, fill = TRAD12)) +
  geom_col(position = "dodge")+
  scale_fill_viridis_d(name = "TRAD12") +
  labs(
    title = "Number of official members - by 12-level religious classification",
    subtitle = "Year 2022",
    x = "TRAD-12\n", 
    y = "\nNumber of members\n"
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "Times"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 40, hjust = 1, vjust = 1)
  )
dev.off()

## DATA  VIZ 3 ##


smaller_viz3 <- NCSS |>
  filter(YEAR == 2022) |>
  na.omit()|>
ggplot(aes(x = INCOME, y = GDREGION, fill = GDREGION)) +
  geom_density_ridges(alpha = 0.7, color = "white", scale = 1.2,
                      rel_min_height = 0.01)  +
  coord_cartesian(xlim = c(0, 1000000))+
  scale_fill_viridis_d(name = "GDREGION") +
  labs(title = "Income Distribution - by Swiss Region",
       subtitle = "Year 2022",
        x = "\nIncome (Maximum: 1 milion)",
       y = "Region\n",
       fill = "Region") +
  theme_bw() +
  theme(
    text = element_text(family = "Times"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none"
  )
bigger_viz3 <- NCSS |>
  filter(YEAR == 2022) |>
  na.omit()|>
  ggplot(aes(x = INCOME, y = GDREGION, fill = GDREGION)) +
  geom_density_ridges(alpha = 0.7, color = "white", scale = 1.2,
                      rel_min_height = 0.01)  +
  coord_cartesian(xlim = c(0, 3000000))+
  scale_fill_viridis_d(name = "GDREGION") +
  labs(title = "Income Distribution - by Swiss Region",
       subtitle = "Year 2022",
       x = "\nIncome (Maximum: 3 milion)",
       y = "Region\n",
       fill = "Region") +
  theme_bw() +
  theme(
    text = element_text(family = "Times"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none"
  )

pdf("viz3.pdf", width = 10, height = 8, family = "Times")

smaller_viz3 / bigger_viz3

dev.off()


## DATA  VIZ 4 ##

pdf("viz4.pdf", width = 8, height = 8, family = "Times")
NCSS|>
  filter(YEAR == 2022) |>
  ggplot(aes(x = TRAD6, y = NUMOFFMBR, fill = GDREGION)) +
  geom_boxplot() +
  facet_wrap(~ GDREGION, scales = "free_y")+
  scale_fill_viridis_d(name = "GDREGION") +
  labs(title = "Number of members in congregations - by Swiss Region",
       subtitle = "Year 2022",
       x = "\nTRAD6",
       y = "Number of members\n",
       fill = "Region") +
  theme_bw() +
  theme(
    text = element_text(family = "Times"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 40, hjust = 1, vjust = 1)
  )

dev.off()

  

