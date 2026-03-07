############## MATILDA TOMATIS - FINAL PROJECT - DATA VISUALISATION##########

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
lapply(c("tidyverse", "lubridate", "ggridges", "ggplot2", "kableExtra", "scales", 
         "viridis", "patchwork", "extrafont", "ggrepel", "sf", "patchwork", 
         "purrr", "stringr", "fixest", "dotwhisker"),
       pkgTest)

# Set working directory for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


##### DATA WRANGLING #####
### Countries selected ###
selected_countries <- c(
  "Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia",
  "Costa Rica", "Czech Republic", "Denmark", "Estonia", "Finland", "France",
  "Germany", "Greece", "Hungary", "Iceland", "Israel",
  "Ireland", "Italy", "Japan","South Korea", "Latvia", "Lithuania","Luxembourg", 
  "Mexico", "Netherlands", "New Zealand", "Norway",
  "Poland", "Portugal", "Slovakia", "Slovenia","Spain", 
  "Sweden", "Switzerland", "Turkey", "United Kingdom", "United States"
)

### Filtering the datasets for the needed variables ###

## Manifesto Project ##

MP_raw <- read.csv("MPDataset.csv")

MP <- MP_raw |>
  select(c("countryname", "edate", "partyname", "rile"))

MP <- MP |>
  mutate(
    edate = dmy(edate),
    year  = year(edate),
    country = countryname,
    party = partyname
  ) |>
  select(-c("edate", "countryname", "partyname"))|>
  filter(between(year, 1990, 2023),
         country %in% selected_countries)

# unique(MP$country)

## WhoGov ##
WG_within_raw <- read.csv("WhoGov_within.csv")

WG_within <- WG_within_raw |>
  select(c("country_name", "year", "party_english", "minister",
          "name", "gender"))

WG_within <- WG_within |>
  rename(
    country = country_name,
    party = party_english
  ) |>
  filter(between(year, 1990, 2024),
         country %in% c(selected_countries, "Czechia"),
         minister == 1)


# unique(WG_within$country)

WG_cross_raw <- read.csv("WhoGov_crosssectional.csv")

WG_cross <- WG_cross_raw |>
  select(c("country_name", "year", "n_female_minister", "n_minister"))

WG_cross <- WG_cross |>
  rename(
    country = country_name,
  ) |>
  filter(between(year, 1990, 2024),
         country %in% c(selected_countries, "Czechia")) |>
  mutate(fem_min_share = n_female_minister / n_minister ) 

WG_avg <- WG_cross |>
  group_by(year) |>
  summarise(
    fem_min_share = mean(fem_min_share, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(country = "OECD Average")

WG_cross <- WG_cross |>
  group_by(year) |>
  mutate(
    q_share = ntile(fem_min_share, 4),
    q_share = as.factor(q_share)
  ) |>
  ungroup()

quantiles_table <- WG_cross|>
  filter(year %in% c(1990, 2000, 2010, 2020))|>
  group_by(year) |>
  summarise(
    Q0 = min(fem_min_share, na.rm = TRUE),
    Q25 = quantile(fem_min_share, 0.25, na.rm = TRUE),
    Q50 = quantile(fem_min_share, 0.50, na.rm = TRUE),
    Q75 = quantile(fem_min_share, 0.75, na.rm = TRUE),
    Q100 = max(fem_min_share, na.rm = TRUE),
    .groups = "drop"
  )
quantiles_table

# unique(WG_cross$country)
str(WG_cross)

## Joining datasets - within and rile ##

MP_unique <- MP|>
  group_by(country, year, party) |>
  summarise(rile = mean(rile, na.rm = TRUE), .groups = "drop")

MP_expanded <- MP_unique |>
  group_by(country, party) |>
  filter(!is.na(year)) |>
  complete(year = full_seq(year, 1)) |> 
  arrange(country, party, year) |>
  fill(rile, .direction = "down") |>
  fill(rile, .direction = "up") |>
  ungroup()

WG_within <- WG_within |>
  filter(!is.na(party), party != "independent")

clean_party <- function(x) {
  x %>%
    str_replace_all("’", "'") %>%
    str_replace_all("\\s+", " ") %>%
    str_replace_all("\\[.*\\]", "") %>%
    str_trim() %>%
    str_to_lower()
}

WG_within <- WG_within |>
  mutate(party_clean = clean_party(party)) 

MP_expanded <- MP_expanded |>
  mutate(party_clean = clean_party(party))

WG_within_rile <- WG_within |>
  mutate(
    rile = pmap_dbl(
      list(country, year, party_clean),
      function(cntry, yr, wg_party) {
        mp_candidates <- MP_expanded |>
          filter(country == cntry, year == yr)
        match_row <- mp_candidates |>
          filter(str_detect(party_clean, fixed(wg_party)) |
                   str_detect(wg_party, fixed(party_clean)))
        
        if(nrow(match_row) > 0) {
          match_row$rile[1]  
        } else {
          NA_real_
        }
      }
    )
  )


WG_rile_avg <- WG_within_rile |>
  group_by(country, year) |>
  summarise(avg_rile = mean(rile, na.rm = TRUE),
            .groups = "drop") |>
  filter(!is.na(avg_rile))

WG_rile_avg <- WG_rile_avg |>
  group_by(year) |>
  mutate(
    q_rile = ntile(avg_rile, 4),
    q_rile = as.factor(q_rile)
  ) |>
  ungroup()

quantiles_table_rile <- WG_rile_avg|>
  filter(year %in% c(1990, 2000, 2010, 2020))|>
  group_by(year) |>
  summarise(
    Q0 = min(avg_rile, na.rm = TRUE),
    Q25 = quantile(avg_rile, 0.25, na.rm = TRUE),
    Q50 = quantile(avg_rile, 0.50, na.rm = TRUE),
    Q75 = quantile(avg_rile, 0.75, na.rm = TRUE),
    Q100 = max(avg_rile, na.rm = TRUE),
    .groups = "drop"
  )
quantiles_table_rile



### Data Visualisation 1 ### 

final_theme <- theme_bw(base_family = "serif", base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = rel(1.5), hjust = 0.5),
    plot.subtitle = element_text(face = "plain", size = rel(1.2), color = "grey20", hjust = 0.5),
    plot.caption = element_text(face = "italic", size = rel(0.8), color = "grey30", hjust = 0),
    strip.text = element_text(face = "italic", size = 9, hjust = 0.5),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    strip.background = element_rect(fill = "grey90", color = NA),
    axis.title = element_text(face = "bold", hjust = 0.5),
    panel.border = element_rect(color = "darkgrey", fill = NA),
    panel.grid.major = element_line(color = "grey85")
  )

#windows()

pdf("linegraph_share.pdf", width = 10, height = 7)
ggplot(data = WG_cross, aes(x = year, y = fem_min_share)) +
  geom_line() +
  geom_line(
    data = WG_avg,
    aes(x = year, y = fem_min_share),
    inherit.aes = FALSE,
    color = "darkred"
  ) +
  facet_wrap(~ country) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Female Ministers Share in OECD Countries",
    subtitle = "Share computed as the number of female ministers over the total number of ministers - by OECD country and year",
    caption = "Data source: Manifesto Project; from 1990 to 2023.\nCounts are unweighted and the panel is unbalanced. \nA general pattern of increase in the number of women is observable, with a few exceptions of a flatter trend. \n The OECD average female ministers share is computed as the unweighted mean of all OECD members ",
    x = "\nYear",
    y = "Share of Female Ministers\n"
  ) +
  final_theme
dev.off()

### Data Visualisation 2 ### 

world_map <- read_sf("ne_110m_admin_0_countries.shp") |>
  filter(ISO_A3 != "ATA") |>
  mutate(NAME = recode(NAME, "United States of America" = "United States"))
common_fill <- scale_fill_viridis_d(
  option = "viridis",
  na.value = "grey80",
  name = "Quantiles"
)

#1990
world_map_1990 <- world_map |>
  left_join(
    WG_cross |> 
      filter(year == 1990) |> 
      select(country, q_share),
    by = c("NAME" = "country")
  )

wm_1990 <- ggplot() + 
  geom_sf(data = world_map_1990, 
          aes(fill = q_share),
          linewidth = 0.25,
          alpha = 0.7) +
  coord_sf(crs = "+proj=robin") + 
  common_fill +
  labs(fill = "Quantiles",
       subtitle = "Year 1990") +
  theme_bw() +
  final_theme
#The same visualisation is repeated for the next 3 years chosen

#2000
world_map_00 <- world_map |>
  left_join(
    WG_cross |> 
      filter(year == 2000) |> 
      select(country, q_share),
    by = c("NAME" = "country")
  )

wm_2000 <- ggplot() + 
  geom_sf(data = world_map_00, 
          aes(fill = q_share),
          linewidth = 0.25,
          alpha = 0.7) +
  coord_sf(crs = "+proj=robin") + 
  common_fill+
  labs(fill = "Quantiles",
       subtitle = "Year 2000") +
  theme_bw() +
  final_theme

#2010
world_map_10 <- world_map |>
  left_join(
    WG_cross |> 
      filter(year == 2010) |> 
      select(country, q_share),
    by = c("NAME" = "country")
  )

wm_2010 <- ggplot() + 
  geom_sf(data = world_map_10, 
          aes(fill = q_share),
          linewidth = 0.25,
          alpha = 0.7) +
  coord_sf(crs = "+proj=robin") + 
  common_fill +
  labs(fill = "Quantiles",
       subtitle = "Year 2010") +
  theme_bw() +
  final_theme

#2020
world_map_2020 <- world_map |>
  left_join(
    WG_cross |> 
      filter(year == 2020) |> 
      select(country, q_share),
    by = c("NAME" = "country")
  )

wm_2020 <- ggplot() + 
  geom_sf(data = world_map_2020, 
          aes(fill = q_share),
          linewidth = 0.25,
          alpha = 0.7) +
  coord_sf(crs = "+proj=robin") + 
  common_fill +
  labs(fill = "Quantiles",
       subtitle = "Year 2020") +
  theme_bw() +
  final_theme

#windows()

pdf("wm_9020_share.pdf", width = 10, height = 7)
(wm_1990 + wm_2000 + wm_2010 +wm_2020) + 
  plot_layout(ncol = 2) +
  plot_annotation(
    title = "Quantiles in the share of female ministers in OECD countries",
    caption = "Source: Manifesto Project; 1990, 2000, 2010 & 2020. \nQuantiles are computed for each year, considering the share of female ministers over the total number of ministers, \nwith unweighted values.",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14, family = "serif", hjust = 0.5),
      plot.caption = element_text(face = "italic", size = 10, family = "serif", color = "grey30", hjust = 0)
    )
  )
dev.off()

## Data Visualisation 3 ## 

WG_avg <- WG_within_rile |>
  group_by(year) |>
  summarise(
    avg_rile = mean(rile, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(country = "OECD Average")

#windows()

pdf("linegraph_rile.pdf", width = 10, height = 7)
ggplot(WG_rile_avg, aes(x = year, y = avg_rile)) +
  geom_line() +
  geom_line(
    data = WG_avg %>% filter(!is.na(avg_rile)),
    aes(x = year, y = avg_rile),
    inherit.aes = FALSE,
    color = "darkred"
  ) +
  facet_wrap(~ country) +
  scale_y_continuous() +
  labs(
    title = "Average left-right positioning of ministers in OECD Countries",
    subtitle = "The scoring is computed averaging the 'rile' score of each minister's party of affiliation - by year and OECD country.",
    x = "\nYear",
    y = "Left-right average score\n",
    caption = "Source: WhoGov & Manifesto Project; from 1990 to 2023. \nCounts are unweighted and the panel is unbalanced. \nA higher score indicates a more right-wing party manifesto. The possible values span from -50 (extreme left) to 50 (extreme right). "
  ) +
  final_theme
dev.off()

## Data Visualisation 4 ## 


# 1990
world_map_90_rile <- world_map |>
  left_join(
    WG_rile_avg |> 
      filter(year == 1990) |> 
      select(country, q_rile),
    by = c("NAME" = "country")
  )

wm_rile_1990 <- ggplot() + 
  geom_sf(data = world_map_90_rile, 
          aes(fill = q_rile),
          linewidth = 0.25,
          alpha = 0.7) +
  coord_sf(crs = "+proj=robin") + 
  common_fill+
  labs(fill = "Quantiles",
       subtitle = "Year 1990") +
  theme_bw() +
  final_theme

#2000
world_map_00_rile <- world_map |>
  left_join(
    WG_rile_avg |> 
      filter(year == 2000) |> 
      select(country, q_rile),
    by = c("NAME" = "country")
  )

wm_rile_2000 <- ggplot() + 
  geom_sf(data = world_map_00_rile, 
          aes(fill = q_rile),
          linewidth = 0.25,
          alpha = 0.7) +
  coord_sf(crs = "+proj=robin") + 
  common_fill+
  labs(fill = "Quantiles",
       subtitle = "Year 2000") +
  theme_bw() +
  final_theme

#2010
world_map_10_rile <- world_map |>
  left_join(
    WG_rile_avg |> 
      filter(year == 2010) |> 
      select(country, q_rile),
    by = c("NAME" = "country")
  )

wm_rile_2010 <- ggplot() + 
  geom_sf(data = world_map_10_rile, 
          aes(fill = q_rile),
          linewidth = 0.25,
          alpha = 0.7) +
  coord_sf(crs = "+proj=robin") + 
  common_fill +
  labs(fill = "Quantiles",
       subtitle = "Year 2010") +
  theme_bw() +
  final_theme

#2020

world_map_20_rile <- world_map |>
  left_join(
    WG_rile_avg|> 
      filter(year == 2020) |> 
      select(country, q_rile),
    by = c("NAME" = "country")
  )

wm_rile_2020 <- ggplot() + 
  geom_sf(data = world_map_20_rile, 
          aes(fill = q_rile),
          linewidth = 0.25,
          alpha = 0.7) +
  coord_sf(crs = "+proj=robin") + 
  common_fill +
  labs(fill = "Quantiles",
       subtitle = "Year 2020") +
  theme_bw() +
  final_theme

#All four 
pdf("wm_9020_rile.pdf", width = 10, height = 7)
(wm_rile_1990 + wm_rile_2000 + wm_rile_2010 + wm_rile_2020) + 
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(
    title = "Quantiles in left-right positioning of ministers in OECD countries",
    caption = "Source: Manifesto Project & WhoGov; 1990, 2000, 2010 & 2020. \nQuantiles are computed for each year, considering the right-left postioning of the national party of afilliation of ministers, \nwith unweighted values.",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14, family = "serif", hjust = 0.5),
      plot.caption = element_text(face = "italic", size = 10, family = "serif", color = "grey30", hjust = 0)
    )
  )
dev.off()


### Data Visualisation 5 ### 

## Let's compare the boxplots of left right positioning of men and women 

pdf("boxplot_gender.pdf", width = 10, height = 7)
ggplot(
  WG_within_rile |> filter(year %in% c(1990, 2000, 2010, 2020)),
  aes(x = gender, y = rile, color = gender)) +
  geom_boxplot(width = 0.5, outlier.shape = NA) +
  geom_jitter(
    width = 0.12,
    size = 0.8,
    alpha = 0.25) +
  scale_y_continuous(limits = c(-50,50)) +
  facet_wrap(~ year) +
  scale_color_viridis_d(name = "gender")+
  labs(
    title = "Ideological positioning by gender of ministers in OECD countries",
    x = "\nGender",
    y = "Left-Right Position\n",
    caption = "Source: WhoGov & Manifesto Project; 1990, 2000, 2010 & 2020. \nBoxplots are obtained from the unweighted left-right positiong of ministers's party of affiliation, from all OECD countries."
  ) +
  guides(color = "none") +
  theme_bw() +
  final_theme
dev.off()

##Let's check this gender gap ideology over time ##

WG_within_gap <- WG_within_rile |>
  group_by(country, year, gender) |>
  summarise(
    avg_rile = mean(rile, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(names_from = gender, values_from = avg_rile) |>
  mutate(gender_gap = Female - Male)

pdf("heatmap_gender.pdf", width = 10, height = 7)
ggplot(WG_within_gap,
       aes(x = year, y = country, fill = gender_gap)) +
  geom_tile(color = "grey70", linewidth = 0.2) +
  scale_fill_gradient2(
    low = viridis(1, option = "D", direction = -1),  
    mid = "white",                                  
    high = viridis(1, option = "C"),                
    midpoint = 0,
    na.value = "grey90",
    name = "Gender gap\n(Female − Male)") +
  labs(
    x = "\nYear",
    y = "Country\n",
    title = "Within-government gender gap in ministers' Left–Right positioning in OECD countries",
    subtitle = "Negative values indicate female ministers are more left-leaning",
    caption = "Source: WhoGov & Manifesto Project, from 1990 to 2023. \n Gender-Gap, for each country-year combination, is calculated by subtractive the average left-right positioning of men ministries from that of female ministries; unweighted."
  ) +
  theme_bw() +
  final_theme
dev.off()