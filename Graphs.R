library(dplyr)
library(tidyverse)
library(ggplot2)
library(janitor)
library(data.table)
library(ggthemes)
library(gt)

###
#This time, we will create a summary table that shows 
# the share of sector-by-sector GHG for the world and Germany in 2017.
# Like this: https://ourworldindata.org/uploads/2020/09/Emissions-by-sector-%E2%80%93-pie-charts.png
###

# Load data and make sure that the data formats are appropriate. Pivot the dataset longer to create "year" column. 
cait_raw <- read.csv("data/historical_emissions.csv", sep=";", stringsAsFactors=FALSE) %>% clean_names()
cait_raw[] <- lapply(cait_raw, function(x) gsub("N/A", NA, x))
cait_raw[,6:33] <- as.numeric(unlist(cait_raw[,6:33]))
cait_raw <- cait_raw %>% pivot_longer(cols=starts_with("x"), names_to="year", values_to="value", names_prefix="x") %>% mutate(year=as.numeric(year))

# Make a subset for sub-sector level values. Filter out the energy category and total values, since the level of granularity is higher. Mark energy sub-sectors with a prefix "Energy". 
category <- read.csv("data/sector_category.csv", sep=";") 
energy_sub <- category$subsector_1[category$sector=="Energy"]
big_sector <- unique(category$sector)

# Sector-level dataset
cait_subsector <- cait_raw %>% filter(!str_detect(sector, "^Total")) %>% 
  filter(sector != "Energy") %>% na.omit() 

# Create a new column for the base level (GHG level in 2000). # Calculate the relative change in GHG compared to the base value. 
base <- cait_subsector %>% filter(year==2000) %>% select(country, sector, base = value) %>% na.omit()
cait_ch <- inner_join(cait_subsector,  base, by=c("country", "sector")) %>% mutate(change = round((value-base)/base*100,2)) %>% filter(year>= 2000)

# Add country metadata, in case we need that later. 
meta <- read.csv("data/WITSCountryProfile.csv", sep=",") %>% clean_names() %>% mutate(OECD = ifelse(other_groups=="OECD", T, F)) %>% 
  select(country=country_name, region, OECD) 
cait_meta <- left_join(cait_ch, meta, by="country") # %>% filter(region!=is.na(region)) %>% filter(region!="") %>% select(-base, -change)

#### This is a function that gives you a summary table of 
# GHG in 2017 by sector. 

## Parameters
select_country <- "World"
select_year <- 2017

# 1. Subset data
summary_table <- function(select_country, select_year) {

country <- cait_meta %>% filter(country==select_country, year==select_year) 

# Calculate shares per sector
country_summary_sector <- country %>% group_by(sector) %>%
  summarize(sector_total = round(sum(value),0)) %>% 
  mutate(share=sector_total/sum(sector_total)) %>%
  mutate(category = ifelse(sector %in% energy_sub, "Energy", sector)) %>%
  mutate(sector = ifelse(sector %in% energy_sub, sector, "-")) %>%
  arrange(desc(share), .by_group=TRUE)


# Create a gt table

country_summary <- country_summary_sector %>%
  dplyr::group_by(category) %>% 
  gt(rowname_col="sector", groupname_col="category") %>%
  tab_header(
    title=md(paste("Sector breakdown of ", select_country,"'s", " GHG in ",select_year))
  ) %>%
  tab_source_note(md("Data source: Climate Watch Data Explorer")) %>%
  cols_align(align="center", columns=vars(sector_total, share)) %>%
  cols_label(sector_total = "GHG in MtCO₂e", share="Share") %>%
  fmt_number(columns=vars(sector_total), sep_mark=",", decimals=0) %>%
  fmt_percent(columns=vars(share), decimals=1)

country_summary

}

summary_table("United States", 2017)

#### Germany's GHG in 2017 by sector. 
# 1. Subset data
germany <- cait_meta %>% filter(country=="Germany", year==2017) 

# Calculate shares per sector
germany_summary_sector <- germany %>% group_by(sector) %>%
  summarize(sector_total = round(sum(value),0)) %>% 
  mutate(share=sector_total/sum(sector_total)) %>%
  mutate(category = ifelse(sector %in% energy_sub, "Energy", sector)) %>%
  mutate(sector = ifelse(sector %in% energy_sub, sector, "-")) %>%
  arrange(desc(share), .by_group=TRUE)


# Create a gt table

germany_summary <- germany_summary_sector %>%
  dplyr::group_by(category) %>% 
  gt(rowname_col="sector", groupname_col="category") %>%
  tab_header(
    title=md("Sector breakdown of Germany's GHG in 2017")
  ) %>%
  tab_source_note(md("Data source: Climate Watch Data Explorer")) %>%
  cols_align(align="center", columns=vars(sector_total, share)) %>%
  cols_label(sector_total = "GHG in MtCO₂e", share="Share") %>%
  fmt_number(columns=vars(sector_total), sep_mark=",", decimals=0) %>%
  fmt_percent(columns=vars(share), decimals=1)

gtsave(germany_summary, "images/germany_summary.png")

# Step 1. Make a basic circular barplot

world_circle <- ggplot(world_summary_bigsector, aes(x=as.factor(big_sector), y=share)) +
  geom_bar(stat="identity") + 
  ylim(-3, max(world_summary_bigsector$share)) +
  theme_minimal() +
  theme(
    axis.text=element_blank(),
    axis.title = element_blank(),
    panel.grid=element_blank(),
    plot.margin = unit(rep(-2,4), "cm")) +
  coord_polar(start=0) 

world_circle 
# Step 2. Add labels
## Prepare a dataframe for labels
label_world <- world %>% mutate(id = row_number())

## Calculate the angle of labels
number_of_bar <- nrow(label_world)
angle <- 90-360*(label_world$id-0.5)/number_of_bar

## Calculate the alignment of labels: right or left
label_world$hjust <- ifelse(angle < -90, 1, 0)

## Flip angle to make them readable
label_world$angle <- ifelse(angle < -90, angle+180, angle)

## Add the labels using the label dataframe.
world_circle_2 <- world_circle + 
  geom_text(data=label_world, aes(x=id, y=value+1000, 
                                  label=sector, hjust=hjust),
                                  color="black", fontface="bold", alpha=0.6, size=2.5, 
                                  angle=label_world$angle, inherit_aes=FALSE)

world_circle_2

#### Regional-level Analysis ####
# Calculate the total GHG and their change since 2000 by region
region <- cait_meta %>% group_by(region, sector, year) %>% summarise(value=sum(value, rm.na=T)) 
base_reg <-  region %>% filter(year==2000) %>% select(region, sector, base = value) 
region_ch <- inner_join(region,  base_reg, by=c("region", "sector")) %>% mutate(change = round((value-base)/base*100,2)) %>% filter(year>= 2000)

# Calculate the total GHG and their change in 2000 by OECD membership
oecd <- cait_meta %>% group_by(OECD, sector, year) %>% summarise(value=sum(value, rm.na=T)) 
base_oecd <- oecd %>% filter(year==2000) %>% select(OECD, sector, base = value) 
oecd_ch <- inner_join(oecd,  base_oecd, by=c("OECD", "sector")) %>% mutate(change = round((value-base)/base*100,2)) %>% filter(year>= 2000) %>% mutate(country=ifelse(OECD==TRUE, "OECD", "non-OECD")) %>% ungroup() %>% select(-OECD)

##############