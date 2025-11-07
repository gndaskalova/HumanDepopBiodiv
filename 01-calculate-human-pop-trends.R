# Packages -----
library(XML)
library(RCurl)
library(rlist)
library(rvest)
library(tidyverse)
library(readxl)
library(broom)
library(elevatr)
library(sp)
library(rgbif)
library(brms)
library(broom)

# Functions ----
# To get the EKATTE code for each village
get_title <- . %>% html_node(xpath = '//*[@id="main_content"]/div[2]/div[1]/div[1]/div/h3/text()[2]') %>%
  html_text()



# To get the table with the human population data
get_table <- . %>% html_node(xpath = '//*[@id="main_content"]/div[2]/div[2]/table') %>% 
  html_table()



# Make a string with all the urls
# urls <- paste0(sprintf("https://www.nsi.bg/nrnm/show9.php?sid=%s&ezik=en",seq(1:5458)))
#urls <- paste0(sprintf("https://www.nsi.bg/nrnm/reports/population/list/", seq(1:7000)))

urls <- paste0("https://www.nsi.bg/nrnm/reports/population/list/", seq(1:1000))

#urls <- "https://www.nsi.bg/nrnm/reports/population/list/2585"

# Extract the data
human_pop1 <- tibble(urls) %>% 
  mutate(
    page = map(urls, read_html), 
    ekatte = map_chr(page, get_title), 
    data = map(page, get_table), 
    page = NULL, urls = NULL
  ) %>% 
  unnest(data)

urls2 <- paste0("https://www.nsi.bg/nrnm/reports/population/list/", seq(from = 1001, to = 2000))

human_pop2 <- tibble(urls2) %>% 
  mutate(
    page = map(urls2, read_html), 
    ekatte = map_chr(page, get_title), 
    data = map(page, get_table), 
    page = NULL, urls2 = NULL
  ) %>% 
  unnest(data)

urls3 <- paste0("https://www.nsi.bg/nrnm/reports/population/list/", seq(from = 2001, to = 3000))

human_pop3 <- tibble(urls3) %>% 
  mutate(
    page = map(urls3, read_html), 
    ekatte = map_chr(page, get_title), 
    data = map(page, get_table), 
    page = NULL, urls3 = NULL
  ) %>% 
  unnest(data)

urls4 <- paste0("https://www.nsi.bg/nrnm/reports/population/list/", seq(from = 3001, to = 4000))

human_pop4 <- tibble(urls4) %>% 
  mutate(
    page = map(urls4, read_html), 
    ekatte = map_chr(page, get_title), 
    data = map(page, get_table), 
    page = NULL, urls4 = NULL
  ) %>% 
  unnest(data)

urls5 <- paste0("https://www.nsi.bg/nrnm/reports/population/list/", seq(from = 4001, to = 5000))

human_pop5 <- tibble(urls5) %>% 
  mutate(
    page = map(urls5, read_html), 
    ekatte = map_chr(page, get_title), 
    data = map(page, get_table), 
    page = NULL, urls5 = NULL
  ) %>% 
  unnest(data)

urls6 <- paste0("https://www.nsi.bg/nrnm/reports/population/list/", seq(from = 5001, to = 5200))

human_pop6 <- tibble(urls6) %>% 
  mutate(
    page = map(urls6, read_html), 
    ekatte = map_chr(page, get_title), 
    data = map(page, get_table), 
    page = NULL, urls6 = NULL
  ) %>% 
  unnest(data)

urls7 <- paste0("https://www.nsi.bg/nrnm/reports/population/list/", seq(from = 5201, to = 5300))

human_pop7 <- tibble(urls7) %>% 
  mutate(
    page = map(urls7, read_html), 
    ekatte = map_chr(page, get_title), 
    data = map(page, get_table), 
    page = NULL, urls7 = NULL
  ) %>% 
  unnest(data)

urls8 <- paste0("https://www.nsi.bg/nrnm/reports/population/list/", seq(from = 5301, to = 5350))

human_pop8 <- tibble(urls8) %>% 
  mutate(
    page = map(urls8, read_html), 
    ekatte = map_chr(page, get_title), 
    data = map(page, get_table), 
    page = NULL, urls8 = NULL
  ) %>% 
  unnest(data)

# Combine them all
human_pop <- bind_rows(human_pop1, human_pop2,
                       human_pop3, human_pop4,
                       human_pop5, human_pop6,
                       human_pop7, human_pop8)

save(human_pop, file = "data/human_pop_all_2025.RData")

load("data/human_pop_all_2025.RData")

human_pop <- human_pop %>% dplyr::select(-`Пореден номер`, -`Вид статистическо наблюдение`)

colnames(human_pop)
colnames(human_pop) <- c("ekatte", "Date", "Population")

# Tidy up
human_pop_tidy <- human_pop %>%
  separate(Date, c("day", "month", "year")) %>%
  separate(ekatte, c("text", "text2", "ekatte")) %>%
  dplyr::select(ekatte, year, Population) %>%
  rename(population = Population)

# How many villages?
length(unique(human_pop_tidy$ekatte))

# 5350 villages

save(human_pop_tidy, file = "data/human_pop_tidy2025.RData")

# Now match them with names and metadata

# Latitude, longitude and ekatte data
# ekatte code is the unique identifier for settlements in Bulgaria
settlements_loc <- read_csv("03_bulgaria_monitoring/data/settlements_loc.csv")
# Village area
settlements_area <- read_csv("03_bulgaria_monitoring/data/settlements.csv")

# Format lat and long data
settlements <- settlements_loc %>%
  filter(village == 1) %>%
  dplyr::select(-village, -`postal code`, -`municipality code`) %>%
  separate(geo, c("long","lat"), sep = ",") %>%
  rename(village = name) %>%
  dplyr::select(province, municipality, village, lat, long, ekatte)

# Extract elevation data for the lat and long of villages
# use your GeoNames username after enabling webservices for it
# register here https://www.geonames.org

coords <- settlements %>% dplyr::select(lat, long) %>%
  rename(decimalLatitude = lat,
         decimalLongitude = long)

coords$decimalLatitude <- as.numeric(coords$decimalLatitude)
coords$decimalLongitude <- as.numeric(coords$decimalLongitude)
elevations<- elevation(coords, username = "gndaskalova")

# rename columns
elevations <- elevations %>%
  rename(lat = latitude,
         long = longitude,
         elevation = elevation_geonames)

# make columns the same types in both data frames
settlements$lat <- as.numeric(settlements$lat)
settlements$long <- as.numeric(settlements$long)

settlements <- left_join(settlements, elevations, by = c("lat", "long"))

# Add village areas
settlements_area <- settlements_area %>%
  dplyr::select(ekatte, area) %>% distinct()

settlements <- left_join(settlements, settlements_area, by = "ekatte")

# reorder columns
settlements <- settlements %>%
  dplyr::select(province, municipality, village, lat, long, 
                elevation, area, ekatte)

# Add human population data for 1934 - 2020
villages <- left_join(settlements, human_pop_tidy, by = "ekatte")

str(villages)

villages$province <- as.factor(as.character(villages$province))
villages$municipality <- as.factor(as.character(villages$municipality))
villages$village <- as.factor(as.character(villages$village))
villages$ekatte <- as.factor(as.character(villages$ekatte))
villages$year <- as.numeric(as.character(villages$year))

str(villages)

# Save the file
save(villages, file = "data/villages2025.RData")

# Calculate the human population trends

# Calculate human population trends
# Do it in three iterations, to account for surveys being in different years
# Up to 2022 (plants) 
# Up to 2023 (birds, amphibians, reptiles, odonata) 
# Up to 2024 (plants)

# models ----

# Up to 2024 ----
# Human population over time
summary(villages$year)  # starts in 1934, ends in 2024
villages <- villages %>% drop_na(year)

villages$year2 <- villages$year - 1933  # make years go 1, 2, 3, 4...
villages$year.scaled <- scale(villages$year2, center = T)
summary(villages$year.scaled)  # centered on zero

hist(villages$population)
summary(villages$population)

colnames(villages)
villages2 <- villages %>% drop_na()

pop_models <- villages2 %>%
  group_by(province, municipality, village) %>% 
  do(broom::tidy(glm(population ~ year.scaled, 
                     family = "poisson", .))) %>%
  dplyr::select(., province, municipality, village, term, estimate, std.error, p.value) %>%  # select the columns we want
  spread(., term, estimate) %>%  # transform them in a useful format
  ungroup()  # get rid of the grouping

pop_models2 <- pop_models %>%
  dplyr::select(-`(Intercept)`) %>%
  drop_na(year.scaled) %>%
  rename(human_pop_trend_2024 = year.scaled) %>% ungroup()

# 2000 to 2024 ----
summary(villages$year)  # starts in 1934, ends in 2024
villages <- villages %>% drop_na(year)

villages_recent <- villages %>% filter(year > 1999)
summary(villages_recent$year)  # starts in 2000, ends in 2024

villages_recent$year2 <- villages_recent$year - 1999  # make years go 1, 2, 3, 4...
villages_recent$year.scaled <- scale(villages_recent$year2, center = T)
summary(villages_recent$year.scaled)  # centered on zero

hist(villages_recent$population)
summary(villages_recent$population)

colnames(villages_recent)
villages_recent2 <- villages_recent %>% drop_na()

pop_models_recent <- villages_recent2 %>%
  group_by(province, municipality, village) %>% 
  do(broom::tidy(glm(population ~ year.scaled, 
                     family = "poisson", .))) %>%
  dplyr::select(., province, municipality, village, term, estimate, std.error, p.value) %>%  # select the columns we want
  spread(., term, estimate) %>%  # transform them in a useful format
  ungroup()  # get rid of the grouping

pop_models_recent2 <- pop_models_recent %>%
  dplyr::select(-`(Intercept)`) %>%
  drop_na(year.scaled) %>%
  rename(human_pop_trend_2024 = year.scaled) %>% ungroup()

# compare historic and recent trends for the study villages only
village_pairs_status2024 <- read_csv("data/village_pairs_status2024.csv")
village_pairs_status <- read_csv("data/village_pairs_status.csv")

meta_village_status <- village_pairs_status %>%
  dplyr::select(village_en, region) %>% distinct()
meta_village_status2 <- village_pairs_status2024 %>%
  dplyr::select(village_en, region) %>% distinct()

metas <- full_join(meta_village_status, meta_village_status2)
metas <- metas %>% filter(village_en != "Stanchov han")

load("data/richness_demo_land.RData")

village_names <- richness_demo_land %>%
  dplyr::select(village, village_en) %>% distinct()
metas <- left_join(metas, village_names)

pop_models_recent2b <- pop_models_recent2 %>%
  filter(province %in% c("Пловдив", "Габрово",
                         "Велико Търново")) %>%
  dplyr::select(municipality, village, human_pop_trend_2024) %>% distinct()

metas <- left_join(metas, pop_models_recent2b)

colnames(metas)[5] <- "human_pop_trend_2000_2024"

# Add 1934-2024 trend

historic_trends <- pop_models2 %>% dplyr::select(municipality, village, human_pop_trend_2024)
colnames(historic_trends)[3] <- "human_pop_trend_1934_2024"

metas <- left_join(metas, historic_trends)

# A few mountain villages have the same names, pick the ones that are correct for the study sites
metas <- metas[-c(14, 18, 21),]

change_theme <- function(){
  theme_bw() +
    ggplot2::theme(#text = element_text(family = "Helvetica"),
      axis.text = element_text(size = 13), 
      axis.title = element_text(size = 15),
      axis.line.x = element_line(color="black"), 
      axis.line.y = element_line(color="black"),
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),                                          
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),  
      plot.margin = unit(c(1, 1, 1, 1), units = , "cm"),
      plot.title = element_text(size = 15, vjust = 1, hjust = 0),
      legend.text = element_text(size = 12),          
      legend.title = element_blank(),                              
      legend.position = c(0.9, 0.9), 
      legend.key = element_blank(),
      legend.background = element_rect(size = 2,
                                       linetype = "blank"))
}

(trend_comparison1 <- ggplot(metas[metas$region == "Lowland",], aes(x = human_pop_trend_1934_2024,
                                      y = human_pop_trend_2000_2024)) +
                             geom_point(colour = "#df7a5f") +
                             geom_smooth(method = "lm", colour = "#df7a5f", fill = "#df7a5f") +
    geom_vline(xintercept = 0, linetype = "dotted") +
    geom_hline(yintercept = 0, linetype = "dotted") +
    scale_x_continuous(limits = c(-0.5, 0.2)) +
    scale_y_continuous(limits = c(-0.5, 0.2)) +
                             change_theme() +
    labs(x = "\nHuman population trend (1934 - 2024)",            # labels
         y = "Human population trend (2000 - 2024)\n",
         title = "A   Lowland\n"))

(trend_comparison2 <- ggplot(metas[metas$region == "Mountain",], aes(x = human_pop_trend_1934_2024,
                                                                    y = human_pop_trend_2000_2024)) +
    geom_point(colour = "#8cbca4") +
    geom_smooth(method = "lm", colour = "#8cbca4", fill = "#8cbca4") +
    geom_vline(xintercept = 0, linetype = "dotted") +
    geom_hline(yintercept = 0, linetype = "dotted") +
    scale_x_continuous(limits = c(-1.2, 1)) +
    scale_y_continuous(limits = c(-1.2, 1)) +
    change_theme() +
    labs(x = "\nHuman population trend (1934 - 2024)",            # labels
         y = "Human population trend (2000 - 2024)\n",
         title = "B   Mountain\n"))

(trend_comparison3 <- ggplot(metas[metas$region == "Mountain",], aes(x = human_pop_trend_1934_2024,
                                                                     y = human_pop_trend_2000_2024)) +
    geom_point(colour = "#8cbca4") +
    geom_smooth(method = "lm", colour = "#8cbca4", fill = "#8cbca4") +
    geom_vline(xintercept = 0, linetype = "dotted") +
    geom_hline(yintercept = 0, linetype = "dotted") +
    scale_x_continuous(limits = c(-1.2, 0.2)) +
    scale_y_continuous(limits = c(-1.2, 0.2)) +
    change_theme() +
    labs(x = "\nHuman population trend (1934 - 2024)",            # labels
         y = "Human population trend (2000 - 2024)\n",
         title = "C   Mountain (without outliers)\n"))

trend_comparison_panel <- grid.arrange(trend_comparison1, trend_comparison2, trend_comparison3,
                                       ncol = 3)
ggsave(trend_comparison_panel, filename = "figures/trend_comparison_panel.png", height = 5, width = 15)


# Up to 2023 ----
# Human population over time
villages_2023 <- villages %>%
  filter(year != "2024")

summary(villages_2023$year)  # starts in 1934, ends in 2024

villages_2023$year2 <- villages_2023$year - 1933  # make years go 1, 2, 3, 4...
villages_2023$year.scaled <- scale(villages_2023$year2, center = T)
summary(villages_2023$year.scaled)  # centered on zero

hist(villages_2023$population)
summary(villages_2023$population)

colnames(villages_2023)
villages2023_2 <- villages_2023 %>% drop_na()

pop_models_2023 <- villages2023_2 %>%
  group_by(province, municipality, village) %>% 
  do(broom::tidy(glm(population ~ year.scaled, 
                     family = "poisson", .))) %>%
  dplyr::select(., province, municipality, village, term, estimate, std.error, p.value) %>%  # select the columns we want
  spread(., term, estimate) %>%  # transform them in a useful format
  ungroup()  # get rid of the grouping

pop_models_2023_2 <- pop_models_2023 %>%
  dplyr::select(-`(Intercept)`) %>%
  drop_na(year.scaled) %>%
  rename(human_pop_trend_2023 = year.scaled) %>% ungroup()

# Up to 2022 ----
# Human population over time
villages_2022 <- villages %>%
  filter(year != "2024",
         year != "2023")

summary(villages_2022$year)  # starts in 1934, ends in 2024

villages_2022$year2 <- villages_2022$year - 1933  # make years go 1, 2, 3, 4...
villages_2022$year.scaled <- scale(villages_2022$year2, center = T)
summary(villages_2022$year.scaled)  # centered on zero

hist(villages_2022$population)
summary(villages_2022$population)

colnames(villages_2022)
villages2022_2 <- villages_2022 %>% drop_na()

pop_models_2022 <- villages2022_2 %>%
  group_by(province, municipality, village) %>% 
  do(broom::tidy(glm(population ~ year.scaled, 
                     family = "poisson", .))) %>%
  dplyr::select(., province, municipality, village, term, estimate, std.error, p.value) %>%  # select the columns we want
  spread(., term, estimate) %>%  # transform them in a useful format
  ungroup()  # get rid of the grouping

pop_models_2022_2 <- pop_models_2022 %>%
  dplyr::select(-`(Intercept)`) %>%
  drop_na(year.scaled) %>%
  rename(human_pop_trend_2022 = year.scaled) %>% ungroup()

# Integrate the human pop change slopes up to the three different years
trends <- left_join(pop_models_2022_2, pop_models_2023_2, 
                    by = c('village', "municipality", "province"),
                    relationship = "many-to-many") %>%
  dplyr::select(human_pop_trend_2022, human_pop_trend_2023, 
                province, municipality, village)

pop_models2 <- left_join(pop_models2, trends)
# Combine

villages_all <- left_join(villages2, pop_models2, 
                          by = c("province", "municipality", "village"))

# Reorder and remove unnecessary columns

villages_all <- villages_all %>%
  dplyr::select(province, municipality, village, lat, long,
                elevation, area, ekatte, year, population, 
                human_pop_trend_2024, human_pop_trend_2023, human_pop_trend_2022,
                std.error, p.value) %>% ungroup()

save(villages_all, file = "data/villages_all_pop_trends.RData")

# Determining which ones are abandoned

villages_all$test <- villages_all$population == 0
abandoned_villages <- villages_all %>%
  group_by(municipality, village) %>%
  mutate(last_year = max(year)) %>% ungroup()

abandoned_villages$year_test <- abandoned_villages$last_year == abandoned_villages$year

abandoned_villages <- abandoned_villages %>%
  filter(year_test == TRUE) %>%
  filter(test == TRUE)

abandoned_villages$status <- "abandoned"

abandoned_villages <- distinct(abandoned_villages)

length(unique(abandoned_villages$village)) # 204 but actually 206 as two
# villages have the same name

abandoned_villages <- abandoned_villages %>%
  dplyr::select(municipality, village, status)

abandoned_villages <- left_join(abandoned_villages, villages_all, 
                                by = c("municipality", "village"))

# determine year of abandoning

aband_year <- abandoned_villages %>% ungroup() %>% 
  filter(test == T) %>%
  group_by(province, municipality, village) %>%
  mutate(min_year = min(year))

aband_year$test2 <- aband_year$year == aband_year$min_year  
aband_year <- aband_year %>%
  filter(test2 == T) %>%
  rename(aband_year = year) %>%
  dplyr::select(province, municipality, village, aband_year)

aband_year$years_since_aband <- 2024 - aband_year$aband_year

abandoned_villages <- left_join(abandoned_villages, aband_year, 
                                by = c("province", "municipality", "village"))

colnames(abandoned_villages)
colnames(villages_all)

abandoned_villages <- abandoned_villages %>%
  dplyr::select(province, municipality, village,  lat,
                long, elevation, area, ekatte, year, population, human_pop_trend, 
                std.error, p.value, status, aband_year, years_since_aband)

villages_all_except_aband <- anti_join(villages_all,
                                       abandoned_villages, 
                                       by = c("province", "municipality", "village"))

villages_all_except_aband <- villages_all_except_aband %>%
  # filter(test == FALSE) %>%
  mutate(status = case_when(
    p.value < 0.050001 & human_pop_trend_2024 > 0 ~ "increasing",
    p.value > 0.050001 ~ "stable",
    p.value < 0.050001 & human_pop_trend_2024 < 0 ~ "decreasing"))

length(unique(villages_all$village))

length(unique(villages_all_except_aband$village))

villages_all_except_aband <- as.data.frame(villages_all_except_aband)

# Tidy up
villages_all_except_aband <- villages_all_except_aband %>%
  ungroup() %>%
  dplyr::select(province, municipality, village, lat, long,
                elevation, area, ekatte, year, population, human_pop_trend_2024,
                human_pop_trend_2023, human_pop_trend_2022,
                std.error, p.value, status)

villages_all_except_aband$aband_year <- NA
villages_all_except_aband$years_since_aband <- NA

villages_bg <- bind_rows(abandoned_villages, villages_all_except_aband)

villages_bg$status <- as.factor(as.character(villages_bg$status))

save(villages_bg, file = "data/villages_bg.RData")

# summarising df to get mean population trend across the years
villages_summary <- villages_bg %>% 
  group_by(municipality, village, province, status, aband_year, lat, long, elevation) %>% 
  summarise(human_pop_trend = mean(human_pop_trend_2024)) %>% 
  ungroup()

save(villages_summary, file = "data/villages_bg_summary.RData")


#adding new human pop trends to village key
load("data/village_key_w_extra.RData")

village_key_2025 <- village_key %>% 
  dplyr::select(-c(human_pop_trend, status, aband_year)) %>% 
  left_join(villages_summary, join_by(village, province)) 

save(village_key_2025, file = "data/village_key_2025.RData")
