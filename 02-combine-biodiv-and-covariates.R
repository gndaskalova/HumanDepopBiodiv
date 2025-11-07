# Create master data frame of the biodiversity data 
# and the human pop trends and land cover metrics

# Load data ----
# Land cover/use data
load("data/landuse_summary_2024.RData")

# Human population trends
load("data/villages_all_pop_trends.RData")

# Biodiversity data
load("data/all_taxa_demo22ndOct.RData")
load("data/all_taxa_abundance_2025.RData")

# Note the abundance df doesn't include plants as those are measured in % cover

colnames(landuse_summary_2024)
colnames(villages_all)
colnames(all_taxa_demo)

# richness + covariates master df

villages_all2 <- villages_all %>%
  dplyr::select(municipality, village, lat, long, elevation, province,
                area, human_pop_trend_2022,
                human_pop_trend_2023,
                human_pop_trend_2024) %>%
  filter(province == "Пловдив" | province == "Габрово") %>% distinct()
  
all_taxa_demo_simple <- all_taxa_demo %>%
  dplyr::select(village, site, sp_richness, taxa, year,
                aband_year, years_since_aband) %>%
  filter(site != "village_NA")

richness_demo_land <- landuse_summary_2024 %>%
  dplyr::select(village_en, village, ever_abandoned, perm_abandoned,
                recultivated, current_fallow,
                current_cropland, ever_abandoned_crop_ratio,
                permanent_abandoned_crop_ratio, region,
                province, status) %>%
  left_join(villages_all2, by = c("village", "province")) %>%
  left_join(all_taxa_demo_simple, by = c("village")) %>% distinct()

richness_demo_land <- richness_demo_land %>%
  mutate(site = str_extract_all(site, "[[:digit:]]+", simplify = TRUE),
         site = paste(village, site, sep = "_"))

all_taxa_demo_simple_abun <- all_taxa_abundance %>% ungroup() %>%
  dplyr::select(village, site, abundance, taxa,
                aband_year, years_since_aband) %>%
  filter(site != "village_NA")

abundance_demo_land <- landuse_summary_2024 %>%
  dplyr::select(village_en, village, ever_abandoned, perm_abandoned,
                recultivated, current_fallow,
                current_cropland, ever_abandoned_crop_ratio,
                permanent_abandoned_crop_ratio, region,
                province, status) %>%
  left_join(villages_all2, by = c("village", "province")) %>%
  left_join(all_taxa_demo_simple_abun, by = c("village")) %>% distinct()

abundance_demo_land$site <- paste(abundance_demo_land$village_en,
                                  abundance_demo_land$site, sep = "_")

# Save for further analysis
save(abundance_demo_land, file = "data/abundance_demo_land.RData")
save(richness_demo_land, file = "data/richness_demo_land.RData")
