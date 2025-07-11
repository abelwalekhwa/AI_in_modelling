
##Importing the systematic review data

library(readxl)
library(sf)
library(raster)
library(spData)
library(tmap) # for static and interactive maps
library(leaflet) # for interactive maps
library(mapview) # for interactive maps
library(RColorBrewer) #color palette
library(wesanderson)

dat <- read_excel("~/OneDrive - University of Cambridge/R/AI in Modeling/data_11072025.xlsx")


setwd("~/OneDrive - University of Cambridge/R/FMD_Meta-analysis/world-administrative-boundaries")

worldMap <- st_read("world-administrative-boundaries.shp")

##Merging the two datasets
library(dplyr)
library(stringr)

# Standardize country names in both datasets
worldMap <- worldMap %>%
  mutate(name = str_to_title(str_trim(name))) %>% 
  filter(continent == "Africa")
  
  # Capitalize the first letter of each word, remove extra spaces

dat <- dat %>%
  mutate(Country = str_to_title(str_trim(Country)))

#Checking the names of countries in Africa

name_mapping <- tibble(
  dat_country = c("Dr Congo"),
  worldMap_country = c("Democratic Republic Of The Congo"))

# Clean and standardize country names in dat

dat <- dat %>%
  # Ensure country names are trimmed and title case
  mutate(Country = str_to_title(str_trim(Country))) %>%
  
  # Apply name mapping
  left_join(name_mapping, by = c("Country" = "dat_country")) %>%
  mutate(Country = coalesce(worldMap_country, Country)) %>%
  select(-worldMap_country) %>%
 
   # Remove invalid entries like "Not Specified"
  filter(Country != "Not Specified" & !is.na(Country))

# Standardize worldMap (already filtered for Africa)
worldMap <- worldMap %>%
  mutate(name = str_to_title(str_trim(name))) %>%
  filter(continent == "Africa")

# Re-compare country names
dat_countries <- unique(dat$Country)
worldMap_countries <- unique(worldMap$name)

# Countries in dat but not in worldMap
missing_in_worldMap <- setdiff(dat_countries, worldMap_countries)
# Countries in worldMap but not in dat
missing_in_dat <- setdiff(worldMap_countries, dat_countries)
# Common countries
matched_countries <- intersect(dat_countries, worldMap_countries)

#Counting the number of studies per country
 study_count <- dat %>%
  group_by(Country) %>%
 summarise(study_count = n()) %>% 
   rename(region = Country)

 dat_final <- dat %>% 
   left_join(study_count, by = c("Country" = "region"))
 
 library(tidyverse)
 library(sf) # For handling spatial data
 
 # Load world map data (using rnaturalearth for Africa)
 worldMap <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
   mutate(name = str_to_title(str_trim(name))) %>%
   filter(continent == "Africa")
 
 # Your study_count data (from previous output)
 study_count <- tibble(
   region = c("Benin", "Burundi", "Cameroon", "Côte D'ivoire", "Democratic Republic Of The Congo",
              "Ghana", "Kenya", "Morocco", "Mozambique", "Nigeria", "Rwanda", "Senegal",
              "Sierra Leone", "South Africa", "Uganda", "United Republic Of Tanzania", "Zimbabwe"),
   study_count = c(1, 1, 2, 1, 1, 2, 8, 1, 2, 8, 1, 1, 2, 5, 7, 2, 6)
 ) %>%
   mutate(region = str_to_title(str_trim(region)))
 
 # Standardize country names in dat_final
 dat_final <- study_count %>%
   rename(Country = region)
 
 # Join dat_final with worldMap
 map_data <- worldMap %>%
   left_join(dat_final, by = c("name" = "Country"))
 
 # Plot the map
 ggplot(map_data) +
   geom_sf(aes(fill = study_count, geometry = geometry)) +
   scale_fill_viridis_c(option = "plasma", na.value = "white", name = "Number of Studies") +
   labs(title = "Number of countries Included in the review") +
   theme_minimal() +
   theme(
     legend.position = "bottom",
     panel.background = element_rect(fill = "white", color = NA), 
     plot.background = element_rect(fill = "white", color = NA),  
     panel.grid = element_blank()
   ) +
   geom_sf_text(aes(label = study_count, geometry = geometry), size = 3, na.rm = TRUE) +
   coord_sf(datum = NA) 
 
 #### AI tools used####

 library(tidyverse)
 library(viridis)
 
 # Create aggregated data with proper categories
 plot_data <- dat_final %>%
   mutate(
     Country = str_to_title(str_trim(Country)),
     # Create a new category for visualization
     AI_category = case_when(
       AI_tool == "None" ~ "No AI",
       AI_tool == "No studies" ~ "No studies",
       TRUE ~ "AI Used"  # All other cases
     )
   ) %>%
   group_by(Country, year) %>%
   summarise(
     study_count = n(),
     AI_category = ifelse(any(AI_category == "AI Used"), "AI Used", first(AI_category)),
     .groups = "drop") %>%
   
   # Create complete grid of all country-year combinations
   complete(Country, year, fill = list(study_count = 0, AI_category = "No studies"))
 
 # Create country ordering by total study count
 country_order <- plot_data %>%
   group_by(Country) %>%
   summarise(total_studies = sum(study_count)) %>%
   arrange(desc(total_studies)) %>%
   pull(Country)
 
 # Define color scheme
 ai_colors <- c(
   "AI Used" = "#E64B35",  # Distinct red for AI studies
   "No AI" = "#4DBBD5",    # Blue for non-AI studies
   "No studies" = "white"  # White for no studies
 )
 # Generate the plot
 ggplot(plot_data, aes(x = factor(year), y = factor(Country, levels = country_order))) +
   geom_tile(fill = "white", color = "grey90", linewidth = 0.3, width = 0.95, height = 0.95) +
   geom_tile(
     data = filter(plot_data, AI_category != "No studies"),
     aes(fill = AI_category),
     color = "white", linewidth = 0.8, width = 0.9, height = 0.9) +
   geom_text(
     aes(label = ifelse(study_count > 0, study_count, "")),
     color = "black", 
     size = 3.0,
     na.rm = TRUE) +
   scale_fill_manual(
     name = "Study Type",
     values = ai_colors,
     breaks = c("AI Used", "No AI"),  # Only show these in legend
     labels = c("AI Used", "Non-AI Study"),
     na.value = "white"
   ) +
   labs(
     title = "AI Tools in Infectious Diseases Mathematical Modeling in Africa",
     x = "Publication Year",
     y = "Country") +
   theme_minimal(base_size = 12) +
   theme(
     plot.background = element_rect(fill = "white", color = NA),
     panel.background = element_rect(fill = "white", color = NA),
     panel.grid = element_blank(),
     axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
     axis.text.y = element_text(size = 9),
     legend.position = "bottom",
     plot.title = element_text(face = "bold", hjust = 0.5),
     plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
     plot.caption = element_text(color = "gray50", margin = margin(t = 10)),
     legend.title = element_text(face = "bold")) +
   coord_fixed(ratio = 0.75)
 ### Programming language

 