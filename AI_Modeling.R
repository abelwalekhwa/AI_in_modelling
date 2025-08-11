
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

dat <- read_excel("OneDrive - University of Cambridge/IDEMU UNIT/AI in Modeling- Systematic review/data_11082025.xlsx")

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

 ##Risk of bias assessment
 
 library(tibble)
 library(dplyr)
 library(gt)
 
 data <- tribble(
   ~study, ~year, ~`Design of the Mathematical Modeling Study`, ~`Description of the Model assumptions, tools and why they were used`, ~`Availability of the modelling code for replicability of their models`, ~`Process of developing the models; Engagement with relevant stakeholders`, ~`Quality of the data used for the models`, ~`Transparency/disclosure of study limitations`,
   "Muleia 2021", 2021, "High", "High", "High", "Low", "Low", "High",
   "Mfisimana 2022", 2022, "High", "High", "Low", "High", NA, NA,
   "Mbah 2023", 2022, "High", "High", "Low", "Low", "Low", "Low",
   "Kaba 2023", 2023, "High", "Low", "Low", "Unsure", "High", "Low",
   "Mwasa 2011", 2011, "High", "High", "Low", "High", "Low", "High",
   "Bhunu 2011", 2011, "High", "High", "Low", "Low", "Low", "Low",
   "Nannyonga 2012", 2012, "High", "High", "Low", "High", "Low", "Low",
   "Nguefack 2016", 2016, "High", "High", "Low", "High", "Low", "Low",
   "Azeez 2016", 2016, "High", "High", "Low", "High", "Low", "Low",
   "Kabaria 2016", 2016, "High", "High", "Low", "Low", "High", "Low",
   "Mushanyu 2018", 2018, "High", "High", "Low", "Low", "Low", "Low",
   "Okuonghae 2019", 2019, "High", "High", "Low", "Low", "Low", "Low",
   "Kusel 2019", 2019, "High", "High", "Low", "Low", "Low", "Low",
   "Sekamatte 2019", 2019, "High", "High", "Low", "Low", "Unsure", "Low",
   "Nyabadza 2019", 2019, "High", "High", "Low", "High", "High", "High",
   "Leo 2019", 2019, "High", "High", "Low", "Low", "Unsure", "High",
   "Nkamba 2019", 2019, "High", "High", "High", "High", "Low", "Low",
   "Daoudi 2020", 2020, "High", "Low", "Low", "Low", "Low", "Low",
   "Ogola 2020", 2020, "High", "High", "Low", "High", "Unsure", "High",
   "Mhlanga 2020", 2020, "High", "High", "Low", "Low", "High", "Low",
   "Garba 2020", 2020, "High", "High", "Low", "High", "Low", "Low",
   "Lambura 2020", 2020, "High", "High", "High", "Low", "High", "Low",
   "Kwarteng 2021", 2021, "High", "Low", "Low", "Low", "High", "Low",
   "Mduluza-Jokonya 2021", 2020, "High", "High", "Low", "High", "Low", "High",
   "Wangari 2021", 2021, "High", "High", "High", "High", "High", "Low",
   "Diouf 2022", 2022, "High", "Low", "Low", "Low", "High", "Low",
   "Nishimwe 2022", 2022, "High", "Low", "Low", "Low", "Low", "Low",
   "Kabaria 2016", 2016, "High", "Low", "Low", "High", "High", "Low",
   "Omondi 2018", 2018, "High", "High", "Low", "High", "Low", "High",
   "Omondi 2019", 2019, "High", "High", "Low", "High", "Low", "High",
   "Ogana 2024", 2024, "High", "High", "Low", "High", "High", "High",
   "Nannyonga 2020", 2020, "High", "High", "Low", "High", "High", "High",
   "Nannyonga 2013", 2013, "High", "High", "Low", "Low", "Low", "Low",
   "Nannyonga et al., 2011", 2011, "High", "High", "Low", "Low", "Low", "Low",
   "Yaga et al 2025", 2025, "High", "High", "Low", "Low", "High", "Low",
   "Yaga and Saporu, 2025", 2025, "High", "High", "Low", "High", "Low", "Low",
   "Montcho 2024", 2024, "High", "High", "Low", "Low", "Low", "High",
   "Olaniyi 2025", 2025, "High", "High", "Low", "High", "Low", "High",
   "Gathungu 2024", 2024, "High", "High", "High", "Low", "Low", "Low",
   "Aguegboh 2024", 2024, "High", "High", "Low", "Low", "Low", "Low")
 
 # Filter for years 2016 to 2025, create Citation without year, and arrange by year
 
 data <- data %>%
   mutate(
     Year = year,
     Citation = sub(",? \\d{4}$", "", study)) %>%
   filter(Year >= 2016 & Year <= 2025) %>%
   arrange(Year) %>%
   select(Year, Citation, `Design of the Mathematical Modeling Study`, 
          `Description of the Model assumptions, tools and why they were used`, 
          `Availability of the modelling code for replicability of their models`, 
          `Process of developing the models; Engagement with relevant stakeholders`, 
          `Quality of the data used for the models`, 
          `Transparency/disclosure of study limitations`)
 
 # Create the table with gt
 
 table <- data %>%
   gt() %>%
   tab_header(title = "Risk of Bias Assessment for Mathematical Modeling Studies (2016-2025)") %>%
   cols_label(
     Year = "Year",
     Citation = "Study Citation",
     `Design of the Mathematical Modeling Study` = "Study Design",
     `Description of the Model assumptions, tools and why they were used` = "Model Assumptions",
     `Availability of the modelling code for replicability of their models` = "Code Availability",
     `Process of developing the models; Engagement with relevant stakeholders` = "Stakeholder Engagement",
     `Quality of the data used for the models` = "Data Quality",
     `Transparency/disclosure of study limitations` = "Limitations Disclosure"
   ) %>%
   # Apply color coding for High (red), Low (green), and Unsure (yellow)
   data_color(
     columns = c(`Design of the Mathematical Modeling Study`, 
                 `Description of the Model assumptions, tools and why they were used`, 
                 `Availability of the modelling code for replicability of their models`, 
                 `Process of developing the models; Engagement with relevant stakeholders`, 
                 `Quality of the data used for the models`, 
                 `Transparency/disclosure of study limitations`),
     colors = scales::col_factor(
       palette = c("High" = "#FF9999", "Low" = "#99FF99", "Unsure" = "#FFFF99"),
       domain = c("High", "Low", "Unsure")
     )) %>%
  
    # Adding a legend as a footnote
   
   tab_footnote(
     footnote = "Color Legend: High Risk = Red, Low Risk = Green, Unsure = Yellow",
     locations = cells_title(groups = "title")) %>%
   
   # Improving table aesthetics
   tab_options(
     table.font.size = 12,
     column_labels.font.weight = "bold",
     table.align = "center",
     table.width = pct(100))
 
 table
 