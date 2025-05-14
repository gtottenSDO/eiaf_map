# Title: Colorado Energy Impact Grant Awards Map
# Description: Recreates a map of Colorado counties showing energy grant awards,
#              based on an example image and data from awards.csv.
#              Handles NA grant amounts as 0, refined styling.

# --------------------------------------------------------------------------
# 1. INSTALL AND LOAD NECESSARY PACKAGES
# --------------------------------------------------------------------------
# Install packages if you haven't already:
# install.packages(c("sf", "ggplot2", "dplyr", "readr", "stringr", "tigris", "scales", "shadowtext"))

library(sf)
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(tigris) # For fetching county shapefiles
library(scales) # For formatting numbers (e.g., dollar amounts)
library(shadowtext) # For outlined text labels

# Optional: Cache downloaded shapefiles from tigris
options(tigris_use_cache = TRUE)

# --------------------------------------------------------------------------
# 2. DEFINE STYLING ELEMENTS (COLORS, LABELS)
# --------------------------------------------------------------------------
# Colors picked from the example image (pale green/cyan to dark blue)
color_palette <- c(
  "< $100,000" = "#ffffcc",
  "$100,000 - $500,000" = "#a1dab4",
  "$500,000 - $1,000,000" = "#41b6c4",
  "$1,000,000 - $5,000,000" = "#2c7fb8",
  "> $5,000,000" = "#253494"
)

# Grant categories and their order for the legend
grant_breaks <- c(0, 100000, 500000, 1000000, 5000000, Inf)
grant_labels <- c(
  "< $100,000",
  "$100,000 - $500,000",
  "$500,000 - $1,000,000",
  "$1,000,000 - $5,000,000",
  "> $5,000,000"
)

# --------------------------------------------------------------------------
# 3. LOAD AND PREPARE AWARDS DATA (awards.csv)
# --------------------------------------------------------------------------
# Load the awards data
# Make sure 'awards.csv' is in your working directory or provide the full path.
tryCatch(
  {
    awards_data_raw <- readr::read_csv("awards.csv", show_col_types = FALSE)
  },
  error = function(e) {
    stop(
      "Error reading awards.csv: ",
      e$message,
      "\nPlease ensure 'awards.csv' is in your working directory or provide the correct path."
    )
  }
)


# --- Data Cleaning and Aggregation ---
# The script expects 'County' and 'AwardAmount' columns in your awards.csv file.
if (
  !"County" %in% names(awards_data_raw) ||
    !"AwardAmount" %in% names(awards_data_raw)
) {
  stop(
    "The 'awards.csv' file must contain 'County' and 'AwardAmount' columns. Please check your CSV file."
  )
}

awards_data_processed <- awards_data_raw %>%
  # Clean AwardAmount: remove '$', ',', and convert to numeric
  mutate(
    AwardAmount = as.numeric(str_replace_all(AwardAmount, "[\\$,]", ""))
  ) %>%
  # Clean County names:
  # - Remove " County" suffix if present (e.g., "Adams County" -> "Adams")
  # - Trim whitespace from county names
  # - Convert to title case (e.g., "ADAMS" or "adams" becomes "Adams") to match tigris data
  mutate(
    County = str_to_title(trimws(str_replace_all(County, " County", "")))
  ) %>%
  # Group by county and sum the award amounts
  group_by(County) %>%
  summarise(TotalGrantAmount = sum(AwardAmount, na.rm = TRUE), .groups = 'drop')

# --------------------------------------------------------------------------
# 4. LOAD COLORADO COUNTY GEOGRAPHICAL DATA
# --------------------------------------------------------------------------
# Fetch Colorado county boundaries using tigris
# Using cb = TRUE for cartographic boundary files (generalized for faster plotting)
# Using resolution = "500k" for a reasonable level of detail
colorado_counties_sf <- tigris::counties(
  state = "CO",
  cb = TRUE,
  resolution = "500k"
) %>%
  select(County = NAME, geometry) %>% # Select and rename county name column from NAME to County
  mutate(County = str_to_title(County)) # Ensure county names are title case for merging

# --------------------------------------------------------------------------
# 5. MERGE AWARDS DATA WITH GEOGRAPHICAL DATA
# --------------------------------------------------------------------------
# Perform a left join to keep all counties from the shapefile,
# and match them with grant data. Counties with no grants will have NA for grant amounts.
map_data <- colorado_counties_sf %>%
  left_join(awards_data_processed, by = "County") %>%
  # IMPORTANT: Treat counties with no grant data (NA TotalGrantAmount) as having 0 grants.
  mutate(
    TotalGrantAmount = ifelse(is.na(TotalGrantAmount), 0, TotalGrantAmount)
  )

# Calculate centroids for label placement *before* potential filtering
# This ensures that labels are based on the full geometry.
# shadowtext needs x and y aesthetics, so we extract centroids.
map_data_centroids <- map_data %>%
  mutate(
    centroid_coords = sf::st_centroid(geometry), # Suppress warning for st_centroid on lon/lat
    lon = sf::st_coordinates(centroid_coords)[, 1],
    lat = sf::st_coordinates(centroid_coords)[, 2]
  )

# --------------------------------------------------------------------------
# 6. CREATE GRANT CATEGORIES FOR MAPPING
# --------------------------------------------------------------------------
map_data_centroids <- map_data_centroids %>% # Continue with the data that has centroids
  mutate(
    GrantCategory = cut(
      TotalGrantAmount,
      breaks = grant_breaks,
      labels = grant_labels,
      right = FALSE, # Intervals are [min, max) e.g. [0, 100000)
      include.lowest = TRUE # Includes values equal to the lowest break (0 in this case)
    )
  )

# --------------------------------------------------------------------------
# 7. CREATE THE MAP
# --------------------------------------------------------------------------
# Define title, subtitle, and caption text
plot_title <- "Energy Impact Grant Awards by County"
plot_subtitle <- "January 1, 2024 to December 31, 2024"
plot_caption <- paste0(
  "DOLA: ",
  today(),
  "\nMunicipal and Special District Grants included in their corresponding County Totals. Statewide grants excluded."
)

# Create the ggplot map
grant_map <- ggplot(data = map_data_centroids) + # Use data with centroid coordinates
  # County polygons, filled by grant category
  geom_sf(
    aes(fill = GrantCategory, geometry = geometry),
    color = "gray30",
    linewidth = 0.2
  ) + # Specify geometry aesthetic for geom_sf

  # County labels with white text and black outline using shadowtext::geom_shadowtext
  shadowtext::geom_shadowtext(
    aes(x = lon, y = lat, label = County), # Use lon and lat for position
    color = "white", # Text color
    bg.color = "black", # Outline color (background for the text)
    bg.r = 0.1, # Radius of the outline effect (adjust as needed)
    fontface = "bold", # Bold font
    size = 4, # Font size for labels (adjust as needed)
    check_overlap = TRUE # Attempt to avoid overlapping labels
  ) +

  # Manual color scale for fill
  scale_fill_manual(
    values = color_palette,
    name = "Total of Grants", # Legend title
    labels = grant_labels,
    # na.value is still good practice, though ideally all counties now have a category
    na.value = "#DFF2F2", # Default NA counties to the lowest category color
    drop = FALSE # Ensure all defined categories appear in the legend
  ) +

  # Titles and caption
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    caption = plot_caption
  ) +

  # Legend Guide
  # guides(
  #   colour = guide_legend(position = "left")
  # ) +

  # Theme adjustments
  theme_void(base_family = "sans") + # Start with a minimal theme, use a generic sans-serif font
  theme(
    # Plot title styling (centered, bold, larger size)
    plot.title = element_text(
      hjust = 0.5,
      size = 36,
      face = "bold"
      # margin = margin(b = 5)
    ),
    # Plot subtitle styling (centered, adjusted size, more space below)
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 28
      # margin = margin(b = 25)
    ),
    plot.title.position = "plot",
    # Plot caption styling (left-aligned at the bottom)
    plot.caption = element_text(
      hjust = 0,
      size = 12,
      margin = margin(t = 15, l = 10),
      lineheight = 1.2
    ), # Added left margin to caption
    plot.caption.position = "plot", # Position caption relative to the plot area

    # Legend styling (top-left inside, no border)
    # legend.position.inside = c(1, 0.85), # Adjust x, y (0-1) for precise top-left placement
    legend.position = c("left"), # Anchor legend by its top-left corner
    legend.justification.left = "top",
    legend.margin = margin(10, 0, 0, 0),
    legend.background = element_blank(), # Remove legend background/border
    legend.box.background = element_blank(), # Ensure no box around legend items if any
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 16),

    # Overall plot border
    plot.background = element_rect(color = "black", linewidth = 0.5, fill = NA), # Add border around the full plot

    # Remove panel border if theme_void doesn't fully remove it (it usually does)
    panel.border = element_blank(),
    # Add some margin around the entire plot (inside the overall border)
    plot.margin = margin(15, 15, 15, 15) # top, right, bottom, left margins
  )

# --------------------------------------------------------------------------
# 8. DISPLAY THE MAP
# --------------------------------------------------------------------------
print(grant_map)

# To save the map to a file:
ggsave(
  "energy_grants_map_final_na_handled.png",
  plot = grant_map,
  width = 11,
  height = 8.5,
  dpi = 300,
  bg = "white"
)
# Adjust width/height in ggsave as needed for optimal layout.
# bg = "white" ensures the background is white if saving as PNG.
