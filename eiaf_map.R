# R Script to Create a Choropleth Map of Colorado County Grants

# --------------------------------------------------------------------------
# 1. INSTALL AND LOAD NECESSARY PACKAGES
# --------------------------------------------------------------------------
# If you haven't installed these packages yet, uncomment and run the following lines:
# install.packages("sf")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("readr")
# install.packages("tigris")
# install.packages("scales")     # For formatting numbers
# install.packages("stringr")    # For string manipulation
# install.packages("ggrepel")    # For non-overlapping text labels
# install.packages("gridExtra")  # For creating table grobs
# install.packages("patchwork")  # For combining plots
# install.packages("cowplot")    # For extracting legends
# install.packages("grid")       # Explicitly load grid if not already

library(sf)
library(dplyr)
library(ggplot2)
library(readr)
library(tigris)
library(scales)
library(stringr)
library(ggrepel)
library(gridExtra)
library(patchwork)
library(cowplot)
library(grid) # Explicitly load grid

# --------------------------------------------------------------------------
# 2. LOAD AND PREPARE THE GRANT DATA
# --------------------------------------------------------------------------
file_path <- "awards.csv" # Or "path/to/your/awards.csv"
if (!file.exists(file_path)) {
  stop(paste(
    "Error: The file '",
    file_path,
    "' was not found. Please check the path.",
    sep = ""
  ))
}

grant_data_raw <- readr::read_csv(file_path, show_col_types = FALSE)

if (ncol(grant_data_raw) >= 2) {
  colnames(grant_data_raw)[1] <- "County"
  colnames(grant_data_raw)[2] <- "TotalGrants"
} else {
  stop("Error: The CSV file does not have at least two columns.")
}

grant_data <- grant_data_raw %>%
  mutate(
    County = stringr::str_replace(County, " County", ""),
    County = stringr::str_trim(County),
    TotalGrants = as.numeric(TotalGrants)
  ) %>%
  filter(!is.na(County) & !is.na(TotalGrants))

# --------------------------------------------------------------------------
# 3. FETCH COLORADO COUNTY GEOSPATIAL DATA
# --------------------------------------------------------------------------
colorado_counties_sf <- tigris::counties(
  state = "CO",
  cb = TRUE,
  resolution = "500k"
) %>%
  st_transform(3857) # Project to a common CRS (Web Mercator often good for visualization)

# --------------------------------------------------------------------------
# 4. MERGE GRANT DATA WITH GEOSPATIAL DATA
# --------------------------------------------------------------------------
merged_data_sf <- dplyr::left_join(
  colorado_counties_sf,
  grant_data,
  by = c("NAME" = "County")
) %>%
  mutate(TotalGrants = ifelse(is.na(TotalGrants), 0, TotalGrants))

# Calculate centroids and extract coordinates for labels
if (!("geometry" %in% class(merged_data_sf$geometry))) {
  merged_data_sf <- st_set_geometry(merged_data_sf, "geometry")
}
merged_data_sf <- merged_data_sf %>%
  mutate(
    centroid = sf::st_centroid(geometry),
    x_coord = sf::st_coordinates(centroid)[, 1],
    y_coord = sf::st_coordinates(centroid)[, 2]
  )

# --------------------------------------------------------------------------
# 5. CREATE GRANT CATEGORIES
# --------------------------------------------------------------------------
breaks <- c(0, 100000, 500000, 1000000, 5000000, Inf)
category_labels <- c(
  "< $100,000",
  "$100,000 - $500,000",
  "$500,000 - $1,000,000",
  "$1,000,000 - $5,000,000",
  "> $5,000,000"
)

merged_data_sf$GrantCategory <- cut(
  merged_data_sf$TotalGrants,
  breaks = breaks,
  labels = category_labels,
  right = FALSE,
  include.lowest = TRUE
)
merged_data_sf$GrantCategory[
  is.na(merged_data_sf$GrantCategory) & merged_data_sf$TotalGrants == 0
] <- category_labels[1]


color_palette <- c(
  "< $100,000" = "#FEFBEA",
  "$100,000 - $500,000" = "#E0F3DB",
  "$500,000 - $1,000,000" = "#A8DDB5",
  "$1,000,000 - $5,000,000" = "#72B2D7",
  "> $5,000,000" = "#2A7FB8"
)

# --------------------------------------------------------------------------
# 6. CREATE THE MAP PLOT
# --------------------------------------------------------------------------
map_plot_main <- ggplot(data = merged_data_sf) +
  geom_sf(aes(fill = GrantCategory), color = "grey50", size = 0.2) +
  ggrepel::geom_text_repel(
    aes(x = x_coord, y = y_coord, label = NAME),
    size = 2.5,
    color = "black",
    fontface = "bold",
    box.padding = unit(0.25, "lines"),
    point.padding = unit(0.2, "lines"),
    segment.color = 'grey50',
    segment.size = 0.3,
    max.overlaps = Inf,
    bg.color = "white",
    bg.r = 0.05
  ) +
  scale_fill_manual(
    values = color_palette,
    name = "Total of Grants",
    na.value = "grey90",
    guide = guide_legend(
      reverse = FALSE,
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  labs(
    caption = "Source: User-provided 2024 data"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold",
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 12,
      margin = margin(b = 10)
    ),
    plot.caption = element_text(
      hjust = 0,
      size = 8,
      color = "grey50",
      margin = margin(t = 10)
    ),
    legend.position = "none"
  )

map_legend_plot <- ggplot(data = merged_data_sf) +
  geom_sf(aes(fill = GrantCategory)) +
  scale_fill_manual(
    values = color_palette,
    name = "Total of Grants",
    na.value = "grey90",
    guide = guide_legend(title.position = "top", title.hjust = 0.5)
  ) +
  theme_void() +
  theme(
    legend.position = "left",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.5, "cm"),
    legend.box.margin = margin(0, 0, 0, 0)
  )

common_legend <- cowplot::get_legend(map_legend_plot)

# --------------------------------------------------------------------------
# 7. PREPARE DATA AND CREATE THE TABLE OF GRANTS
# --------------------------------------------------------------------------
table_data <- grant_data %>%
  filter(TotalGrants > 0) %>%
  arrange(desc(TotalGrants)) %>%
  mutate(TotalGrantsFormatted = scales::dollar(TotalGrants, accuracy = 1)) %>%
  select(County, TotalGrantsFormatted)

n_rows_total <- nrow(table_data)
n_rows_col <- ceiling(n_rows_total / 2)

table_df1 <- table_data[1:n_rows_col, ]
table_df2 <- if (n_rows_total > n_rows_col) {
  table_data[(n_rows_col + 1):n_rows_total, ]
} else {
  data.frame(County = character(0), TotalGrantsFormatted = character(0))
}

if (nrow(table_df1) > nrow(table_df2)) {
  diff_rows <- nrow(table_df1) - nrow(table_df2)
  padding_df <- data.frame(
    County = rep("", diff_rows),
    TotalGrantsFormatted = rep("", diff_rows)
  )
  table_df2 <- rbind(table_df2, padding_df)
} else if (nrow(table_df2) > nrow(table_df1) && nrow(table_df1) > 0) {
  diff_rows <- nrow(table_df2) - nrow(table_df1)
  padding_df <- data.frame(
    County = rep("", diff_rows),
    TotalGrantsFormatted = rep("", diff_rows)
  )
  table_df1 <- rbind(table_df1, padding_df)
}


final_table_df <- cbind(table_df1, table_df2)
colnames(final_table_df) <- c("County ", "$ Amount", " County", "$ Amount ")

ttheme_minimal <- gridExtra::ttheme_minimal(
  core = list(
    fg_params = list(cex = 0.7, hjust = 0, x = 0.05),
    bg_params = list(fill = "white", col = "grey70")
  ),
  colhead = list(
    fg_params = list(cex = 0.7, fontface = "bold"),
    bg_params = list(fill = "grey90", col = "grey70")
  )
)

if (nrow(final_table_df) > 0 && ncol(final_table_df) > 0) {
  table_grob_full <- gridExtra::tableGrob(
    final_table_df,
    rows = NULL,
    theme = ttheme_minimal
  )
  table_title_grob <- grid::textGrob(
    "Table:",
    gp = grid::gpar(fontsize = 11, fontface = "bold"),
    hjust = 0,
    x = 0.05
  )
  table_with_title_grob <- gridExtra::arrangeGrob(
    table_title_grob,
    table_grob_full,
    ncol = 1,
    heights = grid::unit.c(unit(2, "lines"), unit(1, "npc") - unit(2, "lines"))
  )
} else {
  table_with_title_grob <- grid::textGrob(
    "No grant data to display in table.",
    gp = grid::gpar(fontsize = 10)
  )
}

max_table_rows_display <- 20
if (n_rows_col > max_table_rows_display) {
  n_rows_col_display <- max_table_rows_display
  table_df1_display <- table_data[1:n_rows_col_display, ]
  table_df2_display <- if (n_rows_total > n_rows_col_display) {
    table_data[
      (n_rows_col_display + 1):min(n_rows_total, 2 * n_rows_col_display),
    ]
  } else {
    data.frame(County = character(0), TotalGrantsFormatted = character(0))
  }

  if (nrow(table_df1_display) > nrow(table_df2_display)) {
    diff_rows <- nrow(table_df1_display) - nrow(table_df2_display)
    padding_df_display <- data.frame(
      County = rep("", diff_rows),
      TotalGrantsFormatted = rep("", diff_rows)
    )
    table_df2_display <- rbind(table_df2_display, padding_df_display)
  } else if (
    nrow(table_df2_display) > nrow(table_df1_display) &&
      nrow(table_df1_display) > 0
  ) {
    diff_rows_display <- nrow(table_df2_display) - nrow(table_df1_display)
    padding_df_display <- data.frame(
      County = rep("", diff_rows_display),
      TotalGrantsFormatted = rep("", diff_rows_display)
    )
    table_df1_display <- rbind(table_df1_display, padding_df_display)
  }

  final_table_df_display <- cbind(table_df1_display, table_df2_display)

  if (ncol(final_table_df_display) == 4) {
    colnames(final_table_df_display) <- c(
      "County ",
      "$ Amount",
      " County",
      "$ Amount "
    )
  } else if (
    ncol(final_table_df_display) == 2 && nrow(table_df2_display) == 0
  ) {
    colnames(final_table_df_display) <- c("County ", "$ Amount")
  }

  if (nrow(final_table_df_display) > 0 && ncol(final_table_df_display) > 0) {
    table_grob_display <- gridExtra::tableGrob(
      final_table_df_display,
      rows = NULL,
      theme = ttheme_minimal
    )
    table_title_grob_display <- grid::textGrob(
      "Table (Top Entries):",
      gp = grid::gpar(fontsize = 11, fontface = "bold"),
      hjust = 0,
      x = 0.05
    )
    table_with_title_grob <- gridExtra::arrangeGrob(
      table_title_grob_display,
      table_grob_display,
      ncol = 1,
      heights = grid::unit.c(
        unit(2, "lines"),
        unit(1, "npc") - unit(2, "lines")
      )
    )
  } else {
    table_with_title_grob <- grid::textGrob(
      "No grant data for table (display limit).",
      gp = grid::gpar(fontsize = 10)
    )
  }
}

# --------------------------------------------------------------------------
# 8. COMBINE MAP, LEGEND, AND TABLE USING PATCHWORK
# --------------------------------------------------------------------------
# Wrap grobs for robust patchwork combination
wrapped_common_legend <- patchwork::wrap_elements(common_legend)
wrapped_table_with_title <- patchwork::wrap_elements(table_with_title_grob)

# Combine legend and table vertically, applying height proportions
left_panel <- (wrapped_common_legend / wrapped_table_with_title) +
  plot_layout(heights = grid::unit.c(unit(0.25, "npc"), unit(0.75, "npc")))

# Combine the left panel and the main map horizontally
# map_plot_main is already a ggplot object
final_plot_layout <- left_panel | map_plot_main

# Apply width proportions to the horizontally combined plot
final_plot <- final_plot_layout +
  plot_layout(widths = grid::unit.c(unit(1, 'null'), unit(2.5, 'null')))

# Add overall titles
final_plot_with_titles <- final_plot +
  plot_annotation(
    title = "Energy Impact Grant Awards by County - 2024",
    subtitle = "Calendar Year 2024",
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12)
    )
  )

print(final_plot_with_titles)

# --------------------------------------------------------------------------
# 9. SAVE THE MAP (OPTIONAL)
# --------------------------------------------------------------------------
ggsave("colorado_grants_map_2024_full.png", plot = final_plot_with_titles, width = 14, height = 9, dpi = 300)
print("Map saved as colorado_grants_map_2024_full.png")
