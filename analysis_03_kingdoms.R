# ==============================================================================
# Arquivo: analysis_03_kingdoms.R

# Input: data_04_gbif_year
# Output: PDF: lista das espécies mais comuns e porcentagem de uso em cada reino
#
# Modificado em: 2026-01-08
# Autor: Mateus Silva Figueiredo

# dif: primeira versão

# ==============================================================================
# Load necessary libraries
library(dplyr)
# library(tidyr)
# library(stringr)

setwd("C:/Users/Mateus/Desktop/R/epitetos")

list.files()

# ==============================================================================
# Last file of each df_
list.files(pattern = "df_gbif")[length(list.files(pattern = "df_gbif"))]

# Import data
dados_gbif <- read.csv2(list.files(pattern = "df_gbif")[length(list.files(pattern = "df_gbif"))],
                        sep=",")

# criar df
df <- dados_gbif

# ==============================================================================

# Step 1: Create a frequency table for the 'specific_epithet' column and order it by frequency
freq_table <- table(df$specific_epithet)

# Convert the table to a data frame and order by frequency (in descending order)
freq_df <- as.data.frame(freq_table) %>%
  arrange(desc(Freq))

# View the ordered frequency table
# freq_df %>% head(20)
# View(freq_df)

n_epitetos <- 100 # top quantos epitetos especificos analisar?
top_specific_epithets <- as.character(head(freq_df,n_epitetos)$Var)
#top_specific_epithets
# ------------------------------------------------------------------------------

df_top <- df[df$specific_epithet %in% top_specific_epithets,]
df_top$kingdom |> table()

# df_top$kingdom <- as.factor(df_top$kingdom)
df <- df_top

# ========================================================================
# DeepSeek
# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Assuming your dataframe is named 'df'
# If your dataframe has a different name, replace 'df' with your dataframe name

# Calculate counts for each epithet-kingdom combination
epithet_counts <- df %>%
  group_by(specific_epithet, kingdom) %>%
  summarise(count = n(), .groups = 'drop')

# Calculate total counts for each epithet (for ordering)
epithet_totals <- df %>%
  count(specific_epithet, name = "total_count") %>%
  arrange(desc(total_count))

# Join totals back to calculate percentages and add order
epithet_percentages <- epithet_counts %>%
  left_join(epithet_totals, by = "specific_epithet") %>%
  mutate(percentage = (count / total_count) * 100)

# Complete the data to include all epithet-kingdom combinations
epithet_percentages_complete <- epithet_percentages %>%
  complete(specific_epithet, kingdom, fill = list(count = 0, total_count = 0, percentage = 0)) %>%
  # Fill in total_count for epithets that might have been missing
  group_by(specific_epithet) %>%
  mutate(
    total_count = max(total_count, na.rm = TRUE),  # Get the total from existing rows
    percentage = ifelse(total_count == 0, 0, (count / total_count) * 100)
  ) %>%
  ungroup() %>%
  # Add epithet order based on total_count
  mutate(specific_epithet = factor(specific_epithet, 
                                   levels = epithet_totals$specific_epithet))

rm(epithet_percentages,epithet_counts)

# --------------------
# make graph

# percentage of the epithets
epithet_pct <- epithet_percentages_complete %>%
  mutate(specific_epithet = factor(specific_epithet, 
                                   levels = rev(levels(specific_epithet))))
# kingdom order based on pct of kingdom shows up
kingdom_order <- epithet_pct %>%
  group_by(kingdom) %>%
  summarize(total = sum(percentage)) %>%
  arrange(desc(total)) %>%
  pull(kingdom)

epithet_pct$kingdom <- factor(epithet_pct$kingdom, levels = kingdom_order)

# make graph
main_plot <- ggplot(epithet_pct, 
                    aes(x = percentage, y = specific_epithet, fill = kingdom)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.7) +
  scale_x_continuous(labels = percent_format(scale = 1), expand = c(0, 0)) +
  labs(
    title = "Specific Epithets by Total Usage",
    subtitle = "Epithets ordered by total frequency across all kingdoms",
    x = "Percentage of Epithet's Total Usage",
    y = "Specific Epithet",
    fill = "Kingdom"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0, color = "gray40", size = 11),
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_text(size = 9),
    plot.margin = margin(1, 3, 1, 2, "cm") # Top, Right, Bottom, Left margins
  ) +
  scale_fill_brewer(palette = "Set3", drop = FALSE) +  # drop = FALSE preserves all factor levels
  guides(fill = guide_legend(reverse = F))  # This makes legend match stack order

# print plot
print(main_plot)
# ===

# ============================================================
# EXPORT AS PDF (TEXT PRESERVED AS TEXT, NOT OUTLINES)
# ============================================================

# Exportar gráfico pronto

# Pega tempo atual para nome do arquivo

{ # começa salvar
  tempo_atual <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S")
  filename<-paste0("graph_01_epi_pct_kingdom_",tempo_atual,".pdf")
  
  # Function to export plot with text preserved
  export_pdf_with_text <- function(plot_obj, filename, width = 2, height = 9) {
    # Create PDF device with Cairo for better text handling
    if (capabilities("cairo")) {
      cairo_pdf(
        filename = filename,
        width = width,
        height = height,
        family = "Helvetica",  # Use standard, widely available font
        pointsize = 11
      )
    } else {
      print("no cairo")
    }
    
    # Print the plot
    print(plot_obj)
    
    # Close the device
    dev.off()
    
    cat("PDF exported successfully to:", filename, "\n")
    cat("File size:", file.info(filename)$size / 1024, "KB\n")
  }
  
  # Determine optimal height based on number of epithets
  num_epithets <- length(unique(df$specific_epithet))
  plot_height <- max(7, min(20, num_epithets * 0.3))  # Dynamic height adjustment
  
  # Export Option 1: All epithets (adjust width/height as needed)
  export_pdf_with_text(
    plot_obj = main_plot,
    filename = filename,
    width = 12,   # Increased from 10
    height = plot_height)
  
} # encerra salvar
# ===
