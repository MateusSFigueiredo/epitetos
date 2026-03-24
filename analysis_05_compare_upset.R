# ==============================================================================
# Arquivo: analysis_05_compare_upset.R

# Comparar bases de dados do GBIF, Wikidata e CoL
# Ver quantas espécies tem em cada uma delas, e nas interseções
#
# Input: df_col df_gbif df_wd
# Output: gráfico de Upset
#
# Modificado em: 2026-03-24
# Autor: Mateus Silva Figueiredo

# difs: primeira versão

# comentários:

# Usa apenas espécies únicas
# Ou seja, perde hemihomonimos :(
# mas remove subespecies etc. que podem ter passado :)
# Gráfico de Euler criado é bem feio
# usar ele de base para gráfico manual no PowerPoint ou similar

# ==============================================================================

# ==============================================================================

setwd("C:/Users/Mateus/Desktop/R/epitetos")

list.files()

# ==============================================================================
# functions
# ---
# cria função clean para apagar "?" e "=", espaços sobrando
clean <- function(x) {
  x <- gsub("[?=]", "", x)            # remove only "?" and "=" characters
  x <- gsub("\\s+", " ", x)           # collapse multiple spaces
  x <- trimws(x)                      # remove leading/trailing spaces
  return(x)
}

# ==============================================================================
# Import data

# Check last file of df_col df_wd and wd_gbif
list.files(pattern = "df_col")[length(list.files(pattern = "df_col"))]
list.files(pattern = "df_wd")[length(list.files(pattern = "df_wd"))]
list.files(pattern = "df_gbif")[length(list.files(pattern = "df_gbif"))]


# Import data # a few seconds
dados_col <- read.csv2(list.files(pattern = "df_col")[length(list.files(pattern = "df_col"))],sep=",")

dados_wd <- read.csv2(list.files(pattern = "df_wd")[length(list.files(pattern = "df_wd"))],sep=",")

dados_gbif <- read.csv2(list.files(pattern = "df_gbif")[length(list.files(pattern = "df_gbif"))],sep=",")


# ==============================================================================
# tratar dados col
# limpa col. remove símbolos = e ? do taxon_name. mantém -
dados_col$taxon_name <- clean(dados_col$taxon_name)

# cria coluna status para dados_col, juntando colunas equal e question
dados_col <- dados_col %>%
  mutate(status = case_when(
    !is.na(equal) & !is.na(question) ~ paste(equal, question),
    !is.na(equal) & is.na(question) ~ as.character(equal),
    is.na(equal) & !is.na(question) ~ as.character(question),
    TRUE ~ "accepted"
  ))

# ---
# tratar dados wd

dados_wd$status <- "accepted"

# ==============================================================================

# Load required libraries
library(dplyr)
library(tidyr)

# Function to handle duplicates within each dataset
# Priority: "accepted" > first occurrence
deduplicate_dataset <- function(df, dataset_name) {
  df %>%
    group_by(taxon_name) %>%
    summarise(
      status = {
        # If "accepted" exists, use it; otherwise use first status
        if("accepted" %in% status) "accepted" else first(status)
      },
      .groups = "drop"
    ) %>%
    rename(!!paste0(dataset_name, "_status") := status)
}


# Deduplicate each dataset
gbif_clean <- deduplicate_dataset(dados_gbif, "gbif")
col_clean <- deduplicate_dataset(dados_col, "col")
wd_clean <- deduplicate_dataset(dados_wd, "wd")

# Combine all datasets by taxon_name
combined <- full_join(gbif_clean, col_clean, by = "taxon_name") %>%
  full_join(wd_clean, by = "taxon_name")

# Check the result
head(combined)
summary(combined)

# Optional: Save the combined dataset
# write.csv(combined, "combined_status.csv", row.names = FALSE)

# ======================================

# make upset graph

library(UpSetR)
library(dplyr)

# Create presence/absence matrix for the upset plot
upset_data <- combined %>%
  mutate(
    in_gbif = !is.na(gbif_status),
    in_col = !is.na(col_status),
    in_wd = !is.na(wd_status)
  ) %>%
  select(in_gbif, in_col, in_wd)

# Convert to binary matrix (UpSetR expects a data frame with binary columns)
upset_matrix <- upset_data %>%
  mutate(across(everything(), as.numeric))

# # Method 1: Using upset() directly with the binary matrix
# upset(upset_matrix, 
#       sets = c("in_gbif", "in_col", "in_wd"), 
#       keep.order = TRUE,
#       mainbar.y.label = "Intersection Size",
#       sets.x.label = "Set Size")

# ---

# Alternatively, Method 2: Create the binary matrix in a simpler way
upset_matrix_simple <- data.frame(
  in_gbif = as.numeric(!is.na(combined$gbif_status)),
  in_col = as.numeric(!is.na(combined$col_status)),
  in_wd = as.numeric(!is.na(combined$wd_status))
)

# Generate upset plot with the simple matrix
upset(upset_matrix_simple, 
      sets = c("in_gbif", "in_col", "in_wd"), 
      keep.order = TRUE,
      mainbar.y.label = "Number of taxa",
      sets.x.label = "Total taxa per dataset")

# only presence / absensce

# ---

gbif_clean$gbif_status %>% table()

c('accepted','doubtful','heterotypic synonym',
  'homotypic synonym','proparte synonym','synonym')

# ---

# Alternatively, Method 3: Create the binary matrix in a simpler way
upset_matrix_simple <- data.frame(
  gbif_acc = as.numeric(combined$gbif_status=="accepted" & !is.na(combined$gbif_status)),
  gbif_oth = as.numeric(combined$gbif_status!="accepted" & !is.na(combined$gbif_status)),
  col_acc = as.numeric(combined$col_status=="accepted" & !is.na(combined$col_status)),
  col_oth = as.numeric(combined$col_status!="accepted" & !is.na(combined$col_status)),
  wd_acc = as.numeric(combined$wd_status=="accepted" & !is.na(combined$wd_status)))

# Generate upset plot with the simple matrix
upset(upset_matrix_simple, 
      sets = c(
        "gbif_acc", "col_acc", "wd_acc",
               "gbif_oth","col_oth"), 
      keep.order = TRUE, # for the legend
      order.by = "freq",  # This sorts intersections by frequency (descending)
      mainbar.y.label = "Number of taxa",
      sets.x.label = "Total taxa per dataset")

#many statuses

# ---

# Alternatively, Method 3: Create the binary matrix in a simpler way
upset_matrix_simple <- data.frame(
  gbif_acc = as.numeric(combined$gbif_status=="accepted" & !is.na(combined$gbif_status)),
  gbif_oth = as.numeric(combined$gbif_status!="accepted" & !is.na(combined$gbif_status)),
  col_acc = as.numeric(combined$col_status=="accepted" & !is.na(combined$col_status)),
  col_oth = as.numeric(combined$col_status!="accepted" & !is.na(combined$col_status)),
  wd_acc = as.numeric(combined$wd_status=="accepted" & !is.na(combined$wd_status)))

# Generate upset plot with the simple matrix
upset(upset_matrix_simple, 
      sets = c("gbif_oth","col_oth",
        "gbif_acc", "col_acc", "wd_acc"
        ), 
      mainbar.y.max = 1600000,
      keep.order = TRUE, # for the legend
      order.by = c("freq"),  # This sorts intersections by frequency (descending)
      mainbar.y.label = "Number of taxa",
#      sets.bar.color = "gray",
#      main.bar.color = "gray80",
#      number.angles = 10,
#      number.colors= "darkred",  # Single color for all numbers
#      group.by = "degree",
      sets.x.label = "Total taxa per dataset")

# ==============================================================================

cora3 <- "red"
cora2 <- "red2"
cora1 <- "red3"
corb1 <- "blue"
corb2 <- "blue2"

# Define queries for accepted species intersections
accepted_queries <- list(
  list(query = intersects, 
       params = list("gbif_acc", "col_acc", "wd_acc"), 
       color = cora3, 
       active = TRUE,
       query.name = "All accepted"),
  list(query = intersects, 
       params = list("gbif_acc", "col_acc"), 
       color = cora2, 
       active = TRUE,
       query.name = "GBIF+COL accepted"),
  list(query = intersects, 
       params = list("gbif_acc", "wd_acc"), 
       color = cora2, 
       active = TRUE,
       query.name = "GBIF+WD accepted"),
  list(query = intersects, 
       params = list("col_acc", "wd_acc"), 
       color = cora2, 
       active = TRUE,
       query.name = "COL+WD accepted"),
  list(query = intersects, 
       params = list("wd_acc"), 
       color = cora1, 
       active = TRUE,
       query.name = "Only WD accepted"),
  list(query = intersects, 
       params = list("col_acc"), 
       color = cora1, 
       active = TRUE,
       query.name = "Only COL accepted"),
  list(query = intersects, 
       params = list("gbif_acc"), 
       color = cora1, 
       active = TRUE,
       query.name = "Only GBIF accepted"),
  list(query = intersects, 
       params = list("col_oth","gbif_oth"), 
       color = corb1, 
       active = TRUE,
       query.name = "Only GBIF accepted"),
  list(query = intersects, 
       params = list("col_oth"), 
       color = corb2, 
       active = TRUE,
       query.name = "Only GBIF accepted"),
  list(query = intersects, 
       params = list("gbif_oth"), 
       color = corb2, 
       active = TRUE,
       query.name = "Only GBIF accepted")
)

# Generate plot with queries
upset(upset_matrix_simple, 
      sets = c(
        "gbif_acc", "col_acc", "wd_acc",
        "gbif_oth", "col_oth"), 
      keep.order = TRUE,
      order.by = "freq",
      queries = accepted_queries,
      mainbar.y.label = "Number of taxa",
      sets.x.label = "Total taxa per dataset",
      number.angles = 0,
      text.scale = 1.2)