# ==============================================================================
# Arquivo: analysis_04_compare_binary.R

# Comparar bases de dados do GBIF, Wikidata e CoL
# Ver quantas espécies tem em cada uma delas, e nas interseções
#
# Input: df_col df_gbif df_wd
# Output: gráfico de Euler e de Upset
#
# Modificado em: 2026-03-25
# Autor: Mateus Silva Figueiredo

# difs: removed deduplicate function bc it was weird
# converted NAs into 0 and adapted subsets

# comentários:

# Usa apenas espécies únicas
# Ou seja, perde hemihomonimos :(
# mas remove subespecies etc. que podem ter passado :)

# A seguir: fazer gráfico de upset não binário

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

# check
dados_col |> nrow() |> paste("linhas em dados_col")

# cria clean_col sem taxon repetido, usa distinct
clean_col <- distinct(dados_col, taxon_name, .keep_all = TRUE)
clean_col |> nrow() |> paste("linhas em clean_col")

# select columns taxon_name and status, rename it to status_col
clean_col <- select(clean_col,c("taxon_name","status"))
colnames(clean_col) <- c("taxon_name","col_status")


# ----------------------------------------
# tratar dados gbif

# limpar, remove = and ? # talvez desnecessario
dados_gbif$taxon_name <- clean(dados_gbif$taxon_name)

# dados_gbif |> summary()

# only accepted, at first
gbif_acc <- dados_gbif[dados_gbif$status=="accepted",]
unique(gbif_acc$taxon_name) |> length()

# remove duplicates of accepted
gbif_acc <- gbif_acc[!duplicated(gbif_acc$taxon_name), ]
gbif_acc %>% nrow() # check

# only non accepted (other)
gbif_oth <- dados_gbif[dados_gbif$status!="accepted",]
gbif_oth %>% nrow() # check

# remove duplicates of other, maybe unneeded
# gbif_oth <- gbif_oth[!duplicated(gbif_oth$taxon_name), ]
# gbif_oth %>% nrow() # check

# combine acc and oth for uni (unique)
gbif_uni <- rbind(gbif_acc,gbif_oth)
gbif_uni %>% nrow() # check


# remove duplicates. only keep first of each. should be accepted.
clean_gbif <- distinct(gbif_uni, taxon_name, .keep_all = TRUE)
clean_gbif[clean_gbif$status == "accepted",] |> nrow() |> paste("accepted in gbif_clean")
clean_gbif |> nrow() |> paste("nrow in gbif_clean")

# select columns taxon_name and status, rename it to status_gbif
clean_gbif <- select(clean_gbif,c("taxon_name","status"))
colnames(clean_gbif) <- c("taxon_name","gbif_status")

# remove not needed
rm(gbif_uni,gbif_oth,gbif_acc)

# ----------------------------------------
# tratar dados wd

dados_wd$taxon_name <- clean(dados_wd$taxon_name)

dados_wd$status <- "accepted"

# check
dados_wd |> nrow() |> paste("linhas em dados_wd")

# cria clean_wd sem taxon repetido
clean_wd <- distinct(dados_wd, taxon_name, .keep_all = TRUE)

clean_wd |> nrow() |> paste("linhas em clean_wd")

# select columns taxon_name and status, rename it to status_wd
clean_wd <- select(clean_wd,c("taxon_name","status"))
colnames(clean_wd) <- c("taxon_name","wd_status")

# ==============================================================================

# Load required libraries
library(dplyr)
library(tidyr)



# Combine all datasets by taxon_name
combined <- full_join(clean_col, clean_gbif, by = "taxon_name") %>%
  full_join(clean_wd, by = "taxon_name")

# Replace all NA values with 0 in the entire data frame
combined[is.na(combined)] <- 0

# Check the result
head(combined)
summary(combined)

# ---

# analysis
sum(is.na(combined$col_status))  |> paste("NA in combined$col")
sum(is.na(combined$gbif_status)) |> paste("NA in combined$gbif")
sum(is.na(combined$wd_status))   |> paste("NA in combined$wd")

(!is.na(combined$col_status) & combined$col_status=="accepted")  |> sum() |> paste("non-NA and accepted in combined$col")
(!is.na(combined$gbif_status) & combined$gbif_status=="accepted")|> sum() |> paste("non-NA and accepted in combined$gbif")
(!is.na(combined$wd_status) & combined$wd_status=="accepted")    |> sum() |> paste("non-NA and accepted in combined$wd")

sum(duplicated(combined$taxon_name)) |> paste("duplicated in combined$taxon_name")

combined

# analysis

subset(combined,combined$gbif_status=="accepted",) |> nrow()
subset(combined,combined$gbif_status=="synonym") |> nrow()
subset(combined,combined$gbif_status=="doubtful") |> nrow()


# Optional: Save the combined dataset
# write.csv(combined, "combined_status.csv", row.names = FALSE)

# ======================================
# Convert non-accepted statuses to "0" for consistent comparison -> combined binary
combined_bi <- combined %>%
  mutate(
    col_status = ifelse(col_status == "accepted", "accepted", "0"),
    gbif_status = ifelse(gbif_status == "accepted", "accepted", "0"),
    wd_status = ifelse(wd_status == "accepted", "accepted", "0")
  )

# Check the conversion
table(combined_bi$col_status)
table(combined_bi$gbif_status)
table(combined_bi$wd_status)

# ==============================================================================
# ============================ START EULER DIAGRAM =============================
# ==============================================================================# prepare for euler diagram - create number of each accepted

# accepted in all three
combined_bi[combined_bi$col_status=="accepted" &
         combined_bi$gbif_status=="accepted" & 
         combined_bi$wd_status=="accepted",] |> nrow() -> nCGW

# accepted in only two
combined_bi[combined_bi$col_status=="0" & 
             combined_bi$gbif_status=="accepted" & 
             combined_bi$wd_status=="accepted",] |> nrow()-> nGW

combined_bi[combined_bi$col_status=="accepted" & 
           combined_bi$gbif_status=="0" & 
           combined_bi$wd_status=="accepted",]|> nrow() -> nCW

combined_bi[combined_bi$col_status=="accepted" & 
           combined_bi$gbif_status=="accepted" & 
           combined_bi$wd_status=="0",] |> nrow() -> nCG

# accepted in only one
combined_bi[combined_bi$col_status=="0" & 
           combined_bi$gbif_status=="0" & 
           combined_bi$wd_status=="accepted",] |> nrow() -> nW

combined_bi[combined_bi$col_status=="accepted" & 
           combined_bi$gbif_status=="0" & 
           combined_bi$wd_status=="0",] |> nrow()-> nC

combined_bi[combined_bi$col_status=="0" & 
           combined_bi$gbif_status=="accepted" & 
           combined_bi$wd_status=="0",] |> nrow() -> nG

# ==============================================================================

# Create the named numeric vector for eulerr from summary_df
euler_counts <- c(
  "C" = as.numeric(nC),
  "G" = as.numeric(nG),
  "W" = as.numeric(nW),
  
  "C&G" = as.numeric(nCG),
  "C&W" = as.numeric(nCW),
  "G&W" = as.numeric(nGW),
  
  "C&G&W" = as.numeric(nCGW)
)

names(euler_counts)

# Use it with eulerr
fit1 <- euler(euler_counts) # areas = number

# fit1 <- venn(euler_counts) # pretty venn diagram, with 0 if needed

# ========================================================================
# make graphs

# Add text after the plot using grid graphics
library(grid)

# Automatic simple graph
plot(fit1, 
     quantities = TRUE,  # Show numbers
     fills = c("skyblue", "lightgreen", "lightcoral"),  # Custom colors
     labels = c("CoL", "GBIF", "WD"))  # Custom labels

# ---
# ==============================================================================
# ============================== END EULER DIAGRAM =============================
# ==============================================================================
# ========================= START UPSET BINARY DIAGRAM =========================
# ==============================================================================

# make upset graph - binary

library(UpSetR)
library(dplyr)

# ---


# Create upset matrix
upset_matrix_acc <- data.frame(
  col_acc = as.numeric(combined_bi$col_status=="accepted"),
  gbif_acc = as.numeric(combined_bi$gbif_status=="accepted"),
  wd_acc = as.numeric(combined_bi$wd_status=="accepted")
)


# Generate upset plot with the simple matrix
upset(upset_matrix_acc, 
      sets = c("gbif_acc", "col_acc", "wd_acc"), 
      keep.order = TRUE,
      text.scale = c(1, 1, 1, 1, 1, 1.2), # sixth is number size
      mainbar.y.max=1900000, # height of graph
      mainbar.y.label = "Number of taxa",
      sets.x.label = "Total taxa per dataset - only accepted")

# ==============================================================================
# =========================== END UPSET BINARY DIAGRAM =========================
# ==============================================================================
print("there be monsters below")
# ---

# ==============================================================================
# ==============================================================================
# ==============================================================================


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
      