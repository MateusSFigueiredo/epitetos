# ==============================================================================
# Arquivo: analysis_04_compare_venn.R

# Comparar bases de dados do GBIF, Wikidata e CoL
# Ver quantas espécies tem em cada uma delas, e nas interseções
#
# Input: df_col df_gbif df_wd
# Output: gráfico de Euler / Venn
#
# Modificado em: 2026-03-23
# Autor: Mateus Silva Figueiredo

# difs: primeira versão

# comentários:

# Usa apenas espécies únicas
# Ou seja, perde hemihomonimos :(
# mas remove subespecies etc. que podem ter passado :)
# Gráfico de Euler criado é bem feio
# usar ele de base para gráfico manual no PowerPoint ou similar

# ==============================================================================
# Load necessary libraries
library(dplyr)
#library(tidyr)
library(stringr) # for str_detect
library(readr) # para read_tsv
library(stringi) # para stri_extract_first_regex

# install.packages('venn')
# library(venn)
library(eulerr)

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
# count arrange
# criar tabela comparativa. qual nome tem em qual database e qual status em cada?


# ---
# tratar dados col
# limpa col. remove símbolos como = e ? do taxon_name
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

# juntar todas as espécies
all_species <- c(dados_col$taxon_name,dados_gbif$taxon_name,dados_wd$taxon_name)

all_species <- clean(all_species) # talvez desnecessário

length(all_species) # check soma simples

# manter uma de cada
all_species <- unique(all_species)

length(all_species) # check lista única

# all_species <- sort(all_species) # opcional

# salva backup
all_species_bckp <- all_species
# restaura backup
# all_species <- all_species_bckp

# teste com menos linhas
# all_species <- all_species[1:10000]

# cria dataframe compare
compare <- data.frame(taxon_name = all_species,
                      col=NA,
                      gbif=NA,
                      wd=NA)

# ----------------------
# começa a preencher

library(data.table)

# Convert to data.table if not already
setDT(compare)


# ----------------------

# for gbif

# Convert to data.table if not already
setDT(dados_gbif)

# Create a named vector for fast lookup
# Use unique to avoid duplicates in dados_gbif (keep first occurrence)
status_lookup <- dados_gbif[, .(status = first(status)), by = taxon_name]

# Or if you want to keep all and handle duplicates differently:
# status_lookup <- unique(dados_gbif[, .(taxon_name, status)])

# Perform the lookup using match (fast for large datasets)
compare[, gbif := status_lookup$status[match(taxon_name, status_lookup$taxon_name)]]

# ---------------------

# for col

# Convert to data.table if not already
setDT(dados_col)

# Create lookup for fast matching
col_lookup <- unique(dados_col[, .(taxon_name, status)])

# Perform the lookup using match
compare[, col := col_lookup$status[match(taxon_name, col_lookup$taxon_name)]]

# -----

# wd

# Convert to data.table if not already
setDT(dados_wd)

# Method 1: Using %in% operator (fast and memory efficient)
compare[, wd := ifelse(taxon_name %in% dados_wd$taxon_name, "accepted", NA_character_)]


paste("df compare está preenchido. Número de linhas =",nrow(compare))
# -----

# check
compare[is.na(gbif) & is.na(wd) & is.na(col), ] %>% nrow() == 0


# ==================================

library(dplyr)

# Count all combinations - option 1
resultados <- compare %>%
  count(gbif, col, wd, name = "lines") %>%
  arrange(desc(lines))

# Count all combinations - option 2, include values of 0
resultados <- compare %>%
  count(gbif, col, wd, name = "lines", .drop = FALSE)

# ==============================================================================
# prepare for euler diagram

# accepted in all three
resultados[col=="accepted" & gbif=="accepted" & wd=="accepted"] [,4] -> CGW

# accepted in only two
resultados[is.na(col) & gbif=="accepted" & wd=="accepted"] [,4] -> GW
resultados[col=="accepted" & is.na(gbif) & wd=="accepted"] [,4] -> CW
resultados[col=="accepted" & gbif=="accepted" & is.na(wd)] [,4] -> CG

# accepted in only one
resultados[is.na(col) & is.na(gbif) & wd=="accepted"] [,4] -> W
resultados[col=="accepted" & is.na(gbif) & is.na(wd)] [,4] -> C
resultados[is.na(col) & gbif=="accepted" & is.na(wd)] [,4] -> G

# ==============================================================================

# Create the named numeric vector for eulerr from summary_df
euler_counts <- c(
  "C" = as.numeric(C),
  "G" = as.numeric(G),
  "W" = as.numeric(W),
  
  "C&G" = as.numeric(CG),
  "C&W" = as.numeric(CW),
  "G&W" = as.numeric(GW),
  
  "C&G&W" = as.numeric(CGW)
)

names(euler_counts)

# Use it with eulerr
fit1 <- euler(euler_counts) # areas = number

fit1 <- venn(euler_counts) # pretty venn diagram with 0 if needed

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

# Blank graph for future photoshop
plot(fit1, 
     quantities = F,
     fills = c("skyblue", "lightgreen", "lightcoral"),
     labels = NULL)

# ==============================================================================
# ==============================================================================
# ==============================================================================
# ==============================================================================


# Add text annotations
grid.text("CoL", x = 0.35, y = 0.1, 
          gp = gpar(fontsize = 12, fontface = "bold"))
grid.text("GBIF", x = 0.65, y = 0.1, 
          gp = gpar(fontsize = 12, fontface = "bold"))
grid.text("WD", x = 0.4, y = 0.9, 
          gp = gpar(fontsize = 12, fontface = "bold"))


# ============================================================



# ==============================================================================
# ==============================================================================
# ==============================================================================
# ==============================================================================

# filter datasets, manter apenas espécies aceitas

# para col, remover linhas com algo em equal e question
df_col_acc <- subset(dados_col,(is.na(dados_col$equal)&is.na(dados_col$question)))

# check
sum(!is.na(df_col_acc$question)) + sum(!is.na(df_col_acc$equal)) == 0

# para gbif, manter apenas status accepted
df_gbif_acc <- subset(dados_gbif,dados_gbif$status=="accepted")

# para wd, nenhuma transformação possível ou necessária na versão atual
df_wd_acc <- dados_wd

# ==============================================================================

# DeepSeek

# Extract the unique species lists - real
list1 <- unique(dados_col$taxon_name)
list2 <- unique(dados_gbif$taxon_name)
list3 <- unique(dados_wd$taxon_name)

paste(length(list1),"= CoL.",length(list2),"= GBIF.",length(list3),"= WD")

# Create a named list for the venn diagram
species_lists <- list(
  CoL.1 = list1,
  GBIF.2 = list2,
  WD.3 = list3
)

# ---

# To get the actual counts and species
# Species in all three
in_all <- Reduce(intersect, species_lists)
cat("Species in all three databases:", length(in_all), "\n")

# Species in exactly two databases
# Fix: Create a list with proper names
pairs <- combn(1:3, 2)
in_two <- list()

for(i in 1:ncol(pairs)) {
  pair_intersect <- intersect(species_lists[[pairs[1,i]]], species_lists[[pairs[2,i]]])
  # Remove species that are in the third database
  third <- setdiff(1:3, pairs[,i])
  # Fix: Use paste0 to create names, not trying to reference object 'DB2'
  pair_name <- paste0("DB", pairs[1,i], "&DB", pairs[2,i])
  in_two[[pair_name]] <- setdiff(pair_intersect, species_lists[[third]])
}

cat("\nSpecies in exactly two databases:\n")
for(i in names(in_two)) {
  cat(i, ":", length(in_two[[i]]), "\n")
}

# Species in only one database
in_one <- list(
  DB1_only = setdiff(list1, union(list2, list3)),
  DB2_only = setdiff(list2, union(list1, list3)),
  DB3_only = setdiff(list3, union(list1, list2))
)

cat("\nSpecies in exactly one database:\n")
for(i in names(in_one)) {
  cat(i, ":", length(in_one[[i]]), "\n")
}

# Summary table - Fix: Use the correct list names
summary_df <- data.frame(
  Category = c("A", "B", "C",
               "A&B", "A&C", "B&C",
               "A&B&C"),
  Count = c(length(in_one$DB1_only),
            length(in_one$DB2_only),
            length(in_one$DB3_only),
            length(in_two[["DB1&DB2"]]),  # Fix: Use double brackets with string name
            length(in_two[["DB1&DB3"]]),  # Fix: Use double brackets with string name
            length(in_two[["DB2&DB3"]]),  # Fix: Use double brackets with string name
            length(in_all))
)

print(summary_df)

# --

# Create the named numeric vector for eulerr from summary_df
euler_counts <- c(
  "A" = summary_df$Count[summary_df$Category == "A"],
  "B" = summary_df$Count[summary_df$Category == "B"],
  "C" = summary_df$Count[summary_df$Category == "C"],
  "A&B" = summary_df$Count[summary_df$Category == "A&B"],
  "A&C" = summary_df$Count[summary_df$Category == "A&C"],
  "B&C" = summary_df$Count[summary_df$Category == "B&C"],
  "A&B&C" = summary_df$Count[summary_df$Category == "A&B&C"]
)

# Use it with eulerr
fit1 <- euler(euler_counts)

# ========================================================================
# make graphs

# Automatic simple graph
plot(fit1, 
     quantities = TRUE,  # Show numbers
     fills = c("skyblue", "lightgreen", "lightcoral"),  # Custom colors
     labels = c("CoL", "GBIF", "WD"))  # Custom labels

# ---

# Choose where to put labels
# Create the plot without labels
plot(fit1, 
     quantities = TRUE,
     fills = c("skyblue", "lightgreen", "lightcoral"),
     labels = NULL)

# Add text after the plot using grid graphics
library(grid)

# Add text annotations
grid.text("CoL", x = 0.35, y = 0.1, 
          gp = gpar(fontsize = 12, fontface = "bold"))
grid.text("GBIF", x = 0.65, y = 0.1, 
          gp = gpar(fontsize = 12, fontface = "bold"))
grid.text("WD", x = 0.4, y = 0.9, 
          gp = gpar(fontsize = 12, fontface = "bold"))

# ============================================================

# Extra analysis
colnames(dados_col)
colnames(dados_wd)
colnames(dados_gbif)


paste(nrow(dados_col) - length(unique(dados_col$taxon_name)),"nomes repetidos em CoL")
paste(nrow(dados_wd) - length(unique(dados_wd$taxon_name)),"nomes repetidos em WD")
paste(nrow(dados_gbif) - length(unique(dados_gbif$taxon_name)),"nomes repetidos em GBIF")
paste("Estes nomes repetidos podem ser hemihomônimos ou subespécies ou cultivares etc.")

# Look for hemihomonym
subset(dados_col,dados_col$taxon_name=="Iris orientalis")
subset(dados_gbif,dados_gbif$taxon_name=="Iris orientalis")
subset(dados_wd,dados_wd$taxon_name=="Iris orientalis")
# all three have the hemihomonym

# ==============================================================================
# Find repeated species in each database

library(dplyr)
# library(data.table)  # For even faster option

# Method 1: Optimized dplyr

# for col
repeated_species_col <- dados_col %>%
  add_count(taxon_name) %>%  # Faster than group_by + filter
  filter(n > 1) %>%
  select(-n)  # Remove the count column if you don't need it

# for gbif
repeated_species_gbif <- dados_gbif %>%
  add_count(taxon_name) %>%  # Faster than group_by + filter
  filter(n > 1) %>%
  select(-n)  # Remove the count column if you don't need it

# for wd
repeated_species_wd <- dados_wd %>%
  add_count(taxon_name) %>%  # Faster than group_by + filter
  filter(n > 1) %>%
  select(-n)  # Remove the count column if you don't need it

