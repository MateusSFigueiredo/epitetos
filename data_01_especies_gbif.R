# Arquivo: data_01_especies_gbif.R

# Obter lista de espécies do GBIF - Global Biodiversity Information Facility

# Input: "Taxon.tsv" obtido do site do GBIF

# Output: dataframe df_gbif com lista de todas as espécie válidas
# com coluna para inicial do gênero, e coluna para epiteto.

# colnames output: ("id", "generic_name",
# "generic_initial", "specific_epithet","kingdom")

# Modificado em: 2025-01-02
# Autor: Mateus Silva Figueiredo

# ==============================================================================
# Setup

getwd()
list.files()

library(stringr)
library(dplyr)
library(readr) # para read_tsv

# Download backbone.zip from GBIF Hosted Datasets, which includes Taxon.tsc
# Current file from 2023-08-28 15:19

# ==============================================================================
# carregar dados a partir de Taxon.tsc
dados <- read_tsv("Taxon.tsv") # lines

# Conferir dados
if(F){ # T para executar, F para ignorar
  head(dados) # primeiras linhas
  
  print( # linha arbitrária
    dados[115124,],
    width = 300) # width 300 para imprimir todas as colunas
  
}

# =========================================================
# Subset apenas aceitos
df_accepted <- subset(dados,dados$taxonomicStatus=="accepted")

# Subset apenas espécies
df_species <- subset(df_accepted,df_accepted$taxonRank=="species")

# Subset remove virus
df <- subset(df_species,df_species$kingdom!="Viruses")

# ------------------------
# Define colunas de interesse
colunas <- c("taxonID","genericName","specificEpithet","kingdom")
# subset apenas colunas de interesse
df <- df[,colunas]

# Create a new column with the first letter of 'genericName'
df <- df %>%
  mutate(genericInitial = substr(genericName, 1, 1))

# check old column names
colnames(df)

# rename column by column with underscore and lower case
df<-rename(df, generic_name = genericName)
df<-rename(df, generic_initial = genericInitial)
df<-rename(df, specific_epithet = specificEpithet)
df<-rename(df, id = taxonID)

# reorder columns
df <- df %>% select(id, generic_name, generic_initial, specific_epithet, kingdom)

# df gbif está pronto para ser exportado
# =======================================
# Export df

# Save file with date and time to avoid a bad overwrite

# create save_path with date and time
save_path <- paste0("df_gbif_", format(Sys.time(), "%Y-%m-%d-%H-%M"), ".csv")
# save csv with date and time in its name
write.csv(df,file=save_path,row.names=F)

# =======================================
# =======================================
# =======================================
print("Fim do código")

# ======================================
# Referências
# GBIF Secretariat (2011) -  GBIF Backbone Taxonomy  https://www.gbif.org/dataset/d7dddbf4-2cf0-4f39-9b2a-bb099caae36c
# GBIF Hosted Datasets https://hosted-datasets.gbif.org/datasets/backbone/

# =======================================

# Análises arbitrárias

# Quantos taxonRank tem? Quantos species, quantos genus, quantos family?
table(dados$taxonRank)
# Quantos accepted, quantos synonimous?
table(dados$taxonomicStatus)

# 
dados_accepted <- subset(dados,dados$taxonomicStatus=="accepted")
# 3395442 táxons aceitos

dados_species <- subset(dados_accepted,dados_accepted$taxonRank=="species") 
# 2191845 espécies aceitas

# ===================================

