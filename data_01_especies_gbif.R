# Arquivo: data_01_especies_gbif.R

# Obter lista de espécies do GBIF - Global Biodiversity Information Facility

# Input: "Taxon.tsv" obtido do site do GBIF

# Output: dataframe df_gbif com lista de todas as espécie válidas
# com coluna para inicial do gênero, e coluna para epiteto.

# Colunas no output:
# df <- df %>% select(id, taxon_name, generic_initial,
# specific_epithet, kingdom)

# Modificado em: 2026-02-27
# Autor: Mateus Silva Figueiredo
# dif: documentation

# ==============================================================================
# Setup

library(stringr)
library(dplyr)
library(readr) # para read_tsv

setwd("C:/Users/Mateus/Desktop/R/epitetos")
getwd()
list.files()

# Download backbone.zip from GBIF Hosted Datasets, which includes Taxon.tsv
# Current file from 2023-08-28 15:19

# ==============================================================================
# carregar dados a partir de Taxon.tsv
dados <- read_tsv("Taxon.tsv") # lines # 1.2 minutes

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

# Subset remove NA
df_no_na <- df_species[!is.na(df_species$specificEpithet), ]

# Subset remove virus
df <- subset(df_no_na,df_no_na$kingdom!="Viruses")

# Remove rows where 'virus|viroid|viriform' is present in 'canonicalName' and save it to df_alive
df_alive <- df %>% filter(!str_detect(canonicalName, regex("virus|viroid|viriform", ignore_case = TRUE)))

# save to df
df <- df_alive

# Remove desnecessários
rm(df_accepted,df_species,df_no_na)

# Análise de texto
nrow(df) |> paste("linhas de espécie aceita sem vírus sem NA")
# ------------------------
# Define colunas de interesse
colunas <- c("taxonID","canonicalName","genericName","specificEpithet","kingdom","namePublishedIn")
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
df<-rename(df, taxon_name = canonicalName)
df<-rename(df, name_published_in = namePublishedIn) # para análises de ano

# reorder columns
df <- df %>% select(id, taxon_name, generic_initial, specific_epithet, kingdom, name_published_in)

# =======================================
# remover epitetos spec
nrow(df) |> paste("espécies em df antes da remoção")

rm(df_spec,df_clean)

df_spec <- subset(df,df$specific_epithet=="spec")

# df_clean <- subset(df,df$specific_epithet!="spec") # subset também remove NA

df_clean <- df[df$specific_epithet != "spec", ]

nrow(df_clean) |> paste("espécies em df_clean após remover spec")

(nrow(df)-nrow(df_clean)) |> paste("espécies spec removidas")

nrow(df) |> paste("linhas de espécies em GBIF no final, após remover spec")

df <- df_clean

paste("As colunas são",list(colnames(df)))

# =============================================

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

# subset aceitos
dados_accepted <- subset(dados,dados$taxonomicStatus=="accepted")
# 3395442 táxons aceitos

dados_species <- subset(dados_accepted,dados_accepted$taxonRank=="species") 
# 2191845 espécies aceitas

dados_species$kingdom |> table()
subset(dados_species,dados_species$kingdom!="Viruses") |> nrow()
# 2181172 espécies não virus

dados_species$kingdom |> table()
df$kingdom |> table()

colnames(df)

archaea <- subset(dados_species,dados_species$kingdom=="Archaea")


subset(dados,dados$canonicalName == "Zea mays",) |> View()
subset(dados,dados$canonicalName %in% c("Zea mays",
                                        "Homo sapiens",
                                        "Musa paradisiaca",
                                        "Penelope obscura",
                                        "Caenorhabditis elegans"),) |> View()

subset(dados,dados$taxonID %in% c("7443716"),) |> View()

# ===================================

