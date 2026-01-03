# Arquivo: especies_col.R

# Obter lista de espécies do Catalogue of Life

# Usa arquivo dataset-303642.txtree
# Obtido em https://www.catalogueoflife.org/data/download opção TextTree

# Modificado em: 2024_10_05
# Autor: Mateus Silva Figueiredo
# dif: mais conciso
# remove espécies dubias com ?, opcional
# remove subgenero entre parentesis
# exporta arquivo csv

# ==============================================================================
# Setup

getwd()
list.files()

library(stringr)
library(dplyr)

# ==============================================================================

# Read the file
lines <- readLines("dataset-303642.txtree")

# Convert the lines into a data frame
df <- data.frame(name = lines, stringsAsFactors = FALSE)

# Backup
df_bckp <- df
# recuperar backup
df <- df_bckp

# df <- head(df,2000) # menos linhas para testes

# ==============================================================================
# Only rows with '[species]' from the original df
df <- df %>% filter(str_detect(name, regex("\\[species\\]", ignore_case = TRUE)))
# Só quero linhas de espécie # 4173078 obs.

# remover × xis
df$name<-gsub(" ×","",df$name)

# ------------------------------------------------------------------------------
# trim white spaces
df$name<-trimws(df$name)

trimws(df$name[3317123])

# ------------------------------------------------------------------------------
# se segunda palavra for subgenero com ()
# entao manter palavras 1 e 3
# do contrario, manter palavras 1 e 2

# Opção 1: base R
df$taxon_name<-ifelse(grepl("\\(", word(df$name, 2, 2)), # if second word has (
                paste(word(df$name, 1, 1), word(df$name, 3, 3)), # then name is words 1 and 3
                paste(word(df$name,1,2))) # else name is words 1 and 2

# Opção 2: mutate
# # Update the 'name' column based on the condition for the second word
# df <- df %>%
#   mutate(name = ifelse(grepl("\\(", word(name, 2, 2)), 
#                        paste(word(name, 1, 1), word(name, 3, 3)), 
#                        paste(word(name,1,2))))
# # works but is slow

# -------------------------------
# check
df[492,]
df[1522515,] # has subgenera and author with ()
df[3317123,] # has × and =
# -------------------------------

# remover coluna name
df$name<-NULL

# Keep only rows where the first character of 'name' is NOT '='
df <- df %>% filter(substr(taxon_name, 1, 1) != "=")
# Remove linhas de sinônimos. Resulta 2181430 linhas

# ==============================================================================
# Export file

file_name <- paste0("especies_col_",nrow(df),".csv")

write.csv(df,file_name,row.names = F)

# ---
# Para exportar sem espécies dúbias
# # Keep only rows where the first character of 'name' is NOT '?'
if(F){df <- df %>% filter(substr(taxon_name, 1, 1) != "?")}
# Remove espécies dubias. Resulta 2101173 linhas.

# ==============================================================================
# Analisar casos individuais

df["Orchigymnadenia" %in% df$name]
