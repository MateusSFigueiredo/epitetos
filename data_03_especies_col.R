# Arquivo: especies_col.R

# Obter lista de espécies do Catalogue of Life

# Input: arquivo dataset-313531.txtree

# Output: arquivo com colunas ("id","taxon_name",
# "generic_initial", "specific_epithet")

# Modificado em: 2026-01-03
# Autor: Mateus Silva Figueiredo
# dif: output padronizado
# planos futuros: incluir coluna kingdom

# ==============================================================================
# Setup

getwd()
list.files()

library(stringr)
library(dplyr)

# Usa arquivo dataset-313531.txtree Version 2025-12-20 XR
# Obtido em https://www.catalogueoflife.org/data/download opção TextTree
# Acessado e download em 2026-01-03

# ==============================================================================

# Read the file # 8 segundos
lines <- readLines("dataset-313531.txtree")

# Convert the lines into a data frame # 0.003 s
dados <- data.frame(name = lines, stringsAsFactors = FALSE)
# manter dados inalterado. Trabalhar com df

# cria coluna id para organização
df <- cbind(id=1:nrow(dados),
            dados=dados)

if(F){df <- head(df,200000)} # menos linhas para testes

rm(lines) # limpeza
# ==============================================================================

# Only rows with '[species]' from the original df # 4,9 s
df <- df %>% filter(str_detect(name, regex("\\[species\\]", ignore_case = TRUE)))
# Só quero linhas de espécie

paste("Há",nrow(df),"linhas com [species], incluindo sinônimos")

# remover × xis que normalmente fica após primeira palavra # ~15 segundos
df$name<-gsub(" ×","",df$name)

# ------------------------------------------------------------------------------
# trim white spaces
df$name<-trimws(df$name)

(df$name[3317123]) # confere linha arbitrária

# ------------------------------------------------------------------------------
# se segunda palavra for subgenero com ()
# entao manter palavras 1 e 3
# do contrario, manter palavras 1 e 2

# cria df_backp com backup
# usa df para remover subgênero
df_bckp <- df

# Usando o base R # 8,6 minutos
df$taxon_name<-ifelse(grepl("\\(", word(df$name, 2, 2)), # if second word has (
                paste(word(df$name, 1, 1), word(df$name, 3, 3)), # then name is words 1 and 3
                paste(word(df$name,1,2))) # else name is words 1 and 2
# -------------------------------
# check
if(F){ # F to ignore, T to run
df[492,] # normal species
df[1522515,] # has =
df[4478746,] # has = and - =Orobanche cirsii-oleracei Casp. [species]]
df[2605582,] # has ? ?Camptonotus amplus Marsh, 1879 [species]
}
# -------------------------------

# remover coluna name
df$name<-NULL

# remover † cruz do início do taxon_name 
df$taxon_name<-gsub("†","",df$taxon_name)
# Espécies extintas com inicial † podem ficar

n_all <- nrow(df) # numero de todas as especies em dados

# Keep only rows where the first character of 'name' is NOT '='
df <- df %>% filter(substr(taxon_name, 1, 1) != "=")
# Remove linhas de sinônimos

n_no_syn <- nrow(df) # numero de especies removendo os sinonimos

# --------------
# Para lista sem espécies dúbias
# # Keep only rows where the first character of 'name' is NOT '?'
if(T){df <- df %>% filter(substr(taxon_name, 1, 1) != "?")}
# Remove espécies dubias

n_no_dub <- nrow(df) # numero de especies removendo sinonimos e dubias

paste("Havia",n_all,"linha de espécie.",
      "Após remover sinônimos (=), sobraram",n_no_syn,"espécies.",
      "Após remover dúbias (?), sobraram",n_no_dub,"espécies.")

# nenhuma espécie com ×, nenhuma com =, nenhuma começando com ?

# ==============================================================================
# Tirar linhas com virus

# Remove rows with 'virus' from the original df
df <- df %>% filter(!str_detect(taxon_name, regex("virus", ignore_case = TRUE)))

print(paste("número de linhas sem virus =", (nrow(df))))

# =======================
# Criar coluna generic_initial
df <- df %>%
  mutate(generic_initial = substr(taxon_name, 1, 1))

# check
table(df$generic_initial)

# -------------------------
# Create a new column with the last word of the 'taxon_name' column # 32 segundos
df <- df %>%
  mutate(specific_epithet = sapply(strsplit(taxon_name, " "), tail, 1))

# Transformar taxon_name em character # parecia lento? medi 0.004816055 secs
df <- df %>%
  mutate(specific_epithet = as.character(specific_epithet))
# ==============================================================================
colnames(df)

# =======================================
# Export df

# Save file with date and time to avoid a bad overwrite

# create save_path with date and time
save_path <- paste0("df_col_", format(Sys.time(), "%Y-%m-%d-%H-%M"), ".csv")
# save csv with date and time in its name
write.csv(df,file=save_path,row.names=F)


# ==============================================================================
# Analisar casos individuais

df["Orchigymnadenia" %in% df$name]
