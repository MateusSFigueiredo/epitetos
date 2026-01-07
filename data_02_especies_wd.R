# Arquivo: especies_wd_v2.R

# Trabalhar com lista de espécies do wikidata

# Input: arquivo wikidata_IX04x2.csv 

# Output: arquivo com colunas ("id", "generic_name",
# "generic_initial", "specific_epithet")

# Colunas no output: df <- df %>% select(id, taxon_name, generic_initial, specific_epithet)

# Modificado em: 2026-01-06
# Autor: Mateus Silva Figueiredo
# dif: padroniza colunas em ordem

# ==============================================================================
# Setup
getwd()
list.files()

library(dplyr)

# Arquivo obtido em https://qlever.dev/wikidata/udSVmA#csv   
# Download em 31 dezembro 2025 = 3,268,594 lines found 

# ==============================================================================
# Carregar dados a partir de wikidata_udSVmA.csv
dados <- read.csv2("wikidata_udSVmA.csv",sep=",") # lines # 45 secs
df <- dados # manter dados imutável, manipular apenas df

# Conferir dados
if(T){ # T para executar, F para ignorar
  
  print(head(dados)) # primeiras linhas
  print(dados[1234,]) # linha arbitrária
  
  colnames(dados)
  
}

# =======================
# primeiro, remover todos que não são taxon ou fossil taxon
# criando assim df_taxon

# View(table(df$instance_of_label)) # conferir antes

antes <- nrow(df)

df_taxon <- subset(df,df$instance_of_label %in% c("taxon","fossil taxon"))

depois <- nrow(df_taxon)

# View(table(df_taxon$instance_of_label)) # conferir depois

# Print texto de análise
paste("Antes de manter apenas taxon e fossil taxon, df tinha",
      antes,"linhas. Depois, tem",depois,"linhas.",
      "Foram removidas",antes-depois,"linhas fora do escopo de taxon."); rm(antes,depois)

# =======================
# fazer df_unique apenas com itens únicos, removendo duplicados
# aplicando unique para o df todo

# check
# df_taxon[df_taxon$item=="http://www.wikidata.org/entity/Q122111980",c(1,3,4)]

antes <- nrow(df_taxon)

df_unique <- df_taxon %>% distinct(item, .keep_all=TRUE)

# check
# df_unique[df_unique$item=="http://www.wikidata.org/entity/Q122111980",c(1,3,4)]

depois <- nrow(df_unique)

paste("Antes de remover dados duplicados, df tinha",
      antes,"linhas.",
      "Depois da remoção, df com itens únicos tem",depois,"linhas.",
      "Foram eliminados",antes-depois,"itens duplicados.")

# atualiza df
df <- df_unique

# =======================
# remover linhas em que todas as colunas de id estejam em branco

# Define the ID columns
id_cols <- colnames(df)[c(5:66)]

# Create df removing ID lines vazias # lento
df_with_ids <- df[!apply(df[id_cols], 1, function(x) {
  all(is.na(x) | grepl("^\\s*$", x))
}), ]

paste("Foram mantidos",nrow(df_with_ids),"linhas com ao menos um id preenchido.",
      "Foram removidas",nrow(df_unique)-nrow(df_with_ids),"linhas sem id.")

# ===============================
# Define colunas de interesse
colunas <- c("item","taxon_name")
# subset apenas colunas de interesse, cria novo df atualizado
df <- df_with_ids[,colunas]

# =============================
# Trecho importado de epitetos_especificos.R

# ==============================================================================
# Step 1: Tirar linhas com virus

# Filter rows where 'virus|viroid|viriform' is present in 'taxon_name' and save it to 'virus'
virus_etc <- df %>% filter(str_detect(taxon_name, regex("virus|viroid|viriform", ignore_case = TRUE)))

# Remove rows with 'virus|viroid|viriform' from the original df
df <- df %>% filter(!str_detect(taxon_name, regex("virus|viroid|viriform", ignore_case = TRUE)))

print(paste("número de linhas sem virus|viroid|viriform =", (nrow(df))))

# ------------------------------------------------------------------------------

# Step 2: Remove rows where the 'taxon_name' column has more than two words

# nao_binomial = apenas linhas em que 'taxon_name' não tenha duas palavras
nao_binomial <- df %>% filter(str_count(taxon_name, "\\S+") != 2)

# manter apenas linhas em que taxon_name tenha duas palavras
df           <- df %>% filter(str_count(taxon_name, "\\S+") == 2)

print(paste("número de linhas com binomial correto =", (nrow(df))))

# ------------------------------------------------------------------------------
# Step 2.5: Remove rows where the taxon_name has numbers

# Filter rows where a number is present in 'taxon_name' and save it to number
number <- df[grepl("\\d", df$taxon_name), ] # 55 linhas, apenas preciocismo

# Remove rows with a number from the original df
df <- df[!grepl("\\d", df$taxon_name), ]

print(paste("número de linhas sem numero =", (nrow(df))))

# cria backup
df_no_numbers <- df

# ------------------------------------------------------------------------------
# Time: less than one minute. 46 secs
# Step 3: Create a new column with the last word of the 'taxon_name' column
df <- df %>%
  mutate(specific_epithet = sapply(strsplit(taxon_name, " "), tail, 1))

# Transformar taxon_name em character # run 1 = 1 min. run 2 = 0.002 secs.
df <- df %>%
  mutate(specific_epithet = as.character(specific_epithet))

# ------------------------------------------------------------------------------
# Lidar com iniciais fora do padrão
# Wikidata tem algumas espécies com x
# Wikidata tem espécies com inicial minúscula

# remover × xis
df$taxon_name<-gsub("× ","",df$taxon_name)

# trim white spaces
df$taxon_name<-trimws(df$taxon_name)

# Optional. Sort the dataframe alphabetically by 'specific_epithet'
# df <- df[order(df$specific_epithet), ]

# Step 4: Create a new column with the first letter of 'taxon_name'
df <- df %>%
  mutate(generic_initial = substr(taxon_name, 1, 1))

# check
# subset(df,df$generic_initial %in% c("a","c","l","m","p"))

# Passar letra inicial minúscula para maiúscula
df$generic_initial <- toupper(df$generic_initial)

# check
# table(df$generic_initial)

# ------------------------------------------------------------------------------
# Eliminar linhas com generic_initial fora do alfabeto latino
# Wikidata tem algumas espécies com x + e «
# Transformar minúsculas em maiúsculas

if(F){ # para inspecionar problema
  print(table(df$generic_initial)) # ver tabela
  non_capital_rows <- df %>% filter(!grepl("^[A-Z]", generic_initial)) 
  print(non_capital_rows)
}

# Filtrar linhas que não começam com letras do alfabeto e salvar
non_alphabetic_rows <- df %>%  filter(!grepl("^[A-Za-z]$", generic_initial))

# Atualizar df e manter apenas linhas que começam com letras do alfabeto
df <- df %>%  filter(grepl("^[A-Za-z]$", generic_initial))

# Passar letra inicial minúscula para maiúscula
df$generic_initial <- toupper(df$generic_initial)

# check
table(df$generic_initial)

# ===========================================================================
# Já há apenas colunas de interesse

# rename column
df<-rename(df, id = item)

# reorder columns
df <- df %>% select(id, taxon_name, generic_initial, specific_epithet)

# nrow(df)

# =======================================
# Export df

# Save file with date and time to avoid a bad overwrite

# create save_path with date and time
save_path <- paste0("df_wd_", format(Sys.time(), "%Y-%m-%d-%H-%M"), ".csv")
# save csv with date and time in its name
write.csv(df,file=save_path,row.names=F)

# ======================================
print("Fim do código")

rm(nao_binomial,non_alphabetic_rows,number,virus)
rm(df_no_numbers,df_taxon,df_unique,df_with_ids)
# ===========================================================================
# Análises variadas arbitrárias

# ver instance of labels mais comuns
View(table(df$instance_of_label))

# =======================
# quantas linhas tem todas as colunas id vazias?

# Define the ID columns
id_cols <- colnames(df)[c(5:66)]

# Create df of these empty lines # wait
empty_df <- df[
  apply(df[id_cols], 1, function(x) {
    all(is.na(x) | grepl("^\\s*$", x))
  }), 
]

# Count rows where ALL ID columns are empty/NA
empty_rows <- nrow(empty_df)

paste("Das",nrow(df),"linhas, apenas",empty_rows,"não tem nenhum id dentre", 
      "as vinte bases de dados analisadas. Ou seja,",round(empty_rows*100/nrow(df),2),"%.",
      "Sobram",nrow(df)-empty_rows,"linhas preenchidas.")