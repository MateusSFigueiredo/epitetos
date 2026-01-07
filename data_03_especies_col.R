# Arquivo: data_03_especies_col.R

# Obter lista de espécies do Catalogue of Life

# Input: arquivo dataset-313531.txtree

# Output: arquivo com colunas ("id","taxon_name",
# "generic_initial", "specific_epithet", "kingdom")
# Colunas no output: df <- df %>% select(id, taxon_name, generic_initial, specific_epithet, kingdom)

# Modificado em: 2026-01-06
# Autor: Mateus Silva Figueiredo
# dif: padroniza colunas em ordem

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

if(F){df <- head(dados,25000)} # menos linhas para testes

rm(lines) # limpeza
# ==============================================================================
# trim white spaces # ~8 segundos
df$name<-trimws(df$name) # corta os espaços em branco no começo das linhas

(df$name[7123]) # confere linha arbitrária

# ==============================================================================
# incluir kingdom
# cria coluna kingdom
df$kingdom <- NA

# quais linhas contém [kingdom]?
reino_linha <- which(grepl(as.character('[kingdom]'), df$name,fixed=T))

# check taxon name of first kingdom
df$name[reino_linha[1]]

# check and get first word
word(df$name[reino_linha[1]], 1)

# prepare for loop para todos os reinos menos o último
i <- 1

for (i in 1:(length(reino_linha)-1)){
# print reino sendo preenchido
    print(reino_nome <- word(df$name[reino_linha[i]], 1)) # para saber até qual reino preencheu
  # preenche reino
df$kingdom[(reino_linha[i]):(reino_linha[i+1]-1)] <- word(df$name[reino_linha[i]], 1)
}; print("fim do loop")


# preencher último reino, por preciocismo
i <- length(reino_linha) # atualiza último valor de i
# preenche da linha do ultimo reino até a ultima linha de df
df$kingdom[(reino_linha[i]):nrow(df)] <- word(df$name[reino_linha[i]], 1)

# --------------------------------
# corrige linhas domain, apenas por preciosismo
# quais linhas contém [domain]?
dominio_linha <- which(grepl(as.character('[domain]'), df$name,fixed=T))
# colocar kingdom = NA para elas
df$kingdom[dominio_linha] <- NA

# check
if(F){
df[dominio_linha[1]:(dominio_linha[1]+5),]
df[dominio_linha[2]:(dominio_linha[2]+5),]
df[dominio_linha[3]:(dominio_linha[3]+5),]

# check valores de kingdom
unique(df$kingdom)
}

# ==============================================================================
# remover vírus
# localizar linhas de inicio e fim de virus
virus_start <- which(grepl(as.character('Viruses [unranked]'), df$name,fixed=T))
virus_end <- 7859252 # obtained from looking at tail of df
# 7859252 Gammatectivirus GC1 [species]  is just before 7859253 ?incertae sedis [unranked]

# cria dataframe para Viruses [unranked]
df_virus <- df[c(virus_start:virus_end),] 

# cria df removendo linhas em Viruses [unranked] interval
df_alive <- df[-c(virus_start:virus_end),] 

# Remove rows with 'virus|viroid|viriform' from the original df which might not me in the Viruses [unranked] interval
df_alive <- df_alive %>% filter(!str_detect(name, regex("virus|viroid|viriform", ignore_case = TRUE)))

# -------------

# atualiza df, agora sem virus
df <- df_alive
# ==============================================================================

# Only rows with '[species]' from the original df # 4,9 s
df <- df %>% filter(str_detect(name, regex("\\[species\\]", ignore_case = TRUE)))
# Só quero linhas de espécie

paste("Há",nrow(df),"linhas com [species], incluindo sinônimos, após remover vírus")

# remover × xis que normalmente fica após primeira palavra # ~15 segundos
df$name<-gsub("× ","",df$name) # "× " com espaço após o xis para remover do começo tb

# check
if(T){
df[c(3698615,3784538,3795399),]  
}

# 3698615 6016626 × Pleuriditrichum marylandicum A.L.
# 3784538 6168189 ?× Gasteraloe prorumpens (A.Berger) G.D.Rowley
# 3795399 6183888 =× Heropaludorchis genevensis (Chenevard)

# ------------------------------------------------------------------------------
# Remover subgêneros

# cria df_backp com backup
# usa df para remover subgênero
df_bckp <- df

# check select lines
if(T){
  df[c(1,63524,64532,83349),]
}

# se segunda palavra for subgenero com ()
# entao manter palavras 1 e 3
# do contrario, manter palavras 1 e 2

# ---
# Sugestão do DeepSeek - Single strsplit call - much faster
split_names <- strsplit(df$name, " ", fixed = TRUE)

df$taxon_name <- sapply(split_names, function(words) {
  if(length(words) >= 2 && grepl("\\(", words[2])) {
    # Second word has parentheses
    if(length(words) >= 3) paste(words[1], words[3]) else words[1]
  } else {
    # Normal case
    if(length(words) >= 2) paste(words[1], words[2]) else words[1]
  }
}) # Time of 1.28 mins

# -------------------------------

# check select lines
if(T){
  df[c(1,63524,64532,83349),c(2,4)]
} # taxon_name should have only two words

# check again
if(F){ # F to ignore, T to run
df[492,] # normal species
df[1522515,] # has =
df[4478746,] # has = and - =Orobanche cirsii-oleracei Casp. [species]]
df[2605582,] # has ? ?Camptonotus amplus Marsh, 1879 [species]
}

# remover split_names, já usado
rm(split_names)
# -------------------------------

# remover coluna name, desnecessária e longa
df$name<-NULL

# remover † cruz do início do taxon_name, pois quero manter extintos
df$taxon_name<-gsub("†","",df$taxon_name)
# Espécies extintas com inicial † podem ficar, mas sem a cruz

n_all <- nrow(df) # numero de todas as especies em dados

# Keep only rows where the first character of 'name' is NOT '='
df <- df %>% filter(substr(taxon_name, 1, 1) != "=")
# Remove linhas de sinônimos

n_no_syn <- nrow(df) # numero de especies removendo os sinonimos

# Análise em texto.
paste("Havia",n_all,"linhas com [species].",
      "Após remoção dos sinônimos com = sobraram",n_no_syn,
      "Foram removidos",n_all-n_no_syn,"sinônimos.")

# --------------
# Para lista sem espécies dúbias
# # Keep only rows where the first character of 'name' is NOT '?'
if(T){df <- df %>% filter(substr(taxon_name, 1, 1) != "?")}
# Remove espécies dubias

n_no_dub <- nrow(df) # numero de especies removendo sinonimos e dubias

# Análise em texto
paste("Havia",n_all,"linha de espécie.",
      "Após remover sinônimos (=), sobraram",n_no_syn,"espécies.",
      "Após remover dúbias (?), sobraram",n_no_dub,"espécies.")

# sobra nenhuma espécie com ×, nenhuma com =, nenhuma começando com ?

# ==============================================================================
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

# rename column by column # no need

# reorder columns
df <- df %>% select(id, taxon_name, generic_initial, specific_epithet, kingdom)

# =======================================
# Export df

# Save file with date and time to avoid a bad overwrite

# create save_path with date and time
save_path <- paste0("df_col_", format(Sys.time(), "%Y-%m-%d-%H-%M"), ".csv")
# save csv with date and time in its name
write.csv(df,file=save_path,row.names=F)

# ==============================================================================
print("Fim do código")
# ==============================================================================
# Analisar casos individuais

df["Orchigymnadenia" %in% df$name]

# Analisar virus
df_virus |> head(); paste("n de linhas de df_virus ==",nrow(df_virus))

# Only rows with '[species]' from the original df # 4,9 s
df_virus <- df_virus %>% filter(str_detect(name, regex("\\[species\\]", ignore_case = TRUE)))
# Só quero linhas de espécie

# Quantas linhas tem escrito virus, viriform ou viroid?
sum(grepl("virus", df_virus$name, ignore.case = TRUE)) |> paste("linhas escrito virus")
sum(grepl("viriform", df_virus$name, ignore.case = TRUE)) |> paste("linhas escrito viriform")
sum(grepl("viroid", df_virus$name, ignore.case = TRUE)) |> paste("linhas escrito viroid")
# analise em texto:
(nrow(df_virus) - sum(grepl("virus|viriform|viroid", df_virus$name, ignore.case = TRUE))) |> paste("sem estar escrito virus, viroid ou viriform")

sum(grepl("satellite", df_virus$name, ignore.case = TRUE)) |> paste("linhas escrito satellite em df_virus")
sum(grepl("satellite", df_alive$name, ignore.case = TRUE)) |> paste("linhas escrito satellite em df_alive")

sum(grepl("virus|viriform|viroid", df_alive$name, ignore.case = TRUE)) |> paste("linhas escrito satellite em df_alive")


## check conferir se há virus|viroid|viriform|satellite|phage nas espécies
df_alive %>% filter(str_detect(name, regex("satellite|phage", ignore_case = TRUE)))
# há muitos animais satellite e phage

# df com 
df_virus_oculto <- df_virus %>%  filter(!str_detect(name, regex("virus|viriform|viroid", ignore_case = TRUE)))

# ----

# get the last lines
dados_fim <- tail(dados, nrow(dados) - 7837434) # - 7837434 inclui Viruses e incertae sedis

# virus actually ends at 7859252 Gammatectivirus GC1 [species]

# -----------
