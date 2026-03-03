# ==============================================================================
# Arquivo: data_04_gbif_year.R

# Em que ano os nomes de espécies foram publicados?
#
# Input: Taxon.tsv
# Output: produz df com probable_year de cada linha
#
# Modificado em: 2026-03-02
# Autor: Mateus Silva Figueiredo

# difs:
# muda input de df_gbif para Taxon.csv original
# importa ano de scientific_name_authorship
# depois, importa ano de name_published_in

# ==============================================================================
# Load necessary libraries
library(dplyr)
#library(tidyr)
library(stringr) # for str_detect
library(readr) # para read_tsv
library(stringi) # para stri_extract_first_regex

setwd("C:/Users/Mateus/Desktop/R/epitetos")

list.files()

# ==============================================================================
# carregar dados a partir de Taxon.tsv
t1 <- Sys.time();dados <- read_tsv("Taxon.tsv");Sys.time() - t1 # lines # 1.2 minutes
data.class(dados) #"spec_tbl_df"

# ==============================================================================
# Criar df. Manter dados originais imutáveis
df <- dados
data.class(df)

# ---------------------------------

# Filtrar df

# Subset apenas espécies
df_species <- subset(df,df$taxonRank=="species") # 4.106 k

# Subset remove NA em specificEpithet
df_no_na <- df_species[!is.na(df_species$specificEpithet), ] # 4.068k

# Subset remove virus kingdom
df_no_virus <- subset(df_no_na,df_no_na$kingdom!="Viruses") # 

# Remove rows where 'virus|viroid|viriform' is present in 'canonicalName' and save it to df_alive
df_alive <- df_no_virus %>% filter(!str_detect(canonicalName, regex("virus|viroid|viriform", ignore_case = TRUE)))

# save to df
df <- df_alive # manter df_alive como backup

# Remove desnecessários
rm(df_species,df_no_na,df_no_virus)
# rm(df_alive) # não remover durante testes

# Análise de texto
nrow(df) |> paste("linhas de espécie sem vírus sem NA")
# ------------------------
# Agora tenho df apenas com espécies vivas
colnames(df) # conferir todas as colunas, em camelCase original

# -----------------------------
# rename column by column with underscore and lower case
# apenas colunas que serão mantidas
df<-rename(df, generic_name = genericName)
# df<-rename(df, generic_initial = genericInitial)
df<-rename(df, specific_epithet = specificEpithet)
df<-rename(df, id = taxonID)
df<-rename(df, taxon_name = canonicalName)
df<-rename(df, taxonomic_status = taxonomicStatus)
df<-rename(df, name_published_in = namePublishedIn) # para análises de ano
df<-rename(df, scientific_name_authorship = scientificNameAuthorship) # para análises de ano
colnames(df) #confere

# --------------------------

# Define colunas de interesse
colunas <- c("id","taxon_name","generic_name","specific_epithet","kingdom","name_published_in","scientific_name_authorship","taxonomic_status")
# subset apenas colunas de interesse
df <- df[,colunas]
colnames(df) #confere

# ==============================================================================
# Começa manipulação mais complexa dos dados
# Extrair último ano de scientific_name_authorship

# -----------
# Escolher linhas para df_min para teste
# df_min <- df[(3024728-1):(3024728+100),] # poucas linhas incluindo ex. com dois anos ex. (Maupas, 1899) Dougherty, 1953

# Correr mutate para criar coluna com ano
t1 <- Sys.time();df <- df %>%
  mutate(
# First extract full matches with optional letter
# Get all matches at once (vectorized)
author_all_matches = stri_extract_all_regex(scientific_name_authorship, "(?<!:)\\b\\d{4}(?=[a-zA-Z]?\\b)"),

# Create all_numbers_str (only for rows without probable_year)
author_all_numbers_str = ifelse(
  !is.na(id), #probable_year
  sapply(author_all_matches, function(x) {
    if (!is.na(x[1]) && length(x) > 0) paste(x, collapse = ", ") else NA_character_
  }),
  NA_character_
),

# Extract first valid year from matches
author_year_from_all = ifelse(
  !is.na(id), #probable_year
  sapply(author_all_matches, function(x) {
    if (!is.na(x[1]) && length(x) > 0) {
      nums <- as.numeric(x)
      valid_years <- nums[nums >= 1735 & nums <= 2026 & !is.na(nums)]
      if (length(valid_years) > 0) return(tail(valid_years,1)) # 1 para último valor (rabo)
    }
    return(NA_real_)
  }),
  NA_real_
)); Sys.time() - t1 #1.91 min
# fim do mutate
# talvez incluir contagem de tempo aqui

# escolher colunas de interesse para manter
# colunas2 <- c("id","taxon_name","scientific_name_authorship","author_all_matches","author_all_numbers_str","author_year_from_all")

# manter colunas2 de interesse
# df_min <- df_min %>% select(all_of(colunas2))

# conferir um exemplo se deu ano
# df[["author_year_from_all"]][2] # sintaxe mais eficiente para 4kk linhas
if (df[["author_year_from_all"]][2] == 1920) {"Tudo certo: 2a linha está 1920"}
# df$

# ==============================================================================
# Próximo passo: rodar extração de name_published_in
# apenas para linhas sem author_year
# Para linhas sem author_year_from_all
# rodar extração de name_published_in

# ---

Sys.time() -> t1; df <- df %>%
  mutate(
    # 1. Extract parentheses year (vectorized)
    # Gets ( + four digits + optional one letter + )
    paren_match = stri_extract_first_regex(name_published_in, "\\(\\d{4}[a-zA-Z]?[^)]*\\)"),
    # Gets only the four digits
    paren_year = as.numeric(stri_extract_first_regex(paren_match, "\\d+")),
    
    # Valid parentheses year
    # Se paren_year preenchido & parent year ano válido,
    # então salva paren_year para probable_year
    probable_paren_year = ifelse(
      !is.na(paren_year) & paren_year >= 1735 & paren_year <= 2026,
      paren_year,
      NA_real_
    ),
    
    # 2. For rows without parentheses year, extract numbers without colon before
    # First extract full matches with optional letter
    # Get all matches at once (vectorized)
    all_matches = stri_extract_all_regex(name_published_in, "(?<!:)\\b\\d{4}(?=[a-zA-Z]?\\b)"),
    
    # Create all_numbers_str (only for rows without probable_paren_year)
    all_numbers_str = ifelse(
      is.na(probable_paren_year),
      sapply(all_matches, function(x) {
        if (!is.na(x[1]) && length(x) > 0) paste(x, collapse = ", ") else NA_character_
      }),
      NA_character_
    ),
    
    # Extract first valid year from matches
    year_from_all = ifelse(
      is.na(probable_paren_year),
      sapply(all_matches, function(x) {
        if (!is.na(x[1]) && length(x) > 0) {
          nums <- as.numeric(x)
          valid_years <- nums[nums >= 1735 & nums <= 2026 & !is.na(nums)]
          if (length(valid_years) > 0) return(valid_years[1])
        }
        return(NA_real_)
      }),
      NA_real_
    ),
    
    # Final probable_year
    probable_year_from_published = coalesce(probable_paren_year, year_from_all)
  ) %>%
  select(-paren_match, -paren_year, -all_matches, -year_from_all); Sys.time() - t1 # 1.65293 mins

# id com author_year_from_all != probable_year_from_published 2130686 

# ==============================================================================
# juntar author_year_from_all e probable_year_from_published

df %>% data.class

colnames(df)

# remover colunas desnecessárias, intermediárias do processo
df <- df %>% select(-author_all_matches, -author_all_numbers_str,
              -probable_paren_year, -all_numbers_str)

# ---------------------------------------------------
# colapsa duas colunas com ano possível

library(data.table)
setDT(df) # transforma df em data.table
# cria probable_year copiando de author_year_from_all
df[, probable_year := author_year_from_all]
# preenche probable_year nas linhas NA pegando de probable_year_from_published
df[is.na(probable_year) | probable_year == "", 
   probable_year := probable_year_from_published]

paste("df tem",nrow(df),"linhas e",ncol(df),"colunas")
# =======================================
# Export df

# Save file with date and time to avoid a bad overwrite
if(F){ # F to not save
  # create save_path with date and time
  save_path <- paste0("df_year_", format(Sys.time(), "%Y-%m-%d-%H-%M"), ".csv")
  # save csv with date and time in its name
  write.csv(df,file=save_path,row.names=F)
}
# =======================================
print("Fim do código")

# ==============================================================================
# ==============================================================================
# ==============================================================================
# Diversas análises caóticas abaixo
# ==============================================================================

# análises de texto com mensagens de erro em print

if(is.na(df[df$id==8241022,'probable_year'])){print('Tudo certo: 8241022 está NA')}else{print('ERRO')} # 

if(df[df$id==1265709,'probable_year']==1924){print('Tudo certo: 1265709 está 1924')}else{print('ERRO')}

if(df[df$id==9109762,'probable_year']==2016){print('Tudo certo: 9109762 está 2016')}else{print('ERRO EM 9109762')}

if(df[df$id==4727186,'probable_year']==1868){print('Tudo certo: 4727186 está 2016')}else{print('ERRO EM 4727186')} # 2016a deve ser lido como 2016

if(df[df$id==2117306,'probable_year']==1914){print('Tudo certo: 2117306 está 1914')}else('ERRO EM 2117306') # 1914: deve ser lido como 1914


# ================
# Analysis
# Discordância entre author_year_from_all e probable_year_from_published
subset(df,df$id %in% c("4638239","2130686"))

# ==============================================================================
# ==============================================================================
# ==============================================================================
# Explorar dados gerais
library(data.table) # for setDT
dadosDT <- dados
setDT(dadosDT)
data.class(dadosDT)

dados[123400:123410,] # animais
dados[423400:423410,] # plantas e animais

# colunas
colnames(dadosDT)
summary(dadosDT)

# Conferir taxonomicStatus
count_taxonomic <- dadosDT[, .N, by = taxonomicStatus]
# accepted 3395k, doubtful 253k
# synonym 2374k, homotypic synonym 152k, heterotypic synonym 121k, proparte synonym 14k

# Conferir taxonRank
count_rank <- dadosDT[, .N, by = taxonRank]
# species 4106332

# taxonRemarks
count_remarks <- dadosDT[, .N, by = taxonRemarks]
count_remarks <- count_remarks[order(-count_remarks$N), ] #order
# very specific for each taxon

# nameAccordingTo
count_according <- dadosDT[, .N, by = nameAccordingTo]
count_according <- count_according[order(-count_according$N), ] #order
# everything is NA

# ----------------------------

dados[1,1]
dados[1,nrow(dados)]

dados$taxonID |> na.omit() |> min()
dados$taxonID |> na.omit() |> max()
dados[dados$taxonID == 0,]
dados[dados$taxonID == 1,]


# ----------------------------
# Explorar dados específicos

# ------
# procurando espécies famosas
# escolher por numero no gbif
df[df$id %in% c("113523472"),] |> View()

# escolher por nome da espécie
especies_interesse <- c("Zea mays","Caenorhabditis elegans", "Amorphophallus titanum")
df[df$canonicalName %in% especies_interesse,] %>% select("id","taxon_name","name_published_in","probable_year") %>% na.omit() %>% View()

dados_gbif[dados_gbif$id == 113523471,]

dados[dados$scientificNameAuthorship == "L.",]
dados[dados$scientificNameAuthorship == "Linnaeus, 1753",]

dados[dados$kingdom == "Bacteria",] |> View()




# =============================================================================


# Velho abaixo
# ==============================================================================
# ==============================================================================
# ==============================================================================
# =============================================================================


# fazer histograma

na.omit(df$probable_year) |> max()
na.omit(df$probable_year) |> min()
hist(df$probable_year,breaks = seq(1725,2030,5))

# ==============================================================================
# ==============================================================================
# ==============================================================================
# ==============================================================================


# Análises arbitrárias

# ----------------------
# Comentários

# ver linhas que podem dar problema e que devem funcionar
df[df$id %in% c("8241022","2688817","4727186","8008504",
                "3939887","1265709","1265910","1266919",
                "9109762"),] |> View()

df[df$specific_epithet=="amazonicus",] |> View()
df[df$taxon_name=="Haplometra cylindracea",]

# Erros persistentes:

# Linhas com "Numer. List [Wallich] n." ex. 3897018, 7377408, 8278233, 7765651
# possuem número que é ano possível, mas o ano real é 1829.
# Possível fazer correção específica para estes, mas parece exagero.

# Linha 3939887 tem dois anos: "(1984 Publ. 1986)". Subjetivo qual seria correto.

# Linha 1567009 tem dois anos: "Melander, A.L. 1961. ... (1960) ... [1961.03.23]"
# Subjetivo qual seria correto.

# Não é possível corrigir estes casos optando sempre pelo primeiro nem pelo último ano.
# Em alguns casos o primeiro é o correto, em alguns casos o último é o correto.

# Talvez fosse aprimoramento ordenar pelo maior ou menor número.
# Porém algumas citações incluem data de acesso, então maior é errado,
# e alguns incluem autoria de gênero antigo, então menor é errado.
# Melhor deixar como está: primeiro ano que aparece na citação.

df[df$id %in% c("8241022","2688817","4727186","8008504","3939887","1265709","1265910"),] |> View()

# -------------------------------------------
# Linhas de interesse em 2026-02-27, com vários casos
df[df$id %in% c("8330049","8030419", "2785657","7775407", "8241022", "7407961",
                "7704103","1673778","10176166",
                "5716758","5716791","4311866",
                "2218992","10196438","8907870"),] |> View()

df[df$id == 113523471,]

linhas_interesse <- c("8241022","2688817","4727186","8008504",
                      "3939887","1265709","1265910","1266919",
                      "9109762",
                      "8027800",
                      "8330049","8030419", "2785657","7775407", "8241022", "7407961",
                      "7704103","1673778","10176166",
                      "5716758","5716791","4311866",
                      "2218992","10196438","8907870") # linhas demais

linhas_interesse <- c("2863877", # (1963) 
                      "7451964", # (1994a)
                      "7902871", # Name (2011)
                      "8241022", # ::1852
                      "8030419", # ::1557
                      "7407961", # ::2822
                      "2785657", # 91(1072): 54 (1983)
                      "1265910", # ... 53(2195):151-176. (1917).
                      "7704103", # 787, 787, 1912
                      "10176166", # 2019 ... 4654
                      "8827938", # 1931 ... 1914-1916
                      "4311947", # ... (Herbst, 1796). Accessed ... on 2020-03-02
                      "2507178", # no possible year
                      "2688818", # no possible year
                      "7522472", # no possible year, no number
                      "10")

df_interesse <- df[df$id %in% linhas_interesse,c("id","taxon_name","name_published_in", "probable_year")]

df_interesse <- df_interesse[order(df_interesse$taxon_name), ]; View(df_interesse)

# apenas linhas sem NA
df_naomit <- na.omit(df)
# apenas all_numbers_str com mais de um numero
df_naomit[nchar(df_naomit$all_numbers_str) > 4,] %>% View()

# apenas algumas linhas e algumas colunas
df_min <- df[1:100000,] %>% select("id","taxon_name","name_published_in","probable_year")

# apenas name_publish_in com até xx caracteres
df_min <- df_min[nchar(df_min$name_published_in) <= 100, ]

# apenas de um ano
df[df$probable_year==1787,] %>% select("id","taxon_name","name_published_in","probable_year") %>% na.omit() %>% View()

df[df$taxon_name=="Diogenes miles",] %>% select("id","taxon_name","name_published_in","probable_year") %>% na.omit() %>% View()

# only if all_matches includes a c
df_matching <- df_min %>%
  filter(sapply(all_matches, function(x) any(grepl("c", x, fixed = TRUE))))

# ==================
# procurando espécies famosas
# escolher por numero no gbif
df[df$id %in% c("113523471"),] |> View()

# escolher por nome da espécie
especies_interesse <- c("Zea mays","Caenorhabditis elegans", "Amorphophallus titanum")
df[df$taxon_name %in% especies_interesse,] %>% select("id","taxon_name","name_published_in","probable_year") %>% na.omit() %>% View()

dados_gbif[dados_gbif$id == 113523471,]



# ---
# apenas linhas com (letra número)

df_min <- df[1:10000,]

# Extract and view the specific matches
df_pp <- df_min %>%
  filter(grepl("[(][a-zA-Z]", name_published_in) & 
           grepl("\\d[)]", name_published_in)) %>%
  mutate(
    letter_after_paren = str_extract_all(name_published_in, "[(][a-zA-Z]"),
    digit_before_paren = str_extract_all(name_published_in, "\\d[)]")
  ) %>%
  select(name_published_in, letter_after_paren, digit_before_paren)

df_pp <- df_pp[nchar(df_pp$name_published_in) <= 100, ]

View(df_pp) # parenthesis parenthesis
