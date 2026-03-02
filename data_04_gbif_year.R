# ==============================================================================
# Arquivo: data_04_gbif_year.R

# Em que ano os nomes de espécies foram publicados?
#
# Input: df_gbif feito por data_01_especies_gbif
# Output: produz df com probable_year de cada linha
#
# Modificado em: 2026-02-28
# Autor: Mateus Silva Figueiredo

# dif: mais linhas de interesse para análise ao final

# ideia: usar Taxon.tsv original, para poder ter mais fontes de dados

# ==============================================================================
# Load necessary libraries
library(dplyr)
library(tidyr)
library(stringr)

setwd("C:/Users/Mateus/Desktop/R/epitetos")

list.files()

# ==============================================================================
# Importar arquivos data de gbif
# Last file df_gbif
list.files(pattern = "df_gbif")[length(list.files(pattern = "df_gbif"))]

# Import data
dados_gbif <- read.csv2(list.files(pattern = "df_gbif")[length(list.files(pattern = "df_gbif"))],
                       sep=",") # 2152965 obs.

# ---------------------------------------

# escolher database. Escolher 1 para gbif
database <- c('gbif','wd','col')[1] # 1 para gbif

# cria df, definir com qual trabalhar agora com base em database
eval(parse(text=(paste("df <- dados_",database,sep="")))) # ou seja, df <- dados_col

# conferir colunas
colnames(df)

# ==============================================================================

# df após remover linhas NA em name_published_in
df <- df[!is.na(df$name_published_in), ]

df |> nrow() |> paste("linhas após removed NA de name_published_in")
#"1418439 linhas após removed NA de name_published_in"
# df_clean <- df

# =============================================================================
# teste com menos linhas
# df <- head(df_clean,20000)
# teste subset com linhas problema
# df <- df_clean[df_clean$id %in% c(1265709:1299924),] # subset incluindo "1265709"
# df <- df_clean[df_clean$id %in% c(9109762:(9109762+10)),] # subset incluindo "9109762"



# DeepSeek - Complete fixed version with the function approach - 2026-01-08

library(data.table)
library(dplyr)
library(stringi)  # Much faster than stringr for large datasets

Sys.time() -> t1; df <- df %>%
  mutate(
    # 1. Extract parentheses year (vectorized)
    paren_match = stri_extract_first_regex(name_published_in, "\\(\\d{4}[a-zA-Z]?[^)]*\\)"),
    paren_year = as.numeric(stri_extract_first_regex(paren_match, "\\d+")),
    
    # Valid parentheses year
    probable_year = ifelse(
      !is.na(paren_year) & paren_year >= 1735 & paren_year <= 2026,
      paren_year,
      NA_real_
    ),
    
    # 2. For rows without parentheses year, extract numbers without colon before
    # First extract full matches with optional letter
    # Get all matches at once (vectorized)
    all_matches = stri_extract_all_regex(name_published_in, "(?<!:)\\b\\d{4}(?=[a-zA-Z]?\\b)"),
    
    # Create all_numbers_str (only for rows without probable_year)
    all_numbers_str = ifelse(
      is.na(probable_year),
      sapply(all_matches, function(x) {
        if (!is.na(x[1]) && length(x) > 0) paste(x, collapse = ", ") else NA_character_
      }),
      NA_character_
    ),
    
    # Extract first valid year from matches
    year_from_all = ifelse(
      is.na(probable_year),
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
    probable_year = coalesce(probable_year, year_from_all)
  ) %>%
  select(-paren_match, -paren_year, -all_matches, -year_from_all); Sys.time() - t1 # 45.6 secs

# =============================================================================
# análises de texto com mensagens de erro em print

if(is.na(df[df$id==8241022,'probable_year'])){print('Tudo certo: 8241022 está NA')}else{print('ERRO')} # 

if(df[df$id==1265709,'probable_year']==1924){print('Tudo certo: 1265709 está 1924')}else{print('ERRO')}

if(df[df$id==9109762,'probable_year']==2016){print('Tudo certo: 9109762 está 2016')}else{print('ERRO EM 9109762')}

if(df[df$id==4727186,'probable_year']==1868){print('Tudo certo: 4727186 está 2016')}else{print('ERRO EM 4727186')} # 2016a deve ser lido como 2016

if(df[df$id==2117306,'probable_year']==1914){print('Tudo certo: 2117306 está 1914')}else('ERRO EM 2117306') # 1914: deve ser lido como 1914


# =============================================================================

# fazer histograma

na.omit(df$probable_year) |> max()
na.omit(df$probable_year) |> min()
hist(df$probable_year,breaks = seq(1725,2030,5))

# ==============================================================================
# ==============================================================================
# ==============================================================================
# ==============================================================================


# =======================================
# Export df

# Save file with date and time to avoid a bad overwrite
if(T){ # F to not save
  # create save_path with date and time
  save_path <- paste0("df_year_", format(Sys.time(), "%Y-%m-%d-%H-%M"), ".csv")
  # save csv with date and time in its name
  write.csv(df,file=save_path,row.names=F)
}
# =======================================
print("Fim do código")

# =========================================
# Análises arbitrárias

# ----------------------
# Comentários

# ver linhas que podem dar problema e que devem funcionar
df[df$id %in% c("8241022","2688817","4727186","8008504",
                "3939887","1265709","1265910","1266919",
                "9109762"),] |> View()

df[df$specific_epithet=="amazonicus",] |> View()
df[df$taxon_name=="Haplometra cylindracea",]

# Limites persistentes:

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
                      "3897018", "7377408", # "Numer. List [Wallich] n."
                      "3939887", "1567009", # two possible years
                      "10")

df_interesse <- df[df$id %in% linhas_interesse,c("id","taxon_name","name_published_in", "probable_year")]

df_interesse <- df_interesse[order(df_interesse$taxon_name), ]; View(df_interesse)

