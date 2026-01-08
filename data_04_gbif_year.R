# ==============================================================================
# Arquivo: data_04_gbif_year.R

# Em que ano os nomes de espécies foram publicados?
#
# Input: df_gbif feito por data_01_especies_gbif
# Output: produz df com probable_year de cada linha
#
# Modificado em: 2026-01-08
# Autor: Mateus Silva Figueiredo

# dif: entre parêntesis só pega o que tiver 4 dígitos

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

# head
# df <- head(dados_gbif,3000) # numero menor para testes

# df após remover linhas NA em name_published_in
df <- df[!is.na(df$name_published_in), ]

df |> nrow() |> paste("linhas após removed NA de name_published_in")

# df_clean <- df

# =============================================================================
# teste com menos linhas
# df <- head(df_clean,20000)
# teste subset com linhas problema
# df <- df_clean[df_clean$id %in% c(1265709:1299924),] # subset incluindo "1265709"

# DeepSeek - Complete fixed version with the function approach - 2026-01-08

library(data.table)
library(dplyr)
library(stringi)  # Much faster than stringr for large datasets

Sys.time() -> t1 

df <- df %>%
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
    
    # 2. For rows without parentheses year, extract numbers without colon
    # Get all matches at once (vectorized)
    all_matches = stri_extract_all_regex(name_published_in, "(?<!:)\\b\\d+\\b(?!:)"),
    
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
  select(-paren_match, -paren_year, -all_matches, -year_from_all)


Sys.time() - t1 # 45.6 secs

# ----------------------
# Comentários

# 8241022 'Int. J. Syst. Evol. Microbiol. 61::1852' não deve ter 1852 lido como ano
# 2688817 'Svensk Bot. Tidskr., 46: 106, 106, 1952' deve ter 1952 lido como ano
# 1265709 'Roman, A. Wissenshaftliche Ergebnisse der schwedischen entomologischen Reise des Herrn Dr. A. Roman in Amazonas 1914-15, 10. Hymenoptera: Braconidae, Cyclostomi pro p. Arkiv foer Zoologi. 16(20):1-40. (1924).' deveria ser lido como 1924, mas não foi


# ver linhas que podem dar problema e que devem funcionar
df[df$id %in% c("8241022","2688817","4727186","8008504","3939887","1265709","1265910"),] |> View()

df[df$specific_epithet=="amazonicus",] |> View()
df[df$taxon_name=="Haplometra cylindracea",]

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
