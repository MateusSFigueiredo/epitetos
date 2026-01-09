# ==============================================================================
# Arquivo: analysis_meta_years_different

# Em quais linhas o código lê anos diferentes no começo e no final da linha?
#
# Modificado em: 2026-01-08
# Autor: Mateus Silva Figueiredo

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

df_clean <- df

# =============================================================================
# teste com menos linhas
# df <- head(df_clean,20000)
# teste subset com linhas problema
# df <- df_clean[df_clean$id %in% c(1265709:1299924),] # subset incluindo "1265709"

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
    probable_year_first = ifelse(
      !is.na(paren_year) & paren_year >= 1735 & paren_year <= 2026,
      paren_year,
      NA_real_
    ),
    probable_year_last = probable_year_first,  # Same for parentheses case
    
    # 2. For rows without parentheses year, extract numbers without colon
    # Get all matches at once (vectorized)
    all_matches = stri_extract_all_regex(name_published_in, "(?<!:)\\b\\d+\\b(?!:)"),
    
    # Create all_numbers_str (only for rows without probable_year_first)
    all_numbers_str = ifelse(
      is.na(probable_year_first),
      sapply(all_matches, function(x) {
        if (!is.na(x[1]) && length(x) > 0) paste(x, collapse = ", ") else NA_character_
      }),
      NA_character_
    ),
    
    # Extract first valid year from matches
    year_from_all_first = ifelse(
      is.na(probable_year_first),
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
    
    # Extract last valid year from matches
    year_from_all_last = ifelse(
      is.na(probable_year_first),
      sapply(all_matches, function(x) {
        if (!is.na(x[1]) && length(x) > 0) {
          nums <- as.numeric(x)
          valid_years <- nums[nums >= 1735 & nums <= 2026 & !is.na(nums)]
          if (length(valid_years) > 0) return(valid_years[length(valid_years)])
        }
        return(NA_real_)
      }),
      NA_real_
    ),
    
    # Final probable_year_first (same as original)
    probable_year_first = coalesce(probable_year_first, year_from_all_first),
    
    # Final probable_year_last
    probable_year_last = coalesce(probable_year_last, year_from_all_last)
  ) %>%
  select(-paren_match, -paren_year, -all_matches, -year_from_all_first, -year_from_all_last); Sys.time() - t1 # 

# ----------------------

# Filter rows where the first and last years are different
df_different <- df %>%
  filter(probable_year_first != probable_year_last | 
           (is.na(probable_year_first) & !is.na(probable_year_last)) |
           (!is.na(probable_year_first) & is.na(probable_year_last)))

# View the result
View(df_different)

# Filter rows where first < last years
df_menor <- df %>%
  filter(probable_year_first < probable_year_last | 
           (is.na(probable_year_first) & !is.na(probable_year_last)) |
           (!is.na(probable_year_first) & is.na(probable_year_last)))

# Filter rows where first > last years
df_maior <- df %>%
  filter(probable_year_first > probable_year_last | 
           (is.na(probable_year_first) & !is.na(probable_year_last)) |
           (!is.na(probable_year_first) & is.na(probable_year_last)))


# ---------------------

df_with_commas <- df %>%
  filter(str_detect(name_published_in, ",")) |> View()

# ----------------------

# Comentários

# 5716758 = 'World Paguroidea & Lomisoidea database. Diogenes miles (Fabricius, 1787). Accessed through: World Register of Marine Species at: http://www.marinespecies.org/aphia.php?p=taxdetails&id=246284 on 2020-03-02'
# last year 2020 is year of access
# first year (Fabricius, 1787) has text before the year

# 2218992 = 'Walker, Alfred O. 1898. Crustacea collected by W.A. Herdman, F.R.S., in Puget Sound, Pacific coast of North America, September 1897. Proceedings and Transactions of the Liverpool Biological Society 12: 268-287.'
# first year 1898 is the correct one.

# 2117306 = 'Hodgson, T. V., 1914: Preliminary report on the Pycnogonida of the German Southpolar Expedition 1901-1903. Zoologischer Anzeiger, Ser. 8, vol. 45, no. 4. 158-165.'
# both years 1901 and 1903 are wrong
# correct year 1914 is adjacent to : and so it's ignored


# =============================================================================

