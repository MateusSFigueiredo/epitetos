# ==============================================================================
# Arquivo: analysis_02_year.R

# Em que ano os epítetos foram publicados?
#
# Input: df_gbif
# Output: 
#
# Modificado em: 2026-01-06
# Autor: Mateus Silva Figueiredo

# ==============================================================================
# Load necessary libraries
library(dplyr)
library(tidyr)
library(stringr)

setwd("C:/Users/Mateus/Desktop/R/epitetos")

list.files()

# Importar arquivos data de gbif

# ==============================================================================
# Last file df_gbif
list.files(pattern = "df_gbif")[length(list.files(pattern = "df_gbif"))]

# Import data
dados_gbif <- read.csv2(list.files(pattern = "df_gbif")[length(list.files(pattern = "df_gbif"))],
                       sep=",")

# ---------------------------------------

# escolher database. Escolher 1 para gbif
database <- c('gbif','wd','col')[1] # 1 para gbif

# definir com qual trabalhar agora com base em database
eval(parse(text=(paste("df <- dados_",database,sep="")))) # ou seja, df <- dados_col

# conferir colunas
colnames(df)

# ==============================================================================

df <- head(dados_gbif,3000) # numero menor para testes

df_all <- df


# df após remover linhas NA em name_published_in
df <- df[!is.na(df$name_published_in), ]

# =============================================================================

# DeepSeek - Extract Years in R Code Efficiently - 2026-01-07

library(dplyr)
library(stringr)

df <- df %>%
  mutate(
    # Step 1: Extract numbers in parentheses (including letters)
    numbers_in_paren = str_extract(name_published_in, "\\([^)]*\\d+[a-zA-Z]?[^)]*\\)"),
    
    # Clean parentheses numbers (remove letters, keep digits)
    paren_number_clean = ifelse(
      !is.na(numbers_in_paren),
      as.numeric(str_extract(numbers_in_paren, "\\d+")),
      NA_real_
    ),
    
    # Only keep as probable_year if between 1735 and 2026
    probable_year_from_paren = ifelse(
      !is.na(paren_number_clean) & 
        paren_number_clean >= 1735 & 
        paren_number_clean <= 2026,
      paren_number_clean,
      NA_real_
    ),
    
    # Step 2: Extract all numbers as string (only if no valid parentheses year)
    # BUT exclude numbers with : immediately before or after
    all_numbers_str = ifelse(
      is.na(probable_year_from_paren),
      sapply(name_published_in, function(text) {
        # Find all numbers
        all_matches <- str_extract_all(text, "\\d+")[[1]]
        if (length(all_matches) == 0) return(NA_character_)
        
        # Find positions of numbers in the original text
        # We need to check context around each number
        all_positions <- str_locate_all(text, "\\d+")[[1]]
        
        # Filter out numbers that have : immediately before or after
        valid_numbers <- character(0)
        for (i in seq_along(all_matches)) {
          start_pos <- all_positions[i, "start"]
          end_pos <- all_positions[i, "end"]
          
          # Check character before (if exists)
          char_before <- ifelse(start_pos > 1, 
                                substr(text, start_pos - 1, start_pos - 1), 
                                "")
          # Check character after (if exists)
          char_after <- ifelse(end_pos < nchar(text),
                               substr(text, end_pos + 1, end_pos + 1),
                               "")
          
          # Keep number only if neither neighbor is :
          if (char_before != ":" & char_after != ":") {
            valid_numbers <- c(valid_numbers, all_matches[i])
          }
        }
        
        if (length(valid_numbers) > 0) {
          return(paste(valid_numbers, collapse = ", "))
        } else {
          return(NA_character_)
        }
      }),
      NA_character_
    ),
    
    # Step 3: Extract first valid year from all numbers (if no parentheses year)
    # Also exclude numbers with : when searching for valid years
    year_from_all = ifelse(
      is.na(probable_year_from_paren) & !is.na(all_numbers_str),
      sapply(name_published_in, function(text) {
        # Find all numbers
        all_matches <- str_extract_all(text, "\\d+")[[1]]
        if (length(all_matches) == 0) return(NA_real_)
        
        # Find positions
        all_positions <- str_locate_all(text, "\\d+")[[1]]
        
        # Get numbers without : neighbors and convert to numeric
        valid_nums <- numeric(0)
        for (i in seq_along(all_matches)) {
          start_pos <- all_positions[i, "start"]
          end_pos <- all_positions[i, "end"]
          
          # Check character before and after
          char_before <- ifelse(start_pos > 1, 
                                substr(text, start_pos - 1, start_pos - 1), 
                                "")
          char_after <- ifelse(end_pos < nchar(text),
                               substr(text, end_pos + 1, end_pos + 1),
                               "")
          
          # Only consider numbers without : neighbors
          if (char_before != ":" & char_after != ":") {
            num <- as.numeric(all_matches[i])
            # Check if it's a valid year
            if (!is.na(num) & num >= 1735 & num <= 2026) {
              valid_nums <- c(valid_nums, num)
            }
          }
        }
        
        if (length(valid_nums) > 0) {
          return(valid_nums[1])  # Return first valid year
        } else {
          return(NA_real_)
        }
      }),
      NA_real_
    ),
    
    # Final probable_year column
    probable_year = coalesce(probable_year_from_paren, year_from_all)
  ) %>%
  # Remove intermediate columns
  select(-numbers_in_paren, -paren_number_clean, -probable_year_from_paren, -year_from_all)

# ----------------------
# Comentários

# 8241022 'Int. J. Syst. Evol. Microbiol. 61::1852' não deve ter 1852 lido como ano
# 2688817 'Svensk Bot. Tidskr., 46: 106, 106, 1952' deve ter 1952 lido como ano
# Parece estar perfeito

# =============================================================================

# fazer histograma

na.omit(df$probable_year) |> max()
na.omit(df$probable_year) |> min()
hist(df$probable_year,breaks = seq(1750,2050,5))

# ==============================================================================
# ==============================================================================
# ==============================================================================
# ==============================================================================

# Abaixo = copiado de analysis_01

# Step 1: Create a frequency table for the 'specific_epithet' column and order it by frequency
freq_table <- table(df$specific_epithet)

# Convert the table to a data frame and order by frequency (in descending order)
freq_df <- as.data.frame(freq_table) %>%
  arrange(desc(Freq))

# View the ordered frequency table
# freq_df %>% head(20)
# View(freq_df)

n_epitetos <- 20 # top quantos epitetos especificos analisar?
top_specific_epithets <- as.character(head(freq_df,n_epitetos)$Var)
#top_specific_epithets
# ------------------------------------------------------------------------------

# Step 6: Filter the dataframe to only include the top 20 'specific_epithet's
filtered_df <- df %>%
  filter(specific_epithet %in% top_specific_epithets)

# Step 7: Get the frequency of 'generic_initial' for each 'specific_epithet'
freq_table <- filtered_df %>%
  group_by(specific_epithet, generic_initial) %>%
  summarise(freq = n()) %>%
  ungroup()

# ==============================================================================
# Produção de gráfico

# Load necessary libraries
library(ggplot2)
library(reshape2)

  # Step 8: Create a wide dataframe with 'specific_epithet's as columns and 'generic_initial's as rows
  # Additionally, order the 'specific_epithet' by total frequency
  specific_epithet_order <- freq_table %>%
  group_by(specific_epithet) %>%
  summarise(total_freq = sum(freq)) %>%
  arrange(desc(total_freq)) %>%
  pull(specific_epithet)

# Update the 'freq_matrix' to use the custom order for 'specific_epithet'
freq_matrix <- freq_table %>%
  spread(key = specific_epithet, value = freq, fill = 0)  # Fill missing values with 0

# Ensure the order of the columns matches the order of 'specific_epithet' by frequency
freq_matrix <- freq_matrix %>%
  select(generic_initial, all_of(specific_epithet_order))

# Step 9: Convert the frequency matrix back to long format for easier plotting
freq_long <- melt(freq_matrix, id.vars = "generic_initial", variable.name = "specific_epithet", value.name = "frequency")

# Colocar ponto após cada inicial generic_initial por estética
freq_long$generic_initial <- paste0(freq_long$generic_initial,".")

# Step 10: Convert 'specific_epithet' to a factor with levels ordered by total frequency
freq_long$specific_epithet <- factor(freq_long$specific_epithet, levels = specific_epithet_order)

# Define titulo com base em query
if (database=='col'){fonte <- "- Cat. of Life"}
if (database=='gbif'){fonte <- "- GBIF"}
if (database=='wd'){fonte <- "- Wikidata"}

# titulo <- paste("Frequência do epíteto por inicial", fonte)
titulo <- paste("Epithet frequency by genus initial", fonte)

# Step 11: Plot the frequency matrix using ggplot with custom ordering on x-axis
grafico<-ggplot(freq_long, aes(x = specific_epithet, y = generic_initial, fill = frequency)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = titulo,
       x = "Specific epithet",
       y = "Genus initial",
       fill = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1));grafico
  

print("Gráfico pronto")
# ==============================================================================
# Exportar gráfico pronto
fonte

# Pega tempo atual para nome do arquivo
tempo_atual <- format(Sys.time(), "%Y-%m-%d-%H-%M")


filename<-paste0("frequency_matrix_",fonte,tempo_atual,".png")
filename
ggsave(filename=filename, plot=last_plot(), scale=1, width=2000, height = 1200, units="px")

print("Imagem salva")
# ==============================================================================

