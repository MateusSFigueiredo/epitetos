# ==============================================================================
# Arquivo: epitetos_especificos.R
#
# Pega Query do Wikidata com nomes de espécies.
# Separa gênero de epíteto específico.
# Pega inicial do gênero
# Gera tabela de frequência do epíteto específico
#
# Modificado em: 2024_10_03
# Autor: Mateus Silva Figueiredo
# dif: tentando deixar mais conciso

# ==============================================================================
# Load necessary libraries
library(dplyr)
library(tidyr)
library(stringr)

getwd()
list.files()

# arquivo csv pode ser obtido por script query_especies_wikidata ou outra fonte

# ==============================================================================
# nome do arquivo csv
# query<-"query_5_10_5.csv"
query<-"wikidata_27ZIHl.csv" # 3196241 linhas

# Read the CSV file
df <- read.csv(query, stringsAsFactors = FALSE)
print(paste("número de linhas original =", (nrow(df))))

# renomeia coluna taxon_name para nome_do_taxon
colnames(df)[colnames(df) == "taxon_name"] <- "nome_do_taxon"

# Reduzir número de linhas para testes
if(F){df <- head(df,10000)}

# ==============================================================================
# Step 1: Tirar linhas com virus

# Filter rows where 'virus' is present in 'nome_do_taxon' and save it to 'virus'
virus <- df %>% filter(str_detect(nome_do_taxon, regex("virus", ignore_case = TRUE)))

# Remove rows with 'virus' from the original df
df <- df %>% filter(!str_detect(nome_do_taxon, regex("virus", ignore_case = TRUE)))

print(paste("número de linhas sem virus =", (nrow(df))))

# ------------------------------------------------------------------------------

# Step 2: Remove rows where the 'nome_do_taxon' column has more than two words

# nao_binomial = apenas linhas em que 'nome_do_taxon' não tenha duas palavras
nao_binomial <- df %>% filter(str_count(nome_do_taxon, "\\S+") != 2)

# manter apenas linhas em que nome_do_taxon tenha duas palavras
df           <- df %>% filter(str_count(nome_do_taxon, "\\S+") == 2)

print(paste("número de linhas com binomial correto =", (nrow(df))))

# ------------------------------------------------------------------------------

# Step 3: Create a new column with the last word of the 'nome_do_taxon' column
df <- df %>%
  mutate(last_word = sapply(strsplit(nome_do_taxon, " "), tail, 1))

# Transformar nome_do_taxon em character
df <- df %>%
  mutate(last_word = as.character(last_word))

# Step 4: Create a new column with the first letter of 'nome_do_taxon'
df <- df %>%
  mutate(first_letter = substr(nome_do_taxon, 1, 1))

# ------------------------------------------------------------------------------
# Eliminar linhas com first_letter fora do alfabeto latino
# Transformar minúsculas em maiúsculas

if(F){ # para inspecionar problema
table(df$first_letter) # ver tabela
non_capital_rows <- df %>% filter(!grepl("^[A-Z]", first_letter)) 
non_capital_rows}

# Filtrar linhas que não começam com letras do alfabeto e salvar
non_alphabetic_rows <- df %>%  filter(!grepl("^[A-Za-z]$", first_letter))

# Atualizar df e manter apenas linhas que começam com letras do alfabeto
df <- df %>%  filter(grepl("^[A-Za-z]$", first_letter))

# Passar letra inicial minúscula para maiúscula
df$first_letter <- toupper(df$first_letter)

# ------------------------------------------------------------------------------

# Step 5: Create a frequency table for the 'last_word' column and order it by frequency
freq_table <- table(df$last_word)

# Convert the table to a data frame and order by frequency (in descending order)
freq_df <- as.data.frame(freq_table) %>%
  arrange(desc(Freq))

# View the ordered frequency table
# freq_df %>% head(20)
# View(freq_df)

n_epitetos <- 50 # top quantos epitetos especificos analisar?
top_last_words <- as.character(head(freq_df,n_epitetos)$Var)

# ------------------------------------------------------------------------------

# Step 6: Filter the dataframe to only include the top 20 'last_word's
filtered_df <- df %>%
  filter(last_word %in% top_last_words)

# Step 7: Get the frequency of 'first_letter' for each 'last_word'
freq_table <- filtered_df %>%
  group_by(last_word, first_letter) %>%
  summarise(freq = n()) %>%
  ungroup()

# ==============================================================================
# Produção de gráfico

# Load necessary libraries
library(ggplot2)
library(reshape2)

  # Step 8: Create a wide dataframe with 'last_word's as columns and 'first_letter's as rows
  # Additionally, order the 'last_word' by total frequency
  last_word_order <- freq_table %>%
  group_by(last_word) %>%
  summarise(total_freq = sum(freq)) %>%
  arrange(desc(total_freq)) %>%
  pull(last_word)

# Update the 'freq_matrix' to use the custom order for 'last_word'
freq_matrix <- freq_table %>%
  spread(key = last_word, value = freq, fill = 0)  # Fill missing values with 0

# Ensure the order of the columns matches the order of 'last_word' by frequency
freq_matrix <- freq_matrix %>%
  select(first_letter, all_of(last_word_order))

# Step 9: Convert the frequency matrix back to long format for easier plotting
freq_long <- melt(freq_matrix, id.vars = "first_letter", variable.name = "last_word", value.name = "frequency")

# Colocar ponto após cada inicial first_letter por estética
freq_long$first_letter <- paste0(freq_long$first_letter,".")

# Step 10: Convert 'last_word' to a factor with levels ordered by total frequency
freq_long$last_word <- factor(freq_long$last_word, levels = last_word_order)

# Step 11: Plot the frequency matrix using ggplot with custom ordering on x-axis
ggplot(freq_long, aes(x = last_word, y = first_letter, fill = frequency)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Frequência da inicial por epíteto",
       x = "Epíteto específico",
       y = "Inicial do gênero",
       fill = "Frequência") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# ==============================================================================
# Investigar nomes arbitrários
c_elegans <- df[df$first_letter == "C" & df$last_word == "elegans", ]
p_gracilis <- df[df$first_letter == "P" & df$last_word == "gracilis", ]

df[df$first_letter == "P" & df$last_word == "gouldii", ]

df[df$first_letter == "C", ]
df[df$last_word == "figueiredoi", ]

