# ==============================================================================
# Arquivo: epitetos_especificos.R
#
# Pega Query do Wikidata com nomes de espécies.
# Separa gênero de epíteto específico.
# Pega inicial do gênero
# Gera tabela de frequência do epíteto específico
#
# Modificado em: 2025-01-05
# Autor: Mateus Silva Figueiredo
# dif: salva freq_df_1000

# ==============================================================================
# Load necessary libraries
library(dplyr)
library(tidyr)
library(stringr)

setwd("C:/Users/Mateus/Desktop/R/epitetos")

list.files()

# arquivo csv pode ser obtido por script query_especies_wikidata ou outra fonte

# ==============================================================================
# nome do arquivo csv

query<-"wikidata_species.csv" # 3201049 linhas em 2025-01-03 criado com especies_wd.R
# query<-"especies_col_2181430.csv" # 2181430 linhas, inclui dubias

# Read the CSV file
df <- read.csv(query, 
#               nrows=1000, # para poucas linhas
               stringsAsFactors = FALSE)
print(paste(query, "número de linhas original =", (nrow(df))))
print("colunas:");print(colnames(df))

# Reduzir número de linhas para testes
if(F){df <- head(df,100000)}

# ==============================================================================
# Step 1: Tirar linhas com virus

# Filter rows where 'virus' is present in 'taxon_name' and save it to 'virus'
virus <- df %>% filter(str_detect(taxon_name, regex("virus", ignore_case = TRUE)))

# Remove rows with 'virus' from the original df
df <- df %>% filter(!str_detect(taxon_name, regex("virus", ignore_case = TRUE)))

print(paste(query, "número de linhas sem virus =", (nrow(df))))

# ------------------------------------------------------------------------------

# Step 2: Remove rows where the 'taxon_name' column has more than two words
# Necessário para query wikidata
# Não necessário para especies_col

# nao_binomial = apenas linhas em que 'taxon_name' não tenha duas palavras
nao_binomial <- df %>% filter(str_count(taxon_name, "\\S+") != 2)

# manter apenas linhas em que taxon_name tenha duas palavras
df           <- df %>% filter(str_count(taxon_name, "\\S+") == 2)

print(paste(query, "número de linhas com binomial correto =", (nrow(df))))

# ------------------------------------------------------------------------------
# Time: less than one minute
# Step 3: Create a new column with the last word of the 'taxon_name' column
df <- df %>%
  mutate(last_word = sapply(strsplit(taxon_name, " "), tail, 1))

# Transformar taxon_name em character # parece lento, ~1 minuto
df <- df %>%
  mutate(last_word = as.character(last_word))

# ------------------------------------------------------------------------------
# Lidar com iniciais fora do padrão
# Wikidata tem algumas espécies com x
# CoL tem espécies com ? † e =
# Wikidata tem espécies com inicial minúscula

# remover † cruz
df$taxon_name<-gsub("†","",df$taxon_name)

# remover ? interrogacao
df$taxon_name<-gsub("\\?","",df$taxon_name)
# \\ necessário pra R interpretar ? como ? literal

# remover × xis
df$taxon_name<-gsub("×","",df$taxon_name)

# trim white spaces
df$taxon_name<-trimws(df$taxon_name)

# Step 4: Create a new column with the first letter of 'taxon_name'
df <- df %>%
  mutate(first_letter = substr(taxon_name, 1, 1))

# check
table(df$first_letter)

# ------------------------------------------------------------------------------
# Eliminar linhas com first_letter fora do alfabeto latino
# Wikidata tem algumas espécies com x
# CoL tem espécies com ? † e =
# Transformar minúsculas em maiúsculas

if(F){ # para inspecionar problema
table(df$first_letter) # ver tabela
non_capital_rows <- df %>% filter(!grepl("^[A-Z]", first_letter)) 
non_capital_rows
}

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

n_epitetos <- 20 # top quantos epitetos especificos analisar?
top_last_words <- as.character(head(freq_df,n_epitetos)$Var)
#top_last_words
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

# Define titulo com base em query
if (grepl("wikidata",query)){fonte <- "- Wikidata"}
if (grepl("col",query)){fonte <- "- Cat. of Life"}

# titulo <- paste("Frequência do epíteto por inicial", fonte)
titulo <- paste("Epithet frequency by genus initial", fonte)

# Step 11: Plot the frequency matrix using ggplot with custom ordering on x-axis
grafico<-ggplot(freq_long, aes(x = last_word, y = first_letter, fill = frequency)) +
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
?ggsave

query
fonte

# Define titulo com base em query
if (grepl("wikidata",query)){fonte <- "wikidata_"}
if (grepl("col",query)){fonte <- "col_"}

tempo_atual <- format(Sys.time(), "%Y-%m-%d-%H-%M")

Sys.time()

filename<-paste0("frequency_matrix_",fonte,tempo_atual,".png")
filename
ggsave(filename=filename, plot=last_plot(), scale=1, width=2000, height = 1200, units="px")

print("Imagem salva")
# ==============================================================================

# Inspect frequency table and matrix

freq_matrix[,2:21] %>% max()
freq_table$freq %>% sort()

# In the Wikidata dataset, the most common abbreviation is C. elegans, with 730 species, followed by P. gracilis, with 664 species, next 570 examples of C. gracilis.

freq_epiteto <- table(df$last_word)
freq_epiteto <- as.data.frame(freq_epiteto)

# ==============================================================================
# Inspect Wikidata dataframe

# Numero total de linhas
nrow(df)

# Numero de linhas em cada categoria
sum(df$instance_of_label == "taxon")         # 3110747
sum(df$instance_of_label == "fossil taxon")  # 69672
sum(df$instance_of_label == "extinct taxon") # 607

# Itens no wikidata sem indicação de CoL ID
sum(df$col_id == "")                         # 1163819 no wikidata sem col
df[df$col_id=="",]

# Wikidata tem muitos c elegans
c_elegans <- df[df$last_word == "elegans" & df$first_letter == "C", ]
sort(c_elegans$taxon_name)

# ==============================================================================
# Produzir gráfico só dos epitetos

# Ordenar freq_epiteto por frequencia descendente
freq_epiteto <- freq_epiteto[order(-freq_epiteto$Freq), ]

# Ensure Var1 is treated as a factor with levels in the original order
freq_epiteto$Var1 <- factor(freq_epiteto$Var1, levels = unique(freq_epiteto$Var1))

dados<-freq_epiteto
dados <- head(freq_epiteto,2000) # para testes menores

# Coluna rank
dados$rank<-c(1:nrow(dados))

# Create a line plot using ggplot2
ggplot(dados, aes(x = log(rank), y = log(Freq),group=1)) +
  geom_line(stat = "identity") +
  geom_abline(intercept = log(max(dados$Freq)), slope = -1, color = "red", linetype = "dashed") +  # Reference line with slope -1
  xlab("long(Rank)") +
  ylab("log(Frequency)") +
  ylim(c(0,8))+
  xlim(c(1,14))+
  ggtitle("Line Plot of Epithets vs log of Frequency") +
  theme_minimal()

# plot(freq_epiteto$Freq ~ freq_epiteto$Var1)

library(ggplot2)
library(dplyr)
#install.packages('themes')
install.packages('gganimate')
#library(themes)
library(gganimate)

zipfs_plot <- ggplot(dados, aes(x = rank, y = 1/Freq)) + 
  geom_point(aes(color = "observed")) +
  theme_bw() + 
  geom_point(aes(y = Freq, color = "theoretical")) +
  labs(x = "rank", y = "count", title = "Zipf's law visualization") +
  scale_colour_manual(name = "Word count", values=c("theoretical" = "red", "observed" = "black")) +
  theme(legend.position = "top")

zipfs_plot

# ==============================================================================
# Epitetos mais frequentes
head(freq_df)
freq_df_1000 <- head(freq_df,1000)

# Deseja salvar arquivo csv?
if(F){write.csv(freq_df_1000,"freq_df_1000.csv")}

# ==============================================================================
print("Fim do código")
# Investigar nomes arbitrários
c_elegans <- df[df$first_letter == "C" & df$last_word == "elegans", ]
p_gracilis <- df[df$first_letter == "P" & df$last_word == "gracilis", ]

df[df$first_letter == "P" & df$last_word == "gouldii", ]

df[df$first_letter == "C", ]
df[df$last_word == "figueiredoi", ]

max(freq_matrix)


