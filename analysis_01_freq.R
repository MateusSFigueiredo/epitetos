# ==============================================================================
# Arquivo: analysis_01_freq.R

# Renomeado de epitetos_especificos.R
#
# Input: tabela com inicial e epitetos
# Output: gráfico e tabelas
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

# Importar arquivos data de gbif, wd e col

# ==============================================================================
# Last file of each df_
list.files(pattern = "df_col")[length(list.files(pattern = "df_col"))]
list.files(pattern = "df_gbif")[length(list.files(pattern = "df_gbif"))]
list.files(pattern = "df_wikidata")[length(list.files(pattern = "df_wikidata"))]

# Import data
dados_col <- read.csv2(list.files(pattern = "df_col")[length(list.files(pattern = "df_col"))],
                       sep=",")

dados_gbif <- read.csv2(list.files(pattern = "df_gbif")[length(list.files(pattern = "df_gbif"))],
                       sep=",")

dados_wd <- read.csv2(list.files(pattern = "df_wikidata")[length(list.files(pattern = "df_wikidata"))],
                      sep=",")

# ---------------------------------------

# escolher database. Escolher entre 1, 2, 3
database <- c('gbif','wd','col')[1]

# definir com qual trabalhar agora com base em database
eval(parse(text=(paste("df <- dados_",database,sep="")))) # ou seja, df <- dados_col

# conferir colunas
colnames(df)

# ==============================================================================

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

