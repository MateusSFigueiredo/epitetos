# ==============================================================================
# Arquivo: analysis_02_year.R
#
# Input: df_year_ produzido por data_04_gbif_year
# Output: a fazer
#
# Modificado em: 2026-01-08
# Autor: Mateus Silva Figueiredo

# ==============================================================================
# Load necessary libraries
# library(dplyr)
# library(tidyr)
# library(stringr)

# install.packages("reshape")
library(reshape) # for melting df

# install.packages("ggplot2")
library(ggplot2)

setwd("C:/Users/Mateus/Desktop/R/epitetos")

list.files()

# start of create functions
# ==============================================================================
# Create functions

# ----------------
# get_highest values of dataframe df_pct_by_year

get_highest <- function(data,value){
  unlist(data, use.names=FALSE) |> sort(decreasing=T) |> head(value) -> highest
  return(highest)
}
# ----------------
# Function to find row_name and column_name which has one or more specific values
# by DeepSeek on 09/01/2026

find_value_positions <- function(data, values) {
  # Convert single value to vector
  if (length(values) == 1) {
    values <- c(values)
  }
  
  # Initialize result dataframe
  all_results <- data.frame(
    row_name = character(),
    column_name = character(),
    value = character(),
    stringsAsFactors = FALSE
  )
  
  # Track if any value was found
  any_found <- FALSE
  
  for (value in values) {
    if (is.na(value)) {
      # Create a matrix of NA positions
      mat_positions <- which(is.na(data), arr.ind = TRUE)
    } else {
      # Convert data to matrix for comparison
      data_matrix <- as.matrix(data)
      mat_positions <- which(data_matrix == value, arr.ind = TRUE)
    }
    
    if (nrow(mat_positions) > 0) {
      any_found <- TRUE
      
      # Convert to data frame with proper names
      result <- data.frame(
        row_name = if (!is.null(rownames(data))) {
          rownames(data)[mat_positions[, "row"]]
        } else {
          as.character(mat_positions[, "row"])
        },
        column_name = colnames(data)[mat_positions[, "col"]],
        value = ifelse(is.na(value), "NA", as.character(value)),
        stringsAsFactors = FALSE
      )
      
      all_results <- rbind(all_results, result)
    }
  }
  
  if (nrow(all_results) == 0) {
    cat("None of the values", paste(values, collapse = ", "), "were found.\n")
    return(NULL)
  }
  
  # Sort by row then column for cleaner output
  all_results <- all_results[order(all_results$row_name, all_results$column_name), ]
  rownames(all_results) <- NULL
  
  return(all_results)
}

# end of create functions
# ----------------

# ==============================================================================
# Last file of df_year
list.files(pattern = "df_year")[length(list.files(pattern = "df_year"))]

# Import data # a few seconds
dados_year <- read.csv2(list.files(pattern = "df_year")[length(list.files(pattern = "df_year"))],
                       sep=",")

# ---------------------------------------

# criar df
df <- dados_year

# conferir colunas
colnames(df)

df_clean <- df[!is.na(df$probable_year),]
df <- df_clean

# ==============================================================================

# Step 1: Create a frequency table for the 'specific_epithet' column and order it by frequency
freq_table <- table(df$specific_epithet)

# Convert the table to a data frame and order by frequency (in descending order)
freq_df <- as.data.frame(freq_table) %>%
  arrange(desc(Freq))

# View the ordered frequency table
# freq_df %>% head(20)
# View(freq_df)

n_epi <- 30 # top quantos epitetos especificos analisar?

top_specific_epithets <- as.vector(head(freq_df$Var1,n_epi))
top_specific_epithets
top_specific_epithets[2]

# talvez seja melhor obter top_specific_epithets a partir de dados mais amplos,
# não apenas deste subset com publicação
# ==============================================================================

# subset df apenas epitetos no top

df_top <- df[df$specific_epithet %in% top_specific_epithets,]

df <- df_top

# ======================================
# cria df_epi_by_year
# colunas = cada epiteto no top
# linhas = cada ano do começo de cada quinquenio

df_epi_by_year <- as.data.frame(matrix(nrow=58,ncol=30))

colnames(df_epi_by_year) <- c(top_specific_epithets)

rownames(df_epi_by_year) <- c(seq(1735,2020,5))


# ======================================

# for loop para preencher df_epi_by_year
# i <- 1  # epitetos
# j <- 36 # ano inicial do quinquenio

for (i in c(1:30)){
  for (j in c(1:58)){
    
    # definir ano inicial e final do quinquenio
    ano0 <- as.numeric(rownames(df_epi_by_year)[j])
    ano4 <- ano0+4
    
    subset(df,df$specific_epithet==top_specific_epithets[i] & df$probable_year %in% c(ano0:ano4),) |> nrow() -> quant
    
    df_epi_by_year[j,i] <- as.numeric(quant)
    
  }
} # takes a few seconds

# check
sum(df_epi_by_year$gracilis)
sum(df_epi_by_year$bicolor)

# ======================================

# # Transforma e melt, mantem o ano. Cria df1 dataframe para gráfico
df1 <- data.frame(x = rownames(df_epi_by_year),
                 df_epi_by_year)
df1 <- melt(df1, id.vars = 'x')

df1$x <- as.numeric(df1$x)

# ----

# make plot 1

ggplot(df1, aes(x = x, y = value, group=variable, color = variable)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# plot 1 but focus on 1930-1960

ggplot(df1, aes(x = x, y = value, group = variable, color = variable)) +
  geom_line() +
  scale_x_continuous(limits = c(1890, 1970)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

plot(df_total$total~rownames(df_total),
     type='l',
#     xlim=c(1890,1970),
#     ylim=c(0,50000),
main="Novas espécies por ano")

# ==============================================================================

# e se df_epi_by_year for em porcentagem de nomes descritos para aquele quinquenio?
# criar df_pct_by_year
df_pct_by_year <- df_epi_by_year

# cria coluna total em df_pct_by_year

# preenche total com soma de todas as espécies daquele quinquenio
for (j in c(1:58)){
  
  # definir ano inicial e final do quinquenio
  ano0 <- as.numeric(rownames(df_pct_by_year)[j])
  ano4 <- ano0+4
  
  subset(df_clean,df_clean$probable_year %in% c(ano0:ano4),) |> nrow() -> quant
  
  df_pct_by_year[j,'total'] <- as.numeric(quant)
}

# cria df_total apenas com ano e total de espécies do quinquenio
df_total <- select(df_pct_by_year,total)

# atualiza ao dividir todas as células por total de cada linha
for (j in 1:58){
df_pct_by_year[j,] <- df_pct_by_year[j,]/df_pct_by_year[j,31]
}

sum(df_pct_by_year[10,])-1

# exclui coluna total
df_pct_by_year$total <- NULL

# ---
# Prepara para gráfico
# # Transforma e melt, mantem o ano. Cria df2 dataframe para gráfico
df2 <- data.frame(x = rownames(df_pct_by_year),
                  df_pct_by_year)
df2 <- melt(df2, id.vars = 'x')

df2$x <- as.numeric(df2$x)

# ----

# install.packages("ggplot2")
# library(ggplot2)

ggplot(df2, aes(x = x, y = value, group=variable, color = variable)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ========================================================================
# analisar picos

# get highest values of df_pct_by_year
highest <- get_highest(df_pct_by_year,10)
# get positions of each of those values
find_value_positions(df_pct_by_year,highest[1])  
find_value_positions(df_pct_by_year,highest)  

# ----------
# ======================================
# print("Gráfico pronto")
# ==============================================================================
# Exportar gráfico pronto

# Pega tempo atual para nome do arquivo
tempo_atual <- format(Sys.time(), "%Y-%m-%d-%H-%M")

if(F){
  filename<-paste0("epi_by_year_",fonte,tempo_atual,".png")
  filename
  ggsave(filename=filename, plot=last_plot(), scale=1, width=2000, height = 1200, units="px")
  
  print("Imagem salva")
}
