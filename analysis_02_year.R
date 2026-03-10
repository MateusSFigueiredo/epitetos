# ==============================================================================
# Arquivo: analysis_02_year.R
#
# Input: df_year_ produzido por data_04_gbif_year
# Output: a fazer
#
# Modificado em: 2026-03-03
# Autor: Mateus Silva Figueiredo

# dif: 
# fixed line 211. subset a partir de df_top em ambas as condições.
# muda ordem de subset em 142-156 para manter df_accepted útil

# ==============================================================================
# define parâmetros da análise

ultimo_ano <- 2019 # ultimo ano da análise. decidi 2020
primeiro_ano <- 1735 # primeiro ano da análise
per <- 5 # periodo de cada intervalo. 5 para quinquenios
n_epi <- 30 # top quantos epitetos especificos analisar?
n_bins <- ((ultimo_ano+1)-primeiro_ano)/per # n_bins = numero de intervalos

# confere parâmetros
paste0("ultimo ano = ",ultimo_ano,
      ". primeiro ano = ",primeiro_ano,
      ". per = ",per,
      ". n epitetos = ",n_epi,
      ". n bins = ",n_bins)

# decisão: usar 1735-2019, 5 e talvez 30 para epitetos
# usar 1 para gráfico de total por ano

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
# df <- dados_year[1:1000,] # menor para testes

# conferir colunas
colnames(df)

# remove epiteto spec
df <- subset(df,df$specific_epithet!="spec")
paste(nrow(df),"linhas sem epiteto spec")

# filtrar taxonomic status apenas accepted
paste(sum(df$taxonomic_status=="accepted"),"linhas com taxonomic status accepted")
df_accepted <- subset(df,df$taxonomic_status=="accepted")
df <- df_accepted

# pegar apenas probable_year preenchido
paste(sum(is.na(df$probable_year)),"linhas com NA em probable_year")
paste(sum(!is.na(df$probable_year)),"linhas com probable_year preenchido")

df_prob <- df[!is.na(df$probable_year),]
df <- df_prob

# ==============================================================================

# Step 1: Create a frequency table for the 'specific_epithet' column and order it by frequency
# usar df_accepted para usar mais espécies aceitas, mesmo que ano desconhecido
freq_table <- table(df_accepted$specific_epithet)

# Convert the table to a data frame and order by frequency (in descending order)
freq_df <- as.data.frame(freq_table) %>%
  arrange(desc(Freq))

# View the ordered frequency table
# freq_df %>% head(20)
# View(freq_df)

n_epi
top_specific_epithets <- as.vector(head(freq_df$Var1,n_epi))
top_specific_epithets
top_specific_epithets[2] # pegar epiteto arbitrario

# ------------------------------------------------------------------------------

# subset df apenas epitetos no top

df_top <- df[df$specific_epithet %in% top_specific_epithets,]

# df <- df_top

# ======================================
# cria df_epi_by_year
# colunas = cada epiteto no top
# linhas = cada ano do começo de cada quinquenio

# cria df_epi_by_year com base em quantos intervalos (bins) e quantos epitetos
df_epi_by_year <- as.data.frame(matrix(nrow=n_bins,ncol=n_epi))

# nome das colunas = top n_epi epitetos
colnames(df_epi_by_year) <- c(top_specific_epithets)

# nome das linhas = primeiro ano de cada intervalo (bin)
rownames(df_epi_by_year) <- c(seq(primeiro_ano,ultimo_ano-per+1,per))


# ======================================

# for loop para preencher df_epi_by_year
# i <- 1  # epitetos
# j <- 36 # ano inicial do quinquenio

for (i in c(1:n_epi)){
  for (j in c(1:n_bins)){
    
    # definir ano inicial e final do quinquenio
    ano0 <- as.numeric(rownames(df_epi_by_year)[j])
    ano4 <- ano0+per-1 #para final do periodo
    
    subset(df_top,df_top$specific_epithet==top_specific_epithets[i] & df_top$probable_year %in% c(ano0:ano4),) |> nrow() -> quant
    
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

# make plot 1 - somente linhas

if(F){ # mudar para T se quiser fazer o gráfico sem graça
ggplot(df1, aes(x = x, y = value, group=variable, color = variable)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }

# -------------------------------------------------
# plot 1 with symbols on peaks and valleys

# Define custom year lists for each line
custom_points <- list(
  "gracilis" = c(1765,1850,1860,1910,1930,1960,2015),  # Multiple years for gracilis # 1910 peak
  "elegans" = c(1830,1885,2015),
  "minor" = 2015,
  "simplex" = c(2015),
  "similis" = c(1965,2015),
  "australis" = c(1810,2015),
  "orientalis" = c(1750,2015),
  "bicolor" = c(1800,2015),
  "minuta" = c(2015),
  "affinis" = c(2015),
  "elongata" = 2015,
  "grandis" = 2015,
  "intermedia" = 2015,
  "robusta" = 2015,
  "indica" = c(1750,2015),
  "occidentalis" = 2015,
  "tenuis" = 2015,
  "major" = 2015,
  "japonica" = c(1780,2015),
  "insularis" = 2015,
  "brasiliensis" = c(1890,2015),
  "minutus" = 2015,
  "mirabilis" = 2015,
  "sinensis" = c(1915,2015),
  "variabilis" = 2015,
  "montana" = 2015,
  "minima" = 2015,
  "elongatus" = 2015,
  "parva" = 2015,
  "insignis" = 2015
)

# Create points dataset - FIXED VERSION
df1_points <- df1 %>%
  group_by(variable) %>%
  filter(x %in% custom_points[[unique(variable)]]) %>%  # Remove [1] to use current group
  ungroup()

# Fazer gráfico
ggplot(df1, aes(x = x, y = value, group = variable, color = variable)) +
  geom_line() +
  geom_point(data = df1_points,
             aes(x = x, y = value, group = variable, color = variable, shape = variable),
             size = 2) +
  scale_shape_manual(values = rep(c(16, 17, 15, 23), 
                                  length.out = length(unique(df1$variable)))) +
  
  # linhas verticais opcionais
  #  geom_vline(xintercept = seq(from = 1870, to = 1930, by = 5),linetype = "solid", color = "gray70",alpha = 0.7) +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# esse aqui em cima tá muito legal

print("gráfico feito")

# ==============================================================================

# e se df_epi_by_year for em porcentagem de nomes descritos para aquele quinquenio?
# criar df_pct_by_year
df_pct_by_year <- df_epi_by_year

# cria coluna total em df_pct_by_year

# preenche total com soma de todas as espécies daquele quinquenio
for (j in c(1:n_bins)){
  
  # definir ano inicial e final do quinquenio
  ano0 <- as.numeric(rownames(df_pct_by_year)[j])
  ano4 <- ano0+per-1
  
  subset(df_clean,df_clean$probable_year %in% c(ano0:ano4),) |> nrow() -> quant
  
  df_pct_by_year[j,'total'] <- as.numeric(quant)
} # takes a bit of time

# cria df_total apenas com ano e total de espécies do quinquenio
df_total <- select(df_pct_by_year,total)

# atualiza ao dividir todas as células por total de cada linha
for (j in 1:n_bins){
df_pct_by_year[j,] <- df_pct_by_year[j,]/df_pct_by_year[j,n_epi+1]
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
print("Gráfico pronto")
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

# ===========================================
# Gráficos de intervalos menores

# plot lines total
plot(df_total$total~rownames(df_total),
     type='l',
     #     xlim=c(1890,1970),
     #     ylim=c(0,50000),
     main="Novas espécies por ano")

# plot 1 but focus on 1930-1960

ggplot(df1, aes(x = x, y = value, group = variable, color = variable)) +
  geom_line() +
  scale_x_continuous(limits = c(1890, 1970)) +
  ggtitle("Guerras?") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# ===========
# rascunho
sum(is.na(dados_year$probable_year))
paste(sum(is.na(dados_year$probable_year)),"linhas com probable_year NA em dados_year")

# top 30 espécies em df_accepted
table(df_accepted$specific_epithet) %>% as.data.frame() %>% arrange(desc(Freq)) %>% head(30)

# top 30 espécies em df_prob
table(df_prob$specific_epithet) %>% as.data.frame() %>% arrange(desc(Freq)) %>% head(30)
