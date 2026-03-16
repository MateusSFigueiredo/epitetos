# ==============================================================================
# Arquivo: analysis_02_year.R
#
# Input: df_year_ produzido por data_04_gbif_year
# Output: graficos
# plot 1: absolute values of each epithet by 5 year
# plot 2: relative values of each epither by 5 year
# plot 3: year by year total amount of species
#
# Modificado em: 2026-03-15
# Autor: Mateus Silva Figueiredo

# dif: 
# permite offset nos quinquenios
# reorder script para impor data no começo
# salva os três gráficos como arquivos com data no nome

# ==============================================================================

# Last file of df_year
list.files(pattern = "df_year")[length(list.files(pattern = "df_year"))]

# Import data # a few seconds
dados_year <- read.csv2(list.files(pattern = "df_year")[length(list.files(pattern = "df_year"))],
                        sep=",")

# ==============================================================================

# define parâmetros da análise

offset <- 0 # 0 a 4, para tentar outros quinquenios
ultimo_ano <- 2019-offset # ultimo ano da análise. decidi 2020
primeiro_ano <- 1735-offset # primeiro ano da análise
per <- 5 # periodo de cada intervalo. 5 para quinquenios
n_epi <- 30 # top quantos epitetos especificos analisar?
n_bins <- ((ultimo_ano+1)-primeiro_ano)/per # n_bins = numero de intervalos

{ # roda tudo até grafico 1

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
library(dplyr) # for arrange
library(tidyr) # for %>% pipe
library(stringr)

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

# ---
# Prepara cores para gráfico
library(scales)  # for hue_pal()
# Create darker versions of the default colors
n_vars <- length(unique(df1$variable))
line_colors <- hue_pal()(n_vars)  # Default ggplot colors
# altered_colors <- colorspace::darken(line_colors, amount = 0.005)
altered_colors <- colorspace::lighten(line_colors, amount = 0.1)

# ----
# make plot 1 - with symbols on peaks and valleys

# Define custom year lists for each line
custom_points <- list(
  "gracilis" = c(1850,1860,1910,1930,2015),  # Multiple years for gracilis # 1910 peak. 1960 pico é artefato
  "elegans" = c(1830,1885,2015),
  "minor" = 2015,
  "simplex" = c(2015),
  "similis" = c(2015), # 1965 pico é artefato
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
  "brasiliensis" = c(2015), # vale em 1890 é artefato
  "minutus" = 2015,
  "mirabilis" = c(2015),
  "sinensis" = c(1915,2015),
  "variabilis" = c(1915,2015),
  "montana" = c(2015),
  "minima" = c(2015),
  "elongatus" = 2015,
  "parva" = 2015,
  "insignis" = 2015
)

# Add offset to every number in the list
custom_points <- lapply(custom_points, function(x) x - offset)

# Create points dataset - FIXED VERSION
df1_points <- data.frame()
for(var in unique(df1$variable)) {
  if(var %in% names(custom_points)) {
    temp <- df1[df1$variable == var & df1$x %in% custom_points[[var]], ]
    df1_points <- rbind(df1_points, temp)
  }
}

# Fazer gráfico
plot1 <- ggplot(df1, aes(x = x, y = value, group = variable, color = variable)) +
  geom_line() +
  geom_point(data = df1_points,
             aes(x = x, y = value, group = variable, shape = variable),
             color = altered_colors[as.numeric(factor(df1_points$variable))],
             size = 2) +
  scale_shape_manual(values = rep(c(16,17,15,23,8), 
                                  length.out = n_vars),
                     guide = guide_legend(override.aes = list(color = altered_colors))) +  # Fix legend colors

    # linhas verticais opcionais
  #  geom_vline(xintercept = seq(from = 1870, to = 1930, by = 5),linetype = "solid", color = "gray70",alpha = 0.7) +
  
  # show every x-axis value
#  scale_x_continuous(breaks = seq(min(df1$x), max(df1$x), by = 5))  + # every 5 years # apenas para análise. comentar para publicação
  
  # Change titles
  labs(
    title = paste("offset =", offset),
    x = "Year",                    # X axis title
    y = "Number of occurrences",    # Y axis title
    color = "Epithet",              # Legend title for color
    shape = "Epithet"               # Legend title for shape
  ) +
  
  ggtitle(paste0("Common epithets by ",per,"-year-period (absolute)")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_line(color = "white", size = 0.5),  # Major grid lines every 5 years
    panel.grid.minor.x = element_blank()  # Remove minor grid lines
  )

  
plot1;print("gráfico 1 feito")

# esse aqui em cima tá muito legal

} # fim roda tudo ate grafico 1

# ==============================================================================
{ # começo do plot 2
  
# e se df_epi_by_year for em porcentagem de nomes descritos para aquele quinquenio?
# criar df_pct_by_year
df_pct_by_year <- df_epi_by_year

# cria coluna total em df_pct_by_year

# preenche total com soma de todas as espécies daquele quinquenio
for (j in c(1:n_bins)){
  
  # definir ano inicial e final do quinquenio
  ano0 <- as.numeric(rownames(df_pct_by_year)[j])
  ano4 <- ano0+per-1
  
  subset(df,df$probable_year %in% c(ano0:ano4),) |> nrow() -> quant
  
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

# Define custom year lists for each line
custom_points <- list(
  "gracilis" = c(1845,1930,2015),  # Multiple years for gracilis # 1910 peak. 1960 pico é artefato # 1850,1860,1910,1930,
  "elegans" = c(1825,1880,2015),# 1830,1885,
  "minor" = 2015,
  "simplex" = c(2015),
  "similis" = c(2015), # 1965 pico é artefato
  "australis" = c(2015), # 1810,
  "orientalis" = c(1750,1765,2015), # 1750,
  "bicolor" = c(1805,2015), # 1800,
  "minuta" = c(2015),
  "affinis" = c(2015),
  "elongata" = 2015,
  "grandis" = 2015,
  "intermedia" = 2015,
  "robusta" = 2015,
  "indica" = c(1750,2015), # 1750,
  "occidentalis" = 2015,
  "tenuis" = 2015,
  "major" = 2015,
  "japonica" = c(1780,2015), # 1780,
  "insularis" = 2015,
  "brasiliensis" = c(2015), # vale em 1890 é artefato
  "minutus" = 2015,
  "mirabilis" = c(2015),
  "sinensis" = c(1915,2015), # 1915,
  "variabilis" = c(2015), # 1915
  "montana" = c(2015),
  "minima" = c(2015),
  "elongatus" = 2015,
  "parva" = 2015,
  "insignis" = 2015
)

# Add offset to every number in the list
custom_points <- lapply(custom_points, function(x) x - offset)

# ----

# install.packages("ggplot2")
# library(ggplot2)

# ---

# plot 2 simples, apenas linhas

if(F){
ggplot(df2, aes(x = x, y = value, group=variable, color = variable)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
# ------------

# plot 2 complexo

# Create points dataset - FIXED VERSION
df2_points <- data.frame()
for(var in unique(df2$variable)) {
  if(var %in% names(custom_points)) {
    temp <- df2[df2$variable == var & df2$x %in% custom_points[[var]], ]
    df2_points <- rbind(df2_points, temp)
  }
}


# Fazer gráfico
plot2 <- ggplot(df2, aes(x = x, y = value, group = variable, color = variable)) +
  geom_line() +
  geom_point(data = df2_points,
             aes(x = x, y = value, group = variable, shape = variable),
             color = altered_colors[as.numeric(factor(df2_points$variable))],
             size = 2) +
  scale_shape_manual(values = rep(c(16,17,15,23,8), 
                                  length.out = n_vars),
                     guide = guide_legend(override.aes = list(color = altered_colors))) +  # Fix legend colors
  
  # linhas verticais opcionais
  #  geom_vline(xintercept = seq(from = 1870, to = 1930, by = 5),linetype = "solid", color = "gray70",alpha = 0.7) +
  
  # show every x-axis value
  #  scale_x_continuous(breaks = seq(min(df1$x), max(df1$x), by = 5))  + # every 5 years # apenas para análise. comentar para publicação
  
  # Change titles
  labs(
    title = paste("offset =", offset),
    x = "Year",                    # X axis title
    y = "Frequency of occurrences",    # Y axis title
    color = "Epithet",              # Legend title for color
    shape = "Epithet"               # Legend title for shape
  ) +
  
  ggtitle(paste0("Common epithets by ",per,"-year-period (proportional)")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_line(color = "white", size = 0.5),  # Major grid lines every 5 years
    panel.grid.minor.x = element_blank()  # Remove minor grid lines
  )


print(plot2);print("gráfico 2 feito")

} # fim do plot 2

# ====================================================================

# Exportar gráfico pronto

# Pega tempo atual para nome do arquivo
tempo_atual <- format(Sys.time(), "%Y-%m-%d-%H-%M")

# salva plot1
if(T){
  filename<-paste0("graph_02_1_n_",tempo_atual,".png")
  filename
  ggsave(filename=filename, plot=plot1, scale=1, width=2000, height = 1400, units="px")
  
  print("Imagem salva")
}

# salva plot 2
if(T){
  filename<-paste0("graph_02_2_f_",tempo_atual,".png")
  filename
  ggsave(filename=filename, plot=plot2, scale=1, width=2000, height = 1400, units="px")
  
  print("Imagem salva")
}

# ===================================================================
# Analisar outros intervalos e períodos
# df tem todas as espécies sendo analisadas

# cria df_tot_by_year
df_tot_by_year <- as.data.frame(matrix(nrow = 2023-1735+1,ncol=2))
colnames(df_tot_by_year) <- c("year","total")
df_tot_by_year$year <- c(1735:2023)

# preenche df_tot_by_year
for (i in 1:nrow(df_tot_by_year)){
df_tot_by_year$total[i] <- sum(df$probable_year==df_tot_by_year$year[i])}

# subset para cortar anos recentes
df_tot_by_year <- subset(df_tot_by_year, year <= 2019)
# porque queda indica info faltando na base de dados do gbif



# ---
# --- Graph by year with smoothed red line and gray rectangles with years
h1 <- 0.28 # 1753 e 1758 e 1775
h2 <- 0.18 # 1804
h3 <- 0.08 # 1844 e 1859
h4 <- 0.08 # 1914 e 1939

h1 <- 0.9 # 1753 e 1758 e 1775
h2 <- 0.9 # 1804
h3 <- 0.9 # 1844 e 1859
h4 <- 0.9 # 1914 e 1939


plot3 <- ggplot(df_tot_by_year, aes(x = year, y = total)) +
  geom_line(alpha = 1, size=0.5,color = "black") +  # Raw data (faint)
  geom_smooth(se = FALSE, color = "red",alpha=0.9, size = 0.5,span = 0.1, method = "loess") +  # Default method # Lower span = less smoothing, higher = more smoothing 
  #uncomment previous geom_smooth line to generate RED_SMOOTH graph
  scale_x_continuous(limits = c(1735, 2020)) +
  labs(
    title = "Total species by year",
#    subtitle = "Red line: smoothed; Black line: raw data",
    x = "Year",
    y = "Number of occurrences"
  ) + 
  annotate("rect",
           xmin = 1939, xmax = 1945,
           ymin = -Inf, ymax = Inf,  # Covers full y-range
           alpha = 0.2,               # Transparency
           fill = "gray50") +          # Grey color
  annotate("text", x = 1941.5, y = max(df_tot_by_year$total) * h4,
           label = "1939 -1945", color = "red", size = 3,angle=90) +
  annotate("rect",
           xmin = 1914, xmax = 1918,
           ymin = -Inf, ymax = Inf,  # Covers full y-range
           alpha = 0.2,               # Transparency
           fill = "gray50") +          # Grey color
  annotate("text", x = 1916, y = max(df_tot_by_year$total) * h4,
           label = "1914 -1918", color = "red", size = 3,angle=90) +
  
  annotate("rect",
           xmin = 1752.5, xmax = 1753.5,
           ymin = -Inf, ymax = Inf,  # Covers full y-range
           alpha = 0.2,               # Transparency
           fill = "gray40") +          # Grey color
  annotate("text", x = 1753, y = max(df_tot_by_year$total) * h1,
           label = "1753", color = "red", size = 3,angle=90) +
  annotate("rect",
           xmin = 1757.5, xmax = 1758.5,
           ymin = -Inf, ymax = Inf,  # Covers full y-range
           alpha = 0.2,               # Transparency
           fill = "gray40") +          # Grey color
  annotate("text", x = 1758, y = max(df_tot_by_year$total) * h1,
           label = "1758", color = "red", size = 3,angle=90) +
  annotate("rect",
           xmin = 1774.5, xmax = 1775.5,
           ymin = -Inf, ymax = Inf,  # Covers full y-range
           alpha = 0.2,               # Transparency
           fill = "gray40") +          # Grey color
  annotate("text", x = 1775, y = max(df_tot_by_year$total) * h1,
           label = "1775", color = "red", size = 3,angle=90) +

  annotate("rect",
           xmin = 1859, xmax = 1872,
           ymin = -Inf, ymax = Inf,  # Covers full y-range
           alpha = 0.2,               # Transparency
           fill = "gray82") +          # Grey color
  annotate("text", x = 1865.5, y = max(df_tot_by_year$total) * h3,
           label = "1859 -1872", color = "red", size = 3,angle=90,alpha=0.5) +
  
annotate("rect",
         xmin = 1844, xmax = 1848,
         ymin = -Inf, ymax = Inf,  # Covers full y-range
         alpha = 0.2,               # Transparency
         fill = "gray82") +          # Grey color
  annotate("text", x = 1846, y = max(df_tot_by_year$total) * h3,
           label = "1844 -1848", color = "red", size = 3,angle=90,alpha=0.5) +

  annotate("rect",
           xmin = 1804, xmax = 1815,
           ymin = -Inf, ymax = Inf,  # Covers full y-range
           alpha = 0.2,               # Transparency
           fill = "gray82") +          # Grey color
  annotate("text", x = 1809.5, y = max(df_tot_by_year$total) * h2,
           label = "1804 -1815", color = "red", size = 3,angle=90, alpha=0.5) +
  
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

print(plot3)

# ---------------------
# Pega tempo atual para nome do arquivo
tempo_atual <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S")

# salva plot3
if(T){
  filename<-paste0("graph_02_3_t_",tempo_atual,".png")
  filename
  ggsave(filename=filename, plot=plot3, scale=1, width=2000, height = 1400, units="px")
  }
  



# ====================================================================
# rascunho
sum(is.na(dados_year$probable_year))
paste(sum(is.na(dados_year$probable_year)),"linhas com probable_year NA em dados_year")

# top 30 espécies em df_accepted
table(df_accepted$specific_epithet) %>% as.data.frame() %>% arrange(desc(Freq)) %>% head(30)

# top 30 espécies em df_prob
table(df_prob$specific_epithet) %>% as.data.frame() %>% arrange(desc(Freq)) %>% head(30)

# --------
# graphs

# ----------------
# plot 1 but focus on 1930-1960

ggplot(df1, aes(x = x, y = value, group = variable, color = variable)) +
  geom_line() +
  scale_x_continuous(limits = c(1890, 1970)) +
  ggtitle("Guerras?") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# plot lines total
plot(df_tot_by_year$total~df_tot_by_year$year,
     type='l',
     xlim=c(1735,2020),
     #     ylim=c(0,50000),
     xlab="Years",
     ylab="Number of species",
     main="Species per year")

# plot lines total
plot(df_tot_by_year$total~df_tot_by_year$year,
     type='l',
     xlim=c(1930,1960),
     #     ylim=c(0,50000),
     xlab="Years",
     ylab="Number of species",
     main="Species per year")

ggplot(df_tot_by_year, aes(x = year, y = total)) +
  geom_line() +
  scale_x_continuous(limits = c(1735, 2020)) +
  labs(
    title = "Total species by year",
    x = "Year",                    # X axis title
    y = "Number of occurrences"
  ) + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))


ggplot(df_tot_by_year, aes(x = year, y = total)) +
  geom_line() +
  scale_x_continuous(limits = c(1890, 1970)) +
  ggtitle("Guerras?") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))