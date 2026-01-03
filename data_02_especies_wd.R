# Arquivo: especies_wd_v2.R

# Trabalhar com lista de espécies do wikidata

# Usa arquivo wikidata_IX04x2.csv 
# Obtido em https://qlever.dev/wikidata/IX04x2#csv   
# Download em 31 dezembro 2025 = 3,268,594 lines found 

# Modificado em: 2025-01-31
# Autor: Mateus Silva Figueiredo

# ==============================================================================
# Setup
getwd()
list.files()

library(dplyr)

# Read the file

origem <- read.csv2("wikidata_udSVmA.csv",sep=",") # lines

df <- origem # manter origem imutavel

# ==============================================================================

colnames(df)

# ver instance of labels mais comuns
View(table(df$instance_of_label))

head(df)

# =======================
# quantas linhas tem todas as colunas id vazias?

# Define the ID columns
id_cols <- colnames(df)[c(5:66)]

# Create df of these empty lines # wait
empty_df <- df[
  apply(df[id_cols], 1, function(x) {
    all(is.na(x) | grepl("^\\s*$", x))
  }), 
]

# Count rows where ALL ID columns are empty/NA
empty_rows <- nrow(empty_df)

paste("Das",nrow(df),"linhas, apenas",empty_rows,"não tem nenhum id dentre", 
      "as vinte bases de dados analisadas. Ou seja,",round(empty_rows*100/nrow(df),2),"%.",
      "Sobram",nrow(df)-empty_rows,"linhas preenchidas.")

# =======================
# primeiro, remover todos que não são taxon ou fossil taxon

View(table(df$instance_of_label))

antes <- nrow(df)

df <- subset(df,df$instance_of_label %in% c("taxon","fossil taxon"))

depois <- nrow(df)

paste("Antes, df tinha",antes,"linhas. Depois, tem",depois,"linhas.",
      "Foram removidas",antes-depois,"linhas fora do escopo de taxon.")

# =======================
# testando unique
unique(df$item)

df_amostra <- head(df,50)

table(df_amostra$item) # percebe-se que http://www.wikidata.org/entity/Q1001586 é repetido

df_amostra_unique <- df_amostra %>% distinct(item, .keep_all=TRUE)

table(df_amostra_unique$item) # agora são todos únicos

# ------------------------
# aplicando unique para o df todo
# fazer novo df apenas com itens únicos

paste("Origem tem",nrow(origem),"linhas")

df <- df %>% distinct(item, .keep_all=TRUE)

paste("Origem tem",nrow(origem),"linhas.",
      "df com itens únicos tem",nrow(df),"linhas.",
      "Foram eliminados",nrow(origem)-nrow(df),"itens duplicados.")


# =======================

############### código antigo abaixo

# head(sort(table(df$instance_of_label), decreasing = TRUE),20)

# subset apenas com instance of mais pertinentes

cats <- c("taxon","fossil taxon","extinct taxon") # categorias aceitas

df <- df[df$instance_of_label %in% cats,]         # cria subset
# novo df = 3,203,003 lines

# View(table(df$instance_of_label))
# apenas 3 categorias, como esperado.

# quais são duplicados?
# which(duplicated(df$taxon_name))
# df[227,]
# df[df$taxon_name=="Horizocerus hartlaubi",]

# remove duplicados
df <- df[!duplicated(df$taxon_name), ]
# nrow(df) # 3201049 lines
# ==============================================================================

# deseja salvar arquivo csv?
if(T){write.csv(df, "wikidata_species.csv", row.names = FALSE); print("arquivo csv exportado")
}

print(paste("script especies_wd.R finalizado. nrow(df) ==",nrow(df)))
