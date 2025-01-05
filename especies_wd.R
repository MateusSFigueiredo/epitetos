# Arquivo: especies_wd.R

# Trabalhar com lista de espécies do wikidata

# Usa arquivo wikidata_SRYMqT.csv 
# Obtido em https://qlever.cs.uni-freiburg.de/wikidata/SRYMqT  
# Download em 2 Janeiro 2025 = 3,212,014 results 

# Modificado em: 2025-03-01
# Autor: Mateus Silva Figueiredo

# ==============================================================================
# Setup
getwd()
list.files()

# Read the file
# df <- read.csv2("wikidata_h7tFUI.csv",sep=",") #3,212,014 lines, sem taxon label

df <- read.csv2("wikidata_y4hmyU.csv",sep=",") #3,212,006 lines

# ==============================================================================
# mudar nome coluna
colnames(df) <- gsub("Catalogue_of_Life_ID","col_id",colnames(df))

# ver instance of labels mais comuns
# View(table(df$instance_of_label))

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
