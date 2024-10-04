# Arquivo: especies_col.R

# Obter lista de espécies do Catalogue of Life
# Cria colunas "rank" "id" "syn" "author" "year" "taxon_name"
# Complexo desnecessáriamente

# Usa arquivo dataset-303642.txtree
# Obtido em https://www.catalogueoflife.org/data/download opção TextTree

# Modificado em: 2024_10_03
# Autor: Mateus Silva Figueiredo

# ==============================================================================
# Setup

getwd()
list.files()

library(stringr)

# ==============================================================================

# Read the file
lines <- readLines("dataset-303642-part.txtree")

# Convert the lines into a data frame
df <- data.frame(name = lines, stringsAsFactors = FALSE)

# ==============================================================================
# Separate into three columns: name, rank and id

# Extract whatever is between [] and place it into a new column
df$rank <- str_extract(df$name, "\\[([^\\]]+)\\]")

# Extract whatever is between {} and place it into another column
df$id <- str_extract(df$name, "\\{([^\\}]+)\\}")

# Clean the original Line column by removing the content inside brackets and braces
df$name <- gsub("\\{([^\\}]+)\\}", "", df$name)
df$name <- gsub("\\[([^\\}]+)\\]", "", df$name)

# ------------------------------------------------------------------------------

# Create column syn for taxons that are synonyms of other taxons
# has "=" == is synonim == TRUE
df$syn<-grepl("=",df$name,fixed=TRUE)

# View the first few rows of the data frame
head(df)

# Trim white spaces
df$name <- trimws(df$name)

# ==============================================================================

# Subsetting the data frame where 'rank' equals "[species]" and 'syn' equals FALSE
species <- df[df$rank == "[species]" & df$syn == FALSE, ]

head(species)
# ------------------------------------------------------------------------------
# create column author
# name's words 3 forward become author
species$author <- word(species$name, start=3, end=-1)
species$author <- gsub("[()]","",species$author) # remove ( and )

# last word becomes column year
species$year <- word(species$name, -1)
# remove )
species$year <- gsub(")","",species$year)

head(species)

# keep only two first word of species
species$taxon_name <- word(species$name, start = 1, end = 2)

species$name<-NULL
