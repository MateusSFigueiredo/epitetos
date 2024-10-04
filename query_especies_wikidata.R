# Obter lista de espécies do Wikidata
# usando pacote WikidataQueryServiceR

# Deu erro: 
# Warning message:
#  One or more parsing issues

# ==============================================================================

# Instala pacotes
# install.packages("WikidataQueryServiceR")
library(WikidataQueryServiceR)

# Corre query
df<-query_wikidata('SELECT DISTINCT ?item ?nome_do_taxon WHERE {
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE]". }
  {
    SELECT DISTINCT ?item WHERE {
      ?item p:P105 ?statement0.
      ?statement0 (ps:P105/(wdt:P279*)) wd:Q7432.
    }

  }
  OPTIONAL { ?item wdt:P225 ?nome_do_taxon. }
} ') # fim do query


nrow(df) # 1723499
Sys.time() # 2024-10-03 18:18:11 -0
# Em 2024-10-03 foram obtidas 1723499 rows com espécies do Wikidata

write.csv(df,"query_r_1723499.csv")
