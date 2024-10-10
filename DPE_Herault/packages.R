install.packages(c("httr", "jsonlite"))
install.packages("bslib")
install.packages("mapsapi")
install.packages("remotes")
remotes::install_github("michaeldorman/mapsapi")
install.packages("shiny")
install.packages("leaflet")
install.packages("opencage")
install.packages("kableExtra")
install.packages("tidyr")
install.packages("knitr")
library(shiny)
library(httr)
library(jsonlite)
library(bslib)
library(mapsapi)
library(leaflet)
library(dplyr)
library(ggplot2)
library(leaflet)
library(opencage)
library(kableExtra)
library(tidyr)
library(knitr)

# Remplacez ceci par vos identifiants de connexion réels
username <- "admin"
password <- "admin"

herault = read.csv(file = "C:/Users/Théo/OneDrive/Bureau/BUT/2ème année/R shiny/Projet-R-Herault/adresses-34.csv", header = TRUE, sep = ";")
cp_herault = unique(herault$code_postal)
df= data.frame()

for (i in cp_herault) {
  base_url <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe-v2-logements-existants/lines"
  # Paramètres de la requête
  params <- list(
    page = 1,
    size = 10000,
    select = "Identifiant__BAN,Code_postal_(BAN),N°DPE,Etiquette_DPE,Date_réception_DPE,Année_construction,Surface_habitable_logement,Type_bâtiment",
    q = i,
    q_fields = "Code_postal_(BAN)"
  ) 
  
  # Encodage des paramètres
  url_encoded <- modify_url(base_url, query = params)
  
  # Effectuer la requête
  response <- GET(url_encoded)
  
  # On convertit le contenu brut (octets) en une chaîne de caractères (texte). Cela permet de transformer les données reçues de l'API, qui sont généralement au format JSON, en une chaîne lisible par R
  content = fromJSON(rawToChar(response$content), flatten = FALSE)
  
  # Afficher les données récupérées
  df = rbind(df, content$result)
}

df$id = df$Identifiant__BAN

df <- merge(df, herault[, c("id", "lon", "lat", "numero", "rep", "nom_voie", "nom_commune")], by = "id", all.x = TRUE)

data_dpe = df[,-2]

# Convertir l'année actuelle en numérique pour effectuer des calculs
annee_actuelle <- as.numeric(format(Sys.Date(), "%Y"))

# Ajouter la colonne Ancien_Neuf
data_dpe <- data_dpe %>%
  mutate(
    Ancien_Neuf = ifelse(Année_construction >= (annee_actuelle - 10), "Neuf", "Ancien")
  )

write.table(df, file = "DPE_Herault.csv", col.names = TRUE, row.names = FALSE, sep = ";", dec = ".")
