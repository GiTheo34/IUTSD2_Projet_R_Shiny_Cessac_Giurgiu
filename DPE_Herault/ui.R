#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

ui <- fluidPage(
  titlePanel("Carte interactive des DPE dans l'Hérault"),
  
  tabsetPanel(
    id = "tabs",
    tabPanel("Carte",
             sidebarLayout(
               sidebarPanel(
                 textInput("code_postal", "Entrez un code postal :", ""),
                 actionButton("rechercher", "Rechercher"),
                 selectInput("ville", "Choisissez une ville :", choices = NULL),
                 actionButton("voir_rapport", "Voir le rapport sur la ville")
               ),
               
               mainPanel(
                 leafletOutput("carte")
               )
             )
    ),
    tabPanel("Rapport sur la ville", 
             h3(textOutput("nom_ville")),
             fluidRow(
               column(6, htmlOutput("rapport_ville")),   # Informations sur la ville
               column(6, imageOutput("image_ville"))    # Image de la ville
             ),
             h4("Répartition des étiquettes DPE"),
             plotOutput("graphique_dpe"),               # Graphique des étiquettes DPE
             h4("Classement de la ville"),
             htmlOutput("classement_ville")             # Classement par étiquette A
    )
  )
)
