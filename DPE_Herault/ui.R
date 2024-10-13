#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(HTML(
      "
      $(document).ready(function() {
        $('#themeSwitch').click(function() {
          $('body').toggleClass('dark-theme');
        });
      });
      "
    ))
  ),
  
  titlePanel("Analyse DPE"),
  
  # Page de connexion
  uiOutput("login_ui"),
  
  # Application principale
  uiOutput("app_ui")
)


app_ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$script(HTML(
      "
      $(document).ready(function() {
        $('#themeSwitch').click(function() {
          $('body').toggleClass('dark-theme');
        });
      });
      "
    ))
  ),
  
  titlePanel("Carte interactive des DPE dans l'Hérault"),
  
  # Ajouter le bouton de basculement de thème
  actionButton("themeSwitch", "Thème sombre", class = "btn"),
  
  tabsetPanel(
    id = "tabs",
    tabPanel("Carte",
             sidebarLayout(
               sidebarPanel(
                 textInput("code_postal", "Entrez un code postal :", ""),
                 actionButton("rechercher", "Rechercher"),
                 selectInput("ville", "Choisissez une ville :", choices = NULL),
                 actionButton("voir_rapport", "Voir le rapport sur la ville"),
                 downloadButton("telecharger_csv", label = "Télécharger datas de la ville"),
                 # Ajout du bouton pour mettre à jour les données
                 actionButton("update_data_btn", "Mettre à jour les données DPE"),
                 
                 # Ajout d'un indicateur de chargement
                 verbatimTextOutput("update_status")
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
               column(6, imageOutput("image_ville")),    # Image de la ville
               downloadButton("telecharger_image_ville", label = "Télécharger l'image de la ville")
             ),
             h4("Répartition des étiquettes DPE"),
             plotOutput("graphique_dpe"),               # Graphique des étiquettes DPE
             downloadButton("telecharger_graphique_dpe", label = "Télécharger le graphique des DPE"),
             h4("Classement de la ville"),
             htmlOutput("classement_ville"),             # Classement par étiquette A
             downloadButton("download_report", "Télécharger le rapport PDF"),
    ),
    tabPanel("Corrélation",
             selectInput("var1", "Choisir la première variable :", 
                         choices = names(data_dpe)[sapply(data_dpe, is.numeric)], 
                         selected = names(data_dpe)[sapply(data_dpe, is.numeric)][1]),
             selectInput("var2", "Choisir la deuxième variable :", 
                         choices = names(data_dpe)[sapply(data_dpe, is.numeric)], 
                         selected = names(data_dpe)[sapply(data_dpe, is.numeric)][2]),
             actionButton("generate_corr_plot", "Générer le graphique de corrélation"),
             plotOutput("corr_plot"),
             downloadButton("download_corr_plot", "Télécharger le graphique")
    )
  )
)
