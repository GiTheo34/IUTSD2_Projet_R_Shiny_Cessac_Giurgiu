#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

server <- function(input, output, session) {
  
  # Mettre à jour les choix de ville en fonction du code postal
  observeEvent(input$rechercher, {
    req(input$code_postal)
    
    # Filtrer les données en fonction du code postal
    communes <- df %>%
      filter(`Code_postal_(BAN)` == input$code_postal) %>%
      select(nom_commune) %>%
      distinct() %>%
      pull(nom_commune)
    
    # Mettre à jour les choix du selectInput pour les villes
    updateSelectInput(session, "ville", choices = communes)
    
    # Afficher la carte avec toutes les villes si aucun choix de ville n'est fait
    if (length(communes) > 0) {
      output$carte <- renderLeaflet({
        leaflet(df %>% filter(`Code_postal_(BAN)` == input$code_postal)) %>%
          addTiles() %>%
          addCircleMarkers(
            lng = ~lon,
            lat = ~lat,
            popup = ~paste("Code Postal:", `Code_postal_(BAN)`, "<br>",
                           "Ville:", nom_commune, "<br>",
                           "Etiquette DPE:", Etiquette_DPE),
            radius = 6,
            color = "blue",
            fillOpacity = 0.7
          )
      })
    } else {
      output$carte <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          setView(lng = 3.8772, lat = 43.6119, zoom = 9) # Vue centrée sur l'Hérault
      })
      showNotification("Aucune donnée trouvée pour ce code postal", type = "error")
    }
  })
  
  # Observer le changement de ville sélectionnée
  observeEvent(input$ville, {
    req(input$code_postal, input$ville)
    
    # Filtrer les données en fonction du code postal et de la ville
    filtered_data <- df %>%
      filter(`Code_postal_(BAN)` == input$code_postal,
             nom_commune == input$ville)
    
    # Créer la carte Leaflet avec les données filtrées
    if (nrow(filtered_data) > 0) {
      output$carte <- renderLeaflet({
        leaflet(filtered_data) %>%
          addTiles() %>%
          addCircleMarkers(
            lng = ~lon,
            lat = ~lat,
            popup = ~paste("Code Postal:", `Code_postal_(BAN)`, "<br>",
                           "Ville:", nom_commune, "<br>",
                           "Etiquette DPE:", Etiquette_DPE),
            radius = 6,
            color = "blue",
            fillOpacity = 0.7
          )
      })
    } else {
      # Si aucun résultat, réinitialiser la carte
      output$carte <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          setView(lng = 3.8772, lat = 43.6119, zoom = 9) # Vue centrée sur l'Hérault
      })
      showNotification("Aucune donnée trouvée pour cette ville", type = "error")
    }
  })
  
  # Générer le rapport sur la ville sélectionnée
  observeEvent(input$voir_rapport, {
    req(input$ville)
    
    # Naviguer vers l'onglet "Rapport sur la ville"
    updateTabsetPanel(session, "tabs", selected = "Rapport sur la ville")
    
    # Générer le rapport pour la ville sélectionnée
    rapport <- df %>%
      filter(nom_commune == input$ville) %>%
      summarise(
        Nombre_DPE = n(),
        Moyenne_Etiquette_DPE = paste0("Moyenne : ", round(mean(as.numeric(factor(Etiquette_DPE, levels = c("A", "B", "C", "D", "E", "F", "G")))), 2)),
        Coordonnées = paste("Latitude:", mean(lat), "Longitude:", mean(lon))
      )
    
    output$nom_ville <- renderText({
      paste("Rapport sur la ville de", input$ville)
    })
    
    output$rapport_ville <- renderPrint({
      rapport
    })
  })
  
  # Initialiser la carte vide
  output$carte <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 3.8772, lat = 43.6119, zoom = 9) # Vue centrée sur l'Hérault
  })
}





