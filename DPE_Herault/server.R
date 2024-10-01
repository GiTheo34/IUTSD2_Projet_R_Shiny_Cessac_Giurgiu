#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

server <- function(input, output, session) {
  
  # Fonction pour générer une icône en fonction de l'étiquette DPE
  generate_icon <- function(etiquette) {
    icons <- list(
      A = "www/img/DPE A.png",
      B = "www/img/DPE B.png",
      C = "www/img/DPE C.png",
      D = "www/img/DPE D.png",
      E = "www/img/DPE E.png",
      F = "www/img/DPE F.png",
      G = "www/img/DPE G.png"
    )
    return(icons[etiquette])
  }
  
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
          addMarkers(
            lng = ~lon,
            lat = ~lat,
            icon = ~generate_icon(Etiquette_DPE),  # Utilisation des icônes
            popup = ~paste("Code Postal:", `Code_postal_(BAN)`, "<br>",
                           "Ville:", nom_commune, "<br>",
                           "<img src='", icons(Etiquette_DPE),">")
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
  
  # Mettre à jour les choix de ville en fonction du code postal
  observeEvent(input$rechercher, {
    req(input$code_postal)
    
    communes <- df %>%
      filter(`Code_postal_(BAN)` == input$code_postal) %>%
      select(nom_commune) %>%
      distinct() %>%
      pull(nom_commune)
    
    updateSelectInput(session, "ville", choices = communes)
  })
  
  observeEvent(input$voir_rapport, {
    req(input$ville)
    
    updateTabsetPanel(session, "tabs", selected = "Rapport sur la ville")
    
    filtered_data <- df %>%
      filter(nom_commune == input$ville)
    
    nombre_dpe <- nrow(filtered_data)
    moyenne_dpe <- round(mean(as.numeric(factor(filtered_data$Etiquette_DPE, 
                                                levels = c("A", "B", "C", "D", "E", "F", "G")))), 2)
    coordonnees <- paste("Latitude:", mean(filtered_data$lat), "Longitude:", mean(filtered_data$lon))
    
    rapport_html <- paste0(
      "<div style='padding: 20px;'>",
      "<h4>Informations principales pour la ville de ", input$ville, "</h4>",
      "<ul>",
      "<li><strong>Nombre total de DPE enregistrés :</strong> ", nombre_dpe, "</li>",
      "<li><strong>Moyenne de l'étiquette DPE :</strong> ", moyenne_dpe, "</li>",
      "<li><strong>Coordonnées moyennes :</strong> ", coordonnees, "</li>",
      "</ul>",
      "</div>"
    )
    
    output$rapport_ville <- renderUI({
      HTML(rapport_html)
    })
    
    output$nom_ville <- renderText({
      paste("Rapport sur la ville de", input$ville)
    })
    
    # Répartition des étiquettes DPE avec ggplot2
    output$graphique_dpe <- renderPlot({
      ggplot(filtered_data, aes(x = Etiquette_DPE)) +
        geom_bar(fill = "steelblue") +
        labs(title = paste("Répartition des étiquettes DPE pour", input$ville),
             x = "Étiquette DPE", y = "Nombre de logements") +
        theme_minimal()
    })
    
    # Image dynamique de la ville via Wikimedia API
    output$image_ville <- renderImage({
      
      # Appel à l'API Wikimedia pour récupérer une image
      ville <- input$ville
      query <- paste0("https://en.wikipedia.org/w/api.php?action=query&prop=pageimages&format=json&piprop=original&titles=", URLencode(ville))
      res <- GET(query)
      data <- fromJSON(content(res, "text"))
      
      # Extraire l'URL de l'image, si trouvée
      page <- data$query$pages
      image_url <- NULL
      for (p in page) {
        if (!is.null(p$original)) {
          image_url <- p$original$source
          break
        }
      }
      
      # Si une image est trouvée
      if (!is.null(image_url)) {
        temp_image <- tempfile(fileext = ".jpg")
        download.file(image_url, temp_image, mode = "wb")
        
        list(
          src = temp_image,
          alt = paste("Image de la ville de", input$ville),
          width = 400, height = 300
        )
      } else {
        list(
          src = "https://via.placeholder.com/400x300?text=Pas+d'image",
          alt = "Pas d'image trouvée pour cette ville",
          width = 400, height = 300
        )
      }
      
    }, deleteFile = TRUE)  # Supprimer l'image temporaire après affichage
    
    # Classement de la ville en fonction du pourcentage de DPE A
    df_classement <- df %>%
      group_by(nom_commune) %>%
      summarise(pourcentage_A = mean(Etiquette_DPE == "A") * 100) %>%
      arrange(desc(pourcentage_A))
    
    rang_ville <- df_classement %>%
      filter(nom_commune == input$ville) %>%
      pull(pourcentage_A)
    
    classement <- which(df_classement$nom_commune == input$ville)
    
    classement_html <- paste0(
      "<div style='padding: 20px;'>",
      "<p><strong>Pourcentage de logements avec étiquette A :</strong> ", round(rang_ville, 2), "%</p>",
      "<p><strong>Classement :</strong> ", classement, "ème sur ", nrow(df_classement), " communes.</p>",
      "</div>"
    )
    
    output$classement_ville <- renderUI({
      HTML(classement_html)
    })
  })
  
  # Initialiser la carte vide
  output$carte <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 3.8772, lat = 43.6119, zoom = 9) # Vue centrée sur l'Hérault
  })
}

