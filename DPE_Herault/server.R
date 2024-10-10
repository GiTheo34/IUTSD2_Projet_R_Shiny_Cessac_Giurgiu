#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

server <- function(input, output, session) {
  
  # Variable pour contrôler l'état de connexion
  user_authenticated <- reactiveVal(FALSE)
  
  # Page de connexion
  output$login_ui <- renderUI({
    if (!user_authenticated()) {
      fluidRow(
        column(4, 
               textInput("login_user", "Nom d'utilisateur"),
               passwordInput("login_pass", "Mot de passe"),
               actionButton("login_btn", "Se connecter")
        )
      )
    }
  })
  
  # Application principale
  output$app_ui <- renderUI({
    if (user_authenticated()) {
      app_ui()
    }
  })
  
  # Authentification
  observeEvent(input$login_btn, {
    if (input$login_user == username && input$login_pass == password) {
      user_authenticated(TRUE)
    } else {
      showNotification("Identifiants incorrects", type = "error")
    }
  })
  
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
    filtered_data <- data_dpe %>%
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
            popup = ~paste(
              "Code Postal:", `Code_postal_(BAN)`, "<br>",
              "Ville:", nom_commune, "<br>",
              "Etiquette DPE:", Etiquette_DPE, "<br>",
              "<img src='", makeIcon(iconUrl = generate_icon(Etiquette_DPE)), "' width='50' height='50'>"
            )
          )
      })
    } else {
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
    
    communes <- data_dpe %>%
      filter(`Code_postal_(BAN)` == input$code_postal) %>%
      select(nom_commune) %>%
      distinct() %>%
      pull(nom_commune)
    
    updateSelectInput(session, "ville", choices = communes)
  })
  
  observeEvent(input$voir_rapport, {
    req(input$ville)
    
    updateTabsetPanel(session, "tabs", selected = "Rapport sur la ville")
    
    filtered_data <- data_dpe %>%
      filter(nom_commune == input$ville)
    
    nombre_dpe <- nrow(filtered_data)
    moyenne_dpe <- round(mean(as.numeric(factor(filtered_data$Etiquette_DPE, 
                                                levels = c("A", "B", "C", "D", "E", "F", "G")))), 2)
    coordonnees <- paste("Latitude:", mean(filtered_data$lat), "Longitude:", mean(filtered_data$lon))
    
    surface_moyenne <- round(mean(filtered_data$Surface_habitable_logement, na.rm = TRUE), 2)
    surface_min <- round(min(filtered_data$Surface_habitable_logement, na.rm = TRUE), 2)
    surface_max <- round(max(filtered_data$Surface_habitable_logement, na.rm = TRUE), 2)
    
    nb_neuf <- sum(filtered_data$Ancien_Neuf == "Neuf", na.rm = TRUE)
    nb_ancien <- sum(filtered_data$Ancien_Neuf == "Ancien", na.rm = TRUE)
    
    filtered_data <- filtered_data %>%
      mutate(Decennie_construction = floor(Année_construction / 10) * 10)
    
    repartition_dpe_decennie <- filtered_data %>%
      group_by(Decennie_construction, Etiquette_DPE) %>%
      summarise(Nombre_logements = n()) %>%
      pivot_wider(names_from = Etiquette_DPE, values_from = Nombre_logements, values_fill = 0) %>%
      arrange(Decennie_construction) %>%
      select(Decennie_construction, A, B, C, D, E, F, G)
    
    repartition_table_html <- knitr::kable(repartition_dpe_decennie, format = "html", table.attr = "class='table table-bordered'")
    
    rapport_html <- paste0(
      "<div style='padding: 20px;'>",
      "<h4>Informations principales pour la ville de ", input$ville, "</h4>",
      "<ul>",
      "<li><strong>Nombre total de DPE enregistrés :</strong> ", nombre_dpe, "</li>",
      "<li><strong>Moyenne de l'étiquette DPE :</strong> ", moyenne_dpe, "</li>",
      "<li><strong>Coordonnées moyennes :</strong> ", coordonnees, "</li>",
      "<li><strong>Surface habitable moyenne :</strong> ", surface_moyenne, " m²</li>",
      "<li><strong>Surface habitable minimum :</strong> ", surface_min, " m²</li>",
      "<li><strong>Surface habitable maximum :</strong> ", surface_max, " m²</li>",
      "<li><strong>Nombre de logements neufs :</strong> ", nb_neuf, "</li>",
      "<li><strong>Nombre de logements anciens :</strong> ", nb_ancien, "</li>",
      "</ul>",
      "<h4>Répartition des DPE par décennie de construction</h4>",
      repartition_table_html,
      "</div>"
    )
    
    output$rapport_ville <- renderUI({
      tagList(
        HTML(rapport_html),
        plotOutput("graphique_type_batiment"),
        plotOutput("graphique_dpe_neuf"),   
        plotOutput("graphique_dpe_ancien"),
        downloadButton("telecharger_graphique_dpe", label = "Télécharger le graphique de répartition des DPE")  # Un seul bouton de téléchargement pour le graphique
      )
    })
    
    output$nom_ville <- renderText({
      paste("Rapport sur la ville de", input$ville)
    })
    
    output$graphique_dpe <- renderPlot({
      ggplot(filtered_data, aes(x = Etiquette_DPE)) +
        geom_bar(fill = "steelblue") +
        labs(title = paste("Répartition des étiquettes DPE pour", input$ville),
             x = "Étiquette DPE", y = "Nombre de logements") +
        theme_minimal()
    })
    
    output$graphique_type_batiment <- renderPlot({
      ggplot(filtered_data, aes(x = "", fill = Type_bâtiment)) +
        geom_bar(width = 1) +
        coord_polar(theta = "y") +
        labs(title = "Répartition des types de bâtiments", fill = "Type de bâtiment") +
        theme_void()
    })
    
    output$graphique_dpe_neuf <- renderPlot({
      data_neuf <- filtered_data %>%
        filter(Ancien_Neuf == "Neuf")
      
      ggplot(data_neuf, aes(x = Etiquette_DPE)) +
        geom_bar(fill = "green") +
        labs(title = paste("Répartition des étiquettes DPE pour les logements neufs à", input$ville),
             x = "Étiquette DPE", y = "Nombre de logements") +
        theme_minimal()
    })
    
    output$graphique_dpe_ancien <- renderPlot({
      data_ancien <- filtered_data %>%
        filter(Ancien_Neuf == "Ancien")
      
      ggplot(data_ancien, aes(x = Etiquette_DPE)) +
        geom_bar(fill = "blue") +
        labs(title = paste("Répartition des étiquettes DPE pour les logements anciens à", input$ville),
             x = "Étiquette DPE", y = "Nombre de logements") +
        theme_minimal()
    })
    
    output$image_ville <- renderImage({
      ville <- input$ville
      query <- paste0("https://en.wikipedia.org/w/api.php?action=query&prop=pageimages&format=json&piprop=original&titles=", URLencode(ville))
      res <- GET(query)
      data <- fromJSON(content(res, "text"))
      
      page <- data$query$pages
      image_url <- NULL
      for (p in page) {
        if (!is.null(p$original)) {
          image_url <- p$original$source
          break
        }
      }
      
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
      
    }, deleteFile = TRUE)
    
    output$telecharger_image_ville <- downloadHandler(
      filename = function() {
        paste("image_ville_", input$ville, ".png", sep = "")
      },
      content = function(file) {
        ville <- input$ville
        query <- paste0("https://en.wikipedia.org/w/api.php?action=query&prop=pageimages&format=json&piprop=original&titles=", URLencode(ville))
        res <- GET(query)
        data <- fromJSON(content(res, "text"))
        
        page <- data$query$pages
        image_url <- NULL
        for (p in page) {
          if (!is.null(p$original)) {
            image_url <- p$original$source
            break
          }
        }
        
        if (!is.null(image_url)) {
          download.file(image_url, file, mode = "wb")
        }
      }
    )
    
    # Graphique pour le téléchargement
    output$telecharger_graphique_dpe <- downloadHandler(
      filename = function() {
        paste("graphique_repartition_dpe_", input$ville, ".png", sep = "")
      },
      content = function(file) {
        # Sauvegarder le graphique en tant qu'image
        png(file)
        print(
          ggplot(filtered_data, aes(x = Etiquette_DPE)) +
            geom_bar(fill = "steelblue") +
            labs(title = paste("Répartition des étiquettes DPE pour", input$ville),
                 x = "Étiquette DPE", y = "Nombre de logements") +
            theme_minimal()
        )
        dev.off()
      }
    )
    
    df_classement <- data_dpe %>%
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
  
  # Fonction pour télécharger les données de la ville au format CSV
  output$telecharger_csv <- downloadHandler(
    filename = function() {
      paste("data_", input$ville, ".csv", sep = "")
    },
    content = function(file) {
      filtered_data <- data_dpe %>%
        filter(nom_commune == input$ville)
      write.csv(filtered_data, file, row.names = FALSE)
    }
  )
  
  observeEvent(input$generate_corr_plot, {
    req(input$var1, input$var2)  # S'assurer que les variables sont choisies
    
    output$corr_plot <- renderPlot({
      ggplot(data_dpe, aes_string(x = input$var1, y = input$var2)) +
        geom_point(color = "steelblue") +
        geom_smooth(method = "lm", color = "red") +
        labs(title = paste("Corrélation entre", input$var1, "et", input$var2),
             x = input$var1, y = input$var2) +
        theme_minimal()
    })
  })
  
  output$download_corr_plot <- downloadHandler(
    filename = function() {
      paste("correlation_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      g <- ggplot(data_dpe, aes_string(x = input$var1, y = input$var2)) +
        geom_point(color = "steelblue") +
        geom_smooth(method = "lm", color = "red") +
        labs(title = paste("Corrélation entre", input$var1, "et", input$var2),
             x = input$var1, y = input$var2) +
        theme_minimal()
      ggsave(file, plot = g, device = "png")
    }
  )
  
  # Initialiser la carte vide
  output$carte <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 3.8772, lat = 43.6119, zoom = 9) # Vue centrée sur l'Hérault
  })
}



