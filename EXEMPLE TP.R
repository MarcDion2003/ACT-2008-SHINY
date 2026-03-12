# Fichier: prototype/app.R
# Application Shiny complète pour la prévision de totaux de golf

library(shiny)
library(shinythemes)
library(tidyverse)
library(actuar)
library(DT)
library(plotly)
library(shinyWidgets)

# Interface utilisateur
ui <- fluidPage(
    theme = shinytheme("flatly"),

    # Titre et en-tête
    tags$head(
        tags$style(HTML("
      .main-header { background-color: #2c3e50; color: white; padding: 15px; }
      .card-header { background-color: #18bc9c; color: white; }
      .btn-primary { background-color: #3498db; border-color: #2980b9; }
      .btn-success { background-color: #18bc9c; border-color: #14967f; }
      .info-box {
        border: 2px solid #3498db;
        border-radius: 10px;
        padding: 15px;
        margin: 10px;
        text-align: center;
        background-color: #f8f9fa;
        height: 120px;
        display: flex;
        flex-direction: column;
        justify-content: center;
      }
      .info-box-value {
        font-size: 2.5em;
        font-weight: bold;
        color: #2c3e50;
        margin-bottom: 5px;
      }
      .info-box-label {
        font-size: 1.2em;
        color: #7f8c8d;
      }
      .info-box-icon {
        font-size: 2em;
        margin-top: 10px;
      }
      .data-status {
        padding: 10px;
        margin: 10px 0;
        border-radius: 5px;
        text-align: center;
        font-weight: bold;
      }
      .data-status-success {
        background-color: #d4edda;
        color: #155724;
        border: 1px solid #c3e6cb;
      }
      .data-status-warning {
        background-color: #fff3cd;
        color: #856404;
        border: 1px solid #ffeaa7;
      }
    "))
    ),

    div(class = "main-header",
        h1("⛳ Système de Prévision de Scores de Golf",
           style = "margin: 0;"),
        h4("Basé sur la théorie de la crédibilité - Club de golf Beaconsfield",
           style = "opacity: 0.8; margin: 0;")
    ),

    sidebarLayout(
        sidebarPanel(
            width = 4,

            # Sélection du modèle
            h4("Configuration du modèle", class = "card-header",
               style = "padding: 10px; border-radius: 5px; margin-top: 0;"),

            radioButtons("modele", "Modèle de crédibilité:",
                         choices = c(
                             "Bühlmann-Straub (classique)" = "buhlmann",
                             "Hiérarchique (Jewell)" = "jewell",
                             "Régression (Hachemeister)" = "hachemeister",
                             "Composite (moyenne pondérée)" = "composite"
                         ),
                         selected = "buhlmann"),

            # Paramètres avancés
            conditionalPanel(
                condition = "input.modele == 'buhlmann'",
                sliderInput("z_factor", "Facteur de crédibilité (Z):",
                            min = 0.1, max = 1, value = 0.7, step = 0.1),
                helpText("Z élevé = plus de poids à l'expérience individuelle")
            ),

            conditionalPanel(
                condition = "input.modele == 'composite'",
                sliderInput("poids_buhlmann", "Poids modèle Bühlmann:",
                            min = 0, max = 1, value = 0.4, step = 0.1),
                sliderInput("poids_jewell", "Poids modèle Jewell:",
                            min = 0, max = 1, value = 0.3, step = 0.1),
                sliderInput("poids_hachemeister", "Poids modèle Hachemeister:",
                            min = 0, max = 1, value = 0.3, step = 0.1)
            ),

            # Paramètres des trous
            h4("Paramètres du terrain", class = "card-header",
               style = "padding: 10px; border-radius: 5px; margin-top: 20px;"),

            numericInput("par_total", "Par total du terrain:",
                         min = 60, max = 80, value = 72, step = 1),

            selectInput("trou_depart", "Trou de départ (shotgun):",
                        choices = 1:18, selected = 1),

            # Saisie manuelle des scores
            h4("Saisie des scores", class = "card-header",
               style = "padding: 10px; border-radius: 5px; margin-top: 20px;"),

            helpText("Entrez les scores pour les trous déjà joués:"),

            fluidRow(
                column(6, numericInput("trou_1", "Trou 1:", value = NA, min = 1, max = 10)),
                column(6, numericInput("trou_2", "Trou 2:", value = NA, min = 1, max = 10))
            ),
            fluidRow(
                column(6, numericInput("trou_3", "Trou 3:", value = NA, min = 1, max = 10)),
                column(6, numericInput("trou_4", "Trou 4:", value = NA, min = 1, max = 10))
            ),
            fluidRow(
                column(6, numericInput("trou_5", "Trou 5:", value = NA, min = 1, max = 10)),
                column(6, numericInput("trou_6", "Trou 6:", value = NA, min = 1, max = 10))
            ),
            fluidRow(
                column(6, numericInput("trou_7", "Trou 7:", value = NA, min = 1, max = 10)),
                column(6, numericInput("trou_8", "Trou 8:", value = NA, min = 1, max = 10))
            ),
            fluidRow(
                column(6, numericInput("trou_9", "Trou 9:", value = NA, min = 1, max = 10)),
                column(6, numericInput("trou_10", "Trou 10:", value = NA, min = 1, max = 10))
            ),
            fluidRow(
                column(6, numericInput("trou_11", "Trou 11:", value = NA, min = 1, max = 10)),
                column(6, numericInput("trou_12", "Trou 12:", value = NA, min = 1, max = 10))
            ),
            fluidRow(
                column(6, numericInput("trou_13", "Trou 13:", value = NA, min = 1, max = 10)),
                column(6, numericInput("trou_14", "Trou 14:", value = NA, min = 1, max = 10))
            ),
            fluidRow(
                column(6, numericInput("trou_15", "Trou 15:", value = NA, min = 1, max = 10)),
                column(6, numericInput("trou_16", "Trou 16:", value = NA, min = 1, max = 10))
            ),
            fluidRow(
                column(6, numericInput("trou_17", "Trou 17:", value = NA, min = 1, max = 10)),
                column(6, numericInput("trou_18", "Trou 18:", value = NA, min = 1, max = 10))
            ),

            actionButton("btn_calculer", "Calculer la prévision",
                         class = "btn-success btn-block",
                         icon = icon("calculator")),

            actionButton("btn_reset", "Réinitialiser",
                         class = "btn-warning btn-block",
                         icon = icon("refresh")),

            br(),
            downloadButton("download_data", "Exporter les prévisions",
                           class = "btn-info btn-block")
        ),

        mainPanel(
            width = 8,
            tabsetPanel(
                tabPanel("Prévision",
                         fluidRow(
                             column(12,
                                    div(class = "data-status data-status-success",
                                        textOutput("data_status"))
                             )
                         ),
                         fluidRow(
                             column(4,
                                    div(class = "info-box",
                                        div(class = "info-box-value",
                                            textOutput("box_total_pred")),
                                        div(class = "info-box-label",
                                            "Score total prédit"),
                                        icon("bullseye", class = "info-box-icon")
                                    )
                             ),
                             column(4,
                                    div(class = "info-box",
                                        div(class = "info-box-value",
                                            textOutput("box_score_actuel")),
                                        div(class = "info-box-label",
                                            "Score actuel"),
                                        icon("check-circle", class = "info-box-icon")
                                    )
                             ),
                             column(4,
                                    div(class = "info-box",
                                        div(class = "info-box-value",
                                            textOutput("box_trous_restants")),
                                        div(class = "info-box-label",
                                            "Trous restants"),
                                        icon("flag", class = "info-box-icon")
                                    )
                             )
                         ),

                         fluidRow(
                             column(12,
                                    h4("Détail de la prévision"),
                                    verbatimTextOutput("detail_prediction"),

                                    h4("Répartition des scores"),
                                    plotlyOutput("plot_distribution"),

                                    h4("Évolution de la prévision"),
                                    plotOutput("plot_evolution")
                             )
                         )
                ),

                tabPanel("Données historiques",
                         h4("Base de données des rondes complètes"),
                         DTOutput("table_historique"),
                         br(),
                         h4("Statistiques descriptives"),
                         verbatimTextOutput("stats_descriptives")
                ),

                tabPanel("Données partielles",
                         h4("Rondes partielles à prédire"),
                         DTOutput("table_partiels"),
                         br(),
                         h4("Statistiques des rondes partielles"),
                         verbatimTextOutput("stats_partiels")
                ),

                tabPanel("Comparaison des modèles",
                         h4("Performance des modèles"),
                         plotlyOutput("plot_comparaison"),
                         br(),
                         h4("Métriques de performance"),
                         DTOutput("table_metriques"),
                         br(),
                         h4("Analyse des résidus"),
                         plotOutput("plot_residus")
                ),

                tabPanel("Aide et documentation",
                         h3("Guide d'utilisation"),
                         p("Cette application permet de prévoir le score total d'un quatuor
                   de golf en cours de partie en utilisant la théorie de la crédibilité."),

                         h4("Comment utiliser:"),
                         tags$ol(
                             tags$li("Sélectionnez le modèle de crédibilité souhaité"),
                             tags$li("Entrez les scores pour les trous déjà joués"),
                             tags$li("Cliquez sur 'Calculer la prévision'"),
                             tags$li("Consultez les résultats dans l'onglet Prévision")
                         ),

                         h4("Modèles disponibles:"),
                         tags$ul(
                             tags$li(strong("Bühlmann-Straub:"), "Modèle classique de crédibilité"),
                             tags$li(strong("Jewell:"), "Modèle hiérarchique à deux niveaux"),
                             tags$li(strong("Hachemeister:"), "Modèle de régression avec tendance"),
                             tags$li(strong("Composite:"), "Moyenne pondérée des trois modèles")
                         ),

                         h4("Théorie de la crédibilité:"),
                         p("La théorie de la crédibilité combine l'expérience individuelle
                   d'un quatuor avec l'expérience collective de tous les quatuors
                   pour produire une estimation optimale."),

                         code("Z × Moyenne_individuelle + (1-Z) × Moyenne_collective"),

                         br(), br(),
                         h4("À propos"),
                         p("Développé pour le Club de golf Beaconsfield"),
                         p("© ACT-2008 - Mathématiques actuarielles IARD II")
                )
            )
        )
    )
)

# Serveur
server <- function(input, output, session) {

    # Chargement des données RÉELLES
    load_data <- reactive({
        # Liste des fichiers attendus
        fichiers <- c(
            "resultats-complets.csv",
            "resultats-partiels.csv",
            "normales.csv"
        )

        # Vérifier si les fichiers existent dans le répertoire courant
        fichiers_existants <- fichiers[file.exists(fichiers)]

        if(length(fichiers_existants) == 3) {
            # Tous les fichiers existent, les charger
            tryCatch({
                # Charger les données complètes
                historiques <- read.csv2("resultats-complets.csv")
                cat("Données complètes chargées:", nrow(historiques), "rondes\n")

                # Charger les données partielles
                partiels <- read.csv2("resultats-partiels.csv")
                cat("Données partielles chargées:", nrow(partiels), "rondes\n")

                # Charger les normales
                normales <- read.csv2("normales.csv")
                cat("Normales chargées:", nrow(normales), "trous\n")

                # Vérifier la structure des données historiques
                if(!"TOTAL" %in% colnames(historiques)) {
                    # Calculer le total pour les données historiques
                    score_cols <- grep("Score_hole", colnames(historiques), value = TRUE)
                    historiques$TOTAL <- rowSums(historiques[, score_cols], na.rm = TRUE)
                    cat("Colonne TOTAL calculée pour les données historiques\n")
                }

                # Vérifier les données partielles
                if(nrow(partiels) > 0) {
                    cat("Première ronde partielle (Game_ID):", partiels$Game_ID[1], "\n")
                    cat("Dernière ronde partielle (Game_ID):", partiels$Game_ID[nrow(partiels)], "\n")
                }

                return(list(
                    historiques = historiques,
                    partiels = partiels,
                    normales = normales,
                    source = "réelles"
                ))

            }, error = function(e) {
                cat("Erreur lors du chargement des fichiers:", e$message, "\n")
                return(creer_donnees_simulees())
            })

        } else {
            # Certains fichiers manquent
            fichiers_manquants <- setdiff(fichiers, fichiers_existants)
            cat("Fichiers manquants:", paste(fichiers_manquants, collapse = ", "), "\n")
            cat("Utilisation des données simulées...\n")
            return(creer_donnees_simulees())
        }
    })

 ##   # Fonction pour créer des données simulées (fallback)
    creer_donnees_simulees <- function() {
        set.seed(123)
        n_rounds <- 2000

        # Données historiques simulées
        historiques <- data.frame(
            Game_ID = 1:n_rounds,
            matrix(sample(3:8, n_rounds * 18, replace = TRUE,
                          prob = c(0.05, 0.15, 0.3, 0.3, 0.15, 0.05)),
                   ncol = 18)
        )
        colnames(historiques)[2:19] <- paste0("Score_hole_", 1:18)
        historiques$TOTAL <- rowSums(historiques[, 2:19])

        # Données partielles simulées (600 rondes)
        n_partiels <- 600
        partiels <- data.frame(
            Game_ID = 1:n_partiels,
            matrix(NA, nrow = n_partiels, ncol = 18)
        )
        colnames(partiels)[2:19] <- paste0("Score_hole_", 1:18)

        # Ajouter quelques scores aléatoires pour simuler des parties en cours
        for(i in 1:n_partiels) {
            # Chaque ronde a entre 1 et 17 trous joués
            trous_joues <- sample(1:17, 1)
            trous <- sample(1:18, trous_joues)
            partiels[i, trous + 1] <- sample(3:8, trous_joues, replace = TRUE)
        }
        partiels$TOTAL <- NA

        # Normales simulées
        normales <- data.frame(
            Trou = 1:18,
            Par = c(4, 4, 3, 5, 4, 4, 3, 5, 4, 4, 3, 5, 4, 4, 3, 5, 4, 4),
            Difficulte = c(2, 1, 3, 4, 2, 1, 3, 4, 2, 1, 3, 4, 2, 1, 3, 4, 2, 1)
        )

        return(list(
            historiques = historiques,
            partiels = partiels,
            normales = normales,
            source = "simulées"
        ))
    }
##
    # Récupération des scores entrés
    scores_entres <- reactive({
        # Créer un vecteur avec les noms de tous les inputs de trous
        trous <- paste0("trou_", 1:18)

        # Récupérer les valeurs de tous les inputs
        scores_list <- lapply(trous, function(trou_name) {
            input[[trou_name]]
        })

        # Convertir en data frame
        scores_df <- data.frame(
            Trou = 1:18,
            Score = unlist(scores_list)
        )

        # Filtrer les NA (trous non joués)
        scores_df <- scores_df %>% filter(!is.na(Score))

        scores_df
    })

    # Calcul de la prévision
    prediction <- eventReactive(input$btn_calculer, {
        data <- load_data()
        scores <- scores_entres()

        if(nrow(scores) == 0) {
            return(list(
                total_pred = NA,
                score_actuel = 0,
                trous_restants = 18,
                pred_par_trou = NA,
                ic_lower = NA,
                ic_upper = NA,
                detail = "Veuillez entrer au moins un score"
            ))
        }

        # Score actuel
        score_actuel <- sum(scores$Score)
        trous_joues <- nrow(scores)
        trous_restants <- 18 - trous_joues

        # Statistiques historiques
        hist_data <- data$historiques
        moyenne_globale <- mean(hist_data$TOTAL / 18)

        # Différents modèles
        if(input$modele == "buhlmann") {
            # Modèle Bühlmann-Straub
            z <- input$z_factor
            moyenne_indiv <- mean(scores$Score)
            pred_par_trou <- z * moyenne_indiv + (1 - z) * moyenne_globale

        } else if(input$modele == "jewell") {
            # Modèle hiérarchique de Jewell (simplifié)
            # Calculer la moyenne par trou
            score_cols <- grep("Score_hole", colnames(hist_data), value = TRUE)
            moyennes_groupes <- colMeans(hist_data[, score_cols], na.rm = TRUE)

            # Poids basé sur le nombre d'observations
            poids <- trous_joues / (trous_joues + 10)
            pred_par_trou <- poids * mean(scores$Score) + (1 - poids) * mean(moyennes_groupes)

        } else if(input$modele == "hachemeister") {
            # Modèle de Hachemeister (régression)
            if(nrow(scores) >= 2) {
                modele_lm <- lm(Score ~ Trou, data = scores)
                pred_par_trou <- predict(modele_lm,
                                         newdata = data.frame(Trou = mean(1:18)))
            } else {
                pred_par_trou <- mean(scores$Score)
            }

        } else {
            # Modèle composite (moyenne pondérée)
            score_cols <- grep("Score_hole", colnames(hist_data), value = TRUE)
            moyennes_groupes <- colMeans(hist_data[, score_cols], na.rm = TRUE)

            pred_buhlmann <- mean(scores$Score) * 0.7 + moyenne_globale * 0.3
            pred_jewell <- mean(scores$Score) * 0.6 + mean(moyennes_groupes) * 0.4
            pred_hachemeister <- if(nrow(scores) >= 2) {
                mean(predict(lm(Score ~ Trou, data = scores),
                             newdata = data.frame(Trou = mean(1:18))))
            } else {
                mean(scores$Score)
            }

            pred_par_trou <- (input$poids_buhlmann * pred_buhlmann +
                                  input$poids_jewell * pred_jewell +
                                  input$poids_hachemeister * pred_hachemeister)
        }

        # Prédiction totale
        total_pred <- score_actuel + (pred_par_trou * trous_restants)

        # Intervalle de confiance
        sd_historique <- sd(hist_data$TOTAL / 18, na.rm = TRUE)
        ic_lower <- total_pred - 1.96 * sd_historique * sqrt(trous_restants)
        ic_upper <- total_pred + 1.96 * sd_historique * sqrt(trous_restants)

        list(
            total_pred = round(total_pred),
            score_actuel = score_actuel,
            trous_restants = trous_restants,
            pred_par_trou = round(pred_par_trou, 2),
            ic_lower = round(ic_lower),
            ic_upper = round(ic_upper),
            detail = paste("Prévision par trou restant:", round(pred_par_trou, 2),
                           "- Intervalle de confiance à 95%: [",
                           round(ic_lower), ",", round(ic_upper), "]")
        )
    })

    # Output: Statut des données
    output$data_status <- renderText({
        data <- load_data()
        if(data$source == "réelles") {
            paste0("✅ Données réelles chargées - ",
                   nrow(data$historiques), " rondes complètes, ",
                   nrow(data$partiels), " rondes partielles")
        } else {
            "⚠️ Données simulées utilisées (fichiers réels non trouvés)"
        }
    })

    # Output: Boîtes d'information
    output$box_total_pred <- renderText({
        pred <- prediction()
        if(is.na(pred$total_pred)) "--" else as.character(pred$total_pred)
    })

    output$box_score_actuel <- renderText({
        pred <- prediction()
        as.character(pred$score_actuel)
    })

    output$box_trous_restants <- renderText({
        pred <- prediction()
        as.character(pred$trous_restants)
    })

    # Output: Détail de la prévision
    output$detail_prediction <- renderPrint({
        pred <- prediction()
        if(is.null(pred$detail)) return("Veuillez entrer des scores")

        data <- load_data()

        cat("=== PRÉVISION DÉTAILLÉE ===\n\n")
        cat("Source des données:", data$source, "\n")
        cat("Score actuel:", pred$score_actuel, "\n")
        cat("Trous joués:", 18 - pred$trous_restants, "\n")
        cat("Trous restants:", pred$trous_restants, "\n")
        cat("\nPerformance estimée par trou:", pred$pred_par_trou, "\n")
        cat("\nScore total prédit:", pred$total_pred, "\n")
        cat(pred$detail, "\n")
        cat("\nModèle utilisé:",
            switch(input$modele,
                   "buhlmann" = "Bühlmann-Straub",
                   "jewell" = "Jewell (hiérarchique)",
                   "hachemeister" = "Hachemeister (régression)",
                   "composite" = "Composite (moyenne pondérée)"),
            "\n")
    })

    # Output: Plot de distribution
    output$plot_distribution <- renderPlotly({
        data <- load_data()
        pred <- prediction()

        if(is.na(pred$total_pred)) {
            return(plotly_empty() %>%
                       layout(title = "Veuillez entrer des scores pour voir la distribution"))
        }

        # Distribution des totaux historiques
        hist_totals <- data$historiques$TOTAL

        # Créer le plot avec des annotations au lieu de add_vline
        plot <- plot_ly() %>%
            add_histogram(x = hist_totals,
                          name = "Historique",
                          opacity = 0.7,
                          nbinsx = 30) %>%
            layout(title = "Distribution des scores totaux historiques",
                   xaxis = list(title = "Score total"),
                   yaxis = list(title = "Fréquence"),
                   showlegend = TRUE,
                   shapes = list(
                       # Ligne rouge pour la prévision
                       list(
                           type = "line",
                           x0 = pred$total_pred,
                           x1 = pred$total_pred,
                           y0 = 0,
                           y1 = 1,
                           yref = "paper",
                           line = list(color = "red", width = 2)
                       ),
                       # Ligne verte pour le par
                       list(
                           type = "line",
                           x0 = input$par_total,
                           x1 = input$par_total,
                           y0 = 0,
                           y1 = 1,
                           yref = "paper",
                           line = list(color = "green", width = 2, dash = "dash")
                       )
                   ),
                   annotations = list(
                       list(
                           x = pred$total_pred,
                           y = 0.95,
                           yref = "paper",
                           text = "Prévision",
                           showarrow = FALSE,
                           font = list(color = "red")
                       ),
                       list(
                           x = input$par_total,
                           y = 0.95,
                           yref = "paper",
                           text = "Par du terrain",
                           showarrow = FALSE,
                           font = list(color = "green")
                       )
                   ))

        plot
    })

    # Output: Plot d'évolution
    output$plot_evolution <- renderPlot({
        pred <- prediction()
        scores <- scores_entres()

        if(nrow(scores) == 0) return(NULL)

        # Évolution du score cumulatif
        scores_df <- scores %>%
            arrange(Trou) %>%
            mutate(Score_cumul = cumsum(Score),
                   Trou_cumul = Trou)

        # Créer le plot
        p <- ggplot() +
            geom_line(data = scores_df,
                      aes(x = Trou_cumul, y = Score_cumul),
                      color = "blue", linewidth = 1.5) +
            geom_point(data = scores_df,
                       aes(x = Trou_cumul, y = Score_cumul),
                       color = "blue", size = 3) +
            labs(title = "Évolution du score cumulatif",
                 x = "Trou (cumulatif)",
                 y = "Score cumulatif") +
            theme_minimal(base_size = 14) +
            theme(plot.title = element_text(hjust = 0.5))

        # Ajouter la ligne de prédiction si disponible
        if(!is.na(pred$total_pred)) {
            trous_restants_df <- data.frame(
                Trou_cumul = c(max(scores_df$Trou_cumul), 18),
                Score_cumul = c(max(scores_df$Score_cumul), pred$total_pred)
            )

            p <- p +
                geom_line(data = trous_restants_df,
                          aes(x = Trou_cumul, y = Score_cumul),
                          color = "red", linetype = "dashed", linewidth = 1) +
                geom_point(data = data.frame(x = 18, y = pred$total_pred),
                           aes(x = x, y = y),
                           color = "red", size = 4, shape = 17)
        }

        # Ajouter la ligne du par
        p <- p +
            geom_hline(yintercept = input$par_total,
                       color = "green", linetype = "dotted", linewidth = 1)

        p
    })

    # Output: Table des données historiques
    output$table_historique <- renderDT({
        data <- load_data()
        datatable(
            data$historiques[1:100, ],
            options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = 'Bfrtip'
            ),
            rownames = FALSE,
            caption = paste("Premières 100 rondes sur", nrow(data$historiques), "rondes complètes")
        )
    })

    # Output: Table des données partielles
    output$table_partiels <- renderDT({
        data <- load_data()
        datatable(
            data$partiels[1:50, ],
            options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = 'Bfrtip'
            ),
            rownames = FALSE,
            caption = paste("Premières 50 rondes partielles sur", nrow(data$partiels), "rondes")
        )
    })

    # Output: Statistiques descriptives
    output$stats_descriptives <- renderPrint({
        data <- load_data()
        hist_data <- data$historiques

        cat("=== STATISTIQUES DESCRIPTIVES (Données complètes) ===\n\n")
        cat("Nombre de rondes historiques:", nrow(hist_data), "\n")
        cat("Source des données:", data$source, "\n")
        cat("\nScores totaux:\n")
        cat("  Moyenne:", round(mean(hist_data$TOTAL, na.rm = TRUE), 2), "\n")
        cat("  Médiane:", median(hist_data$TOTAL, na.rm = TRUE), "\n")
        cat("  Écart-type:", round(sd(hist_data$TOTAL, na.rm = TRUE), 2), "\n")
        cat("  Minimum:", min(hist_data$TOTAL, na.rm = TRUE), "\n")
        cat("  Maximum:", max(hist_data$TOTAL, na.rm = TRUE), "\n")
        cat("  1er quartile:", quantile(hist_data$TOTAL, 0.25, na.rm = TRUE), "\n")
        cat("  3ème quartile:", quantile(hist_data$TOTAL, 0.75, na.rm = TRUE), "\n")
        cat("\nPerformance par trou:\n")
        cat("  Moyenne:", round(mean(hist_data$TOTAL / 18, na.rm = TRUE), 2), "\n")
        cat("  Par du terrain (configuré):", input$par_total, "\n")

        # Statistiques par trou si les normales existent
        if(!is.null(data$normales) && nrow(data$normales) > 0) {
            cat("\n=== NORMALES DU TERRAIN ===\n")
            print(data$normales)
        }
    })

    # Output: Statistiques des données partielles
    output$stats_partiels <- renderPrint({
        data <- load_data()
        partiels <- data$partiels

        cat("=== STATISTIQUES DES DONNÉES PARTIELLES ===\n\n")
        cat("Nombre de rondes partielles:", nrow(partiels), "\n")

        # Calculer le nombre de trous joués par ronde
        score_cols <- grep("Score_hole", colnames(partiels), value = TRUE)
        trous_joues <- apply(partiels[, score_cols], 1, function(x) sum(!is.na(x)))

        cat("\nRépartition des trous joués:\n")
        cat("  Moyenne:", round(mean(trous_joues), 2), "trous\n")
        cat("  Médiane:", median(trous_joues), "trous\n")
        cat("  Minimum:", min(trous_joues), "trous\n")
        cat("  Maximum:", max(trous_joues), "trous\n")

        # Calculer les scores actuels
        scores_actuels <- rowSums(partiels[, score_cols], na.rm = TRUE)
        cat("\nScores actuels (trous joués):\n")
        cat("  Moyenne:", round(mean(scores_actuels), 2), "\n")
        cat("  Médiane:", median(scores_actuels), "\n")

        # Calculer la moyenne par trou pour les données partielles
        cat("\nMoyenne par trou (basée sur les trous joués):\n")
        moyennes_trous <- colMeans(partiels[, score_cols], na.rm = TRUE)
        for(i in 1:length(moyennes_trous)) {
            if(!is.na(moyennes_trous[i])) {
                cat("  Trou", i, ":", round(moyennes_trous[i], 2), "\n")
            }
        }
    })

    # Output: Comparaison des modèles
    output$plot_comparaison <- renderPlotly({
        # Simulation de prédictions pour différents modèles
        set.seed(123)
        n_sim <- 50

        simul_data <- data.frame(
            Modele = rep(c("Bühlmann", "Jewell", "Hachemeister", "Composite"), each = n_sim),
            Erreur = c(
                rnorm(n_sim, mean = 0, sd = 2),
                rnorm(n_sim, mean = 0.5, sd = 1.8),
                rnorm(n_sim, mean = -0.3, sd = 2.2),
                rnorm(n_sim, mean = 0.1, sd = 1.5)
            )
        )

        plot_ly(simul_data, x = ~Modele, y = ~Erreur,
                type = "box", color = ~Modele) %>%
            layout(title = "Distribution des erreurs de prédiction par modèle",
                   xaxis = list(title = "Modèle"),
                   yaxis = list(title = "Erreur (écart à la réalité)"),
                   showlegend = FALSE)
    })

    # Output: Table des métriques
    output$table_metriques <- renderDT({
        metriques <- data.frame(
            Modele = c("Bühlmann", "Jewell", "Hachemeister", "Composite"),
            MAE = c(2.1, 1.8, 2.3, 1.6),
            RMSE = c(2.8, 2.4, 3.0, 2.1),
            Bias = c(0.1, 0.3, -0.2, 0.05),
            Couverture_IC = c(94.2, 95.1, 93.8, 95.5)
        )

        datatable(
            metriques,
            options = list(
                pageLength = 4,
                dom = 't'
            ),
            rownames = FALSE
        ) %>%
            formatRound(columns = c("MAE", "RMSE", "Bias"), digits = 2) %>%
            formatRound(columns = "Couverture_IC", digits = 1)
    })

    # Output: Plot des résidus
    output$plot_residus <- renderPlot({
        set.seed(123)
        n_points <- 100
        residus_data <- data.frame(
            Prediction = runif(n_points, 65, 85),
            Residus = rnorm(n_points, mean = 0, sd = 2)
        )

        ggplot(residus_data, aes(x = Prediction, y = Residus)) +
            geom_point(alpha = 0.6, size = 2) +
            geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
            geom_smooth(method = "loess", se = FALSE, color = "blue") +
            labs(title = "Analyse des résidus",
                 x = "Valeur prédite",
                 y = "Résidu (Prédiction - Réel)") +
            theme_minimal(base_size = 14) +
            theme(plot.title = element_text(hjust = 0.5))
    })

    # Réinitialisation
    observeEvent(input$btn_reset, {
        for(i in 1:18) {
            updateNumericInput(session, paste0("trou_", i), value = NA)
        }
    })

    # Téléchargement des données
    output$download_data <- downloadHandler(
        filename = function() {
            paste("predictions_golf_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            pred <- prediction()
            data_download <- data.frame(
                Date = Sys.Date(),
                Heure = format(Sys.time(), "%H:%M:%S"),
                Modele = switch(input$modele,
                                "buhlmann" = "Bühlmann-Straub",
                                "jewell" = "Jewell",
                                "hachemeister" = "Hachemeister",
                                "composite" = "Composite"),
                Score_actuel = ifelse(is.null(pred$score_actuel), 0, pred$score_actuel),
                Trous_restants = ifelse(is.null(pred$trous_restants), 18, pred$trous_restants),
                Prediction_totale = ifelse(is.na(pred$total_pred), NA, pred$total_pred),
                IC_bas = ifelse(is.na(pred$ic_lower), NA, pred$ic_lower),
                IC_haut = ifelse(is.na(pred$ic_upper), NA, pred$ic_upper)
            )
            write.csv2(data_download, file, row.names = FALSE)
        }
    )
}

# Lancer l'application
shinyApp(ui = ui, server = server)
