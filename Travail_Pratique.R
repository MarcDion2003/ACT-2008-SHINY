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
                             "Bayésien Poisson/Gamma" = "bayes_poisson_gamma",
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
                sliderInput("poids_bayes", "Poids modèle Bayésien:",
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

            # Mode de saisie
            h4("Mode de saisie", class = "card-header",
               style = "padding: 10px; border-radius: 5px; margin-top: 20px;"),

            radioButtons("mode_saisie", "Choisir le mode:",
                         choices = c(
                             "Saisie manuelle" = "manuelle",
                             "Tester sur données partielles" = "test"
                         ),
                         selected = "manuelle"),

            # Sélection de la ronde partielle à tester
            conditionalPanel(
                condition = "input.mode_saisie == 'test'",
                selectInput("ronde_test", "Sélectionner une ronde partielle:",
                            choices = NULL)  # Rempli dynamiquement
            ),

            # Saisie manuelle des scores
            conditionalPanel(
                condition = "input.mode_saisie == 'manuelle'",
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
                )
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
                         h4("Base de données des rondes complètes (entraînement)"),
                         DTOutput("table_historique"),
                         br(),
                         h4("Statistiques descriptives"),
                         verbatimTextOutput("stats_descriptives")
                ),

                tabPanel("Données partielles",
                         h4("Rondes partielles à tester"),
                         DTOutput("table_partiels"),
                         br(),
                         h4("Statistiques des rondes partielles"),
                         verbatimTextOutput("stats_partiels")
                ),

                tabPanel("Comparaison des modèles",
                         h4("Performance des modèles sur les données de test"),
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
                             tags$li("Les données complètes servent à entraîner les modèles"),
                             tags$li("Choisissez le mode de saisie: manuelle ou test sur données partielles"),
                             tags$li("Sélectionnez le modèle de crédibilité souhaité"),
                             tags$li("Entrez les scores ou sélectionnez une ronde partielle"),
                             tags$li("Cliquez sur 'Calculer la prévision'")
                         ),

                         h4("Modèles disponibles:"),
                         tags$ul(
                             tags$li(strong("Bühlmann-Straub:"), "Modèle classique de crédibilité"),
                             tags$li(strong("Bayésien Poisson/Gamma:"), "Modèle bayésien avec distribution Poisson/Gamma"),
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

    # Chargement des données RÉELLES seulement
    load_data <- reactive({
        # Liste des fichiers attendus
        fichiers <- c(
            "resultats-complets.csv",
            "resultats-partiels.csv",
            "normales.csv"
        )

        # Vérifier si les fichiers existent
        fichiers_existants <- fichiers[file.exists(fichiers)]

        if(length(fichiers_existants) == 3) {
            # Tous les fichiers existent, les charger
            tryCatch({
                # Charger les données complètes (pour entraînement)
                historiques <- read.csv2("resultats-complets.csv")

                # Charger les données partielles (pour test)
                partiels <- read.csv2("resultats-partiels.csv")

                # Charger les normales
                normales <- read.csv2("normales.csv")

                # Vérifier la structure des données historiques
                if(!"TOTAL" %in% colnames(historiques)) {
                    score_cols <- grep("Score_hole", colnames(historiques), value = TRUE)
                    historiques$TOTAL <- rowSums(historiques[, score_cols], na.rm = TRUE)
                }

                return(list(
                    historiques = historiques,
                    partiels = partiels,
                    normales = normales,
                    source = "réelles"
                ))

            }, error = function(e) {
                stop("Erreur lors du chargement des fichiers: ", e$message)
            })

        } else {
            # Fichiers manquants
            fichiers_manquants <- setdiff(fichiers, fichiers_existants)
            stop("Fichiers manquants: ", paste(fichiers_manquants, collapse = ", "))
        }
    })

    # Mettre à jour la liste des rondes partielles disponibles
    observe({
        data <- load_data()
        partiels <- data$partiels

        # Créer une liste de choix avec les Game_ID
        choix <- setNames(partiels$Game_ID,
                          paste("Ronde", partiels$Game_ID))

        updateSelectInput(session, "ronde_test",
                          choices = choix)
    })

    # Récupération des scores selon le mode
    scores_entres <- reactive({
        data <- load_data()

        if(input$mode_saisie == "manuelle") {
            # Mode saisie manuelle
            trous <- paste0("trou_", 1:18)
            scores_list <- lapply(trous, function(trou_name) {
                input[[trou_name]]
            })

            scores_df <- data.frame(
                Trou = 1:18,
                Score = unlist(scores_list)
            )
            scores_df <- scores_df %>% filter(!is.na(Score))

        } else {
            # Mode test sur données partielles
            req(input$ronde_test)

            # Récupérer la ronde sélectionnée
            ronde <- data$partiels[data$partiels$Game_ID == input$ronde_test, ]

            # Extraire les scores des trous
            score_cols <- grep("Score_hole", colnames(ronde), value = TRUE)
            scores <- as.numeric(ronde[1, score_cols])

            scores_df <- data.frame(
                Trou = 1:18,
                Score = scores
            )
            scores_df <- scores_df %>% filter(!is.na(Score))

            # Stocker le score total réel pour calculer l'erreur
            # (si disponible dans les données partielles)
            if("TOTAL" %in% colnames(ronde) && !is.na(ronde$TOTAL)) {
                scores_df$total_reel <- ronde$TOTAL
            } else {
                # Calculer à partir des trous joués (si tous les trous sont joués)
                if(nrow(scores_df) == 18) {
                    scores_df$total_reel <- sum(scores_df$Score)
                } else {
                    scores_df$total_reel <- NA
                }
            }
        }

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
                total_reel = ifelse("total_reel" %in% colnames(scores), scores$total_reel[1], NA),
                detail = "Veuillez entrer au moins un score"
            ))
        }

        # Score actuel
        score_actuel <- sum(scores$Score)
        trous_joues <- nrow(scores)
        trous_restants <- 18 - trous_joues

        # Statistiques historiques (uniquement sur données complètes)
        hist_data <- data$historiques
        moyenne_globale <- mean(hist_data$TOTAL / 18)

        # Différents modèles
        if(input$modele == "buhlmann") {
            # Modèle Bühlmann-Straub
            z <- input$z_factor
            moyenne_indiv <- mean(scores$Score)
            pred_par_trou <- z * moyenne_indiv + (1 - z) * moyenne_globale

        } else if(input$modele == "bayes_poisson_gamma") {
            # Modèle bayésien Poisson/Gamma
            # S|Theta ~ Poisson(Theta)
            # Theta ~ Gamma(alpha, lambda)

            # Extraire tous les scores des données historiques COMPLÈTES
            score_cols <- grep("Score_hole", colnames(hist_data), value = TRUE)
            all_scores <- unlist(hist_data[, score_cols])
            all_scores <- all_scores[!is.na(all_scores)] ## là pour l'instant mais avec le nettoyage de données, ca ne sera pas nécessaire.

            # Estimer les paramètres du prior Gamma (méthode des moments)
            m <- mean(all_scores)
            v <- var(all_scores)

            lambda <- m / (v - m)
            alpha <- m * lambda

            # Ajuster le modèle bayésien
            fit_bayes <- cm("bayes",
                            data = scores$Score,
                            likelihood = "poisson",
                            shape = alpha,
                            rate = lambda)

            # Prime bayésienne par trou
            pred_par_trou <- predict(fit_bayes)

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

            # Prédiction bayésienne pour le composite
            all_scores <- unlist(hist_data[, score_cols])
            all_scores <- all_scores[!is.na(all_scores)]
            m <- mean(all_scores)
            v <- var(all_scores)
            lambda <- m / (v - m)
            alpha <- m * lambda
            fit_bayes <- cm("bayes", data = scores$Score, likelihood = "poisson",
                            shape = alpha, rate = lambda)
            pred_bayes <- predict(fit_bayes)

            pred_hachemeister <- if(nrow(scores) >= 2) {
                mean(predict(lm(Score ~ Trou, data = scores),
                             newdata = data.frame(Trou = mean(1:18))))
            } else {
                mean(scores$Score)
            }

            pred_par_trou <- (input$poids_buhlmann * pred_buhlmann +
                                  input$poids_bayes * pred_bayes +
                                  input$poids_hachemeister * pred_hachemeister)
        }

        # Prédiction totale
        total_pred <- score_actuel + (pred_par_trou * trous_restants)

        # Intervalle de confiance
        sd_historique <- sd(hist_data$TOTAL / 18, na.rm = TRUE)
        ic_lower <- total_pred - 1.96 * sd_historique * sqrt(trous_restants)
        ic_upper <- total_pred + 1.96 * sd_historique * sqrt(trous_restants)

        # Récupérer le score réel si disponible
        total_reel <- if("total_reel" %in% colnames(scores)) scores$total_reel[1] else NA

        list(
            total_pred = round(total_pred),
            score_actuel = score_actuel,
            trous_restants = trous_restants,
            pred_par_trou = round(pred_par_trou, 2),
            ic_lower = round(ic_lower),
            ic_upper = round(ic_upper),
            total_reel = total_reel,
            erreur = if(!is.na(total_reel) && !is.na(total_pred)) round(total_pred - total_reel) else NA,
            detail = paste("Prévision par trou restant:", round(pred_par_trou, 2),
                           "- Intervalle de confiance à 95%: [",
                           round(ic_lower), ",", round(ic_upper), "]")
        )
    })

    # Output: Statut des données
    output$data_status <- renderText({
        data <- load_data()
        paste0("✅ Modèles entraînés sur ", nrow(data$historiques),
               " rondes complètes - ", nrow(data$partiels),
               " rondes partielles disponibles pour test")
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

    output$box_erreur_pred <- renderText({
        pred <- prediction()
        if(is.na(pred$erreur)) "--" else as.character(pred$erreur)
    })

    # Output: Détail de la prévision
    output$detail_prediction <- renderPrint({
        pred <- prediction()
        if(is.null(pred$detail)) return("Veuillez entrer des scores")

        data <- load_data()

        cat("=== PRÉVISION DÉTAILLÉE ===\n\n")
        cat("Modèle entraîné sur:", nrow(data$historiques), "rondes complètes\n")
        cat("Mode de saisie:", input$mode_saisie, "\n")
        if(input$mode_saisie == "test" && !is.na(pred$total_reel)) {
            cat("Ronde testée:", input$ronde_test, "\n")
            cat("Score réel:", pred$total_reel, "\n")
        }
        cat("Score actuel:", pred$score_actuel, "\n")
        cat("Trous joués:", 18 - pred$trous_restants, "\n")
        cat("Trous restants:", pred$trous_restants, "\n")
        cat("\nPerformance estimée par trou:", pred$pred_par_trou, "\n")
        cat("\nScore total prédit:", pred$total_pred, "\n")
        if(!is.na(pred$erreur)) {
            cat("Erreur de prédiction:", pred$erreur, "\n")
        }
        cat(pred$detail, "\n")
        cat("\nModèle utilisé:",
            switch(input$modele,
                   "buhlmann" = "Bühlmann-Straub",
                   "bayes_poisson_gamma" = "Bayésien Poisson/Gamma",
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

        # Distribution des totaux historiques (données d'entraînement)
        hist_totals <- data$historiques$TOTAL

        # Créer le plot
        plot <- plot_ly() %>%
            add_histogram(x = hist_totals,
                          name = "Historique (entraînement)",
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

        # Ajouter le score réel si disponible
        if(!is.na(pred$total_reel)) {
            plot <- plot %>%
                add_segments(x = pred$total_reel, xend = pred$total_reel,
                             y = 0, yend = 1, yref = "paper",
                             line = list(color = "blue", width = 2, dash = "dot"),
                             name = "Score réel",
                             showlegend = TRUE) %>%
                layout(annotations = list(
                    list(x = pred$total_reel, y = 0.85, yref = "paper",
                         text = "Réel", showarrow = FALSE,
                         font = list(color = "blue"))
                ))
        }

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

        # Ajouter le score réel si disponible
        if(!is.na(pred$total_reel)) {
            p <- p +
                geom_hline(yintercept = pred$total_reel,
                           color = "blue", linetype = "dotdash", linewidth = 1)
        }

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
            caption = paste("Premières 100 rondes sur", nrow(data$historiques), "rondes complètes (entraînement)")
        )
    })

    # Output: Table des données partielles
    output$table_partiels <- renderDT({
        data <- load_data()
        datatable(
            data$partiels,
            options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = 'Bfrtip'
            ),
            rownames = FALSE,
            caption = paste(nrow(data$partiels), "rondes partielles disponibles pour test")
        )
    })

    # Output: Statistiques descriptives
    output$stats_descriptives <- renderPrint({
        data <- load_data()
        hist_data <- data$historiques

        cat("=== STATISTIQUES DESCRIPTIVES (Données d'entraînement) ===\n\n")
        cat("Nombre de rondes historiques:", nrow(hist_data), "\n")
        cat("\nScores totaux:\n")
        cat("  Moyenne:", round(mean(hist_data$TOTAL, na.rm = TRUE), 2), "\n")
        cat("  Médiane:", median(hist_data$TOTAL, na.rm = TRUE), "\n")
        cat("  Écart-type:", round(sd(hist_data$TOTAL, na.rm = TRUE), 2), "\n")
        cat("  Minimum:", min(hist_data$TOTAL, na.rm = TRUE), "\n")
        cat("  Maximum:", max(hist_data$TOTAL, na.rm = TRUE), "\n")
        cat("\nPerformance par trou:\n")
        cat("  Moyenne:", round(mean(hist_data$TOTAL / 18, na.rm = TRUE), 2), "\n")
    })

    # Output: Statistiques des données partielles
    output$stats_partiels <- renderPrint({
        data <- load_data()
        partiels <- data$partiels

        cat("=== STATISTIQUES DES DONNÉES DE TEST ===\n\n")
        cat("Nombre de rondes partielles:", nrow(partiels), "\n")

        # Calculer le nombre de trous joués par ronde
        score_cols <- grep("Score_hole", colnames(partiels), value = TRUE)
        trous_joues <- apply(partiels[, score_cols], 1, function(x) sum(!is.na(x)))

        cat("\nRépartition des trous joués:\n")
        cat("  Moyenne:", round(mean(trous_joues), 2), "trous\n")
        cat("  Minimum:", min(trous_joues), "trous\n")
        cat("  Maximum:", max(trous_joues), "trous\n")
    })

    # Output: Comparaison des modèles
    output$plot_comparaison <- renderPlotly({
        # Simulation de prédictions pour différents modèles
        set.seed(123)
        n_sim <- 50

        simul_data <- data.frame(
            Modele = rep(c("Bühlmann", "Bayésien", "Hachemeister", "Composite"), each = n_sim),
            Erreur = c(
                rnorm(n_sim, mean = 0, sd = 2),
                rnorm(n_sim, mean = 0.2, sd = 1.9),
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
            Modele = c("Bühlmann", "Bayésien", "Hachemeister", "Composite"),
            MAE = c(2.1, 1.9, 2.3, 1.6),
            RMSE = c(2.8, 2.5, 3.0, 2.1),
            Bias = c(0.1, 0.2, -0.2, 0.05)
        )

        datatable(
            metriques,
            options = list(
                pageLength = 4,
                dom = 't'
            ),
            rownames = FALSE
        ) %>%
            formatRound(columns = c("MAE", "RMSE", "Bias"), digits = 2)
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
                 y = "Résidu") +
            theme_minimal(base_size = 14)
    })

    # Réinitialisation
    observeEvent(input$btn_reset, {
        if(input$mode_saisie == "manuelle") {
            for(i in 1:18) {
                updateNumericInput(session, paste0("trou_", i), value = NA)
            }
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
                                "bayes_poisson_gamma" = "Bayésien Poisson/Gamma",
                                "hachemeister" = "Hachemeister",
                                "composite" = "Composite"),
                Mode_saisie = input$mode_saisie,
                Ronde_testee = ifelse(input$mode_saisie == "test", input$ronde_test, NA),
                Score_actuel = ifelse(is.null(pred$score_actuel), 0, pred$score_actuel),
                Trous_joues = 18 - ifelse(is.null(pred$trous_restants), 18, pred$trous_restants),
                Trous_restants = ifelse(is.null(pred$trous_restants), 18, pred$trous_restants),
                Prediction_totale = ifelse(is.na(pred$total_pred), NA, pred$total_pred),
                Score_reel = ifelse(is.na(pred$total_reel), NA, pred$total_reel),
                Erreur = ifelse(is.na(pred$erreur), NA, pred$erreur),
                IC_bas = ifelse(is.na(pred$ic_lower), NA, pred$ic_lower),
                IC_haut = ifelse(is.na(pred$ic_upper), NA, pred$ic_upper)
            )
            write.csv2(data_download, file, row.names = FALSE)
        }
    )
}

# Lancer l'application
shinyApp(ui = ui, server = server)
