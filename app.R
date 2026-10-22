# BUT Sciences des Données - 2026

library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)
library(bsicons)
library(tidyr)

# ------------------------------------------------------------
# 1. Chargement des données
# ------------------------------------------------------------

url_data <- "https://gist.githubusercontent.com/MNishita/93bdaa8e1db0b2d3a5f341e6d580fe35/raw/videogame_sales.csv"

if (file.exists("vgsales.csv")) {
  jeux <- read.csv("vgsales.csv", stringsAsFactors = FALSE)
} else {
  jeux <- read.csv(url_data, stringsAsFactors = FALSE)
}

jeux <- jeux %>%
  filter(!is.na(Year), !is.na(Genre), !is.na(Global_Sales)) %>%
  mutate(Year = as.numeric(Year))

# Couleurs utilisées dans les graphiques
couleurs <- c("#6558F5", "#00B8A9", "#FF9F43", "#E84393", "#3498DB",
              "#A55EEA", "#20BF6B", "#F7B731", "#FC5C65", "#778CA3")

# ------------------------------------------------------------
# 2. Interface du dashboard
# ------------------------------------------------------------

ui <- page_sidebar(
  title = tagList(bs_icon("controller"), "  Jeux vidéo : ventes et tendances"),
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#6558F5",
    secondary = "#00B8A9",
    bg = "#F4F6FB",
    fg = "#252A40",
    base_font = font_google("Roboto")
  ),
  header = tags$head(tags$style(HTML("
    body { background-color: #F4F6FB; }
    .navbar { box-shadow: 0 3px 12px rgba(37,42,64,0.10); }
    .bslib-sidebar-layout > .sidebar { background: #FFFFFF; border-right: 1px solid #E5E8F0; }
    .dashboard-intro { background: linear-gradient(110deg, #6558F5, #8B5CF6); color: white;
      padding: 18px 24px; border-radius: 16px; margin-bottom: 14px;
      box-shadow: 0 7px 20px rgba(101,88,245,0.18); }
    .dashboard-intro h2 { font-weight: 700; margin-top: 0; }
    .dashboard-intro p { margin-bottom: 0; font-size: 15px; opacity: 0.95; }
    .bslib-value-box { border: 0; border-radius: 15px; overflow: hidden;
      box-shadow: 0 5px 16px rgba(37,42,64,0.08); }
    .card { border: 0; border-radius: 15px; box-shadow: 0 5px 16px rgba(37,42,64,0.07);
      margin-bottom: 18px; }
    .card-header { background: white; color: #252A40; font-weight: 700;
      border-bottom: 1px solid #EDF0F6; padding: 16px 18px; }
    .card-body { padding: 18px; }
    .form-label { font-weight: 600; color: #343A55; }
    .sidebar p { font-size: 13px; color: #68708A; }
    .table { font-size: 14px; }
    .chart-grid { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 16px; align-items: stretch; }
    .chart-card { width: 100%; min-width: 0; margin-bottom: 0; }
    .chart-card .card-body { padding: 12px; min-width: 0; overflow: hidden; }
    .chart-card .card-header { padding: 12px 14px; }
    .shiny-plot-output { display: block !important; width: 100% !important; height: 360px !important; min-height: 360px !important; }
    .shiny-plot-output img { width: 100% !important; height: 100% !important; object-fit: contain; }
    @media (max-width: 900px) {
      .chart-grid { grid-template-columns: minmax(0, 1fr); }
      .shiny-plot-output { height: 380px !important; min-height: 380px !important; }
    }
  "))),

  sidebar = sidebar(
    width = 285,
    h4("Personnaliser les résultats", class = "fw-bold"),
    p("Utilise les filtres ci-dessous pour explorer les ventes."),
    hr(),
    sliderInput(
      "annees", "Période de sortie :",
      min = floor(min(jeux$Year, na.rm = TRUE)),
      max = ceiling(max(jeux$Year, na.rm = TRUE)),
      value = c(max(1980, floor(min(jeux$Year, na.rm = TRUE))),
                ceiling(max(jeux$Year, na.rm = TRUE))),
      sep = ""
    ),
    selectInput(
      "genre", "Genre de jeu :",
      choices = c("Tous les genres", sort(unique(jeux$Genre))),
      selected = "Tous les genres"
    ),
    selectInput(
      "plateforme", "Plateforme :",
      choices = c("Toutes les plateformes", sort(unique(jeux$Platform))),
      selected = "Toutes les plateformes"
    ),
    hr(),
    div(class = "small",
      p(bs_icon("info-circle"), " Les ventes sont exprimées en millions d'exemplaires."),
      p("Source : Video Game Sales Dataset.")
    )
  ),

  div(class = "dashboard-intro",
    h2("Explore l'univers du jeu vidéo "),
    p("Compare les genres, les plateformes et l'évolution des ventes dans le monde. Les graphiques se mettent à jour selon tes filtres.")
  ),

  # Les quatre blocs d’indicateurs ont été retirés pour laisser plus de place aux graphiques.

  # Les graphiques sont affichés en grille 2 x 2 pour mieux utiliser l'espace.
  div(class = "chart-grid",
    card(
      class = "chart-card",
      card_header(tagList(bs_icon("bar-chart-fill"), "  Ventes par genre")),
      plotOutput("graph_genre", height = "360px", width = "100%")
    ),
    card(
      class = "chart-card",
      card_header(tagList(bs_icon("graph-up-arrow"), "  Évolution des ventes")),
      plotOutput("graph_annee", height = "360px", width = "100%")
    ),
    card(
      class = "chart-card",
      card_header(tagList(bs_icon("controller"), "  Top 10 des plateformes")),
      plotOutput("graph_plateforme", height = "360px", width = "100%")
    ),
    card(
      class = "chart-card",
      card_header(tagList(bs_icon("globe2"), "  Ventes par région")),
      plotOutput("graph_region", height = "360px", width = "100%")
    )
  ),

  card(
    card_header(tagList(bs_icon("trophy-fill"), "  Top 10 des jeux vidéo")),
    p("Les jeux sont classés selon leurs ventes mondiales.", class = "text-muted small"),
    tableOutput("table_top")
  )
)

# ------------------------------------------------------------
# 3. Traitement des données et affichage
# ------------------------------------------------------------

server <- function(input, output, session) {

  donnees <- reactive({
    d <- jeux %>% filter(Year >= input$annees[1], Year <= input$annees[2])

    if (input$genre != "Tous les genres") {
      d <- d %>% filter(Genre == input$genre)
    }
    if (input$plateforme != "Toutes les plateformes") {
      d <- d %>% filter(Platform == input$plateforme)
    }
    d
  })

  output$nb_jeux <- renderText({
    format(nrow(donnees()), big.mark = " ")
  })

  output$ventes_globales <- renderText({
    paste0(format(round(sum(donnees()$Global_Sales, na.rm = TRUE), 1), nsmall = 1), " M")
  })

  output$genre_top <- renderText({
    d <- donnees() %>% group_by(Genre) %>%
      summarise(Ventes = sum(Global_Sales, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Ventes))
    if (nrow(d) == 0) "Aucun" else as.character(d$Genre[1])
  })

  output$jeu_top <- renderText({
    d <- donnees() %>% arrange(desc(Global_Sales))
    if (nrow(d) == 0) "Aucun" else as.character(d$Name[1])
  })

  output$graph_genre <- renderPlot({
    d <- donnees() %>% group_by(Genre) %>%
      summarise(Ventes = sum(Global_Sales, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Ventes))
    validate(need(nrow(d) > 0, "Aucune donnée pour ces filtres."))

    ggplot(d, aes(x = reorder(Genre, Ventes), y = Ventes, fill = Genre)) +
      geom_col(show.legend = FALSE, width = 0.75) + coord_flip() +
      scale_fill_manual(values = rep(couleurs, length.out = nrow(d))) +
      labs(x = NULL, y = "Ventes mondiales (millions)") +
      theme_minimal(base_size = 15) +
      theme(plot.title = element_text(face = "bold"), axis.text = element_text(color = "#343A55"),
            panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), axis.text.y = element_text(size = 13), axis.text.x = element_text(size = 12))
  }, res = 96, height = 360)

  output$graph_annee <- renderPlot({
    d <- donnees() %>% group_by(Year) %>%
      summarise(Ventes = sum(Global_Sales, na.rm = TRUE), .groups = "drop")
    validate(need(nrow(d) > 0, "Aucune donnée pour ces filtres."))

    ggplot(d, aes(x = Year, y = Ventes)) +
      geom_area(fill = "#6558F5", alpha = 0.15) +
      geom_line(color = "#6558F5", linewidth = 1.2) +
      geom_point(color = "#00B8A9", size = 2) +
      labs(x = "Année", y = "Ventes mondiales (millions)") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
      theme_minimal(base_size = 15) +
      theme(axis.text = element_text(color = "#343A55", size = 12),
            axis.title = element_text(face = "bold"), panel.grid.minor = element_blank())
  }, res = 96, height = 360)

  output$graph_plateforme <- renderPlot({
    d <- donnees() %>% group_by(Platform) %>%
      summarise(Ventes = sum(Global_Sales, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Ventes)) %>% slice_head(n = 10)
    validate(need(nrow(d) > 0, "Aucune donnée pour ces filtres."))

    ggplot(d, aes(x = reorder(Platform, Ventes), y = Ventes, fill = Ventes)) +
      geom_col(show.legend = FALSE, width = 0.75) + coord_flip() +
      scale_fill_gradient(low = "#A5B4FC", high = "#4F46E5") +
      labs(x = NULL, y = "Ventes mondiales (millions)") +
      theme_minimal(base_size = 15) +
      theme(axis.text = element_text(color = "#343A55"), panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank(), axis.text.y = element_text(size = 13), axis.text.x = element_text(size = 12))
  }, res = 96, height = 360)

  output$graph_region <- renderPlot({
    d <- donnees() %>% summarise(
      `Amérique du Nord` = sum(NA_Sales, na.rm = TRUE),
      `Europe` = sum(EU_Sales, na.rm = TRUE),
      `Japon` = sum(JP_Sales, na.rm = TRUE),
      `Autres régions` = sum(Other_Sales, na.rm = TRUE)
    ) %>% pivot_longer(cols = everything(), names_to = "Region", values_to = "Ventes")
    validate(need(nrow(d) > 0, "Aucune donnée pour ces filtres."))

    ggplot(d, aes(x = reorder(Region, Ventes), y = Ventes, fill = Region)) +
      geom_col(show.legend = FALSE, width = 0.7) + coord_flip() +
      scale_fill_manual(values = c("Amérique du Nord" = "#6558F5", "Europe" = "#00B8A9",
                                   "Japon" = "#FF9F43", "Autres régions" = "#E84393")) +
      labs(x = NULL, y = "Ventes (millions)") +
      theme_minimal(base_size = 15) +
      theme(axis.text = element_text(color = "#343A55"), panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank(), axis.text.y = element_text(size = 13), axis.text.x = element_text(size = 12))
  }, res = 96, height = 360)

  output$table_top <- renderTable({
    donnees() %>% arrange(desc(Global_Sales)) %>%
      select(Rang = Rank, Jeu = Name, Plateforme = Platform,
             Genre, Annee = Year, Ventes_mondiales = Global_Sales) %>%
      slice_head(n = 10) %>%
      mutate(Ventes_mondiales = round(Ventes_mondiales, 2))
  }, striped = TRUE, bordered = FALSE, hover = TRUE, spacing = "s", na = "-")
}

shinyApp(ui = ui, server = server)
