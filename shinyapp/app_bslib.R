library(shiny)
library(bslib)
library(palmerpenguins)
library(ggplot2)

ui <- page_navbar(
  title = "Palmer Penguins Explorer",

  theme = bs_theme(
    bootswatch = "minty", # start with a pre-built theme
    fg = "#293f2fff", # change foreground color
    bg = "#e5f2fcff", # change background color
    "navbar-bg" = "#021d31ff", # navbar background
    primary = "#2f99a7ff", # accents
    base_font = font_google("Roboto"), # fonts
    heading_font = font_google("Roboto Slab") # fonts
  ),

  nav_panel(
    "Scatter Plot",
    layout_sidebar(
      sidebar = sidebar(
        sliderInput(
          "mass_range",
          "Body Mass Range (g):",
          min = 2700,
          max = 6300,
          value = c(3000, 6000)
        ),
        selectInput(
          "color_var",
          "Color by:",
          choices = c(
            "Species" = "species",
            "Island" = "island",
            "Sex" = "sex"
          ),
          selected = "species"
        ),
        checkboxInput(
          "show_smooth",
          "Show trend line",
          value = FALSE
        )
      ),
      card(
        card_header("Penguin Flipper Length vs Body Mass"),
        plotOutput("penguin_plot")
      )
    )
  ),

  nav_panel(
    "Data Table",
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          "species_filter",
          "Filter by Species:",
          choices = c("All", "Adelie", "Chinstrap", "Gentoo"),
          selected = "All"
        ),
        radioButtons(
          "island_filter",
          "Filter by Island:",
          choices = c("All", "Biscoe", "Dream", "Torgersen"),
          selected = "All"
        )
      ),
      card(
        card_header("Penguin Data"),
        tableOutput("penguin_table")
      )
    )
  ),

  nav_panel(
    "Summary",
    card(
      card_header("Dataset Summary"),
      verbatimTextOutput("summary_stats")
    )
  )
)

server <- function(input, output) {
  output$penguin_plot <- renderPlot({
    filtered_data <- penguins[
      penguins$body_mass_g >= input$mass_range[1] &
        penguins$body_mass_g <= input$mass_range[2] &
        !is.na(penguins$body_mass_g) &
        !is.na(penguins$flipper_length_mm),
    ]

    p <- ggplot(
      filtered_data,
      aes(
        x = flipper_length_mm,
        y = body_mass_g,
        color = .data[[input$color_var]]
      )
    ) +
      geom_point(size = 3) +
      labs(
        x = "Flipper Length (mm)",
        y = "Body Mass (g)",
        color = tools::toTitleCase(input$color_var)
      ) +
      theme_minimal()

    if (input$show_smooth) {
      p <- p + geom_smooth(method = "lm", se = FALSE)
    }

    p
  })

  output$penguin_table <- renderTable({
    data <- penguins

    if (input$species_filter != "All") {
      data <- data[data$species == input$species_filter, ]
    }

    if (input$island_filter != "All") {
      data <- data[data$island == input$island_filter, ]
    }

    head(data, 20)
  })

  output$summary_stats <- renderPrint({
    summary(penguins)
  })
}

shinyApp(ui = ui, server = server)
