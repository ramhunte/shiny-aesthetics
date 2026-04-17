library(shiny)
library(bslib)
library(palmerpenguins)
library(ggplot2)
library(fontawesome)
library(thematic)
library(shinyWidgets)

ui <- page_navbar(
  title = "Palmer Penguins Explorer",

  theme = bs_theme(
    bootswatch = "minty", # start with a pre-built theme
    fg = "#293f2fff", # change foreground color
    bg = "#e5f2fcff", # change background color
    primary = "#2f99a7ff", # accents
    base_font = font_google("Roboto"), # fonts
    heading_font = font_google("Pacifico") # fonts
  ),

  navbar_options = bslib::navbar_options(
    bg = "#021d31ff"
  ),


  tags$head(tags$link(
    rel = "stylesheet",
    type = "text/css",
    href = "styles.css"
  )),

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
        shinyWidgets::pickerInput(
          inputId = "color_var",
          label = "Color by:",
          choices = c(
            "Species" = "species",
            "Island" = "island",
            "Sex" = "sex"
          ),
          selected = "species",
          options = list(
            style = "btn-primary"
          )
        ),
        checkboxInput(
          "show_smooth",
          "Show trend line",
          value = FALSE
        ),

        htmltools::tags$img(
          src = "sablefish.jpg",
          width = "100%",
          style = "margin-top: 1rem; border-radius: 6px;"
        ),

        htmltools::tags$p(
          "Sablefish are deep-sea fish found in the North Pacific, known for their rich flavor and high oil content.",
          style = "color: teal; font-size: .8rem; font-style: italic;"
        )
      ),

      layout_column_wrap(
        columns = 3,
        value_box(
          value = nrow(penguins),
          title = "Total Penguins",
          showcase = bsicons::bs_icon("hash"),
          theme = "primary"
        ),
        value_box(
          value = span(
            paste(
              na.omit(unique(as.character(penguins$species))),
              collapse = ", "
            ),
            style = "font-size: 1.5rem;"
          ),
          title = "Penguin Species",
          showcase = bsicons::bs_icon("feather"),
          theme = "success"
        ),
        value_box(
          value = span(
            paste(
              na.omit(unique(as.character(penguins$island))),
              collapse = ", "
            ),
            style = "font-size: 1.5rem;"
          ),
          title = "Islands",
          showcase = bsicons::bs_icon("globe"),
          theme = "info"
        )
      ),

      card(
        # style = "background-color: white;",
        card_header("Penguin Flipper Length vs Body Mass"),

        plotOutput("penguin_plot")
      )
    )
  ),

  nav_panel(
    "Data Table",
    layout_sidebar(
      sidebar = sidebar(
        pickerInput(
          "species_filter",
          "Filter by Species:",
          choices = c("All", "Adelie", "Chinstrap", "Gentoo"),
          selected = "All",
          options = list(
            style = "btn-primary"
          )
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

      theme(
        text = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16)
      )

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

  thematic::thematic_shiny(font = "auto")
}


shinyApp(ui = ui, server = server)
