library("shiny")
library("rsconnect")
library("tidyverse")
library("ggraph")
library("bslib")

# UI ===========================================================================

ui <- fluidPage(

  # App theme ------------------------------------------------------------------
  theme = bs_theme(version = 4, bootswatch = "journal"),

  # App title ------------------------------------------------------------------
  titlePanel("Setworks v0.1", windowTitle = "Shiny Setworks"),
  p(strong("Set visualizations that scale")),

  # Sidebar layout -------------------------------------------------------------
  sidebarLayout(

    # Sidebar panel
    sidebarPanel(

      HTML("<p><em>Moving beyond Venn at the stroke of a pen. Setworks
      capitalize on networks and bring set visualizations the power to scale and
      visualize increasingly complex, hierarchical, relations. Read the
      <a href='https://doi.org/10.31234/osf.io/kj7vg'>preprint</a>. View code,
      make suggestions, and report issues on
      <a href='https://github.com/aosavi/setworks'>github</a>.</em></p>"),


      # Data -------------------------------------------------------------------

      h3("1. Data"),

      # Input: Example data
      h5("Try Example Data"),
      HTML("<p>Barista shows the ingredients of coffee
      drinks. Whigs shows the members of American whig organizations
      (<a href='https://ggraph.data-imaginist.com/reference/whigs.html'>source
      </a>).</p>"),

      actionButton("exampleDataBarista", "Barista"),
      actionButton("exampleDataWhigs", "Whigs"),

      # Input: User data
      h5("Upload CSV File"),
      p("Required format shown in example data. An incidence matrix, with
      elements as rows and sets as columns. One reflects membership, zero
      reflects no membership. First row must contain set names. One column must
      contain element names."),

      fileInput("userData", NULL,
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),

      # Input: Select header
      checkboxInput("header", "First Row Contains Set Names", TRUE),

      # Input: Select delimiter
      radioButtons("delim", "Delimiter",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ",",
                   inline = TRUE),

      # Input: Select quotes
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"',
                   inline = TRUE),

      # Input: Select number of rows to display --------------------------------

      # Horizontal line
      tags$hr(),

      # Input: Header or all rows
      radioButtons("disp", "Display",
                   choices = c("First Rows" = "head",
                               "All Rows" = "all"),
                   selected = "head",
                   inline = TRUE),

      # Select sets and elements -----------------------------------------------

      # Horizontal line
      tags$hr(),

      h3("2. Sets & Elements"),
      p("First select column with element names, then select desired set columns
        (minimum two required)."),

      # Input: select column with elements
      uiOutput("varElement"),

      # Input: select columns with sets
      uiOutput("varSets"),

      # Input: Select setwork --------------------------------------------------

      # Horizontal line
      tags$hr(),

      h3("3. Visualize"),
      p("Pick the desired setwork."),

      # Input: setwork
      radioButtons("setwork", NULL,
                   choices = c(None = "",
                               Pairwise = "pairwise",
                               Chord = "chord",
                               Waterfall = "waterfall"),
                   selected = "",
                   inline = TRUE),

      p(em("Waterfall setwork: numbers represent sets, dictated by the order of
        the selection.")),

      # Download plots ---------------------------------------------------------

      # Horizontal line
      tags$hr(),

      h3("4. Download"),

      # Download plot
      downloadButton("downloadPDF", ".pdf"),
      downloadButton("downloadPNG", ".png"),

      # Citation ---------------------------------------------------------------

      # Horizontal line
      tags$hr(),

      h3("5. Cite"),
      HTML("<p>Savi, A. O. (2021). <em>Setworks: Networks of complex set
           intersections</em>. PsyArXiv. <a href=
           'https://doi.org/10.31234/osf.io/kj7vg'>doi.org/10.31234/osf.io/kj7vg
           </a></p>")

    ),

    # Main panel for output ----------------------------------------------------
    mainPanel(

      # Fixed output: user file ------------------------------------------------
      tableOutput("dataTable"),

      # Conditional output: setworks -------------------------------------------
      conditionalPanel(
        condition = "input.setwork == 'pairwise'",
        plotOutput(outputId = "pairwiseSetwork")
      ),

      conditionalPanel(
        condition = "input.setwork == 'chord'",
        plotOutput(outputId = "chordSetwork")
      ),

      conditionalPanel(
        condition = "input.setwork == 'waterfall'",
        plotOutput(outputId = "waterfallSetwork")
      )
    )
  )
)

# SERVER =======================================================================

server <- function(input, output) {

  # Pick data ------------------------------------------------------------------

  # Dataframe depends on input (user data or example data)
  dataframe <- reactiveVal()

  # Create user data, reactive to options for read_delim
  dataframeUser <- reactive({
    req(input$userData)
    read_delim(input$userData$datapath,
               col_names = input$header,
               delim = input$delim,
               quote = input$quote)
  })

  # If barista data, save to dataframe()
  observeEvent(input$exampleDataBarista, {
    dataframe(read_csv("./data/barista.csv",
                       col_names = TRUE))
  })

  # If whigs data, save to dataframe()
  observeEvent(input$exampleDataWhigs, {
    dataframe(tibble::as_tibble(whigs, rownames = "Member"))
  })

  # If user data, save to dataframe()
  observeEvent(dataframeUser(), {
    dataframe(dataframeUser())
  })

  # Show data ------------------------------------------------------------------

  # Table
  output$dataTable <- renderTable({
    req(dataframe())
    if(input$disp == "head") {
      return(head(dataframe()))
    } else {
      return(dataframe())
    }
  })

  # Select elements and sets ---------------------------------------------------

  # Select elements variable
  output$varElement <- renderUI({
    req(dataframe())
    varSelectInput("elements",
                   "Elements",
                   dataframe())
  })

  # Select set variables
  output$varSets <- renderUI({
    req(dataframe())
    varSelectInput("sets",
                   "Sets",
                   dataframe() %>% select(-!!input$elements),
                   multiple = TRUE)
  })

  # Select setwork -------------------------------------------------------------

  # Pairwise
  output$pairwiseSetwork <- renderPlot({
    pairwise_setwork(data = dataframe(),
                     elements = input$elements,
                     sets = input$sets)
  })

  # Chord
  output$chordSetwork <- renderPlot({
    chord_setwork(data = dataframe(),
                  elements = input$elements,
                  sets = input$sets)
  })

  # Waterfall
  output$waterfallSetwork <- renderPlot({
    waterfall_setwork(data = dataframe(),
                      elements = input$elements,
                      sets = input$sets)
  })

  # Download last plot ---------------------------------------------------------

  # PDF
  output$downloadPDF <- downloadHandler(
    filename = function() { paste("awesome_setwork", ".pdf", sep = "") },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "pdf")
    }
  )

  # PNG
  output$downloadPNG <- downloadHandler(
    filename = function() { paste("awesome_setwork", ".png", sep = "") },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png")
    }
  )
}

shiny::shinyApp(ui = ui, server = server)
