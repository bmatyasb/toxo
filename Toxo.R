library(ggplot2)
library(stats)
library(shinythemes)
library(plotly)

ui <- fluidPage(
  tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
  theme = shinytheme("spacelab"),
  headerPanel("Toxo IgG calculator"), 
  sidebarLayout(
    sidebarPanel(
      h4(strong("Data")),
      condition = "input.tabs=='Data table'",
      textAreaInput("data", label = "Copy your standard curve data here", rows = 10),
      selectInput("separator", label = "Data separator", 
                  choices = c("Tab" = "\t", "Comma" = ",", "Space" = " ")),
      selectInput("decimal", label = "Decimal separator", 
                  choices = c("Point" = ".", "Comma" = ",")),
      actionButton("submit", "Submit"),
      h3(),
      hr(),  
      numericInput("ext", "Give the measured extinction", value = 1)),

  mainPanel(width = 6,
    tabsetPanel(
      type = "tabs",
        tabPanel("Data table",
          DT::DTOutput("out_table")),
        tabPanel("Results",
          h4("Your entered extinction:"),
          textOutput("o_target_ext"),
          h4("Corresponding IU/ml:"),
          textOutput("approx1"),
          plotOutput("ggplot"),
          plotlyOutput("plotly")
          
    )))))
  



server <- function(input, output, session){
  
  
  data <- eventReactive(input$submit, {
    # Adatok beolvasása
    data <- read.table(textConnection(input$data), header = TRUE, sep = input$separator, dec = input$decimal)
    
    # Oszlopok típusának átalakítása
    data[] <- lapply(data, function(x) {
      if(is.numeric(x)) {
        as.numeric(x)
      } else {
        as.factor(x)
      }
    })
    
    return(data)
  })  
  
output$out_table <- DT::renderDataTable(
    data(),
    options = list(autoWidth = FALSE),
  )
            
target_ext <- reactive({(input$ext)})
output$o_target_ext <- renderText({target_ext()})

approximate <- reactive({approx(data()$Ext, data()$IU.ml, xout = target_ext())$y})
output$approx1 <- renderText({approximate()})

plot <- reactive({ggplot(data(), aes(x=IU.ml, y=Ext)) +
    geom_line(color="black") +
    geom_point() + geom_hline(yintercept=target_ext(), 
                              color = "red", size=0.5) +
    geom_vline(xintercept=approximate(), 
               color = "red", size=0.5) +
    scale_x_continuous(n.breaks = 20) +
    annotate(geom="text", size = 5, x=approximate()*1.1, y=target_ext()*1.1, label=round(approximate(), digits=3),
             color="red")  
})

output$ggplot <- renderPlot(plot())
output$plotly <- renderPlotly(plot())



}
  
shinyApp(ui, server)

run_app <- function(options = list()) {
  shiny::shinyApp(ui = ui,
                  server = server,
                  options = options)
}
