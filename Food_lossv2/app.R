#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(networkD3)

##########################################
#       Data
##########################################
### Food loss Data
food <- read.csv("Data_app.csv", na = "")
# Rename columns 
names(food)[5] <- "year"
# Varibles selection
countries <- unique(food$country)

# Linebreaks
linebreaks <- function(n) {HTML(strrep(br(), n))}

############################################
#       User Interface
############################################
# Define UI for application 
ui <- fluidPage(
    theme = shinytheme("slate"),
    tags$head(HTML("<title>Desperdicio de Comida</title>")),
    # Navegation page
    navbarPage(
        # Title
        title = div(tags$a(href = "https://www.facebook.com/An%C3%A1lisis-y-visualizaci%C3%B3n-de-datos-100602148375744",
                           target = "_blank", 
                           img(src = "https://raw.githubusercontent.com/DataFeast71/DataFeast/08c51765aac95160b21e4e6652cf39a9d93b0eb1/docs/img/Logo_feast.svg", style = "margin-top: -15px;padding-right:0px;padding-bottom:0px", 
                                                  height = 50))),
        tabPanel(
            title = "Desperdicio en el mundo",
            #Input values
            sidebarPanel(
                HTML("<h3>Parámetros de entrada</h3>"),
                helpText("Crea un diagrama de Sankey con la información del desperdicio de comida en el año seleccionado."),
                # Año
                sliderInput(inputId = "Year",
                            label = "Año",
                            min = min(food$year),
                            max = max(food$year), step = 1, animate = FALSE, sep = "",
                            value = 2016),
                actionButton("SubmitButton",
                             "Analizar",
                             class = "btn btn-primary")
            ), # Side bar
            mainPanel(
                tabsetPanel(type = "tabs",
                            tabPanel("Cultivo", linebreaks(3),
                                     sankeyNetworkOutput("SankeyCrops", width = "100%"), linebreaks(1),
                                     "Data Source: http://www.fao.org/platform-food-loss-waste/flw-data/en/"),
                            tabPanel("Cadena de producción", linebreaks(3),
                                     sankeyNetworkOutput("SankeySupply", width = "100%"), linebreaks(1),
                                     "Fuente: http://www.fao.org/platform-food-loss-waste/flw-data/en/"),
                            tabPanel("Tabla", dataTableOutput("FoodDataWorld")))
            ) #Main panel 1
        ), #Tab panel 1
        tabPanel(
            title = "Desperdicio por País",
            
            sidebarPanel(
                HTML("<h3>Parámetros de entrada</h3>"),
                helpText("Crea un diagrama de Sankey seleccionando el país y el año para ver como hay sido el desperdicio de comida"),
                # input values 
                # Year
                sliderInput(inputId = "Year2",
                            label = "Año",
                            min = min(food$year),
                            max = max(food$year), step = 1, animate = FALSE, sep = "",
                            value = 2016),
                # Country
                selectInput("Country",
                            label = "País:",
                            choices = countries,
                            selected = "Mexico"),
                actionButton("SubmitButton2",
                             "Analizar",
                             class = "btn btn-primary")
            ),# Sidebar2
            mainPanel(
                tabsetPanel(type = "tabs",
                            tabPanel("Desperdicio", linebreaks(3),
                                     textOutput("YearsData"),
                                     sankeyNetworkOutput("SankeyCountry", width = "100%"), linebreaks(1),
                                     "Fuente: http://www.fao.org/platform-food-loss-waste/flw-data/en/"),
                            tabPanel("Tabla", dataTableOutput("FoodDataCountry"))
                            )
            )
        ) #tab2
    ) # Navegation page
    ) #UI

############################################
#       Server
############################################

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    # Input Tab1
    Food1 <- reactive({
        # year selection 
        df <- food %>%  filter(year == input$Year) %>% distinct()
    })
    
    ## Input Tab2
    Food2 <- reactive({
        #Year and Country
        df <- food %>%  filter(year == input$Year2 & country == input$Country) %>%  distinct()
    })
    
    ## Input tab2 years available
    Food3 <- reactive({
        # years
        years <- food %>% filter(country == input$Country) %>%  pull(year) %>% unique()
    })
    
    # Sankey function data preparation
    sankey_data <- function(df, link1, link2) {
        # Make the links
        links1 <- df %>% 
            group_by(Class, get(link1)) %>% 
            summarise(loss = sum(loss_per_clean)) %>% 
            ungroup() 
        names(links1) <- c("source", "target","loss")
        
        links2 <- df %>% 
            group_by(get(link1), get(link2)) %>% 
            summarise(loss = sum(loss_per_clean)) %>% 
            ungroup() 
        
        names(links2) <- c("source", "target","loss")
        
        links <- rbind(links1, links2)
        
        # From these flows we need to create a node data frame: it lists every entities involved in the flow
        nodes <- data.frame(
            name = c(as.character(links$source),
                     as.character(links$target)) %>% unique()
        )
        
        # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
        links$IDsource <- match(links$source, nodes$name)-1 
        links$IDtarget <- match(links$target, nodes$name)-1
        links$group <- "white"
        
        return(list(links = links, nodes = nodes))
    }
    
    ## Output tab 1 panel 1
    output$SankeyCrops <- renderSankeyNetwork({
        if(input$SubmitButton > 0) {
            isolate({
                network <- sankey_data(df = Food1(), link1 = "crop", link2 = "country")

                sankey <- sankeyNetwork(Links = network$links, Nodes = network$nodes,
                              Source = "IDsource", Target = "IDtarget",
                              Value = "loss", NodeID = "name", sinksRight = FALSE, fontSize = 10, units = "%")
                
                # render with js
                sankey_rendered <- htmlwidgets::onRender(sankey,
                                'function(el, x) {
                                        d3.selectAll(".link")
                                        .style("stroke", "rgb(200,200,200)");
                                        } 
                                '
                )
                sankey_rendered <- htmlwidgets::onRender(sankey_rendered,
                                'function(el, x) {
                                        d3.selectAll(".node text")
                                        .style("fill", "white");
                                }
                                '
                )
                sankey_rendered
            })
        }
    }) 
    
    ## Output tab 1 panel 2
    output$SankeySupply <- renderSankeyNetwork({
        if(input$SubmitButton > 0) {
            isolate({
                network <- sankey_data(df = Food1(), link1 = "crop", link2 = "fsc_location1")
                
                sankey <- sankeyNetwork(Links = network$links, Nodes = network$nodes,
                              Source = "IDsource", Target = "IDtarget",
                              Value = "loss", NodeID = "name", sinksRight = FALSE, fontSize = 10, units = "%")
                # render with js
                sankey_rendered <- htmlwidgets::onRender(sankey,
                                                         'function(el, x) {
                                        d3.selectAll(".link")
                                        .style("stroke", "rgb(200,200,200)");
                                        } 
                                '
                )
                sankey_rendered <- htmlwidgets::onRender(sankey_rendered,
                                                         'function(el, x) {
                                        d3.selectAll(".node text")
                                        .style("fill", "white");
                                }
                                '
                )
                sankey_rendered
            })
        }
    })
    
    ## Output tab 1 panel 3
    output$FoodDataWorld <- renderDataTable({
        if (input$SubmitButton) {
            isolate(Food1())
        }
    }, options = list(pageLength = 10))
    
    ## Output tab 2 panel 1
    output$YearsData <- renderText({
        if(input$SubmitButton2) {
            isolate({
                print(c("Datos disponibles:", paste(Food3(), collapse = ",")))
            })
        }
    })
    output$SankeyCountry <- renderSankeyNetwork({
        if (input$SubmitButton2 > 0) {
            isolate({
                network <- sankey_data(df = Food2(), link1 = "fsc_location1", link2 = "crop")
                
                sankey <- sankeyNetwork(Links = network$links, Nodes = network$nodes,
                              Source = "IDsource", Target = "IDtarget",
                              Value = "loss", NodeID = "name", sinksRight = FALSE, fontSize = 10, units = "%")
                # render with js
                sankey_rendered <- htmlwidgets::onRender(sankey,
                                                         'function(el, x) {
                                        d3.selectAll(".link")
                                        .style("stroke", "rgb(200,200,200)");
                                        } 
                                '
                )
                sankey_rendered <- htmlwidgets::onRender(sankey_rendered,
                                                         'function(el, x) {
                                        d3.selectAll(".node text")
                                        .style("fill", "white");
                                }
                                '
                )
                sankey_rendered
            })
        }
    })
    
    ## Output tab 1 panel 3
    output$FoodDataCountry <- renderDataTable({
        if (input$SubmitButton2 > 0) {
            isolate(Food2())
        }
    }, options = list(pageLength = 10))
}

# Run the application 
shinyApp(ui = ui, server = server)
