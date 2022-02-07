library(shiny)
library(tidyverse)
library(shinydashboard)
library(plotly)
library(reactable)
library(gghighlight)


stats <- read_csv("all_stats_wide.csv")
graph_stats <- read_csv("all_stats_long.csv")

ui <- dashboardPage(
        dashboardHeader(title = "MK8 Builder", titleWidth = 350),
        
        dashboardSidebar(
                
                sidebarMenu(
                        menuItem("Explorer", tabName = "explorer", icon = icon('th')),
                        menuItem("Optimizer", tabName = 'optimizer', icon = icon('tachometer-alt')),
                        menuItem("All Builds", tabName = 'all', icon = icon('globe'))
                )
        ),
        
        dashboardBody(
                tabItems(
                        tabItem(tabName = "explorer",
                                
                                fluidRow(
                                        
                                        box(title = "Build 1",
                                            
                                            box(selectInput(inputId = 'chars1', 
                                                            label = "Driver", 
                                                            choices = unique(graph_stats$Driver)),
                                                width = 3,
                                                height = 100),
                                            box(selectInput(inputId = 'kart1',
                                                            label = "Kart",
                                                            choices = unique(graph_stats$Kart)),
                                                width = 3,
                                                height = 100),
                                            box(selectInput(inputId = 'tire1',
                                                            label = "Wheels",
                                                            choices = unique(graph_stats$Tires)),
                                                width = 2,
                                                height = 100),
                                            box(selectInput(inputId = 'glider1',
                                                            label = "Glider",
                                                            choices = unique(graph_stats$Glider)),
                                                width = 4,
                                                height = 100),
                                            
                                            width = 12,
                                            collapsible = TRUE
                                            
                                        )),
                                fluidRow(
                                        box(title = "Build 2",
                                            
                                            box(selectInput(inputId = 'chars2', 
                                                            label = "Driver", 
                                                            choices = unique(graph_stats$Driver)),
                                                width = 3,
                                                height = 100),
                                            box(selectInput(inputId = 'kart2',
                                                            label = "Kart",
                                                            choices = unique(graph_stats$Kart)),
                                                width = 3,
                                                height = 100),
                                            box(selectInput(inputId = 'tire2',
                                                            label = "Wheels",
                                                            choices = unique(graph_stats$Tires)),
                                                width = 2,
                                                height = 100),
                                            box(selectInput(inputId = 'glider2',
                                                            label = "Glider",
                                                            choices = unique(graph_stats$Glider)),
                                                width = 4,
                                                height = 100),
                                            
                                            width = 12,
                                            collapsible = TRUE
                                            
                                        )
                                ),
                                fluidRow(
                                        box(plotOutput(outputId = 'stat_graph1')),
                                        box(plotOutput(outputId = 'stat_graph2'))
                                        
                                        
                                )
                                
                                
                        ),
                        
                        tabItem(tabName = 'optimizer',
                                
                                fluidRow(
                                        
                                        box(title = "Select Stats in Order of Importance",
                                            
                                            box(selectInput(inputId = 'stat1', 
                                                            label = "Stat 1", 
                                                            choices = unique(graph_stats$stat)),
                                                width = 3,
                                                height = 100),
                                            box(selectInput(inputId = 'stat2',
                                                            label = "Stat 2",
                                                            choices = unique(graph_stats$stat)),
                                                width = 3,
                                                height = 100),
                                            box(selectInput(inputId = 'stat3',
                                                            label = "Stat 3",
                                                            choices = unique(graph_stats$stat)),
                                                width = 2,
                                                height = 100),
                                            box(selectInput(inputId = 'stat4',
                                                            label = "Stat 4",
                                                            choices = unique(graph_stats$stat)),
                                                width = 2,
                                                height = 100),
                                            box(selectInput(inputId = 'stat5',
                                                            label = "Stat 5",
                                                            choices = unique(graph_stats$stat)),
                                                width = 2,
                                                height = 100),
                                            
                                            width = 12,
                                            collapsible = TRUE
                                            
                                        )),
                                
                                fluidRow(box(title = "Builds by Stat1 and Stat2",
                                            plotlyOutput(outputId = "opt_graph"), 
                                                width = 12)),
                                fluidRow(box(title = "Top 5 Builds by Stats",
                                            tableOutput(outputId = "opt_builds"),
                                                width = 12))
                                ),
                        
                        tabItem(tabName = 'all',
                                
                                fluidRow(
                                        box(
                                                reactableOutput(outputId = "all_builds"),
                                                width = 12
                                                
                                        )
                                        
                                        
                                     )
                                
                                
                                )
                        
                        )
                        
                        
                )

        )





# Define server logic required to draw a histogram
server <- function(input, output) {
        

        output$stat_graph1 <- renderPlot({
                
                 graph_stats %>%
                        filter(Driver == input$chars1,
                               Kart == input$kart1,
                               Tires == input$tire1,
                               Glider == input$glider1) %>%
                        ggplot(mapping = aes(x = value, y = stat, fill = stat)) +
                        geom_col() +
                        gghighlight(stat %in% c("Weight", "Ground Handling", "Ground Speed", "Mini Turbo", "Acceleration"))+
                        theme(legend.position = 'none') +
                        labs(title = "Build 1") +
                        xlab("Score")+
                        ylab("Stat")
                        
        })
        
        
        output$stat_graph2 <- renderPlot({
                
                graph_stats %>%
                        filter(Driver == input$chars2,
                               Kart == input$kart2,
                               Tires == input$tire2,
                               Glider == input$glider2) %>%
                        ggplot(mapping = aes(x = value, y = stat, fill = stat)) +
                        geom_col() +
                        gghighlight(stat %in% c("Weight", "Ground Handling", "Ground Speed", "Mini Turbo", "Acceleration"))+
                        theme(legend.position = 'none') +
                        labs(title = "Build 2") +
                        xlab("Score")+
                        ylab("Stat")
        })
        
        
        
        output$opt_graph <- renderPlotly({
                
                best_build <- stats %>%
                        arrange(desc(!!sym(input$stat1)), 
                                desc(!!sym(input$stat2)), 
                                desc(!!sym(input$stat3)), 
                                desc(!!sym(input$stat4)), 
                                desc(!!sym(input$stat5))) %>%
                        slice_head(n = 1) %>%
                        select(-c(key))
                
                
                
                plot <- stats %>%
                        ggplot(mapping = aes(x = !!sym(input$stat1), y = !!sym(input$stat2), label = Driver, label2 = Kart, label3 = Tires, label4 = Glider)) +
                        geom_point()+
                        geom_point(data = best_build, mapping = aes(x = !!sym(input$stat1), y = !!sym(input$stat2), color = 'red', text = 'Best Build'))+
                        theme(legend.position = 'none')
                
                ggplotly(p = plot)
               
                        
        })
        
        
        
        output$opt_builds <- renderTable({
                
                stats %>%
                        arrange(desc(!!sym(input$stat1)), 
                                desc(!!sym(input$stat2)), 
                                desc(!!sym(input$stat3)), 
                                desc(!!sym(input$stat4)), 
                                desc(!!sym(input$stat5))) %>%
                        slice_head(n = 5) %>%
                        select(-c(key))
        })
        
        
        
        output$all_builds <- renderReactable({
                
                
                reactable(stats %>%
                        select(-c(key)),
                        filterable = TRUE,
                        searchable = TRUE,
                        defaultPageSize = 15)
                
                
        })
        
    
}
















# Run the application 
shinyApp(ui = ui, server = server)
