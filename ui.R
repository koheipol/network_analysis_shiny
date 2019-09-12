library(shiny)
library(visNetwork)

shinyUI(
    fluidPage(
        titlePanel("Network Analysis"),
        sidebarLayout(
            sidebarPanel(
                
                fileInput("file", "Choose a CSV File",
                          accept = c(
                              "text/csv",
                              "text/comma-separated-values, text/plain",
                              ".csv")
                ),
                h4("There must be a A_i3 weighted edge from A_i2 to A_i1"),
                tags$hr(),
                selectInput("option", choices = c("Yes", "No"),
                            selected = "No",
                            label = "Your Data is an Adjacent Matrix?"),
                selectInput("node_option", choices = c('closeness centrality',
                                                       'degree centrality',
                                                       'eigenvector centrality',
                                                       'PageRank',
                                                       'Bonacich power centrality',
                                                       'betweeness centrality'),
                            selected = "closeness centrality",
                            label = "Centrality Type ?"),
                conditionalPanel(
                  condition = "input.node_option == 'closeness centrality'",
                  selectInput("closeness_type", choices = c("in", "out", "all", "total"),
                              selected = "out",
                              label = "Type of Closeness ?")
                ),
                conditionalPanel(
                  condition = "input.node_option == 'degree centrality'",
                  selectInput("degree_type", choices = c("in", "out", "all", "total"),
                              selected = "out",
                              label = "Type of Degree ?")
                ),
                conditionalPanel(
                  condition = "input.node_option == 'PageRank'",
                  selectInput("pagerank_type", choices = c("prpack", "arpack", "power"),
                              selected = "out",
                              label = "Page Rank algorithm ?")
                ),
                selectInput("direct", choices = c("undirected", "directed"),
                            selected = "undirected",
                            label = "Directed Graph ?"),
                numericInput("node_size", label = "Node Size ?",
                             min = 0, max = NA, value = 10),
                actionButton("submit", "Plot")
                ),
        fluidRow(
            column(width = 7, class = "well",
                   tabsetPanel(type = "tabs",
                               tabPanel("Raw Data", dataTableOutput("table1")),
                               tabPanel("Adjacent Matrix", tableOutput("table2")),
                               tabPanel("centrality", dataTableOutput("table3"))
                   )),
            column(width = 12,
                   visNetworkOutput("plot1",
                                    height = 500)
                                         )
                              )
                   )
                   )
    )