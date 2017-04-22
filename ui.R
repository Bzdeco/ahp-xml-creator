library(shiny)
library(networkD3)

shinyUI(navbarPage("AHP Criteria Tree XML Creator",
  
  tabPanel("Create Criteria Tree",
    sidebarLayout(position = "right",
      sidebarPanel(
        
        # Add Criterion section
        h3("Add Criterion"),
        textInput("crit_name", "Criterion name", placeholder = "Enter criterion name"),
        selectInput("parent_select", "Select parent criterion", c("Goal")),
        actionButton("add_crit", "Add"),
        
        # Delete Criterion section
        h3("Delete Criterion"),
        p("Deleting selected criterion will also remove all of its subcriterions.", style = "color: red;"),
        selectInput("del_crit_select", "Select criterion to delete", c()),
        actionButton("del_crit", "Delete"),
        
        # Add Alternative section
        h3("Add Alternative"),
        textInput("alt_name", "Alternative name", placeholder = "Enter alternative name"),
        actionButton("add_alt", "Add"),
        h4("Existing alternatives: "),
        tags$ul(`id` = "alternatives_list", `style` = "list-style-type: none; margin-left: 0; padding: 0;"),
        
        # Delete Alternative section
        h3("Delete Alternative"),
        selectInput("del_alt_select", "Select alternative to delete", c()),
        actionButton("del_alt", "Delete")
        
      ),
      mainPanel(
        diagonalNetworkOutput("graph")
      )
    )
  ),
  
  navbarMenu("Rate",
    tabPanel("Criteria",
      fluidRow(
        column(6, uiOutput("rate_criterions_ratios")),
        column(3, offset = 3, actionButton("save_criterions_ratios", "Save comparisons", style = "width: 80%;"))
      )
    ),
    tabPanel("Alternatives",
      fluidRow(
        column(6, uiOutput("rate_alternatives_ratios")),
        column(3, offset = 3, actionButton("save_alternatives_ratios", "Save comparisons", style = "width: 80%;"))
      )
    )
  ),
  
  tabPanel("Export XML")
))