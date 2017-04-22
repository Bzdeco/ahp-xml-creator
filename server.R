library(shiny)
library(data.tree) # for manipulating hierarchical data
library(networkD3) # for plotting beautiful network graphs

setwd(dir = "/home/bzdeco/Documents/agh/badania/")

# Reactive object storing user input
storage <- reactiveValues(
  criterions = c("Goal"),
  criterions_leaves = c(),
  criterions_nonleaves = c(),
  source = c(),
  target = c(),
  alternatives = c(),
  ratios = list()
)

saaty_rates <- c(
  "9 times less important than",
  "8 times less important than",
  "7 times less important than",
  "6 times less important than",
  "5 times less important than",
  "4 times less important than",
  "3 times less important than",
  "2 times less important than",
  "as important as",
  "2 times more important than",
  "3 times more important than",
  "4 times more important than",
  "5 times more important than",
  "6 times more important than",
  "7 times more important than",
  "8 times more important than",
  "9 times more important than"
)

# Random Index - Consistency Index for an avarage randomly generated matrix (from n = 3 to n = 10)
RI <- c(0.5247, 0.8816, 1.1086, 1.2479, 1.3417, 1.4057, 1.4499, 1.4854)

# Transform relation lists into hierarchical list
relationsToList <- function() {
  
  relations <- data.frame()
  if(length(storage$source) < 1)
    relations <- data.frame(c("Goal"), c("<Empty>"))
  else
    relations <- data.frame(storage$source, storage$target)
  
  # Transforms relation data.frame into tree and then into hierarchical list
  tree <- FromDataFrameNetwork(relations)
  
  return(ToListExplicit(tree, unname = TRUE))
}

# Get leaf- and nonleaf-criterions from criteria tree
getCriterionsByType <- function() {
  
  leaves <- list("leaves" = c(), "nonleaves" = c())
  for(criterion in storage$criterions) {
    if(criterion %in% storage$source)
      leaves$nonleaves <- append(leaves$nonleaves, criterion)
  }
  leaves$leaves <- setdiff(storage$criterions, leaves$nonleaves)
  
  return(leaves)
}

getChildrenCriterions <- function(criterion_name) {
  
  result <- c()
  for(i in seq(1, length(storage$source)))
    if(storage$source[i] == criterion_name)
      result <- append(result, storage$target[i])
  
  return(result)
}

# Evaluate Consistency Ratio
evaluateCR <- function(matrix) {
  maxEigenval <- Re(eigen(matrix)$values[1])
  n <- nrow(matrix)
  CI <- (maxEigenval - n) / (n - 1)
  CR <- CI / RI[n - 2]
}

# Convert vector of ordered ratios (from first to last row) to matrix
ratiosToMatrix <- function(ratios) {
  
  # obtain matrix dimension from number of ratios by solving equation
  n <- length(ratios)
  zeros <- ceiling(Re(polyroot(c(-2*n, 1, 1))))
  dim <- zeros[zeros > 0] + 1
  
  # create pairwise comparison matrix from ratios
  PCMatrix <- matrix(1, nrow = dim, ncol = dim)
  r <- 1
  for(i in seq(1, dim)) {
    for(j in seq(1, dim)) {
      if(j > i) {
        PCMatrix[i,j] <- ratios[r]
        PCMatrix[j,i] <- 1 / ratios[r]
        r <- r + 1
      }
    }
  }
  
  return(PCMatrix)
}

# Update all selectInputs with up-to-date values
updateSelections <- function(session, selectedParent = "Goal") {
  
  ## Create Criterion Tree Tab
  
  # Update parent selection dropdown
  updateSelectInput(session, "parent_select", choices = storage$criterions, selected = selectedParent)
  
  # Update delete selection dropdown
  updateSelectInput(session, "del_crit_select", choices = storage$criterions)
  
  # Update delete alternative dropdown
  updateSelectInput(session, "del_alt_select", choices = storage$alternatives)
}

# Update hierarchy network graph
updateGraph <- function(output) {
  output$graph <- renderSimpleNetwork({
    
    networkData <- relationsToList()
    
    # Plot network graph
    diagonalNetwork(List = networkData, fontSize = 15, opacity = 1)
  })
}

shinyServer(function(input, output, session) {
  
  ## Create Criterion Tree Tab
  
  # Add Criterion Button
  observeEvent(input$add_crit, {
    
    # Update storage
    storage$criterions <- append(storage$criterions, input$crit_name)
    storage$source <- append(storage$source, input$parent_select)
    storage$target <- append(storage$target, input$crit_name)
    storage$criterions_leaves <- getCriterionsByType()$leaves
    storage$criterions_nonleaves <- getCriterionsByType()$nonleaves
    
    updateSelections(session, selectedParent = input$parent_select)
    updateGraph(output)
  })
  
  # Delete Criterion Button
  observeEvent(input$del_crit, {
    
    # Remove all relations with this node on source and target list
    toDelete <- c(input$del_crit_select)
    while(length(toDelete) > 0 && length(storage$source) > 0) {
      
      indexes <- c() # indexes to be deleted
      for(i in seq(1, length(storage$source))) {
        
        # Delete node as parent and add child to toDelete list
        if(storage$source[i] == toDelete[1]) {
          # Child of found node will be delated later
          toDelete <- append(toDelete, storage$target[i])
          indexes <- append(indexes, i)
        }
        
        # Delete node as child of another node
        else if(storage$target[i] == toDelete[1]) {
          indexes <- append(indexes, i)
        }
      }
      
      # Delete elements of saved indexes
      if(length(indexes) > 0) {
        storage$source <- storage$source[-indexes]
        storage$target <- storage$target[-indexes]
      }

      # Remove deleted criterion from criterions list
      for(i in seq(1, length(storage$criterions))) {
        if(storage$criterions[i] == toDelete[1]) {
          # Remove this element from list
          storage$criterions <- storage$criterions[-i]
          break
        }
      }
      
      # All sources equal to toDelete[1] has been removed
      toDelete <- toDelete[-1]
    }
    
    storage$criterions_leaves <- getCriterionsByType()$leaves
    storage$criterions_nonleaves <- getCriterionsByType()$nonleaves
    
    updateSelections(session)
    updateGraph(output)
  })
  
  # Add Alternative Button
  observeEvent(input$add_alt, {
    
    storage$alternatives <- append(storage$alternatives, input$alt_name)
    
    insertUI(
      selector = "#alternatives_list",
      where = "beforeEnd",
      ui = tags$li(`id` = input$alt_name, input$alt_name)
    )
    
    updateSelections(session)
  })
  
  # Delete Alternative Button
  observeEvent(input$del_alt, {
    
    # Remove alternative from storage
    for(i in seq(1, length(storage$alternatives))) {
      if(storage$alternatives[i] == input$del_alt_select) {
        storage$alternatives <- storage$alternatives[-i]
        break
      }
    }
    
    # Remove alternative from displayed list
    removeUI(
      selector = paste("#", input$del_alt_select, sep = "")
    )
    
    updateSelections(session)
  })
  
  
  ## Rate Criterions Tab
  
  output$rate_criterions_ratios <- renderUI({
    
    # container for all comparisons lists
    page <- tags$div()
    
    # for each criterion containing subcriterions
    for(order in seq(1, length(storage$criterions_nonleaves))) {
      criterion <- storage$criterions_nonleaves[order]
      children_criterions <- getChildrenCriterions(criterion)
      
      container <- tags$div()
      container <- tagAppendChild(container, tags$h3(paste(criterion, "subcriterions rating")))
      
      # render all subcriterion comparisons
      i <- 1; j <- 1; k <- 1
      while(i <= length(children_criterions)) {
        j <- i + 1
        while(j <= length(children_criterions)) {
          container <- tagAppendChild(container, fluidRow(
            column(3, tags$span(paste(children_criterions[i], "is"))),
            column(6, style = "margin-top: -25px;", selectInput(
              inputId = paste(criterion, k),
              label = "",
              choices = saaty_rates,
              selected = "as important as"
            )),
            column(3, tags$span(children_criterions[j]))
          ))
          j <- j + 1; k <- k + 1
        }
        i <- i + 1
      }
      page <- tagAppendChild(page, container)
    }
    
    return(page)
  })
  
  
  ## Rate Alternatives Tab
  
  output$rate_alternatives_ratios <- renderUI({
    
    # container for all comparisons lists
    page <- tags$div()
    
    # for each leaf-criterion
    for(order in seq(1, length(storage$criterions_leaves))) {
      criterion <- storage$criterions_leaves[order]
      
      container <- tags$div()
      container <- tagAppendChild(container, tags$h3(paste("Rating with regard to", criterion)))
      
      # render all alternatives comparisons
      for(i in seq(1, length(storage$alternatives))) {
        j <- i + 1; k <- 1;
        while(j <= length(storage$alternatives)) {
          container <- tagAppendChild(container, fluidRow(
            column(3, tags$span(paste(storage$alternatives[i], "is"))),
            column(6, style = "margin-top: -25px;", selectInput(
              inputId = paste(criterion, k),
              label = "",
              choices = saaty_rates,
              selected = "as important as"
            )),
            column(3, tags$span(paste(storage$alternatives[j])))
          ))
          j <- j + 1; k <- k + 1;
        }
      }
      page <- tagAppendChild(page, container)
    }
    
    return(page)
  })
  
  
  ## Export XML Tab
  
  
  
})

### test
# m <- ratiosToMatrix(c(2, 3, 5))
# print(evaluateCR(m))
