library(shiny)
library(data.tree) # for manipulating hierarchical data
library(networkD3) # for plotting beautiful network graphs
library(XML)

setwd(dir = "/home/bzdeco/Documents/agh/badania/")

# Reactive object storing user input
storage <- reactiveValues(
  criterions = c("Goal"),
  criterions_leaves = c(),
  criterions_nonleaves = c(),
  source = c(),
  target = c(),
  alternatives = c(),
  ratios_nonleaves = list('Goal' = c()),
  ratios_leaves = list(),
  inconsistencies_nonleaves = list(),
  inconsistencies_leaves = list()
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

saaty_rates_alt <- c(
  "9 times worse than",
  "8 times worse than",
  "7 times worse than",
  "6 times worse than",
  "5 times worse than",
  "4 times worse than",
  "3 times worse than",
  "2 times worse than",
  "as good as",
  "2 times better than",
  "3 times better than",
  "4 times better than",
  "5 times better than",
  "6 times better than",
  "7 times better than",
  "8 times better than",
  "9 times better than"
)

saaty_rates_dict <- list(
  "9 times less important than" = 1/9,
  "8 times less important than" = 1/8,
  "7 times less important than" = 1/7,
  "6 times less important than" = 1/6,
  "5 times less important than" = 1/5,
  "4 times less important than" = 1/4,
  "3 times less important than" = 1/3,
  "2 times less important than" = 1/2,
  "as important as" = 1,
  "2 times more important than" = 2,
  "3 times more important than" = 3,
  "4 times more important than" = 4,
  "5 times more important than" = 5,
  "6 times more important than" = 6,
  "7 times more important than" = 7,
  "8 times more important than" = 8,
  "9 times more important than" = 9
)

saaty_rates_alt_dict <- list(
  "9 times worse than" = 1/9,
  "8 times worse than" = 1/8,
  "7 times worse than" = 1/7,
  "6 times worse than" = 1/6,
  "5 times worse than" = 1/5,
  "4 times worse than" = 1/4,
  "3 times worse than" = 1/3,
  "2 times worse than" = 1/2,
  "as good as" = 1,
  "2 times better than" = 2,
  "3 times better than" = 3,
  "4 times better than" = 4,
  "5 times better than" = 5,
  "6 times better than" = 6,
  "7 times better than" = 7,
  "8 times better than" = 8,
  "9 times better than" = 9
)

saaty_rates_values <- c(1/9, 1/8, 1/7, 1/6, 1/5, 1/4, 1/3, 1/2, 1, 2, 3, 4, 5, 6, 7, 8, 9)

# Random Index - Consistency Index for an avarage randomly generated matrix (from n = 3 to n = 10)
RI <- c(0.5247, 0.8816, 1.1086, 1.2479, 1.3417, 1.4057, 1.4499, 1.4854)

# Recursive function starting from criterion and building XML tree of its subcriterions
createComparionsXMLTree <- function(parentNode, criterion) {
  
  comparisonsT <- xmlNode("comparisons")
  
  # Comparison of criterions
  if(criterion %in% storage$criterions_nonleaves) {
    
    # Comparisons Tag
    if(length(getChildren(criterion)) > 0) {
      ratios <- storage$ratios_nonleaves[[criterion]]
      for(ratio in ratios)
        comparisonsT <- addChildren(comparisonsT, xmlNode("ratio", ratio))
    }
    parentNode <- addChildren(parentNode, comparisonsT)
    
    for(subcriterion in getChildrenCriterions(criterion)) {
      criterionT <- xmlNode("criterion", attrs = c(name = tolower(gsub(" ", "_", subcriterion))))
      
      # recursive call
      criterionT <- createComparionsXMLTree(criterionT, subcriterion)
      
      parentNode <- addChildren(parentNode, criterionT)
    }
    
  } 
  # Comparison of alternatives
  else {
    
    # Comparisons Tag
    if(length(getChildren(criterion)) > 0) {
      ratios <- storage$ratios_leaves[[criterion]]
      for(ratio in ratios)
        comparisonsT <- addChildren(comparisonsT, xmlNode("ratio", ratio))
    }
    
    parentNode <- addChildren(parentNode, comparisonsT)
  }
  
  return(parentNode)
}

# Create XML from data provided by user
convertDataToXML <- function(input) {
  ahpT <- xmlNode("ahp")
  goalT <- xmlNode("goal", attrs = c(name = tolower(gsub(" ", "_", input$goal_name))))
  alternativesT <- xmlNode("alternatives")
  
  # Add criterions tags
  
  goalT <- createComparionsXMLTree(goalT, "Goal")
  
  # Add alternative tags
  for(alternative in storage$alternatives)
    alternativesT <- addChildren(alternativesT, xmlNode("alternative", alternative))
  
  ahpT <- addChildren(ahpT, goalT, alternativesT)
  
  return(saveXML(ahpT))
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

# Get subcriterions of the given criterion
getChildrenCriterions <- function(criterion_name) {
  
  result <- c()
  for(i in seq(1, length(storage$source)))
    if(storage$source[i] == criterion_name)
      result <- append(result, storage$target[i])
  
  return(result)
}

# Get children (subcriterions or alternatives) of given criterion
getChildren <- function(criterion_name) {
  
  if(criterion_name %in% storage$source)
    return(getChildrenCriterions(criterion_name))
  else
    return(storage$alternatives)
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

# Render inconsistency info about comparisons with regard to given criterions
renderInconsistencyInfo <- function(threshold, criterions, inconsistencies) {
  
  container <- tags$div(style = "margin-top: 25px;")
  
  for(criterion in criterions) {
    inconsistency <- inconsistencies[[criterion]]
    info <- tags$p(paste("Inconsistency of", criterion, "comparisons:", inconsistency))
    
    if(inconsistency > threshold)
      info <- tagAppendAttributes(info, style = "color: red; font-weight: bold;")
    else
      info <- tagAppendAttributes(info, style = "color: green;")
    
    container <- tagAppendChild(container, info)
  }
  
  return(container)
}

# Update all selectInputs with up-to-date values
updateSelections <- function(session, selectedParent = "Goal") {
  
  ## Create Criterion Tree Tab
  
  # Update parent selection dropdown
  updateSelectInput(session, "parent_select", choices = storage$criterions, selected = selectedParent)
  
  # Update delete selection dropdown
  deleteChoices <- storage$criterions[-1] # do not include "Goal" criterion
  updateSelectInput(session, "del_crit_select", choices = deleteChoices)
  
  # Update delete alternative dropdown
  updateSelectInput(session, "del_alt_select", choices = storage$alternatives)
}

# Update comparison ratios for given criterions (leaf/non-leaf)
updateStorageRatios <- function(criterions) {
  
  result <- list() # clear ratios list
  
  for(criterion in criterions) {
    n <- length(getChildren(criterion))
    if(n > 1) {
      # evaluate number of comparisons in criterion
      ratios <- rep(1, (n^2-n)/2)
      # add vector of comparisons named <criterion> to list of ratios
      result[[criterion]] <- ratios
    }
  }
  
  return(result)
}

# Update CR values for given criterions (leaf/non-leaf)
updateStorageInconsistencies <- function(criterions) {
  
  if(length(criterions) < 1)
    return(NULL)
  
  result <- list()
  
  for(criterion in criterions) {
    n <- length(getChildren(criterion))
    if(n == 2)
      result[[criterion]] <- 0
    else if(n > 2) {
      pcm <- matrix()
      if(criterion %in% storage$criterions_nonleaves) {
        ratios <- storage$ratios_nonleaves[[criterion]]
        pcm <- ratiosToMatrix(ratios)
      } else { # criterion in criterions_leaves
        ratios <- storage$ratios_leaves[[criterion]]
        pcm <- ratiosToMatrix(ratios)
      }
      result[[criterion]] <- round(evaluateCR(pcm), 2)
    }
  }
  
  if(criterion %in% storage$criterions_nonleaves)
    storage$inconsistencies_nonleaves <- result
  else
    storage$inconsistencies_leaves <- result
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
  
  #  > Add Criterion Button
  observeEvent(input$add_crit, {
    
    # Disable adding already existing criterions
    if(!(input$crit_name %in% storage$criterions)) {
      
      # Update storage
      storage$criterions <- append(storage$criterions, input$crit_name)
      storage$source <- append(storage$source, input$parent_select)
      storage$target <- append(storage$target, input$crit_name)
      storage$criterions_leaves <- getCriterionsByType()$leaves
      storage$criterions_nonleaves <- getCriterionsByType()$nonleaves
      storage$ratios_nonleaves <- updateStorageRatios(storage$criterions_nonleaves)
      storage$ratios_leaves <- updateStorageRatios(storage$criterions_leaves)
      
      updateStorageInconsistencies(storage$criterions_nonleaves)
      updateStorageInconsistencies(storage$criterions_leaves)
      updateSelections(session, selectedParent = input$parent_select)
      updateGraph(output)
    }
  })
  
  #  > Delete Criterion Button
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
    storage$ratios_nonleaves <- updateStorageRatios(storage$criterions_nonleaves)
    storage$ratios_leaves <- updateStorageRatios(storage$criterions_leaves)
    
    updateStorageInconsistencies(storage$criterions_nonleaves)
    updateStorageInconsistencies(storage$criterions_leaves)
    updateSelections(session)
    updateGraph(output)
  })
  
  #  > Add Alternative Button
  observeEvent(input$add_alt, {
    
    storage$alternatives <- append(storage$alternatives, input$alt_name)
    storage$ratios_nonleaves <- updateStorageRatios(storage$criterions_nonleaves)
    storage$ratios_leaves <- updateStorageRatios(storage$criterions_leaves)
    
    insertUI(
      selector = "#alternatives_list",
      where = "beforeEnd",
      ui = tags$li(`id` = input$alt_name, input$alt_name)
    )
    
    updateStorageInconsistencies(storage$criterions_nonleaves)
    updateStorageInconsistencies(storage$criterions_leaves)
    updateSelections(session)
  })
  
  #  > Delete Alternative Button
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
    
    storage$ratios_nonleaves <- updateStorageRatios(storage$criterions_nonleaves)
    storage$ratios_leaves <- updateStorageRatios(storage$criterions_leaves)
    
    updateStorageInconsistencies(storage$criterions_nonleaves)
    updateStorageInconsistencies(storage$criterions_leaves)
    updateSelections(session)
  })
  
  
  ## Rate Criterions Tab
  
  #  > Render selectInputs for comparisons of non-leaf criterions
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
      k <- 1
      for(i in seq(1, length(children_criterions))) {
        j <- i + 1
        while(j <= length(children_criterions)) {
          # single comparison
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
      }
      page <- tagAppendChild(page, container)
    }
    
    return(page)
  })
  
  #  > Render information about inconsistencies in comparisons of criterions
  output$criterions_inconsistencies_info <- renderUI({
    
    renderInconsistencyInfo(input$threshold, storage$criterions_nonleaves, storage$inconsistencies_nonleaves)
    
  })
  
  #  > Save comparisons of non-leaf criterions
  observeEvent(input$save_criterions_ratios, {
    
    result <- list() # clear list for new ratios values
    
    for(criterion in storage$criterions_nonleaves) {
      n <- length(getChildren(criterion))
      count <- (n^2-n)/2 # number of comparisons and consequently selectInputs for one criterion
      ratios <- c()
      if(n > 1) {
        for(i in seq(1, count)) {
          option <- input[[paste(criterion, i)]] # value selected in according selectInput
          ratios <- append(ratios, saaty_rates_dict[[option]])
        }
      }
      
      result[[criterion]] <- ratios
    }
    
    # update storage$ratios_nonleaves and $inconsistencies_nonleaves with new values
    storage$ratios_nonleaves <- result
    updateStorageInconsistencies(storage$criterions_nonleaves)

  })
  
  
  ## Rate Alternatives Tab
  
  #  > Render selectInputs for comparisons of leaf criterions
  output$rate_alternatives_ratios <- renderUI({
    
    # container for all comparisons lists
    page <- tags$div()
    
    # for each leaf-criterion
    for(order in seq(1, length(storage$criterions_leaves))) {
      criterion <- storage$criterions_leaves[order]
      
      container <- tags$div()
      container <- tagAppendChild(container, tags$h3(paste("Rating with regard to", criterion)))
      
      # render all alternatives comparisons
      k <- 1
      for(i in seq(1, length(storage$alternatives))) {
        j <- i + 1
        while(j <= length(storage$alternatives)) {
          # single comparison
          container <- tagAppendChild(container, fluidRow(
            column(3, tags$span(paste(storage$alternatives[i], "is"))),
            column(6, style = "margin-top: -25px;", selectInput(
              inputId = paste(criterion, k),
              label = "",
              choices = saaty_rates_alt,
              selected = "as good as"
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
  
  #  > Render information about inconsistencies in comparisons of alternatives
  output$alternatives_inconsistencies_info <- renderUI({
    
    renderInconsistencyInfo(input$threshold, storage$criterions_leaves, storage$inconsistencies_leaves)
    
  })
  
  #  > Save comparisons of leaf criterions
  observeEvent(input$save_alternatives_ratios, {
    
    result <- list() # clear list for new ratios values
    
    for(criterion in storage$criterions_leaves) {
      n <- length(getChildren(criterion))
      count <- (n^2-n)/2 # number of comparisons and consequently selectInputs for one criterion
      ratios <- c()
      if(n > 1) {
        for(i in seq(1, count)) {
          option <- input[[paste(criterion, i)]] # value selected in according selectInput
          ratios <- append(ratios, saaty_rates_alt_dict[[option]])
        }
      }
      
      result[[criterion]] <- ratios
    }
    
    # update storage$ratios_leaves and $inconsistencies_leaves with new values
    storage$ratios_leaves <- result
    updateStorageInconsistencies(storage$criterions_leaves)

  })
  
  
  ## Export XML Tab
  
  output$download <- downloadHandler(
    filename = "ahp-criteria-tree.xml",
    content = function(file) {
      write(convertDataToXML(input), file)
    },
    contentType = "text/xml"
  )
  
})
