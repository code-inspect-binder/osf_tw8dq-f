################################################################################
### Function for checking transitivity and producing rank order of preferences 
### for SVO Slider Measure. Based on source material at 
### http://ryanomurphy.com/styled-2/styled-4/index.html (Example MATLAB script 
### SVO_Slider. Used for Slider Measure Version A. To adapt for other versions, 
### change item endpoints and categories.
###
### Written by Dieko Bakker July 25th 2018.
### 
### Written for paper 'Comparing the Slider Measure of Social Value Orientation 
### with Its Main Alternatives' published in Social Psychology Quarterly by 
### Dieko Bakker and Jacob Dijkstra, doi: 10.1177/01902725211008938.
################################################################################

# Adjust variables below accordingly with variable names in the dataset
check_row_transitivity = function(row) {
  primary_choices = matrix(data = c(
    row["SVO_Item_01_Self"], row["SVO_Item_01_Other"],
    row["SVO_Item_02_Self"], row["SVO_Item_02_Other"],
    row["SVO_Item_03_Self"], row["SVO_Item_03_Other"],
    row["SVO_Item_04_Self"], row["SVO_Item_04_Other"],
    row["SVO_Item_05_Self"], row["SVO_Item_05_Other"],
    row["SVO_Item_06_Self"], row["SVO_Item_06_Other"]), nrow = 6, ncol = 2, 
    byrow = TRUE, dimnames = list(NULL, c('self', 'other')))
  if (any(is.na(primary_choices))) {return(NA)}
  
  return(transitivity_check(primary_choices))
}

################################################################################

row_ranking = function(row) {
  primary_choices = matrix(data = c(
    row["SVO_Item_01_Self"], row["SVO_Item_01_Other"],
    row["SVO_Item_02_Self"], row["SVO_Item_02_Other"],
    row["SVO_Item_03_Self"], row["SVO_Item_03_Other"],
    row["SVO_Item_04_Self"], row["SVO_Item_04_Other"],
    row["SVO_Item_05_Self"], row["SVO_Item_05_Other"],
    row["SVO_Item_06_Self"], row["SVO_Item_06_Other"]), nrow = 6, ncol = 2, 
    byrow = TRUE, dimnames = list(NULL, c('self', 'other')))
  
  if (any(is.na(primary_choices))) {return(NA)}
  
  return(ranking(primary_choices))
}

################################################################################

# Define left and right endpoints for each of the 6 items
versionA_item_endpoints = matrix(data = c(
  85, 85, 85, 15,
  85, 15, 100, 50,
  50, 100, 85, 85,
  50, 100, 85, 15,
  100, 50, 50, 100,
  100, 50, 85, 85
), nrow = 6, ncol = 4, byrow = TRUE, dimnames = list(NULL, 
                    c('Left Self', 'Left Other', 'Right Self', 'Right Other')))

# Identify the SVOs belonging to the left and right endpoints for each of the 
# 6 items
versionA_categories = matrix(data = c(
  'Prosocial', 'Competitive',
  'Competitive', 'Individualistic',
  'Altruistic', 'Prosocial',
  'Altruistic', 'Competitive',
  'Individualistic', 'Altruistic',
  'Individualistic', 'Prosocial'
), nrow = 6, ncol = 2, byrow = TRUE, dimnames = list(NULL, 
                                c('Left Orientation', 'Right Orientation')))

################################################################################

is_acyclic_graph = function(graph) {
  ### Returns TRUE if the graph is a Directed Acyclic Graph, FALSE if not
  
  # First check that the graph does not contain missing values
  if (any(is.na(graph))) {
    stop("Graph should not contain missing values (NA)")
  }
  
  # If there are no edges in the graph, the graph is acyclic
  if (all(graph == FALSE)) {return(TRUE)}
  
  # If the graph still has edges, but there is no node without outgoing ties, 
  # the graph is not acyclic
  else if (all(apply(graph, 1, any, na.rm = TRUE))) {return(FALSE)}
  
  # If the graph still has edges and some nodes have no outgoing edges, 
  # remove these nodes and all the edges going to these nodes
  # Evaluate the graph again
  rows_with_outgoing_edges = apply(graph, 1, any, na.rm = TRUE)
  new_graph = graph[which(rows_with_outgoing_edges), 
                    which(rows_with_outgoing_edges)]
  return(is_acyclic_graph(new_graph))
  
}

################################################################################

transitivity_check = function(primary_data, item_endpoints = NULL, 
                              categories = NULL) {
  # Returns transitivity of preferences (TRUE or FALSE), takes data in the form 
  # of the example as input
  # EXAMPLE
  # one DMs selected choices from the 6 primary sliders, version 1
  # primary_data = matrix(data = c(85, 85, 100, 50, 85, 85, 50, 100, 75, 75, 85,
  # 85), nrow = 6, ncol = 2, byrow = TRUE, dimnames = list(NULL, c('self', 
  # 'other')))
  
  # Check that the primary_data is passed in the correct format
  if (any(is.na(primary_data))) {
    stop("Choices (primary_data) should not contain missing values (NA)")
  }
  else if (nrow(primary_data) != 6 || ncol(primary_data) != 2) {
    stop("Choices (primary_data) do not have the correct dimensions 
         (should be 6 rows, 2 columns")
  }
  
  # If item_endpoints and/or categories are not passed as parameters, 
  # use defaults and warn
  if (is.null(item_endpoints)) {
    if (!exists('versionA_item_endpoints')) {
      stop('Item endpoints not entered as parameter and defaults for Version A 
           are not loaded.\n Reload source file or enter endpoints as parameter 
           to the function.')
    }
    item_endpoints = versionA_item_endpoints
    warning('Item endpoints not entered as parameter, using Version A endpoints 
            as default')
  }
  if (is.null(categories)) {
    if (!exists('versionA_categories')) {
      stop('Categories not entered as parameter and defaults for Version A are 
           not loaded.\n Reload source file or enter categories as parameter to 
           the function.')
    }    
    categories = versionA_categories
    warning('Categories corresponding to endpoints not entered as parameter, 
            using Version A categories as default')
  }
  
  # Calculate Euclidean distances between chosen points and endpoints on each 
  # item
  distance_matrix = matrix(0L, nrow = 6, ncol = 2, dimnames = list(NULL, 
              c('Distance to left endpoints', 'Distance to right endpoints')))
  for (count in seq(1, 6, 1)) {
    endpoint_left = item_endpoints[count, c('Left Self', 'Left Other')]
    endpoint_right = item_endpoints[count, c('Right Self', 'Right Other')]
    chosen_allocation = primary_data[count,]
    distance_matrix[count, 'Distance to left endpoints'] = dist(matrix(data = 
          c(endpoint_left, chosen_allocation), nrow = 2, byrow = TRUE), 
           method = 'euclidian')
    distance_matrix[count, 'Distance to right endpoints'] = dist(matrix(data = 
          c(endpoint_right, chosen_allocation), nrow = 2, byrow = TRUE), 
           method = 'euclidian')
  }
  
  # On each item, the orientation corresponding to the endpoint with the lowest 
  # Euclidean distance 'wins' and gains in ranking
  # If there is a tie (the exact middle of the item is chosen) there is no 
  # winner, and both orientations gain in ranking
  comparisons = matrix(NA, nrow = 6, ncol = 2, dimnames = list(NULL, 
                                                      c('Won', 'Lost')))

  for (count in seq(1, 6, 1)) {
    if (distance_matrix[count, 'Distance to left endpoints'] < 
        distance_matrix[count, 'Distance to right endpoints']) {
       comparisons[count, 'Won'] = categories[count, 'Left Orientation']
       comparisons[count, 'Lost'] = categories[count, 'Right Orientation']
    }
    else if (distance_matrix[count, 'Distance to left endpoints'] > 
             distance_matrix[count, 'Distance to right endpoints']) {
      comparisons[count, 'Won'] = categories[count, 'Right Orientation']
      comparisons[count, 'Lost'] = categories[count, 'Left Orientation']
    }
  }
  
  # Remove empty rows from comparisons
  comparisons = comparisons[apply(comparisons, 1, function(x) 
    {all(!is.na(x))}),]
  
  # Create a matrix of comparisons where TRUE indicates a directed relationship 
  # (i preferred to j) and FALSE indicates no relationship
  comparisons_matrix = matrix(FALSE, nrow = 4 , ncol = 4, dimnames = 
          list(c("Altruistic", "Prosocial", "Individualistic", "Competitive"),
                c("Altruistic", "Prosocial", "Individualistic", "Competitive")))
  for (row in seq(1, nrow(comparisons), 1)) {
    comparisons_matrix[comparisons[row, "Won"], comparisons[row, "Lost"]] = TRUE
  }
  
  # If preferences are transitive this matrix forms a Directed Acyclic Graph, 
  # which can be checked with the following function
  preferences_transitive = is_acyclic_graph(comparisons_matrix)
  
  return(preferences_transitive)
}

################################################################################

ranking = function(primary_data, item_endpoints = NULL, categories = NULL) {
  # Returns ranking of orientations (if the preferences are transitive), takes 
  # data in the form of the example as input
  # EXAMPLE
  # one DMs selected choices from the 6 primary sliders, version 1
  # primary_data = matrix(data = c(85, 85, 100, 50, 85, 85, 50, 100, 75, 75, 85,
  # 85), nrow = 6, ncol = 2, byrow = TRUE, dimnames = list(NULL, c('self', 
  # 'other')))
  
  # Check that the primary_data is passed in the correct format
  if (any(is.na(primary_data))) {
    stop("Choices (primary_data) should not contain missing values (NA)")
  }
  else if (nrow(primary_data) != 6 || ncol(primary_data) != 2) {
    stop("Choices (primary_data) do not have the correct dimensions (should be 
         6 rows, 2 columns")
  }
  
  # If item_endpoints and/or categories are not passed as parameters, use 
  # defaults and warn
  if (is.null(item_endpoints)) {
    if (!exists('versionA_item_endpoints')) {
      stop('Item endpoints not entered as parameter and defaults for Version A 
           are not loaded.\n Reload source file or enter endpoints as parameter 
           to the function.')
    }
    item_endpoints = versionA_item_endpoints
    warning('Item endpoints not entered as parameter, using Version A endpoints 
            as default')
  }
  if (is.null(categories)) {
    if (!exists('versionA_categories')) {
      stop('Categories not entered as parameter and defaults for Version A are 
           not loaded.\n Reload source file or enter categories as parameter to 
           the function.')
    }    
    categories = versionA_categories
    warning('Categories corresponding to endpoints not entered as parameter, 
            using Version A categories as default')
  }
  
  # Calculate Euclidean distances between chosen points and endpoints on each item
  distance_matrix = matrix(0L, nrow = 6, ncol = 2, dimnames = list(NULL, 
            c('Distance to left endpoints', 'Distance to right endpoints')))
  for (count in seq(1, 6, 1)) {
    endpoint_left = item_endpoints[count, c('Left Self', 'Left Other')]
    endpoint_right = item_endpoints[count, c('Right Self', 'Right Other')]
    chosen_allocation = primary_data[count,]
    distance_matrix[count, 'Distance to left endpoints'] = dist(matrix(data = 
            c(endpoint_left, chosen_allocation), nrow = 2, byrow = TRUE), 
             method = 'euclidian')
    distance_matrix[count, 'Distance to right endpoints'] = dist(matrix(data = 
            c(endpoint_right, chosen_allocation), nrow = 2, byrow = TRUE), 
             method = 'euclidian')
  }
  
  # On each item, the orientation corresponding to the endpoint with the lowest 
  # Euclidean distance 'wins' and gains in ranking
  # If there is a tie (the exact middle of the item is chosen) there is no 
  # winner, and both orientations gain in ranking
  ranking = matrix(0L, nrow = 4, ncol = 2, dimnames = list(c('Altruistic', 
          'Prosocial', 'Individualistic', 'Competitive'), c('Score', 'Rank')))
  
  for (count in seq(1, 6, 1)) {
    if (distance_matrix[count, 'Distance to left endpoints'] < 
        distance_matrix[count, 'Distance to right endpoints']) {
      ranking[categories[count, 'Left Orientation'], 'Score']  = 
        ranking[categories[count, 'Left Orientation'], 'Score'] + 1
      ranking[categories[count, 'Right Orientation'], 'Score']  = 
        ranking[categories[count, 'Right Orientation'], 'Score'] + 0
    }
    else if (distance_matrix[count, 'Distance to left endpoints'] > 
             distance_matrix[count, 'Distance to right endpoints']) {
      ranking[categories[count, 'Left Orientation'], 'Score']  = 
        ranking[categories[count, 'Left Orientation'], 'Score'] + 0
      ranking[categories[count, 'Right Orientation'], 'Score']  = 
        ranking[categories[count, 'Right Orientation'], 'Score'] + 1
    }
    else if (distance_matrix[count, 'Distance to left endpoints'] == 
             distance_matrix[count, 'Distance to right endpoints']) {
      ranking[categories[count, 'Left Orientation'], 'Score']  = 
        ranking[categories[count, 'Left Orientation'], 'Score'] + 0.5
      ranking[categories[count, 'Right Orientation'], 'Score']  = 
        ranking[categories[count, 'Right Orientation'], 'Score'] + 0.5
    }        
  }
  
  # If preferences are transitive this matrix forms a Directed Acyclic Graph, 
  # which can be checked with the following function
  preferences_transitive = transitivity_check(primary_data, item_endpoints = 
                                      item_endpoints, categories = categories)
  
  # If preferences are not transitive, do not return ranking
  if (!preferences_transitive) {ranking = NA}
  # If preferences are transitive, compute ranks
  else {
    ranking[, "Rank"] = 5 - rank(ranking[, "Score"]) # So that a rank of 1 
    # corresponds to the highest preference, 4 to lowest preference
  }
  
  return(ranking)
}
