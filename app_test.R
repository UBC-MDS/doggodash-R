# libraries
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(tibble)
library(knitr)
library(htmlTable)
library(ggthemes)

# Read in data
traits_raw_df <- readr::read_csv("data/breed_traits.csv")
breed_rank_raw_df <- readr::read_csv("data/breed_rank.csv")

# For traits list in checklist input
traits_list_full <- traits_raw_df %>%
  select(-c("Breed" ,"Coat Type", "Coat Length")) %>%
  colnames()

# Setup app and layout/frontend
app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)


# 1. VARIABLES AND CONTAINERS DEFINITIONS
# Header
header <- htmlH1(
    list(
      'Doggodash' 
    ),
    style=list(
      color='white', 
      fontSize=40,
      backgroundColor='Indigo',
      textAlign='center'
    )
)

# Row-1
# Col-1: traits-widget  dropdown
traits_widget <- list(
  htmlLabel(
    'Choose Dog traits you like and dislike',
    style=list(
      color='Indigo', 
      fontSize=18,
      backgroundColor='lavender',
      width='100%',
      textAlign='center'
    )
  ),
  dccDropdown(
    id = 'traits-widget',
    value = traits_list_full[1:4],
    options = purrr::map(traits_list_full, function(value)
      list(label = value, value = value)),
    placeholder = 'Select Doggo traits',
    multi = TRUE,
    style=list(
      height= '80%',
      width= '100%',
      fontSize= 18,
      backgroundColor= 'white'
    )
  )
)

# Col-2 - weights widget slider
weights_slider <- list(
  htmlLabel(
    'Select weights',
    style = list( 
      color= 'Indigo', 
      fontSize= 18,
      textAlign= 'center',
      align= 'top',
      backgroundColor= 'lavender',
      width= '100%'
    )
  ),
  
  htmlLabel(
    'Weights for traits you like',
    style = list( 
      color= 'Indigo', 
      fontSize= 14,
      textAlign= 'center'
    )
  ),
  
  dccSlider(
    id = "xslider_like",
    min = 0,
    max = 2.5,
    step = 0.5,
    marks = list(
      "0" = "0",
      "0.5" = "0.5",
      "1" = "1",
      "1.5" = "1.5",
      "2" = "2",
      "2.5" = "2.5"
    ),
    value = 1
  ),
  
  htmlBr(),
  
  htmlLabel(
    'Weight for traits you dislike',
    style = list( 
      color= 'Indigo', 
      fontSize= 14,
      textAlign= 'center'
    )
  ),
  dccSlider(
    id = "xslider_dislike",
    min = -2.5,
    max = 0,
    step = 0.5,
    marks = list(
      "-2.5" = "-2.5",
      "-2" = "-2",
      "-1.5" = "-1.5",
      "-1" = "-1",
      "-0.5" = "-0.5",
      "0" = "0"
    ),
    value = -0.5
  ),
  
  htmlBr(),
  
  htmlLabel(
    'Higher weights increase scoring on likeable traits and decrease scoring on dislikeable traits (drooling and shedding levels)',
    style = list( 
      color= 'Indigo', 
      fontSize= 14,
      textAlign= 'center'
    )
  )
)


# Col3 - top5dogs recommender Plot
top5dogs_plot <- list(
    htmlP(
      'Top 5 recommended Dog breeds as per chosen traits',
      style = list( 
        color= 'Indigo', 
        fontSize= 18,
        textAlign= 'center',
        backgroundColor= 'lavender',
        width= '100%'
      )
    ),
    
    dccGraph(
      id='top5dogs_plot',
      style=list(
        display= 'block',
        borderWidth= '1', 
        height= '230px',
        width= '100%' 
      )
    ),
    
    htmlP(
      'Hover over to see more',
      style = list( 
        color= 'Indigo', 
        fontSize= 14,
        textAlign= 'center'
      )
    )
)

# Row-2
# Col-1: Table of details on the recommended breeds
details_table <- list(
  htmlP(
    'Details of the recommended dog breeds',
    style = list( 
      color= 'Indigo', 
      fontSize= 18,
      textAlign= 'center',
      backgroundColor= 'lavender'
    )
  ),
  htmlIframe(
    id = 'table',
    style=list(
      display= 'block',
      borderWidth= '0', 
      width= '100%', 
      height= '75%',
      backgroundColor= 'lavender',
      align= 'center'
    )
  )
)

# Col-2: Ranking plot
ranking_plot_fun <- function(){
  # Reading datasets
  traits_raw_df <- read_csv("data/breed_traits.csv")
  breed_rank_raw_df <- read_csv("data/breed_rank.csv")
  
  # Some data wrangling in preparation for the ranking trend plot
  # Adding "BreedID" is needed so that the dataframes can be joined correctly with
  # "BreedID" as the key.
  traits_raw_df <- tibble::rowid_to_column(traits_raw_df, "BreedID")
  #head(traits_raw_df)
  breed_rank_df <- tibble::rowid_to_column(breed_rank_raw_df, "BreedID")
  #head(breed_rank_df)
  
  # Generate random score in lieu of input from the user.
  # This block should be replaced when it is the code is integrated to Dash
  traits_df <- traits_raw_df %>%
    mutate(score = stats::runif(nrow(traits_raw_df), 1, 100))
  #head(traits_df)
  
  # BEGINNING of more data wrangling.
  top_5_raw_df <- traits_df %>%
    slice_max(n=5, order_by = score)
  
  top_5_raw_df <- top_5_raw_df %>%
    inner_join(breed_rank_df, by = c("BreedID")) %>%
    mutate(Breed = Breed.x) %>%
    select(!Breed.x)
  
  top_5_raw_df <- rename_with(top_5_raw_df, ~ gsub(" Rank", "", .x, fixed=TRUE))
  
  top_5_rank_df <- top_5_raw_df %>%
    pivot_longer(20:27, names_to = "Rank_year", values_to = "Rank")
  # END of data wrangling
  
  traits_list_full <- top_5_rank_df %>%
    select(-c("Breed" ,"Coat Type", "Coat Length")) %>%
    colnames()
  
  return(top_5_rank_df)
}

ranking_plot <- list(
  htmlH1('Ranking of breeds'),
  dccGraph(id='plot-area'),
  htmlDiv(id='output-area'),
  htmlBr(),
  htmlDiv(id='output-area2'),
  htmlBr(),
  dccDropdown(
    id='col-select',
    options = ranking_plot_fun() %>% colnames %>% purrr::map(function(col) list(label = col, value = col)),
    value='Rank_year',
    style=list(
      display= 'block',
      borderWidth= '0',
      width= '100%',
      height= '250px',
      backgroundColor= 'lavender'
    )
  )
)

# 2. APP LAYOUT
app$layout(
  dbcContainer(
    list(
      # Header
      header,
      
      # Row1 
      dbcRow(
        list(
          # Traits multi-option dropdown widget
          dbcCol(
            traits_widget,
            md=3
          ),
          
          # Weights widget for traits
          dbcCol(
            weights_slider,
            md=3
          ),
          
          # Top 5 recommended dogs plot based on selected traits
          dbcCol(
            top5dogs_plot,
            md=6
          )
        )
      ),
      
      htmlHr(
        style=list(
          height= '2px',
          borderWidth= '0',
          color= 'Indigo'
        )
      ),
      
      # Second row panel
      dbcRow(
        #Table container
        dbcCol(
          details_table,
          md=6,
          align = 'left'
        ),
        
        dbcCol(
          ranking_plot,
          md=6,
          align = 'right'
        ),
        
        style = list(
          height = '300px'
        )
      )
    ),
    style=list(
      backgroundColor= 'Beige'
    ),
    fluid=TRUE
  )
)

# 3. CALLBACKS

# Recommended Dogs plot
app$callback(
  output('top5dogs_plot', 'figure'),
  list(
    input('traits-widget', 'value'),
    input('xslider_like', 'value'),
    input('xslider_dislike', 'value')
  ),
  function(traits_list, positive_weight, negative_weight) {
    traits_df <- tibble::rowid_to_column(traits_raw_df, "BreedID")
    breed_rank_df <- tibble::rowid_to_column(breed_rank_raw_df, "BreedID")
    
    traits_df["score"] <- 0
    
    traits_dislikeable <- c("Shedding Level", "Drooling Level")
    
    traits_list <- unlist(traits_list)
    
    for (i in 1:length(traits_list)) {
      if (length(traits_list) == 0) {
        next
      } else if (traits_list[i] %in% traits_dislikeable) {
        traits_df["score"] <- traits_df["score"] + traits_df[traits_list[i]] * negative_weight
      } else {
        traits_df["score"] <- traits_df["score"] + traits_df[traits_list[i]] * positive_weight
      }
    }
    
    merged_df <- merge(traits_df, breed_rank_df, by = "BreedID")
    
    top_5_df <- merged_df[order(-merged_df$score, merged_df$"2020 Rank"),] %>%
      slice_head(n = 5)
    
    top_5_plot <- top_5_df %>%
      ggplot(aes(x = score,
                 y = reorder(Breed.x, score))
      ) +
      labs(x = "Score", y = "Breed")  +
      ggtitle("Your top 5 Dog breeds") +
      geom_bar(stat = "identity")+
      ggthemes::scale_color_tableau()+
      theme(
        plot.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 07),
        axis.title = element_text(size = 10)
      )
    
    ggplotly(top_5_plot) %>% layout()
  }
)

# Table
app$callback(
  output(id='table',property='srcDoc'),
  list(input(id='traits-widget', property='value')),
  function(traits){
    traits_df <- tibble::rowid_to_column(traits_raw_df, 'BreedID')
    breed_rank_df <- tibble::rowid_to_column(breed_rank_raw_df, "BreedID")
    
    traits_df['Score'] <- 0
    
    selected_traits <- unlist(traits)
    
    for (i in 1:length(selected_traits)) {
      if (length(selected_traits) == 0) {
        next
      } else {
        traits_df['Score'] <- traits_df['Score'] + traits_df[selected_traits[i]]
      }
    }
    
    merged_df <- merge(traits_df, breed_rank_df, by="BreedID")
    
    top_5_df <- merged_df[order(-merged_df$'Score', merged_df$"2020 Rank"),] %>%
      slice_head(n = 5)
    
    return(top_5_df %>% htmlTable)
  }
)

# Ranking plot

app$callback(
  output('plot-area', 'figure'),
  list(input('col-select', 'value')),
  function(xcol) {
    top_5_rank_df <- ranking_plot_fun()
    #top_5_rank_df <- return_list[1]
    p <- top_5_rank_df %>%
      ggplot(aes(x=!!sym(xcol),
                 y=Rank,
                 color=Breed)) +
      geom_point() + 
      scale_y_reverse() +
      xlab("Rank year") +
      ggtitle("Popularity ranking of breeds in recent years")
    ggthemes::scale_color_tableau()
    ggplotly(p) %>% layout(dragmode = 'select')
  }
)

app$callback(
  list(output('output-area', 'children'),
       output('output-area2', 'children')),
  list(input('plot-area', 'selectedData'),
       input('plot-area', 'hoverData')),
  function(selected_data, hover_data) {
    list(toString(selected_data), toString(hover_data))
  }
)

app$run_server(host = "0.0.0.0")