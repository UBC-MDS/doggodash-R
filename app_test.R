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
      'Top 5 recommended Dog breeds as per chosen traits. Click to view them!',
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
      'Hover over to see 2020 rank and click to see image. To go back, click on browser back button',
      style = list( 
        color= 'Indigo', 
        fontSize= 14,
        textAlign= 'center'
      )
    )
)

# Row-2
#Col-1: Table of details on the recommended breeds
details_table <- list(
  htmlIframe(
    id = 'table',
    style = list(
      height = "600px",
      width = '800px'
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
      
      # Second row panel
      dbcRow(
        #Table container
        dbcCol(
          details_table
        ),
        
        
      )
    ),
    style=list(
      backgroundColor= 'Beige'
    ),
    fluid=TRUE
  )
)


# 3. CALLBACKS

# Recommended Dogs
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

app$run_server(host = "0.0.0.0")