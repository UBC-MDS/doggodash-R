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

# Read in data
traits_raw_df <- read_csv("data/breed_traits.csv")
breed_rank_raw_df <- read_csv("data/breed_rank.csv")

# For traits list in checklist input
traits_list_full <- traits_raw_df %>%
  select(-c("Breed" ,"Coat Type", "Coat Length")) %>%
  colnames()

# Setup app and layout/frontend
app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(
  dbcContainer(
    list(
      htmlH1('DoggoDash'),
      dccGraph(id='plot'), 
      dbcRow(
        list(
          dbcCol(
            list(
              htmlLabel('Choose Dog traits you like and dislike'),
              dccDropdown(
                id = 'traits-widget',
                options = purrr::map(traits_list_full, function(value)
                  list(label = value, value = value)),
                value = traits_list_full[1:4],
                multi = TRUE
              )
            )
          ),
          dbcCol(
            list(
              htmlLabel('Weight for traits you like'),
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
              htmlLabel('Weight for traits you dislike'),
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
              )
            )
          )
        )
      )
    )
  )
)

# Setup callback and backend
app$callback(
  output('plot', 'figure'),
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
                 y = reorder(Breed.x, score))) +
      labs(x = "Score", y = "Breed")  +
      ggtitle("Top 5 recommended Dog breeds as per chosen traits") +
      geom_bar(stat = "identity")
    
    ggplotly(top_5_plot) %>% layout()
  }
)

app$run_server(host = "0.0.0.0")