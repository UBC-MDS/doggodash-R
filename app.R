library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
#library(tidyverse)
library(readr)
library(tidyr)
library(dplyr)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

traits_raw_df <- read_csv("data/breed_traits.csv")
breed_rank_raw_df <- read_csv("data/breed_rank.csv")

# Some data wrangling in preparation for the ranking trend plot

traits_raw_df <- tibble::rowid_to_column(traits_raw_df, "BreedID")
#head(traits_raw_df)
breed_rank_df <- tibble::rowid_to_column(breed_rank_raw_df, "BreedID")
#head(breed_rank_df)

# Generate random score in lieu of input from the user.
traits_df <- traits_raw_df %>%
    mutate(score = stats::runif(nrow(traits_raw_df), 1, 100))
#head(traits_df)

top_5_raw_df <- traits_df %>%
    slice_max(n=5, order_by = score)

top_5_raw_df <- top_5_raw_df %>%
    inner_join(breed_rank_df, by = c("BreedID")) %>%
    mutate(Breed = Breed.x) %>%
    select(!Breed.x)

top_5_raw_df <- rename_with(top_5_raw_df, ~ gsub(" Rank", "", .x, fixed=TRUE))

top_5_rank_df <- top_5_raw_df %>%
    pivot_longer(20:27, names_to = "Rank_year", values_to = "Rank")

traits_list_full <- top_5_rank_df %>%
    select(-c("Breed" ,"Coat Type", "Coat Length")) %>%
    colnames()

app$layout(
    dbcContainer(
        list(
            htmlH1('Ranking of breeds'),
            dccGraph(id='plot-area'),
            htmlDiv(id='output-area'),
            htmlBr(),
            htmlDiv(id='output-area2'),
            htmlBr(),
            dccDropdown(
                id='col-select',
                options = top_5_rank_df %>% colnames %>% purrr::map(function(col) list(label = col, value = col)),
                value='Rank_year')
        )
    )
)

app$callback(
    output('plot-area', 'figure'),
    list(input('col-select', 'value')),
    function(xcol) {
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

app$run_server(host = '0.0.0.0')
#app$run_server(debug = T)