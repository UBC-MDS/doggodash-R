#libraries
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
library(tibble)
library(dplyr)
library(purrr)
library(knitr)
library(htmlTable)

#data
traits_raw_df <- readr::read_csv("data/breed_traits.csv")
breed_rank_raw_df <- readr::read_csv("data/breed_rank.csv")

traits <- traits_raw_df %>%
  select(-c("Breed","Coat Type","Coat Length")) %>%
  colnames()

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(
    dbcContainer(
        list(
            htmlH1("DoggoDash"),
            #dbcContainer(id='table'),
            dbcRow(
                list(
                    dbcCol(
                        list(
                            htmlLabel('Choose traits'),
                            dccDropdown(
                                id = 'traits-widget',
                                options = purrr::map(traits, function(value)
                                list(label = value, value = value)),
                                value = traits[1:4],
                                multi = TRUE
                            )
                        )
                    )
                )
            ),
            dbcRow(
                dbcCol(
                    list(
                        htmlIframe(
                                id = 'table',
                                style = list(
                                    height = "600px",
                                    width = '800px'
                                )
                            )
                    )
                )
            )
        )
    )
)

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
})



app$run_server(host = '0.0.0.0')

