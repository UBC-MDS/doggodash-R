library(dash)
library(dashBootstrapComponents)
library(dashHtmlComponents)
library(dashCoreComponents)
library(ggplot2)
library(plotly)
library(purrr)
library(ggthemes)

# Read in data
traits_raw_df <-  read_csv("data/breed_traits.csv")
breed_rank_raw_df  <- read_csv("data/breed_rank.csv")

# For traits list in checklist input
traits_list_full <- traits_raw_df %>%
  subset(select=- c(`Breed` ,`Coat Type`, `Coat Length`)) %>% 
  colnames()

# Setup app and layout/frontend
app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

# Add title
# app$title <- "doggodash"


app$layout(
  dbcContainer(
    list(
      htmlH1(
        list(
          'Doggodash' 
        ),
        style=list(
          color='white', 
          fontSize=40,
          backgroundColor='Indigo',
          textAlign='center'
        )
        
      ),
      
      htmlDiv(
        list(
          htmlP(
            'Choose Dog traits you like and dislike'
          ),
          dccDropdown(
            id = 'col-select',
            value= traits_list_full[1],
            options = traits_list_full %>%
              purrr::map(function (col) list(label = col, value = col)),
            placeholder='Select Doggo traits',
            multi=FALSE
          ),
          dccGraph(id='plot-area')
        )
      )
    )
  )
)


 
app$callback(
  output('plot-area', 'figure'),
  list(input('col-select', 'value')),
  
  # function for widget inputs and figure outputs
  function(xcol) {
      p <- ggplot(traits_raw_df)+
        aes(
          x=as.factor(!!sym(xcol)),
          y= ..count..
        )+
        geom_bar()+
        ggthemes::scale_color_tableau()+
        labs(
          x=xcol,
          y='Total number of breeds',
          title='Breed distribution by trait scores'
        )
      return(ggplotly(p))
    }
)
app$run_server(host = '0.0.0.0')