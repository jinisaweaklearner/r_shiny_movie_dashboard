#---------------------------------------------------------#
# Name: Xiaocheng Jin                                     #
#                                                         # 
# Plese connect to the internet, as the movie posters are #
# used by urls online.                                    # 
#                                                         # 
# Libraries:                                              #
# shiny,ggplot2,DT,shinydashboard,shinyWidgets,           #
# xtable,plotly,data.table                                #
#                                                         #
# if these packages are in local machine, please use      #
# e.g. install.packages('DT') to install packages         #
#                                                         # 
#---------------------------------------------------------#


library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(DT)
library(xtable)
library(shinyWidgets)

df_stat <- read.csv(file = 'df.csv')
film_choice <- df_stat$title

ui <- fluidPage(
  # customize ui
  tags$style(
    HTML(
      "
      .navbar-header {
        font-family: monospace;
      }


      .container-fluid {
        color:grey;
      }

      .navbar {
        position: relative;
        min-height: 15px;
        margin-bottom: 10px;
        border: 1px solid transparent;
      }

      .navbar-default .navbar-nav>.active>a, .navbar-default .navbar-nav>.active>a:focus, .navbar-default .navbar-nav>.active>a:hover{
        color:#3E81F6;
      }

      div#vir_one_name{
        color:#F95A1F;
      }


      div#vir_two_name{
        color:#F95A1F;
      }

      span.irs-bar{
        background:#3E81F6;
      }

      span.irs-to{
        background:#3E81F6;
      }

      span.irs-from{
        background:#3E81F6;
      }

    "
    )
  ),
  
  
  navbarPage(
  
    title = "- Movie Analysis Dashboard -",
    
    # 1st tab overview
    tabPanel(
      title = "Overview",
      
      # first row num of movies
      fluidRow(
        column(
          width = 3,
          h3('Data Source', style = "color:#3E81F6"),
          p(
            "The data is from the TMDB (The Movie Database)
            which is a community built movie and TV database.
            Overall, there are about top 5000 movies with related features.
            not let you clear the selection."
          ),
          
          br(),
          # select Year
          sliderInput(
            inputId = "select_year",
            label = "Year",
            min = 1950,
            max = 2016,
            value = c(1970, 2016)
          )
        ),
        column(
          width = 9,
          h5(textOutput(outputId = "time_1")),
          plotlyOutput(outputId = "plot", height = "250px")
        )
      ),
      
      hr(),
      
      # second row avg
      fluidRow(
        column(
          width = 3,
          h4('Yearly average of the selected feature', style = "color:#3E81F6"),
          br(),
          # select summary_type_1
          selectInput(
            inputId = 'summary_type_1',
            label = 'Select Feature',
            choices = c(
              "budget",
              'popularity',
              "revenue",
              "runtime" ,
              "vote_average",
              "vote_count"
            ),
            selected = 'vote_average'
          ),
          p('Interesting find: Average voting score decreases over time.')
        ),
        column(
          width = 9,
          h5(textOutput(outputId = "time_2")),
          plotlyOutput(outputId = "summary_plot_1", height = "200px")
        )
      ),
      
      hr(),
      
      # third row sum
      fluidRow(
        column(
          width = 3,
          h4('Yearly sum of the selected feature', style = "color:#3E81F6"),
          br(),
          # select summary_type_2
          selectInput(
            inputId = 'summary_type_2',
            label = 'Select Feature',
            choices = list(
              'budget' = "sum_budget",
              "revenue" = 'sum_revenue',
              "vote_count" = 'sum_vote_count'
            ),
            selected = 'revenue'
          ),
          p('Please notice that the number of films is different over time.')
        ),
        column(
          width = 9,
          h5(textOutput(outputId = "time_3")),
          plotlyOutput(outputId = "summary_plot_2", height = "200px")
        )
      )
    ),
    
    # 2nd tab Feature analysis
    tabPanel(
      title = "Feature analysis",
      
      # info and statistics
      fluidRow(
        column(width = 3, h3('Feature analysis'), style = "color:#F95A1F"),
        column(
          width = 9,
          valueBox(
            textOutput(outputId = 'vir_one_name'),
            textOutput(outputId = 'vir_one_mean'),
            width = 2,
          ),
          valueBox(
            p('95% CI range', style = "color:#F95A1F"),
            textOutput(outputId = 'vir_one_ci'),
            width = 2
          ),
          valueBox(
            textOutput(outputId = 'vir_two_name'),
            textOutput(outputId = 'vir_two_mean'),
            width = 2
          ),
          valueBox(
            p('95% CI range', style = "color:#F95A1F"),
            textOutput(outputId = 'vir_two_ci'),
            width = 2
          ),
          valueBox(
            p('correlation', style = "color:#F95A1F"),
            textOutput(outputId = 'cor_value'),
            width = 4
          )
        )
      ),
      br(),
      
      # 2nd row introduction / selection / plot
      fluidRow(
        # introduction
        column(
          width = 3,
          p(
            "
          In the tab feature analysis, the relationships among features can be identified.
          The interesting find is that budget/revenue have low-correlation with rating.
          Also, some wired values/outliers can be detected by scatter plots.
            "
          ),
          p('*95% CI range: 95% confidence interval'),
          p('*correlation: the value from -1 to 1'),
          # select feature
          selectInput(
            inputId = 'cor_one',
            label = 'feature #1',
            choices = c(
              "budget",
              'popularity',
              "revenue",
              "runtime" ,
              "vote_average",
              "vote_count"
            ),
            selected = 'budget'
          ),
          # select feature
          selectInput(
            inputId = 'cor_two',
            label = 'feature #2',
            choices = c(
              "budget",
              'popularity',
              "revenue",
              "runtime" ,
              "vote_average",
              "vote_count"
            ),
            selected = 'revenue'
          )
        ),
        
        # scatter plot
        column(width = 9,
               plotlyOutput(outputId = "correlation", height = "400px"))
      ),
      hr(),
      fluidRow(column(
        width = 12, h3('More details', style = "color:#3E81F6")
      )),
      br(),
      # 3rd row dataset
      fluidRow(DT::dataTableOutput("table"))
    ),
    
    # 3rd tab top Movies
    tabPanel(
      title = "Top Movies",
      # 1st row 
      fluidRow(
        # left column title/intro/selection
        column(
          width = 3,
          # title
          h3('Overall Ranking', style = "color:#3E81F6"),
          # intro
          p(
            '
          In the tab "top movies", top 10 movies are selected
          and shown
          based on revenue, popularity and rating. Also, users
          can choose the range of years and particular genre to
          filter the rank.
          '
          ),
          p('*unit of revenue: billion '),
          p(
            '*definition of popularity: measurement of user perference definded by TMDB'
          ),
          br(),
          # select year
          sliderInput(
            inputId = "select_year_top",
            label = "Select years",
            min = 1950,
            max = 2016,
            value = c(1970, 2016)
          )
          
        ),
        # these 3 columns are overall ranking (top10) 
        column(
          width = 3,
          h4('Top 10 Revenue', style = "color:#3E81F6"),
          p('unit: billion dollars'),
          br(),
          tableOutput("top_revenue")
        ),
        column(
          width = 3,
          h4('Top 10 Popularity', style = "color:#3E81F6"),
          p('measurement of user perference definded by TMDB'),
          br(),
          tableOutput("top_pop")
        ),
        column(
          width = 3,
          h4('Top 10 Rating', style = "color:#3E81F6"),
          br(),
          br(),
          tableOutput("top_vote")
        )
      ),
      hr(),
      # 2nd row select genre / ranking by genre
      fluidRow(
        column(
          width = 3,
          h3('Ranking by Genre', style = "color:#F95A1F"),
          br(),
          selectInput(
            inputId = 'select_genre_top',
            label = 'Choose Genre',
            choices = c(
              'Action',
              'Adventure',
              'Animation',
              'Comedy',
              'Crime',
              'Documentary',
              'Drama',
              'Family',
              'Fantasy',
              'Foreign',
              'History',
              'Horror',
              'Music',
              'Mystery',
              'Romance',
              'Thriller',
              'War',
              'Western'
            ),
            selected = 'Comedy'
          )
          
        ),
        # these 3 columns are ranking by genre
        column(
          width = 3,
          h4('Top 10 Revenue', style = "color:#F95A1F"),
          p('unit: billion dollars'),
          br(),
          tableOutput("top_revenue_genre")
        ),
        column(
          width = 3,
          h4('Top 10 Popularity', style = "color:#F95A1F"),
          p('measurement of user perference definded by TMDB'),
          br(),
          tableOutput("top_pop_genre")
        ),
        column(
          width = 3,
          h4('Top 10 Rating', style = "color:#F95A1F"),
          br(),
          br(),
          tableOutput("top_vote_genre")
        )
      )
    ),
    # 4th tab Movies & Recommendation
    tabPanel(
      title = "Movies & Recommendation",
      # detials of movies and pic of movie
      fluidRow(
        column(
          width = 4,
          align = "center",
          # select movie
          selectInput(
            inputId = 'search_film',
            label = 'Search for a movie',
            choices = film_choice,
            selected = 'Batman Begins'
          ),
          # pic of particular movie
          uiOutput("img")
        ),
        column(
          width = 8,
          h3(textOutput(outputId = 'film_name'), style = "color:#F95A1F"),
          hr(),
          p('Introduction:', style = 'font-size:120%'),
          p(textOutput(outputId = 'film_intro'), style = 'font-size:120%'),
          p(textOutput(outputId = 'film_genre'), style = 'font-size:120%'),
          p(textOutput(outputId = 'film_pop'), style = 'font-size:120%'),
          p(textOutput(outputId = 'film_revenue'), style = 'font-size:120%'),
          p(textOutput(outputId = 'film_vote'), style = 'font-size:120%'),
          p(textOutput(outputId = 'film_num_vote'), style = 'font-size:120%'),
          p(textOutput(outputId = 'film_date'), style = 'font-size:120%')
        )
      ),
      hr(),
      # 2nd row information of Recommendation
      fluidRow(column(width = 1),
               column(
                 width = 11,
                 h3('Recommendation', style = "color:#3E81F6"),
                 p(
                   '*Please notice that the recommend engine is  based on
            textual data (feature: introduction). Also, all images (links)
            are linked with movies based on movie_id which can be changeable
            by the website.'
                 )
               )),
      br(),
      # 3nd row show 5 recommend movies
      fluidRow(
        column(width = 1),
        column(
          width = 2,
          align = "center",
          uiOutput('re_img_1'),
          p(textOutput(outputId = 'film_re_1'))
        ),
        column(
          width = 2,
          align = "center",
          uiOutput('re_img_2'),
          p(textOutput(outputId = 'film_re_2'))
        ),
        column(
          width = 2,
          align = "center",
          uiOutput('re_img_3'),
          p(textOutput(outputId = 'film_re_3'))
        ),
        column(
          width = 2,
          align = "center",
          uiOutput('re_img_4'),
          p(textOutput(outputId = 'film_re_4'))
        ),
        column(
          width = 2,
          align = "center",
          uiOutput('re_img_5'),
          p(textOutput(outputId = 'film_re_5'))
        )
      )
    )
    
  )
)
