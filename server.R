#---------------------------------------------------------#
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
library(data.table)

# import data
df <- read.csv(file = "data_per_year.csv")
df_stat <- read.csv(file = 'df.csv')
df_recommend <- read.csv(file = 'recommendation.csv')
df_link <- read.csv(file = 'link.csv')

# server
server <- function(input, output, session) {
  
  # ----------------------------------------------------
  # tab 1
  
  # select range of years and features
  df_select_main <- reactive({
    df_temp <-df[df[, "year"] > input$select_year[1] &
           df[, "year"] < input$select_year[2], c('year',
                                                  'num_film',
                                                  input$summary_type_1,
                                                  input$summary_type_2)]
  })
  
  # plot num of films
  output$plot <- renderPlotly({
    p <- ggplot(df_select_main(),
                aes_string(x = 'year',
                           y = 'num_film',
                           group = 1)) +
      geom_bar(stat = "identity", fill = '#3E81F6') +
      theme(axis.title = element_blank())
    show <- ggplotly(p, tooltip = c("year", 'num_film'))
  })
  
  # plot avg value of selected feautre each year
  output$summary_plot_1 <- renderPlotly({
    p <- ggplot(df_select_main(),
                aes_string(
                  x = 'year',
                  y = input$summary_type_1,
                  group = 1
                )) +
      geom_line(color = '#3E81F6') +
      geom_point(color = '#3E81F6') +
      geom_area(fill = "#F95A1F") +
      theme(axis.title = element_blank())
    show <- ggplotly(p, tooltip = c("year", input$summary_type_1))
  })
  
  # plot sum of value for selected feautre each year
  output$summary_plot_2 <- renderPlotly({
    p <- ggplot(df_select_main(),
                aes_string(
                  x = 'year',
                  y = input$summary_type_2,
                  group = 1
                )) +
      geom_line(color = '#3E81F6') +
      geom_point(color = '#3E81F6') +
      geom_area(fill = "#FFCB08") +
      theme(axis.title = element_blank())
    show <- ggplotly(p, tooltip = c("year", input$summary_type_2))
  })
  
  
  # output text
  output$time_1 <- renderText({
    paste('Number of Films from ',
          input$select_year[1],
          ' to ',
          input$select_year[2])
  })
  
  # output text
  output$time_2 <- renderText({
    paste(
      'yearly avg of feature',
      input$summary_type_1 ,
      ' from ',
      input$select_year[1],
      ' to ',
      input$select_year[2]
    )
  })

  # output text
  output$time_3 <- renderText({
    paste(
      'yearly sum of feature',
      input$summary_type_2 ,
      ' from ',
      input$select_year[1],
      ' to ',
      input$select_year[2]
    )
  })
  
  # ----------------------------------------------------
  # tab 2
  
  # two selected features
  df_select <-
    reactive({
      df_stat[, c(input$cor_one, input$cor_two)]
    })
  
  # name of variable
  output$vir_one_name <- renderText({
    paste('Avg ', input$cor_one)
  })
  
  # avg of variable
  output$vir_one_mean <- renderText({
    paste(prettyNum(round(mean(df_select(
    )[, 1]), 2), big.mark = ","))
  })
  
  # name of variable
  output$vir_two_name <- renderText({
    paste('Avg ', input$cor_two)
  })
  
  # avg of variable
  output$vir_two_mean <- renderText({
    paste(prettyNum(round(mean(df_select(
    )[, 2]), 2), big.mark = ","))
  })
  
  # the function to calculate confidence interval 1
  ci_one <- reactive({
    avg <- mean(df_select()[, 1])
    sd <- sd(df_select()[, 1])
    error <- qt(0.975, df = 4803) * sd / sqrt(4803)
    right <- round((avg + error), 2)
    left <- round((avg - error), 2)
    ci_one <- c(left, right)
  })
  
  # show confidence interval 1
  output$vir_one_ci <- renderText({
    paste(prettyNum(ci_one()[1], big.mark = ","),
          ' to ',
          prettyNum(ci_one()[2], big.mark = ","))
  })
  
  # the function to calculate confidence interval 2
  ci_two <- reactive({
    avg <- mean(df_select()[, 2])
    sd <- sd(df_select()[, 2])
    error <- qt(0.975, df = 4803) * sd / sqrt(4803)
    right <- round((avg + error), 2)
    left <- round((avg - error), 2)
    ci_two <- c(left, right)
  })
  
  # show confidence interval 2
  output$vir_two_ci <- renderText({
    paste(prettyNum(ci_two()[1], big.mark = ","),
          ' to ',
          prettyNum(ci_two()[2], big.mark = ","))
  })
  
  # correlation value
  output$cor_value <- renderText({
    round(cor(df_select()[, 1], df_select()[, 2], method = "pearson"), digits =
            4)
  })
  
  # scatter plot
  output$correlation <- renderPlotly({
   p<- ggplot(df_stat, aes_string(x = input$cor_one,
                               y = input$cor_two,
                              text= paste('title'))) +
      geom_point(size = 0.5,color = '#F95A1F' ) 

  })
  
  # raw table
  output$table <- DT::renderDataTable({
    DT::datatable(xtable(df_stat))
  })
  
  # ----------------------------------------------------
  # tab 3
  
  # top 10 revenue overall
  df_top_revenue <- reactive({
    df_temp <-
      df_stat[df_stat[, "year"] > input$select_year_top[1] &
                df_stat[, "year"] < input$select_year_top[2], c('title', 'revenue')]
    df_temp$revenue <- df_temp$revenue / 1000000000
    df_temp <-
      df_temp[order(df_temp$revenue, decreasing = TRUE)[1:10], ]
    
  })
  
  output$top_revenue <- renderTable(
    xtable(df_top_revenue()),
    rownames = FALSE,
    colnames = FALSE,
    digits = 4
  )
  
  # top 10 popularity overall
  df_top_pop <- reactive({
    df_temp <-
      df_stat[df_stat[, "year"] > input$select_year_top[1] &
                df_stat[, "year"] < input$select_year_top[2], c('title', 'popularity')]
    df_temp <-
      df_temp[order(df_temp$popularity, decreasing = TRUE)[1:10], ]
  })
  
  output$top_pop <- renderTable(xtable(df_top_pop()),
                                rownames = FALSE,
                                colnames = FALSE)
  
  # top 10 rating overall
  df_top_vote <- reactive({
    df_temp <-
      df_stat[df_stat[, "year"] > input$select_year_top[1] &
                df_stat[, "year"] < input$select_year_top[2], c('title', 'vote_average')]
    df_temp <-
      df_temp[order(df_temp$vote_average, decreasing = TRUE)[1:10], ]
  })
  
  output$top_vote <- renderTable(xtable(df_top_vote()),
                                 rownames = FALSE,
                                 colnames = FALSE)
  
  # top 10 revenue by genre
  df_top_revenue_genre <- reactive({
    df_temp <- df_stat[df_stat$genres %like% input$select_genre_top,]
    df_temp <-
      df_temp[df_temp[, "year"] > input$select_year_top[1] &
                df_temp[, "year"] < input$select_year_top[2], c('title', 'revenue')]
    df_temp$revenue <- df_temp$revenue / 1000000000
    df_temp <-
      df_temp[order(df_temp$revenue, decreasing = TRUE)[1:10], ]
  })
  
  output$top_revenue_genre <- renderTable(
    xtable(df_top_revenue_genre()),
    rownames = FALSE,
    colnames = FALSE,
    digits = 4
  )
  
  # top 10 popularity by genre
  df_top_pop_genre <- reactive({
    df_temp <- df_stat[df_stat$genres %like% input$select_genre_top,]
    df_temp <-
      df_temp[df_temp[, "year"] > input$select_year_top[1] &
                df_temp[, "year"] < input$select_year_top[2], c('title', 'popularity')]
    df_temp <-
      df_temp[order(df_temp$popularity, decreasing = TRUE)[1:10], ]
  })
  
  output$top_pop_genre <- renderTable(xtable(df_top_pop_genre()),
                                      rownames = FALSE,
                                      colnames = FALSE)
  
  # top 10 rating by genre
  df_top_vote_genre <- reactive({
    df_temp <- df_stat[df_stat$genres %like% input$select_genre_top,]
    df_temp <-
      df_temp[df_temp[, "year"] > input$select_year_top[1] &
                df_temp[, "year"] < input$select_year_top[2], c('title', 'vote_average')]
    df_temp <-
      df_temp[order(df_temp$vote_average, decreasing = TRUE)[1:10], ]
  })
  
  output$top_vote_genre <- renderTable(xtable(df_top_vote_genre()),
                                       rownames = FALSE,
                                       colnames = FALSE)
  
  # ----------------------------------------------------
  # tab 4 movie details
  
  # name of movie
  output$film_name <- renderText({
    paste(input$search_film)
  })
  
  # get related link and pic of movie
  output$img <- renderUI({
    df_temp <- df_link[df_link$title == input$search_film, c('link')]
    tags$img(src =  df_temp)
  })
  
  
  # function for show movie details
  film_digits <- reactive({
    df_temp <- df_stat[df_stat$title == input$search_film,]
  })
  
  # output popularity
  output$film_pop <- renderText({
    paste('Popularity: ',
          prettyNum(film_digits()['popularity'], big.mark = ',', digits = 0))
  })
  
  # output revenue
  output$film_revenue <- renderText({
    paste('Revenue: ', prettyNum(film_digits()['revenue'], big.mark = ','))
  })
  
  # output voting
  output$film_vote <- renderText({
    paste('Rating: ', film_digits()['vote_average'])
  })
  
  # output num of vote
  output$film_num_vote <- renderText({
    paste('Number of votes: ', prettyNum(film_digits()['vote_count'], big.mark = ','))
  })
  
  # function for introduction
  film_intro <- reactive({
    df_temp <- df_stat[df_stat$title == input$search_film, c('overview')]
  })
  
  # output introduction
  output$film_intro <- renderText({
    paste(film_intro())
  })
  
  # function for Release date
  film_date <- reactive({
    df_temp <-
      df_stat[df_stat$title == input$search_film, c('release_date')]
  })
  
  # output Release date
  output$film_date <- renderText({
    paste('Release date: ', film_date())
  })
  
  # function for genres
  film_genre <- reactive({
    df_temp <- df_stat[df_stat$title == input$search_film, c('genres')]
  })
  
  # output genres
  output$film_genre <- renderText({
    paste('Genres: ', film_genre())
  })
  
  # ----------------------------------------------------
  # tab 4 recommendation
  
  # function for recommendation data
  film_re_stat <- reactive({
    df_temp <- df_recommend[df_recommend$title == input$search_film,]
  })
  
  # recommaed no 1
  film_1_name <- reactive({
    df_temp <-
      df_recommend[df_recommend$title == input$search_film, c('top_1_name')]
  })
  
  output$re_img_1 <- renderUI({
    df_temp <- df_link[df_link$title == paste(film_1_name()), c('link')]
    tags$img(src = df_temp, width = '70%')
  })
  
  output$film_re_1 <- renderText({
    paste('No 1: ', film_1_name())
  })
  
  # recommaed no 2
  film_2_name <- reactive({
    df_temp <-
      df_recommend[df_recommend$title == input$search_film, c('top_2_name')]
  })
  
  output$re_img_2 <- renderUI({
    df_temp <- df_link[df_link$title == paste(film_2_name()), c('link')]
    tags$img(src = df_temp, width = '70%')
  })
  
  output$film_re_2 <- renderText({
    paste('No 2: ', film_2_name())
  })
  
  # recommaed no 3
  film_3_name <- reactive({
    df_temp <-
      df_recommend[df_recommend$title == input$search_film, c('top_3_name')]
  })
  
  output$re_img_3 <- renderUI({
    df_temp <- df_link[df_link$title == paste(film_3_name()), c('link')]
    tags$img(src = df_temp, width = '70%')
  })
  
  output$film_re_3 <- renderText({
    paste('No 3: ', film_3_name())
  })
  
  # recommaed no 4
  film_4_name <- reactive({
    df_temp <-
      df_recommend[df_recommend$title == input$search_film, c('top_4_name')]
  })
  
  output$re_img_4 <- renderUI({
    df_temp <- df_link[df_link$title == paste(film_4_name()), c('link')]
    tags$img(src = df_temp, width = '70%')
  })
  
  output$film_re_4 <- renderText({
    paste('No 4: ', film_4_name())
  })
  
  # recommaed no 5
  film_5_name <- reactive({
    df_temp <-
      df_recommend[df_recommend$title == input$search_film, c('top_5_name')]
  })
  
  output$re_img_5 <- renderUI({
    df_temp <- df_link[df_link$title == paste(film_5_name()), c('link')]
    tags$img(src = df_temp, width = '70%')
  })
  
  output$film_re_5 <- renderText({
    paste('No 5: ', film_5_name())
  })
  
}
