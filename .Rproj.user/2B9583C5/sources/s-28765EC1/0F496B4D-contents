#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load Libraries 

library(shiny)
library(tidyverse)
library(tidymodels)
library(rstanarm)
library(gt)
library(gtsummary)
library(broom.mixed)

# Read in data sets. Set col-types

Compsheet <- read_csv("eventssheet/compsheet.csv",
                      col_types = cols(X1 = col_double(),
                                       Name = col_character(),
                                       judge_country = col_character(),
                                       number = col_double(),
                                       segment = col_character(),
                                       skater = col_character(),
                                       skater_country = col_character(),
                                       TSS = col_double()))

# Add significant mutations to the data set. 
Compsheet <- Compsheet %>% 
    group_by(segment, skater) %>% 
    mutate(meanTSS = mean(TSS)) %>% 
    mutate(stddevTSS = sd(TSS)) %>% 
    mutate(normTSS = (TSS - meanTSS)/stddevTSS) %>%
    mutate(same_country = ifelse(skater_country == judge_country, TRUE, FALSE)) 

CompsheetNAMES <- tibble(inputnames = c("2018 Olympics", 
                                        "2017 Grand Prix Final",
                                        "2017 Skate America",
                                        "2017 Internationaux de France",
                                        "2017 NHK Trophy",
                                        "2017 Cup of China",
                                        "2017 Skate Canada International",
                                        "2017 Rostelecom Cup"),
                         firstfilter = c("2018 Olympic Games Ice Dance Short",
                                         "2017 Grand Prix Final Ice Dance Short",
                                         "2017 Grand Prix USA Ice Dance Short",
                                         "2017 Grand Prix IDF Ice Dance Short",
                                         "2017 Grand Prix NHK Ice Dance Short",
                                         "2017 Grand Prix COC Ice Dance Short",
                                         "2017 Grand Prix SCI Ice Dance Short",
                                         "2017 Grand Prix RCM Ice Dance Short"),
                         secondfilter = c("2018 Olympic Games Ice Dance Free",
                                          "2017 Grand Prix Final Ice Dance Free",
                                          "2017 Grand Prix USA Ice Dance Free",
                                          "2017 Grand Prix IDF Ice Dance Free",
                                          "2017 Grand Prix NHK Ice Dance Free",
                                          "2017 Grand Prix COC Ice Dance Free",
                                          "2017 Grand Prix SCI Ice Dance Free",
                                          "2017 Grand Prix RCM Ice Dance Free"))
    

# Define UI for application that draws a histogram
ui <- navbarPage(
    "International Judging Biases in Ice Dance over the 2017-2018 Season",
    theme = shinytheme("journal"),
    
#### PAGE 1: COMPETITION SCORES ####
    tabPanel("Competition Scores", 
             fluidPage(
                 titlePanel("Competition Scores"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("competitionid", 
                        "Competition",
                        c("2018 Olympics", 
                          "2017 Grand Prix Final",
                          "2017 Skate America",
                          "2017 Internationaux de France",
                          "2017 NHK Trophy",
                          "2017 Cup of China",
                          "2017 Skate Canada International",
                          "2017 Rostelecom Cup"))),
    mainPanel(
        p("Pick a competition to see the calculated Total Competition Scores of Ice Dance Pairs", style = "color:#010203;"),
        plotOutput("competitionplot")
        )))),

#### PAGE 2: EXPLAINING THE 2017-2017 Season ######
    tabPanel("Explaining the 2017-2018 Season",
             titlePanel("Interactive History of the 2017-2018 Season"),
             p("Tessa Virtue and Scott Moir of Canada were the veterans at these Olympic games, going for their 4th and 5th Olympic Gold Medals"),
             HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/rt1W8ADSJ08" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
             p("INFO TO COME ")),

#### Page 3: Guide to Ice Dance Scoring ####
    tabPanel("Guide To Ice Dance Scoring",
             titlePanel("Guide To Ice Dance Scoring"),
             p("INFO TO COME ")),

#### PAGE 4: Model ####### 
    tabPanel("Model",
             titlePanel("Model"),
             plotOutput("judgescoresplot"),
             sidebarPanel(
                 h3("So what do we know?"),
                 p("Intercept: The coefficient under the Beta for (intercept) in my model is -0.9. 
                   In terms of scoring, this means that a judge of an opposing country is likely to score you   ")
             ),
             gt_output("samecountrytable"),
             p("INTERPRETATION TO COME")),

##### PAGE 5: ADVICE #####
    tabPanel("Advice",
             titlePanel("Discussion Title"),
             p("ANALYSIS TO COME")),

#### PAGE 6: ABOUT ####
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("This project is a project about international judging biases on subjective olympic sports. 
               For this project, I will be looking ice dancing in the 2017-2018 olympic season. The 
               purposes of this is to investigate whether judges have national biases that are reflected in the 
               scores and whether these biases have a statistically significant impact on the competitive outcomes."),
             p("I did this project because it allows me to get to explore one of my favorite topics: olympic sports. 
             My love for olympic sports, especially the subjective ones, is really exciting. This is the perfect 
             opportuntiy for me to research these in an academic setting."),
             h3("About Me"),
             p("My name is Julia Blank, a first year in Government 50 data. 
             I intend to study Government on the Data Science track.  
             You can reach me at juliablank@college.harvard.edu."),
             p("To see my code, check out my github repo here: ")))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$competitionplot <- renderPlot({
        #taking first filter column and passes a vector that is either true or false for each entry in the vector
        icedanceshort <- CompsheetNAMES$firstfilter[CompsheetNAMES$inputnames == input$competitionid]
        icedancefree <- CompsheetNAMES$secondfilter[CompsheetNAMES$inputnames == input$competitionid]
        print(icedanceshort)
        print(icedancefree)
        
            Compsheet %>%
                filter(segment %in% c(icedanceshort, 
                                      icedancefree)) %>% 
                group_by(skater) %>%
                summarize(sum_TSS = (sum(meanTSS)/9), country = skater_country[1], .groups = "drop") %>%
                ggplot(aes(x = fct_reorder(skater, sum_TSS), y = sum_TSS, fill = country)) +
                geom_bar(stat = "identity") + 
                coord_flip() +
                theme_classic() +
                labs(title = input$competitionid,
                     x = "Skating Pair", 
                     y = "Total Competition Score (sum of mean segment scores)")    
        })
    
    output$judgescoresplot <- renderPlot({
        pointsize <- ifelse(Compsheet$same_country == TRUE, 3 , 1)
        Compsheet %>% 
            group_by(skater_country, segment) %>% 
            ggplot(aes(x = judge_country, y = normTSS, color = same_country )) +
            geom_point(size = pointsize) +
            facet_wrap(~ segment, scales = "free_x") +
            labs(title = "Normalized Total Short Program Score of Skating Dance Pairs by Judge Country",
                 x = "Judge by Country",
                 y = "Normalized Total Segment Score") +
            theme_bw() +
            scale_color_manual(name = "Are the Judge and Skater from the same country?",
                               values = c("Red", "Blue"),
                               labels = c("No", "Yes"))
    }) 
   
    output$samecountrytable <- render_gt({
        judgemodel <- stan_glm(normTSS ~ same_country,
                               data = Compsheet,
                               refresh = 0)
        tbl_regression(judgemodel, intercept = TRUE) %>% 
            as_gt() %>%
            tab_header(title = "Influence of Judge's Nationality on Ice Dance Pair's Score",
                       subtitle = "The Effect of Nationalism on Ice Dance Judging")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
