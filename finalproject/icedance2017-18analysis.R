#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

# Load Libraries 

library(shiny)
library(tidyverse)
library(tidymodels)
library(rstanarm)
library(gt)
library(gtsummary)
library(broom.mixed)
library(shinythemes)


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


# Add significant mutations to the data set. Since skating scores for  different 
# programs within a competition are on different scales, I decided to normalize 
# the scores using a z score formula where I set the mean equal to 0. To do 
# this, I calculated the mean segment score and standard deviation of the score
# spread for each skater for each program. Then, I used the z score formula to 
# normalize the scores. 

Compsheet <- Compsheet %>% 
    group_by(segment, skater) %>% 
    mutate(meanTSS = mean(TSS)) %>% 
    mutate(stddevTSS = sd(TSS)) %>% 
    mutate(normTSS = (TSS - meanTSS)/stddevTSS) %>%
    
# Additionally, I created a column that identified whether a skater and judge
# were from the same country. This allowed me to create more complex graphs 
# later on and to have a same_country variable in my model. 
    
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
    
    
#### PAGE 1: COMPETITION SCORES & ICE DANCE 2017-2018 HISTORY ####

    tabPanel("Competition Scores", 
             fluidPage(
                 titlePanel("Competition Scores"),
                 sidebarLayout(
                     sidebarPanel(

# Here, I wanted to create an input button that allowed for users to pick which
# competition they wanted to see the scores for. At first I tried different 
# input choices like Radio Buttons, but realized that select input was the 
# easier and more aesthetically pleasing choice. 

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
        h4("Pick a competition to see the calculated Total Competition Scores of
           Ice Dance Pairs", style = "color:#010203;"),
        plotOutput("competitionplot"),
        HTML('<center><iframe width="560" height="315" 
             src="https://www.youtube.com/embed/q20ga_6VYyk" frameborder="0" 
             allow="accelerometer; autoplay; clipboard-write; encrypted-media; 
             gyroscope; picture-in-picture" allowfullscreen></iframe></center>'),
        h2("Explaining the 2017-2018 Season"),
        p("The 2017-2018 Ice Dance season was a highly anticipated Olympic 
        Season. In Ice Dance, there were two top teams: the French Team of 
          Gabriella Papadakis and Guillaume Cizeron and the Canadian Team of 
          Tessa Virtue and Scott Moir. Virtue and Moir were the veterans, having
          won Olympic Gold in Vancouver 2010 (the first non European team to 
          every win Gold) and 2 Olympic Silvers at Sochi. After Sochi, the team 
          took a two year hiatus but returned to competition in 2016-17 to win 
          the World Championship over the French. Meanwhile, Papadakis and 
          Cizeron rose to Ice Dance fame in the 2014-15 season and were 
          undefeated winning 2 world championships and countless Grand Prixs 
          until the Canadians returned. To add even more competitive fuel to the
          fire, both teams trained with the same coaches in Montreal."),
    
# I learned that I could embed youtube videos throughout the website by 
# using HTML and getting an HTML code directly from youtube via the share 
# button. 
    
        HTML('<center><iframe width="560" height="315" 
              src="https://www.youtube.com/embed/rt1W8ADSJ08" frameborder="0" 
              allow="accelerometer; autoplay; clipboard-write; encrypted-media; 
              gyroscope; picture-in-picture" allowfullscreen></iframe></center>'),
        p("The 2017-2018 season was also a very good year for American Ice Dance,
          with the US having 3 top viable teams. The US had two teams 
          Madison Hubbell and Zachary Donohue and the sibling pair of Maia 
          Shibutaniand Alex Shibutani. Despite a crushing loss to 
          Hubbell/Donohue, Shibutani/Shibutani came back at the Olympics to win
          the Bronze."),
        HTML('<center><iframe width="560" height="315" 
             src="https://www.youtube.com/embed/JMRCezJxiDc" frameborder="0" 
             allow="accelerometer; autoplay; clipboard-write; encrypted-media; 
             gyroscope; picture-in-picture" allowfullscreen></iframe></center>')
        )))),

#### PAGE 2: Guide to Ice Dance Scoring ####

    tabPanel("Guide To Ice Dance Scoring",
         titlePanel("Guide To Ice Dance Scoring"),

# I definitely struggled with finding ways to align images. Eventually, I 
# realized to do so I would need to do it in HTML. So I used the HTML function 
# and then learned the HTML centering syntax to allign my images.

         HTML('<center><img src = "vm1scoring.jpg" height = 300 width = 500 >
              <center>'),
         h3("What even is Ice Dancing?"),
         p("Ice dance is one of the 4 competitive disciplines within Figure 
         Skating. The discipline of Ice Dance was historically rooted in 
         traditional Ballroom movements and focused on translating these to the
         ice. Where the other disciplines in figure skating all rely heavily on 
         jumps, Ice Dance forbids jumps and instead focuses much more heavily on
         lifts, spins, glide of the blade, and overall grace of the skaters. In 
         Ice Dance, pairs (1 male and 1 female, according to ISU rules) skate 
         two programs within a competition: 1 short dance that is 2:50 minutes 
         long (renamed the Rhythm dance in 2018) and 1 free dance that’s 4 
         minutes long. For the short dance, every team is required to a 
         specific required ballroom pattern for an entire season as dictated by 
           the ISU.For the free program, the teams get to be incredibly creative
           and choose whatever dance pattern they would like. Uniquely, Ice 
           Dance is a discipline that is highlights a balance between expression
           of artistry and grace, as well as fierce athleticism."),
         HTML('<center><img src = "Scorecard.png" height = 300 
              width = 400><center>'),
         h3("So that's cool.... but how do you score this?"),
         p("In Ice Dance, each team performs 2 programs: the short dance and 
           free dance. These two scores are added up for each team at the end of
           the competition. The team with the highest Total Competition Score 
           gets the Gold Medal, 2nd highest gets Silver, 3rd highest gets 
           Bronze, and 4th highest gets the Pewter. The question remains…. how 
           is each program scored"),
         p("Each program is a combination of two types of scores: the Technical
           Elements Score (TES) and the Program Component Score (PCS)."),
         p("The Technical Element Score is a score that values the difficulty 
         and execution of the technical requirements and elements within the 
         program. In Ice Dance, this includes moves like spins, lifts, and step
         sequences. The TES is decided upon by 2 groups of people: the judges 
         (a panel of 9, all from various countries) and a technical panel of 3 
         specialists. The technical panel decides whether moves were completed 
         at a certain difficulty and then gives an assigned base value of 
         difficulty (Levels 1-4, with 4 being the most difficult). The judging 
         panel of 9 then assigned a Grade of Execution (GOE) to each element
           the pair completes. This GOE is a score between -3 and +3. The better
           the team executes the elements in performance the higher the score 
           they receive. Finally, the highest and lowest GOE scores for each 
           program are dropped and the remaining 7 scores are averaged. To get a
           complete TES, the GOE scores are added to the Base Value difficulty 
           scores assigned by the Technical Panel."),
         p("Now, onto the more ambiguous Program Component Score or (PCS) of a 
           program. The PCS is determined by the judges overall impressions of 
           the program and not focused on any one element in particular. The PCS
           is broken down into 5 categories: skating skills, interpretation, 
           transitions, composition, performance. Each category gets scored 
           between 0.25 and 10, with 10 being the best. Finally, these 5 
           category scores are averaged to form a total PCS score out of 10, 
           that is then multiplied by a factor that depends on which program 
           it is (short/free)."),
         p("At the end, the PCS and TES are added together for the final 
           total score.")
         ), 

#### PAGE 3: Model ####### 

    tabPanel("Model",
             h1("Model"),
             plotOutput("judgescoresplot"),
             h4("This graph illustrates the spread of normalized scores that 
                each judge gave skaters for each program of every competition 
                in the 2017-2018 Ice Dance Season. The blue dots represent 
                skater pairs from the same country as the judge. As we can see, 
                the blue dots top almost every judge’s column, illustrating that 
                almost every judge scored own nation’s skater pairs far above 
                the mean score given to them. This consistency of blue dots 
                being higher than red dots in nearly every program for every 
                judge likely indicates the persistence of nationalistic judging
                bias in Ice Dance for the 2017-2018 season."),
             h3("Equation"),
             HTML('<center><img src = "latex2.png"  height = 300 width = 800>
                  <center>'),

# I couldn't figure out how to size output graphs. Looking through R 
# Documentation, I found a width argument within p.otOutput that would allow you
# to scale the graph by a percentage, so while not as easy as dimensions it did
# sucessfully resize my graph. 

             plotOutput("Posterior", width =  "50%"),
             h3("Regression Table and Interpretation"),
             sidebarPanel(
                 tags$style(".well {background-color: darkturquoise;}"),
                 h3("So what do we know?"),
                 p("NormTSS: NormTSS is the Normalized Total Segment Score, a 
                   z-score estimate of how many standard deviations above a mean
                   of 0 that a particular judge will score a skater during a 
                   segment compared to other judges. So, the higher the score 
                   the much higher a judge scored a team in comparison to the 
                   other judges on the panel."),
                 p("Intercept (Beta0): The coefficient under the beta for 
                 (intercept) in my model represents the average Normalized Total
                 Segment Score a judge would give to a team not from the same 
                 country. With a median value of -.1, this means that a judge of
                 a differing country is likely to score give a team a score that
                 is .1 standard deviations lower than the average score of the 
                   whole judging panel."),
                 p("Same_country (Beta1): The coefficient under the beta for 
                   same_country in my model represents the average Normalized 
                   Total Segment Score a judge would give to a team from the same
                   country. With a median value of 1, this means that a judge 
                   would score a team from the same country on average 1 standard
                   deviation higher than average of the whole judging panel."),
             ),

# At first, I used a table output for my gt regression table but was 
# continuously getting numerous extra variables in the table in the wrong 
# format. So, in looking for alternatives I discovered that there is a gt 
# package for shiny that would allow me to use gt_output as its own function to
# print my regression table. 

             gt_output("samecountrytable")
             ), 

##### PAGE 4: ADVICE #####

    tabPanel("Advice",
             titlePanel("So, why should I care?"),
             h4("Well, if you are not involved or invested in Figure Skating, 
             then you may not  be devastated at the presence of bias but don’t 
             underestimate its importance. For many, the Olympic Games is meant 
             to be a pillar of hope and a time when nationalistic biases are put
             aside in favor of the spirit of international community. However,
             with fierce competition at stake, we can wonder if it really is 
             this paragon of virtue it is made out to be. The presence of bias 
             within judges in Ice Dance may not be a unique phenomenon and 
             numerous seemingly “subjective-scoring” sports may also experience
             this. So my advice to you would be that hopefully this project 
             prompts you to look at your own favorite sports and investigate
                this."),
             h2("So, what if I am a federation head? How does this help me?"),
             HTML('<center><img = src = "didiergailhaguet.jpg" width = 200 
                  height = 200></center>'),

             h4("Now, if you are one of the many involved (see Image of a 
             scheming Didier, French Federation Coordinator) then you might be
             wondering how you can apply this knowledge to get results. In a 
             sport decided by tenths of a point, any boost you can give a team 
             (from bias or not) is a huge asset. So there are a couple questions
             that my data answers that you may find helpful. The first is: If 
             you only get a judge from your country for one program, which 
             program should it be?
 
 # In order to create an appropriate advice section, I did do some math to 
 # convert the results of my model. My model predicted the influence of 
 # a judge and skater being from the same country in terms of a normalized TSS
 # score. This was done in order to account for the difference in scales between
 # the short and free programs in skating. However, for large competitions like
 # the olympics federations often need to jockey to get their judge on even 1 
 # panel. I wanted to figure out whether it is more valuable to have a judge
 # from your country in the short or free based on how much of an average boost
 # they could give to your skaters. To determine this, I did some algerba to turn
 # the norm TSS back into a mean TSS of the short and free programs individually. 
 # I did this by calculating the average standard deviations of scores across
 # short programs and free programs individually. Since the my model predicted 
 # that a judge would score a skater 1 standard deviation higher than the rest 
 # of the panel would, the average score increase for each program that a skater
 # might get from a judge of the same country is simply the average standard
 # deviation. Thus, you will see in the explanation below a free program judge
 # gives more of an advantage. 
 
 
            Our model determined that a judge of the same country as the skating
            team is likely to give a score that’s 1 standard deviation higher 
            than the average of the judging panel. So, to find out where to place
            these judges we look at how large a standard deviation in points is 
            in the short program versus the free program. The data shows that a 
            1 standard deviation in the short program is approximately 2.2898 
            points. However, a standard deviation in the free program is 
            approximately 3.6424 points. Therefore, if you want a larger overall
            competition boost, you get a greater advantage from having your 
            judge in the free program rather than the short program.")),

# Code that got me to this analysis:      
        # ShortNorm2 <- Compsheet %>%
        #     filter(str_detect(segment, "Ice Dance Short")) %>%       
        #     group_by(skater, segment) %>%
        #     summarize(meansd = mean(stddevTSS), .groups = "drop") %>%
        #     summarize(meansd2 = mean(meansd))
        # 
        # FreeNorm <- Compsheet %>%
        #     filter(str_detect(segment,"Ice Dance Free")) %>%
        #     group_by(skater, segment) %>%
        #     summarize(meansd = mean(stddevTSS), .groups = "drop") %>%
        #     summarize(meansd2 = mean(meansd))


#### PAGE 5: ABOUT ####

    tabPanel("About", 
             titlePanel("About"),
             HTML('<center><img src = "ISU1.jpg" height = 300 width = 300>
                  </center>'),
             h3("Project Background and Motivations"),
             p("This project is a project focused on understanding the presence 
             of international judging bias in competitive Ice Dance, a discipline
             of Figure Skating. Specifically for this project, I looked at the 
             2017-2018 Ice Dance season, which was an olympic season. The 
               purpose of this project is to investigate whether judges have 
               national biases that are elected in the scores and if the size 
               of this impact in competitive outcomes like scoring. "),
             p("I chose to do this project because of my love for niche sports,
             especially Olympic sports. There has been a long history of 
             speculation within figure skating of judges harboring biases 
             against other skaters, colluding, and trying to game the system in 
             order to have competitive dominance. Ice dance is a particularly 
             unique discipline since flaws in its technical elements are less 
             objectively successful or not since they are the only discipline 
             in figure skating that doesn’t have jumps, which are either landed
               or not. This less obviously objective metric may leave Ice Dance 
               to be a particularly vulnerable discipline to judging bias."),
             h3("My Data"),
             p("The data I used for this project was all retrieved from the 
             International Skating Union's competition score card records. They
             can be accessed through the ISU's website: 
             https://www.isu.org/figure-skating/entries-results/fsk-results 
             Additionally, to get the judging breakdowns I used the online 
             scores compilation site SkatingScores: https://skatingscores.com/ .
             Finally, I refrenced the list of all ISU judges and nations through
             the helpful work from Jessica Zhu's Senior Thesis, Figure Skating 
             Scores: Prediction and Assessing Bias. The link to which can be 
             found here: 
             https://dash.harvard.edu/bitstream/handle/1/39011778/ZHU-SENIORTHE
             SIS-2018.pdf?sequence=3&isAllowed=y . 
             After finding the data, I worked to manually enter it into an 
               spreadsheet with a row each specific judge, program, skater, and 
               competition in the 2017-2018 season of ice dance competitions, 
               concluding with the 2018 Olympic Games."),
             h3("About Me"),
             p("My name is Julia Blank. I'm a first year in Government 50 data 
             and Secretary of the Harvard Sports Analysis Collective. I intend 
               to study Government on the Data Science track. If you are curious
               about this project or my other work, you can reach me at 
               juliablank@college.harvard.edu."),
             p("To see the code for this project, check out my github repo here:
               https://github.com/Julia-Blank/IceDance2017-18App")))


server <- function(input, output) {

# This plot is meant to illustrate overal total competition scores for each team
# by adding mean TSS from both the short and free programs for each team. 
    
    output$competitionplot <- renderPlot({
        
        # What this does is take the first filter column and passes a vector 
        # through it that is either true or false for each entry in the vector.
        
        icedanceshort <- 
            CompsheetNAMES$firstfilter[CompsheetNAMES$inputnames == input$competitionid]
        icedancefree <- 
            CompsheetNAMES$secondfilter[CompsheetNAMES$inputnames == input$competitionid]
        print(icedanceshort)
        print(icedancefree)
        
        
            Compsheet %>%
                
# This filters out the competitions we want by the selected competition name in
# selected by the user input button. Icedanceshort and Icedancefree are changed
# when a user selects which competition they want to view. This allows the graph  
# to illustrate the  total TSS scores for each team in each competition. 
                
                filter(segment %in% c(icedanceshort, 
                                      icedancefree)) %>% 
                group_by(skater) %>%
                summarize(sum_TSS = (sum(meanTSS)/9), 
                          country = skater_country[1], 
                          .groups = "drop") %>%
                ggplot(aes(x = fct_reorder(skater, sum_TSS),
                           y = sum_TSS, 
                           fill = country)) +
                geom_bar(stat = "identity") + 
                coord_flip() +
                theme_classic() +
                labs(title = input$competitionid,
                     x = "Skating Pair", 
                     y = "Total Competition Score (sum of mean segment scores)")    
        })
    
# This graph is meant to illustrate the normalized TSS score each judge gives
# skaters within each competition. It uses the color aesthetic to label whether
# a skater was from the same country or different country. 
    
    output$judgescoresplot <- renderPlot({
        
# I used the pointsize variable so I could make the points where a judge is  
# judging a skater from their own country larger and thus more visible. 

        pointsize <- ifelse(Compsheet$same_country == TRUE, 3 , 1)
        Compsheet %>% 
            group_by(skater_country, segment) %>% 

# I used a normalized TSS score instead of just a mean TSS score since point 
# maximums are different between short and free programs in ice dance. Thus, 
# using the normTSS I can evaluate the overall impact of a judge being from the
# same country as a skater without concern for the different scales of different
# programs. To see the values in actual program TSS scores, go look at the notes
# in my advice tab of the ui.  
            
            ggplot(aes(x = judge_country, y = normTSS, color = same_country )) +
            geom_point(size = pointsize) +
            facet_wrap(~ segment, scales = "free_x") +
            labs(title = "Normalized Total Short Program Score of Skating Dance
                 Pairs by Judge Country",
                 x = "Judge by Country",
                 y = "Normalized Total Segment Score") +
            theme_bw() +
            scale_color_manual(name = "Are the Judge and Skater from the same
                               country?",
                               values = c("Red", "Blue"),
                               labels = c("No", "Yes"))
    }) 
   
 # This table is meant to show the results of my model including intercepts and
 # confidence intervals for each variable. My model only contains two variables
 # an intercept and the same_country variable which is whether or not a judge
 # and skater are from the same country. This allowed me to effectively analyze
 # strictly the impact of national bias on a judge's scoring of skaters. 
    
    output$samecountrytable <- render_gt({
        judgemodel <- stan_glm(normTSS ~ same_country,
                               data = Compsheet,
                               refresh = 0)
        tbl_regression(judgemodel, intercept = TRUE) %>% 
            as_gt() %>%
            tab_header(title = "Influence of Judge's Nationality on Ice Dance 
                       Pair's Score",
                       subtitle = "The Effect of Nationalism on Ice Dance 
                       Judging")
    })
    
# This graph is a graph of the posterior of my model. Shows the distribution of
# influence of a skater and judge being from  same country in terms of a 
# normalized TSS score.
    
    output$Posterior <- renderPlot({
        judgemodel <- stan_glm(normTSS ~ same_country,
                               data = Compsheet,
                               refresh = 0)
        
        newobs <- tibble(same_country = c(TRUE, FALSE))
        pp <- posterior_predict(judgemodel, newdata = newobs)  
        
        judgemodel %>%
            as_tibble() %>% 
            ggplot(aes(x = same_countryTRUE)) +
            geom_histogram(aes(y = after_stat(count/sum(count))),
                           bins = 100, fill = "seagreen") +
            labs(title = "Posterior Distribution of Influence of a Same-Country 
                 Judge on Score", 
                 y = "Probability",
                 x = "Influence (in normalized points) of Judge Being From Same
                 Country") +
            scale_y_continuous(labels = scales::percent_format())+
            theme_classic()
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
