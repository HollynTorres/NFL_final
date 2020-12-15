library(shiny) 
library(tidyverse)
library(shiny)
library(fec16)
library(readxl) 
library(shinythemes)
library(rstanarm)
library(ggplot2)
library(patchwork)
library(broom.mixed)
library(gt)


# All of the data below are the objects for the graphs I created. I copied 
# and pasted all of this data from an Rmd file I made. I also read in the data
# that I used down below. The first part is mainly reading in the data and the 
# second is the name of objects I created. 


# The skip 3 argument was useful to make the data chart fit appropriate once it 
# was read in.

ticket_price <- read_xlsx("Final Project data/statistic_id193595_average-ticket-price-in-the-nfl-by-team-2019.xlsx", sheet = "Data", skip = 3)
nfl_ticket <- ticket_price %>%
  slice(2:34)
colnames(nfl_ticket) <- c("NFL_team", "Average_ticket_price")
ticket_price_overtime <- read_csv("Final Project data/Ticket Prices OT 4.csv",
                                  skip = 1) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(names_to = "Year", values_to = "Price", 
               cols = `1985`:`2019`)  %>% 
  mutate(Price = gsub("[$]", "", Price)) %>% 
  mutate(Price = as.numeric(Price)) 

ten_year_record <- read_xlsx("Final Project data/10 year NFL record.xlsx")



NFL_data <- inner_join(ten_year_record, ticket_price_overtime, by = "Team")
price_2019 <- read_xlsx("Final Project data/statistic_id193595_average-ticket-price-in-the-nfl-by-team-2019.xlsx", sheet = "Data", skip = 3)
nfl_2019 <- price_2019 %>%
  slice(2:34)
colnames(nfl_2019) <- c("NFL_team", "Average_ticket_price")
Winning_percent <- ten_year_record %>% 
  mutate(win_percent = (Win/(Win + Loss + Tie))*100) 

Winning_percent$Team[Winning_percent$Team == "Pittsburg Steelers"] <- "Pittsburgh Steelers"

NFL_correlation <- inner_join(Winning_percent, nfl_2019, by = c("Team" = "NFL_team"))

stan_glm(data = NFL_correlation,
         formula = Average_ticket_price ~ win_percent,
         refresh = 0,
         family = gaussian()) 
NFL_data <- NFL_data %>% 
  mutate(region = case_when(Team %in% c("New England Patriots", "New York Jets", 
                                        "New York Giants","Philadelphia Eagles") ~ "Atlantic",
         Team %in% c("Baltimore Ravens", "Carolina Panthers", 
            "Tennessee Titans","Washington Football Team") ~ "Mid Atlantic",
         Team %in% c("Oakland Raiders", "Los Angeles Chargers", 
                     "San Francisco 49ers","Seattle Seahawks") ~ "Pacific",
         Team %in% c("Atlanta Falcons", "Jacksonville Jaguars", 
                     "Miami Dolphins","Tampa Bay Buccaneers") ~ "Southeast",
         Team %in% c("Buffalo Bills", "Cincinnati Bengals", 
                     "Cleveland Browns","Pittsburg Steelers") ~ "Erie",
         Team %in% c("Kansas City Chiefs", "Minnesota Vikings", 
                     "New Orleans Saints","Los Angeles Rams") ~ "Mississippi",
         Team %in% c("Arizona Cardinals", "Dallas Cowboys", 
                     "Denver Broncos","Houston Texans") ~ "Southwest",
         Team %in% c("Chicago Bears", "Detroit Lions", 
                     "Green Bay Packers","Indianapolis Colts") ~ "Central"))

# Below are all of the objects I created to name all of the regions for each
#NFL team. 

Atlantic <- NFL_data %>% 
  filter(Team %in% c("New England Patriots", "New York Jets", 
                     "New York Giants","Philadelphia Eagles")) %>% 
  summarise(Average_Atlantic = mean(Price))

Mid_Atlantic <- NFL_data %>% 
  filter(Team %in% c("Baltimore Ravens", "Carolina Panthers", 
                     "Tennessee Titans","Washington Football Team")) %>% 
  summarise(Mid_Atlantic_Average = mean(Price, na.rm = TRUE))

Southeast <- NFL_data %>% 
  filter(Team %in% c("Atlanta Falcons", "Jacksonville Jaguars", 
                     "Miami Dolphins","Tampa Bay Buccaneers")) %>% 
  summarise(Southeast_Average = mean(Price, na.rm = TRUE))

Erie <- NFL_data %>% 
  filter(Team %in% c("Buffalo Bills", "Cincinnati Bengals", 
                     "Cleveland Browns","Pittsburg Steelers"))  %>% 
  summarise(Erie_Average = mean(Price, na.rm = TRUE))

Pacific <- NFL_data %>% 
  filter(Team %in% c("Oakland Raiders", "Los Angeles Chargers", 
                     "San Francisco 49ers","Seattle Seahawks"))  %>% 
  summarise(Pacific_Average = mean(Price, na.rm = TRUE))

Mississippi <- NFL_data %>% 
  filter(Team %in% c("Kansas City Chiefs", "Minnesota Vikings", 
                     "New Orleans Saints","Los Angeles Rams")) %>% 
  summarise(Mississippi_Average = mean(Price, na.rm = TRUE))

Southwest <- NFL_data %>% 
  filter(Team %in% c("Arizona Cardinals", "Dallas Cowboys", 
                     "Denver Broncos","Houston Texans")) %>% 
  summarise(Southwest_Average = mean(Price, na.rm = TRUE))

Central <- NFL_data %>% 
  filter(Team %in% c("Chicago Bears", "Detroit Lions", 
                     "Green Bay Packers","Indianapolis Colts")) %>% 
  summarise(Central_Average = mean(Price, na.rm = TRUE))

# Below is the code I used to create each "p" object, when then when combined
# created the entire average price representation for each region. 

p1 <- Atlantic %>% 
  ggplot(aes(y = Average_Atlantic)) + 
  geom_bar(fill = "blue") +
  labs(y = "Average Price",
       x = "Atlantic Region") +
  theme(axis.text.x = element_blank())


p2 <- Mid_Atlantic %>% 
  ggplot(aes(y = Mid_Atlantic_Average)) +
  geom_bar(fill = "maroon") +
  labs(y = "Average Price",
       x = "Mid Atlantic Region") +
  theme(axis.text.x = element_blank())

p3 <- Southeast %>% 
  ggplot(aes(y = Southeast_Average)) +
  geom_bar(fill = "pink") +
  labs(y = "Average Price",
       x = "Southeast Region")  +
  theme(axis.text.x = element_blank())

p4 <- Erie %>% 
  ggplot(aes(y = Erie_Average)) +
  geom_bar(fill = "purple") + 
  labs(y = "Average Price",
       x = "Erie Region")  +
  theme(axis.text.x = element_blank())

p5 <- Pacific %>% 
  ggplot(aes(y = Pacific_Average)) +
  geom_bar(fill = "lightblue") + 
  labs(y = "Average Price",
       x = "Pacific Region")  +
  theme(axis.text.x = element_blank())

p6 <- Mississippi %>% 
  ggplot(aes(y = Mississippi_Average)) +
  geom_bar(fill = "light green") + 
  labs(y = "Average Price",
       x = "Mississippi Region")  +
  theme(axis.text.x = element_blank())

p7 <- Southwest %>% 
  ggplot(aes(y = Southwest_Average)) +
  geom_bar(fill = "yellow") + 
  labs(y = "Average Price",
       x = "Southwest Region")  +
  theme(axis.text.x = element_blank())

p8 <- Central %>% 
  ggplot(aes(y = Central_Average)) +
  geom_bar(fill = "orange") + 
  labs(y = "Average Price",
       x = "Central Region")  +
  theme(axis.text.x = element_blank())


reg <- stan_glm(data = NFL_correlation,
                formula = Average_ticket_price ~ win_percent,
                refresh = 0,
                family = gaussian()) %>% 
  tidy()

reg_table <- reg %>% 
  gt()

reg_table
######################################################################################
######################################################################################
#
# 1. Shiny Apps have two basic parts to them
#
#   - The user interface (UI) defines how the app should look.
#
#     -- For example, the text on the page, the placement of the elements, etc.
#
#   - The server defines how the app should behave.
#
#     -- This is how live elements are updated - like selecting a state from a list.
#
#   - Those two pieces are combined by running shinyApp(ui, server) to create the app.
#
#      -- You can also click the green "Run App" button on the top right or
#         run runApp() in the console
ui <- navbarPage(
  "NFL Data",
  
 # Using the theme =  function I was able to use a theme throughout my project
 # to make it look more professional and clean. 
  
  theme = shinytheme("cosmo"),
  tabPanel(
    "Main", 
    fluidPage(
      
# The sidebar panel function was able to create a nice looking side bar box
# for me to put some introductionary comments in. 
      
      
    sidebarPanel(
      h3("Welcome to my final project"),
      p("This project takes data from the past and recent NFL seasons and 
      explores the relationships between average ticket price and success and location."),
      tags$img(src = "https://upload.wikimedia.org/wikipedia/en/thumb/a/a2/National_Football_League_logo.svg/1024px-National_Football_League_logo.svg.png",
               height = 200, width = 200)
    )),
    # And here is your "main panel" for the page.
    mainPanel(
      # - You can also make your UI more complicated with UI elements.
      #
      #   -- In general, these are defined by functions that you give arguments to
      #      (e.g. min and max values).
      #
      # - These include:
      #
      #   -- selectInput() to choose from multiple options.
      #
      #   -- sliderInput() lets you choose a value from a slider of values you define.
      #
      #   -- radioButtons() let you choose a button from a number of options
      #
      #   -- textInput() lets you enter whatever text you want.
      #
      #   -- Lots of other options, like entering a date. Look at the resources for
      #      other choices!
      #
      # - You then assign these inputs to a value and use those values in other places,
      #   like in plots!
      #
      # - All of these functions have their own arguments. For example:
   
      textInput(
        inputId = "entered_text",               # a name for the value you choose here
        label = "NFL Data",  # a label above the text box
        value = "NFL Average Ticket Price Overtime"                 # an initial value for the box
      ),
      plotOutput("average_ticket_overtime")
      
    )
    
  
  ),

# Every time I want to start a new tab I use the tabPanel function like below.

  tabPanel("About",
           mainPanel(
           h3("About me"),
           p("My name is Hollyn and I am a rising sophomore in Winthrop house.
             I play on the Varsity Women's Soccer team at Harvard, which has inspired
             my love and interest for all sports. Growing up in Texas, I always enjoyed watching
             football with my family and friends. My love for sports has inspired the 
             creation of this project as I will  be looking further into NFL data.
             I hope you enjoy the data and learn something new about the NFL! 
             Thanks for checking out my project and feel free to say hi at 
             hollyn_torres@harvard.college.edu and checkout my github whenever at ",
             
# I was able  to use the a and href function to hyperlink the links to the 
# sources that I wanted mt audience to be able to go to from my project. 
                         
             a("this link.",
               href = "https://github.com/HollynTorres")),
            h3("Background"),
           p("My project displays the average ticket price
             in recent and past years of NFL tickets. NFL admission ticket prices vary 
             by team and I will be investigating potential influential facotrs on why 
             this may be. Specfically, I will be looking into success-amount of wins/losses for the past
             10 years-and regional location, as factors that may influence the ticket price for 
             each team."),
             h3("Data"),
           p("The data I used for the 2019 average ticket price can be found at",
             a("this link.",
               href = "https://www.statista.com/statistics/193595/average-ticket-price-in-the-nfl-by-team/")),
           p("The data I used for the average ticket price for each NFL team overtime can be found at",
             a("this link.",
               href = " http://bl.ocks.org/CafeConVega/raw/ed643eefaee0879d3947/")),
           p("The data I used for the records for each NFL team, for the past 10 years, can be found at",
             a("this link.",
               href = "https://www.foxsports.com/nfl/gallery/every-nfl-teams-10-year-record-ranked-32-1-010417")),
           tags$img(src = "https://images-na.ssl-images-amazon.com/images/I/51IyYCZuqvL._AC_SX425_.jpg",
                    height = 200, width = 500),

           
  )),

# I created two seperate tabs for my success and regional data to keep it 
# organized and seperated from eachother. 

  tabPanel("Success Data",
           mainPanel(
             h2("NFL Data on Success"),
             p("This data illustrates the success of each NFL team over the 
               past 10 years. There is the total win amount and loss amount for 
               each NFL team. The graph is arranged in most to least order.
               The graph demonstrates the teams with the most and least success.
               If you look back to the graph on the About tab, you can see that 
               the success graph is not an exact replication of ticket price. This 
               is what inspired looking into other factors, such as location."),
         
           plotOutput("ten_year_record"),
           plotOutput("ten_year_record_loss")),
           
           
  ),   
  
  tabPanel("Regional Location Data",
           mainPanel(
             h2("NFL Data on Regional Location"),
             p("This data illustrated the average ticket price for each NFL
               team, with consideration of regional location. The NFL teams 
               were divided into eight groups, four teams in each group,
               based on their location. The graphs are displaying each of 
               these regional locations and the average ticket of each 
               team within that region. As you can see there does seem to be more
               trends among the regions, leading me to believe there may be a 
               correlation between the two."),
             
             selectInput(
               inputId = "SelectRegion", label = "Please Choose a Region",
               choices = unique(NFL_data$region)
  ),
            plotOutput("regionplot"),
            p("The graph below is illustrating the average NFL ticket price by
              region. Similarlly to the graph above excpet this graph is the 
              average NFL ticket price for each region as a whole. This allows
              a more clear comparision amongst each region ticket prices. As
              you can see, there are trends that seem to follow amongst certain 
              locations, this leads to me to beleive there may be a correlation between 
              location and average ticket price. The averages range anywhere from 
              64 dollars to 51 dollars. Location could possible account for few outliers
              in the success and average ticket price correlation model."),
            
# I figured out that the layout of the UI is the layout of the app, 
# so by inserting text between two graphs, the text the appeared on my app 
# between the graphs. 
  
  
   plotOutput("p1"),
         
          
           )       
           
  ),   
  
  tabPanel("Model",
           mainPanel(
           h2("NFL Ticket Price and Success Model"),
           p("This model illustrates the correlation between average ticket
             price and win percentage, a function of success. As you can see from
             the graph there is a positive correlation between average ticket price and 
             success. The graph is showing that the more successful winning percentage,
             the ticket price will likely be more expensive. The win percentage was 
             gathered from the past 10 year record for each team and the ticket 
             price was an average overtime.")
           ),
           textInput(
             inputId = "entered_text",               
             label = "",  
             value = "Correlation Model"                 
           ),
           plotOutput("NFL_correlation"),
           tableOutput("reg"),
           p("The table output intercept is showing that on average, not considering winning percentage 
           or without winning any games, the average ticket price in the NFL is about 64 dollars.
           The win_percent estimate is predicting that on average with every one increase in winning percent
            the ticket price will increase by about .75 dollars. This correlation can be used 
            as insight for fans or football lovers when questioning the ticket price of their 
             favorite NFL team. Although success is not the sole factor, it is a factor.")
  )   

)

# The server is where I put all of my code to create the graphs. I was then able
# to call the graphs above to make them appear on the app. I code from below is 
# copied from my .Rmd where I originally made all of my graphs. 

server <- function(input, output, session) {
  # - Then, you use these named objects to update the data on your site via the input object.
  #
  #   -- render() functions are what show content that will change live on your site.
  #
  #   -- so here, renderText() is updating live text based on your choice.
  # Just like renderText(), we can renderPlot()!
  output$state_plot <- renderPlot({
    # we need to use () here after the name of our dataset because it is reactive!
    nfl_ticket %>%
      # notice we are using the selected_state variable defined above!
      # this plot is just like normal!
      ggplot(aes(x = NFL_team, y = Average_ticket_price)) +
      geom_col(
        fill = input$selected_color) +
      labs(title = input$entered_text) +
      theme_bw() +
      coord_flip() +
      labs(x = "Ticket price(dollars)", y = "NFL Team")
  })
  output$nfl_2019 <- renderPlot({
  nfl_2019 %>% 
    filter(NFL_team != "NFL Average")  %>% 
    ggplot(aes(x = NFL_team, y = Average_ticket_price,
               fill = NFL_team)) +
    geom_col() + 
    labs(title = "Ticket Prices for each NFL Team in 2019",
         x = "Price",
         y = "Team") +
    theme_bw() +
    coord_flip() +
    labs(x = "NFL Team", y = "Ticket Price") +
    scale_fill_manual(values = c("#97233F", "#A71930", "#241773", "#00338D",
                                 "#0085CA", "#0B162A", "#FB4F14", "#311D00", 
                                 "#003594", "#FB4F14", "#0076B6", "#203731", 
                                 "#A71930", "#002C5F", "#9F792C", "#E31837", 
                                 "#0080C6", "#FFD100", "#008E97", "#4F2683",
                                 "#002A5E","#101820", "#0B2265", "#125740", 
                                 "#101820", "#004C54", "#FFB612", "#AA0000",
                                 "#69BE28","#D50A0A", "#4B92DB", "#773141")) 
  
  })
  output$ten_year_record <- renderPlot({
  ten_year_record %>% 
    ggplot(aes(x = Win, y = fct_reorder(Team, (Win)), fill = Team)) +
    geom_col() +
    labs(title = "Total Amount of Wins for each NFL Team in the Past 10 years",
         x = "Win Amount",
         y = "NFL Team") +
    scale_fill_manual(values = c("#97233F", "#A71930", "#241773", "#00338D",
                                 "#0085CA", "#0B162A", "#FB4F14", "#311D00", 
                                 "#003594", "#FB4F14", "#0076B6", "#203731", 
                                 "#A71930", "#002C5F", "#9F792C", "#E31837", 
                                 "#0080C6", "#FFD100", "#008E97", "#4F2683",
                                 "#002A5E","#101820", "#0B2265", "#125740", 
                                 "#101820", "#004C54", "#FFB612", "#AA0000",
                                 "#69BE28","#D50A0A", "#4B92DB", "#773141")) +
    theme_bw()
  })
  
  output$ten_year_record_loss <- renderPlot({ 
  ten_year_record %>% 
    ggplot(aes(x = Loss, y = fct_reorder(Team, (Loss)), fill = Team)) +
    geom_col() +
    labs(title = "Total Amount of Losses for each NFL Team in the Past 10 Years",
         x = "Loss Total",
         y = "NFL Team") +
    scale_fill_manual(values = c("#97233F", "#A71930", "#241773", "#00338D",
                                 "#0085CA", "#0B162A", "#FB4F14", "#311D00", 
                                 "#003594", "#FB4F14", "#0076B6", "#203731", 
                                 "#A71930", "#002C5F", "#9F792C", "#E31837", 
                                 "#0080C6", "#FFD100", "#008E97", "#4F2683",
                                 "#002A5E","#101820", "#0B2265", "#125740", 
                                 "#101820", "#004C54", "#FFB612", "#AA0000",
                                 "#69BE28","#D50A0A", "#4B92DB", "#773141")) +
    theme_bw()
  })
  
  output$ten_year_record_tie <- renderPlot({ 
  ten_year_record %>% 
    select(Tie, Team) %>% 
    filter(Tie == 1:3) %>% 
    ggplot(aes(x = Tie, y = fct_reorder(Team, (Tie)), fill = Team)) +
    geom_col() +
    
    labs(title = "Total amount of Ties for each NFL Team in the Past 10 Years",
         x = "Tie Total",
         y = "NFL Team") +
    scale_fill_manual(values = c("#97233F","#FB4F14", "#203731", "#AA0000",
                                 "#69BE28")) +
    
    
    theme_bw()
    
  })
  
  output$average_ticket_overtime <- renderPlot({ 
  NFL_data %>% 
    group_by(Team) %>% 
    summarise(Prices = mean(Price, na.rm = TRUE), .groups = "drop") %>% 
    ggplot(aes(x = Prices, y = fct_reorder(Team, (Prices)), fill = Team)) +
    geom_col() +
    labs(title = "NFL Average Ticket Price Overtime",
         x = "Prices", y = "NFL Team") +
    scale_fill_manual(values = c("#97233F", "#A71930", "#241773", "#00338D",
                                 "#0085CA", "#0B162A", "#FB4F14", "#311D00", 
                                 "#003594", "#FB4F14", "#0076B6", "#203731", 
                                 "#A71930", "#002C5F", "#9F792C", "#E31837", 
                                 "#0080C6", "#FFD100", "#008E97", "#4F2683",
                                 "#002A5E","#101820", "#0B2265", "#125740", 
                                 "#101820", "#004C54", "#FFB612", "#AA0000",
                                 "#69BE28","#D50A0A", "#4B92DB", "#773141")) +
    theme_bw() 
  })
  
  output$Atlantic_Region <- renderPlot({ 
  
  Atlantic_Region <- NFL_data %>% 
    filter(Team %in% c("New England Patriots", "New York Jets", 
                       "New York Giants","Philadelphia Eagles")) %>% 
    group_by(Team) %>% 
    summarise(Price = mean(Price), .groups = "drop") %>% 
    ggplot(aes(x = Team, y = Price, fill = Team)) +
    geom_col() +
    labs(title = "NFL Atlantic Region Average Ticket Prices",
         x = "NFL Team", 
         y = "Price") +
    theme(axis.text.x = element_text(angle = 20)) +
    scale_fill_manual(values = c("#002244", "#0B2265", "#125740",
                                 "#004C54")) 
  })
  
  output$Mid_Atlantic_Region <- renderPlot({ 
  
 NFL_data %>% 
    filter(Team %in% c("Baltimore Ravens", "Carolina Panthers", 
                       "Tennessee Titans","Washington Football Team")) %>% 
    group_by(Team) %>% 
    summarise(Price = mean(Price, na.rm = TRUE), .groups = "drop") %>% 
    ggplot(aes(x = Team, y = Price, fill = Team)) +
    geom_col() +
    labs(title = "NFL Mid Atlantic Region Average Ticket Prices",
         x = "NFL Team", 
         y = "Price") +
    theme(axis.text.x = element_text(angle = 20)) +
    scale_fill_manual(values = c("#241773", "#0085CA", "#0C2340",
                                 "#773141")) 
  
  })
  
  output$Southeast_Region <- renderPlot({ 
  
    Southeast_Region <- NFL_data %>% 
    filter(Team %in% c("Atlanta Falcons", "Jacksonville Jaguars", 
                       "Miami Dolphins","Tampa Bay Buccaneers")) %>% 
    group_by(Team) %>% 
    summarise(Price = mean(Price, na.rm = TRUE), .groups = "drop") %>% 
    ggplot(aes(x = Team, y = Price, fill = Team)) +
    geom_col() +
    labs(title = "NFL Southeast Region Average Ticket Prices",
         x = "NFL Team", 
         y = "Price") +
    theme(axis.text.x = element_text(angle = 20)) +
    scale_fill_manual(values = c("#A71930", "#9F792C", "#008E97",
                                 "#D50A0A")) 
  })
 
  output$Erie_Region <- renderPlot({ 
    Erie_Region <- NFL_data %>% 
      filter(Team %in% c("Buffalo Bills", "Cincinnati Bengals", 
                         "Cleveland Browns","Pittsburg Steelers")) %>% 
      group_by(Team) %>% 
      summarise(Price = mean(Price, na.rm = TRUE), .groups = "drop") %>% 
      ggplot(aes(x = Team, y = Price, fill = Team)) +
      geom_col() +
      labs(title = "NFL Erie Region Average Ticket Prices",
           x = "NFL Team", 
           y = "Price") +
      theme(axis.text.x = element_text(angle = 20)) +
      scale_fill_manual(values = c("#00338D", "#FB4F14", "#311D00",
                                   "#FFB612")) 
  
  })
  
  output$Central_Region <- renderPlot({ 
  
   Central_Region <- NFL_data %>% 
    filter(Team %in% c("Chicago Bears", "Detroit Lions", 
                       "Green Bay Packers","Indianapolis Colts")) %>% 
    group_by(Team) %>% 
    summarise(Price = mean(Price, na.rm = TRUE), .groups = "drop") %>% 
    ggplot(aes(x = Team, y = Price, fill = Team)) +
    geom_col() +
    labs(title = "NFL Central Region Average Ticket Prices",
         x = "NFL Team", 
         y = "Price") +
    theme(axis.text.x = element_text(angle = 20)) +
    scale_fill_manual(values = c("#0B162A", "#0076B6", "#203731",
                                 "#002C5F")) 
  })
  
  output$Southwest_Region <- renderPlot({ 
  
  Southwest_Region <- NFL_data %>% 
    filter(Team %in% c("Arizona Cardinals", "Dallas Cowboys", 
                       "Denver Broncos","Houston Texans")) %>% 
    group_by(Team) %>% 
    summarise(Price = mean(Price, na.rm = TRUE), .groups = "drop") %>% 
    ggplot(aes(x = Team, y = Price, fill = Team)) +
    geom_col() +
    labs(title = "NFL Southwest Region Average Ticket Prices",
         x = "NFL Team", 
         y = "Price") +
    theme(axis.text.x = element_text(angle = 20)) +
    scale_fill_manual(values = c("#97233F", "#003594", "#FB4F14",
                                 "#A71930")) 
  })
  
  output$Mississippi_Region <- renderPlot({ 
  
   Mississippi_Region <- NFL_data %>% 
    filter(Team %in% c("Kansas City Chiefs", "Minnesota Vikings", 
                       "New Orleans Saints","Los Angeles Rams")) %>% 
    group_by(Team) %>% 
    summarise(Price = mean(Price, na.rm = TRUE), .groups = "drop") %>% 
    ggplot(aes(x = Team, y = Price, fill = Team)) +
    geom_col() +
    labs(title = "NFL Southwest Region Average Ticket Prices",
         x = "NFL Team", 
         y = "Price") +
    theme(axis.text.x = element_text(angle = 20)) +
    scale_fill_manual(values = c("#E31837", "#FFA300", "#4F2683",
                                 "#D3BC8D")) 
  })
  
  output$Pacific_Region <- renderPlot({ 
  
  Pacific_Division <- NFL_data %>% 
    filter(Team %in% c("Oakland Raiders", "Los Angeles Chargers", 
                       "San Francisco 49ers","Seattle Seahawks")) %>% 
    group_by(Team) %>% 
    summarise(Price = mean(Price, na.rm = TRUE), .groups = "drop") %>% 
    ggplot(aes(x = Team, y = Price, fill = Team)) +
    geom_col() +
    labs(title = "NFL Southwest Region Average Ticket Prices",
         x = "NFL Team", 
         y = "Price") +
    theme(axis.text.x = element_text(angle = 20)) +
    scale_fill_manual(values = c("#0080C6", "#000000", "#AA0000",
                                 "#69BE28"))
  
  })
  
 output$NFL_correlation <- renderPlot({
   
  NFL_correlation %>% 
    ggplot(aes(x = win_percent, y = Average_ticket_price)) +
    geom_point() +
    geom_smooth(method = "lm") +
    labs(title = "Correlation between Win Percentage and Average Ticket Price",
         x = "Win percent",
         y = "Average Ticket Price")
 }) 
   
output$p1 <- renderPlot ({ 
  
  p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8
  
  })

output$reg <- renderTable ({ 

reg <- stan_glm(data = NFL_correlation,
                formula = Average_ticket_price ~ win_percent,
                refresh = 0,
                family = gaussian()) %>% 
  tidy()

reg_table <- reg %>% 
  gt()

reg_table

})

output$regionplot <- renderPlot ({ 
  
 NFL_data %>% 
filter(region == input$SelectRegion) %>% 
    group_by(Team) %>% 
    summarise(Price = mean(Price, na.rm = TRUE), .groups = "drop") %>% 
    ggplot(aes(x = Team, y = Price, fill = Team)) +
    geom_col() +
    labs(title = "NFL Southeast Region Average Ticket Prices",
         x = "NFL Team", 
         y = "Price") +
    theme(axis.text.x = element_text(angle = 20)) +
    theme_bw()
  
  
  })



}


shinyApp(ui, server)