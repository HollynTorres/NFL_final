library(shiny) # you may need to install.packages() this
library(tidyverse)

library(shiny)
library(fec16)
library(readxl)
library(shinythemes)
# This is just a normal object

state.names <- c("CA", "NY", "KS")


# Make change to your dataset
results_house <- results_house %>%
  select(-footnotes)

ticket_price <- read_xlsx("Final Project data/statistic_id193595_average-ticket-price-in-the-nfl-by-team-2019.xlsx", sheet = "Data", skip = 3)





nfl_ticket <- ticket_price %>% 
  slice(2:34) 

colnames(nfl_ticket) <- c("NFL_team", "Average_ticket_price")
  

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

ui <- fluidPage(navbarPage(
  "Shiny Example",
  theme = shinytheme("cosmo"),
  
  tabPanel(
    "Main",
    
    # - UIs are built from "panel" functions, which specify areas of your page.
    #
    #   -- There is a "main panel," a "sidebar," a "title," etc.
    
    # Here is a sidebar!
    
    sidebarPanel(
     h3("Welcome to my final project"),
     p("This project takes data from the 2019 NFL Season and shows the 
       average ticket prices for each NFL team."),
     tags$img(src = "https://upload.wikimedia.org/wikipedia/en/thumb/a/a2/National_Football_League_logo.svg/1024px-National_Football_League_logo.svg.png", 
              height = 200, width = 200) 
   
      ),
    
    
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
      
      radioButtons(
        inputId = "selected_color",             # a name for the value you choose here
        label = "Choose a color!",              # the label to display above the buttons
        choices = c("red", "blue", "green")     # the button values to choose from
      ),
      
      textInput(
        inputId = "entered_text",               # a name for the value you choose here
        label = "Place your title text here:",  # a label above the text box
        value = "Average Price of NFL 2019 tickets"                 # an initial value for the box
      ),
      
    
      plotOutput("state_plot")
    )
  ),
  tabPanel("About",
             h3("Hi!"),
             p("My name is Hollyn and I am a rising sophomore in Winthrop house. 
             I play on the Varsity Women's Soccer team at Harvard, which has inspired 
             my love and interest for all sports. Growing up in Texas, I always enjoyed watching 
             football with my family and friends. My project shows the average ticket price 
             in 2019 to be admitted to a specific teams NFL game. It is super interesting to 
             see the team in relation to price and the location of the team in relation as well.
             I hope you enjoy the data and learn something new about the NFL!
            "))
  
  )
)

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
      labs(x = "Ticket price(dollars", y = "NFL Team")
    
  
  })
  
}

shinyApp(ui, server)