#
# This is the user-interface definition of a ShinyRio web application. You can
# run the application by clicking 'Run App' in Rstudio
#
library(shiny)
library(DT)
library(plotly)
library(crosstalk)
library(ggplot2)
library(stats)
library(dplyr)
library(leaflet)

shinyUI(fluidPage(
  
  # Application title
  #h4("W. Mathie, 25th March 20"),
  sidebarLayout(
    sidebarPanel(
      HTML('<center><img src="2016_1.png"></center>'),
      br(),
      helpText("BMI = Body Mass Index"),
      helpText("Height [m]"),
      helpText("Weight [kg]"),
      helpText("Age at Rio [years]"),
      selectInput("selected_data",
                  label = "Select an attribute for Histogram",
                  choices = c("BMI", "Height", "Weight", "Age"),
                  selected = "BMI"),
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      # Add a new checkboxInput to your app
      checkboxInput("addmean",
                    label = "Show mean line?",
                    value = TRUE),
      
      
      HTML('<center><img src="rio.png"></center>'),
      br(),
      br(),
      p("The data and images were sourced from the official ",
        a("Rio 2016 website.", 
          href = "https://www.rio2016.com/")),
      br(),
      width=3),
    
    # Show a plot of the generated distribution
    mainPanel(
      titlePanel(h1("Triathletes at the 2016 Rio Olympic Games")),
      tabsetPanel(type = "tabs",
                  tabPanel("About", 
                           br(),
                           HTML('<center><img src="logo-90.png"></center>'),
                           br(),
                           p("This is my first Shiny app, and I am planning to include other sports and make it more interactive as I get more time to do 
                             so. Hopefully you enjoyed it!"),
                           br(),
                           h3("Usage:"),
                           p("- Use the side panel to control the histogram displayed, and click on the tabs to explore some stats."),
                           p("- Use the Data Table tab to see, search and sort data. You can also filter data by selecting rows with your moust. You can 
                             download the selected rows to a csv file
                             by clicking on the button at the bottom of the data table."),
                           p("- In the Extras tab you can calculate your BMI (Body Mass Index) and 
                             compare it to Olympic triathletes."), 
                           h3("Data:"),
                           p("The data and images were sourced from the official ",
                             a("Rio 2016 website.", href = "https://www.rio2016.com/")),
                           p("Currently I am only displaying the data of 110 triathletes with known height and weight. BMI data in the Extras tab is based on ",
                           a("Wikipedia facts.", href = "https://en.wikipedia.org/wiki/Body_mass_index")),
                           br(),
                           HTML('<center><img src="Gwen.png"></center>')
                           ),
                  tabPanel("Histogram", plotOutput("histoPlot"),
                           helpText("Use the side panel to select your favourite attribute from the drop-down list and change the number of bins. You can display the
                                    mean of the attribute by checking the Show mean line box.")),  
                  
                  tabPanel("Data Table",
                           #data table and scatter graph with selection
                           br(),
                           p("The Olympic triathlon: 1500m swim, 40km bike (drafting allowed), 10km run. What does it take to get to the Olympics? Here is 
                             the data! Also see the Extras tab to compare."),
                          br(),
                          plotlyOutput("x2"),
                          helpText("Select a row with your mouse. Selected athlete is shown as a green data point."),
                           DT::DTOutput("x1"),
                           fluidRow(
                             p(class = 'text-center', downloadButton('x3', 'Download Selected Data'))
                           )),
                  tabPanel("Countries", 
                           br(),
                           helpText("Hover over the Olympic Ring! Scroll down the legend to see all countries."),
                           br(),
                           plotlyOutput("piechart"),
                           h3("Pick N random countries!"),
                           numericInput("obs", "Enter number:", 10, min = 1, max = 42),
                           leafletOutput("trimap")
                           
                           ),
                  tabPanel("Extras",
                           br(),
                           p("How do you compare to an Olympic triathlete?"),
                           numericInput("yourmass", label = em("Your weight:"),
                                        value = 65
                           ),
                           numericInput("yourheight", label = em("Your height:"),
                                        value = 1.75,
                                        step  = 0.10
                           ),
                           uiOutput("YourInput"),
                           br(),
                           actionButton("Calculate", "Calculate and Compare"),
                           br(),
                           br(),
                           uiOutput("BMIresult"),
                           br(),
                           plotlyOutput("BMIplot")
                           )
      )
      
    )
  )
))


