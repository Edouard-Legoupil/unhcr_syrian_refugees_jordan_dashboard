library(shinydashboard)
library(shiny)
library(jsonlite)
library(dplyr)
library(dygraphs)
library(ggvis)
library(choroplethrAdmin1)
library(choroplethr)
library(curl)



header <- dashboardHeader(title = "UNHCR - JORDAN")
sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Jordan - Country Level", tabName = "jordan", icon = icon("dashboard")),
  menuItem("Jordan - Governorates Level", tabName = "governorates", icon = icon("dashboard")),
  menuItem("Disclaimer", tabName = "disclaimer", icon = icon("legal")),
  menuItem("About", tabName = "about", icon = icon("question"))
))

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "jordan",
            h5("Welcome! This dashboard is connected to the", a("UNHCR data API.", href="http://data.unhcr.org/", target="_blank"),
               "It specifically focuses on data related to Syrian refugees in Jordan.
               The dashboard handles the data at two levels: the entire Kingdom, and governorates levels.
               You can access either by selecting the related tap to the left.
               The dashboard is built with an intuitive interface. Kindly note
               that it might take a few minutes for the data to load and process. If it takes
               too long, then the API is probably down and will
               be back shortly (click on the UNHCR hyperlink
                                above to check). If a figure or table does not
               render properly, kindly refresh the page."),
            fluidRow(
              tabBox(
                title = NULL,
                id = "tabset1", height = "1500px", width = "600px",
                tabPanel(h4("Country Map"), 
                         h4("Distribution of Registered Syrian Refugees per Governorate"),
                         ("Hover your pointer over the map to display exact numbers."),
                         ggvisOutput("all_of_jordan_map")),
                tabPanel(h4("Demographics"), 
                         h4("Age Group Demographics of Registered 
                                 Syrian Refugees (Broken Down by Gender)"),
                         h4(textOutput("date_demo_total")),
                         ("Hover your pointer over the bars to display
                               exact numbers."),
                         htmlOutput("demo_jordan")),
                tabPanel(h4("Refugees Influx"),
                         h4("A Time Line Showing the Total Number of Registered
                            Refugees per Month, and their Cumulative Number
                            up until that Month"),
                         h4(textOutput("date_time_srs")),
                         ("Hover your pointer over the lines to display
                          cumulative and recent registered Syrian refugees for a given month.
                          Please note that the total number of registered refugees
                          per month can be negative. This is either a data entry
                          error, or reflects a net of returning refugees
                          to their home country. Note also the date range
                          selector at the bottom of the graph, which you can
                          use to narrow your range of search. Also, please note that
                          gaps in the time series below indicate a missing value
                          from the original data source"),
                         dygraphOutput("time_series")),
                tabPanel(h4("Documents Published"),
                         h4("List of Documents Published"),
                         ("A table summarizing some of the documents published
                          on Syrian refugees in Jordan. Note that you can navigate
                          the table using more than one option (scroll down to
                          the bottom of the page). Kindly note that it is
                          inadvisable to list more than 10 rows at a time."),
                         dataTableOutput("docs_jor"))
                ))),
            
    
    tabItem(tabName = "governorates",
            h5("Welcome! This dashboard is connected to the", a("UNHCR data API.", href="http://data.unhcr.org/", target="_blank"),
               "It specifically focuses on data related to Syrian refugees in Jordan.
               The dashboard handles the data at two levels: the entire Kingdom, and governorates levels.
               You can access either by selecting the related tap to the left.
               The dashboard is built with an intuitive interface. Kindly note
               that it might take a few minutes for the data to load and process. If it takes
               too long, then the API is probably down and will
              be back shortly (click on the UNHCR hyperlink
               above to check). If a figure or table does not
               render properly, kindly refresh the page."),
            fluidRow(
              box(title = "Kindly Select Governorate of Interest", 
                  width = 4,selectInput("gov", label =NULL,
                          choices = list("Ajlun" = 1, "Amman" = 2, "Aqaba" = 3,
                                         "Balqa" = 4, "Irbid" = 5, "Jarash" = 6,
                                         "Karak" = 7, "Maan" = 8, "Madaba" = 9,
                                         "Mafraq" = 10, "Tafilah" = 11,
                                         "Zarqa" = 12), 
                          selected = 1)),
              
              box(title = NULL,
                  width = 8,
                  h5("Tip: this application is reactive; meaning that the output
                     will change shortly after you select another governorate.
                     If you are not sure if the output matches your selection, just have
                     a look at the first line right beneath the tabs below,
                     which should display the name of the governorate the data relates to."))),
            fluidRow(tabBox(title = NULL,
                            id = "tabset2", height = "1000px", width = "600px",
                            tabPanel(h4("Demographics"),
                                     h2(textOutput("gov_name_selected")),
                                     h4("Age Group Demographics of Registered 
                                 Syrian Refugees (Broken Down by Gender)"),
                                     h4(textOutput("update_gov")),
                                     ("Hover your pointer over the bars to display
                               exact numbers."),
                                     htmlOutput("demo_per_gov")),
                            tabPanel(h4("Who is Doing What"),
                                     h2(textOutput("gov_name_selected_2")),
                                     h4("Table of Local, Regional, and International
                               Entities Operating to Serve the Refugees in the Governorate"),
                                     h5("The table shows the sector, webpage link (if available), 
                               and acrynom of each entity. Kindly note that it is
                          inadvisable to list more than 10 rows at a time."),
                                     dataTableOutput("who_what"))))),
    
    tabItem(tabName = "disclaimer",
            fluidRow(
              tabBox(title = NULL,
                     id = "tabset3", height = "150px", width = "600px",
                h3("DISCLAIMER"),
                h4("Kindly note that the data presented in this dashboard is the
                property of the UNHCR alone, and is subject to their usage policy,
                and any other policies and/or laws related to them. The dashboard is merely meant
                as a vehicle to convey the data as taken from the source, with modifications to its form
                and representation, but not content. Therefore,
                we are not liable to damage or harm caused by errors (if any) found in this data."
                   )))),
    
    tabItem(tabName = "about",
            fluidRow(
              tabBox(title = NULL,
                     id = "tabset4", height = "150px", width = "600px",
                     h3("ABOUT"),
                      h4("This dashboard was created by", a("Delphi Data Analytics", href="http://ddata.me/", target="_blank"),
                         "for demonstrative purposes. However, it is released
                         under a GNU General Public License for modification,
                         development, and expansion, under the condition that it
                         remains GNU GPL. The source code is written in R and Shiny,
                         and can be found at the following", a("github repo.", href="https://github.com/hseelawi/unhcr_syrian_refugees_jordan_dashboard", target="_blank"),
                         "For further information, kindly drop us an email at",
                         a("hello@ddata.me.", href="mailto:hello@ddata.me", target="_blank"))
                     )))))


dashboardPage(header, sidebar, body)