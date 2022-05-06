#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(ggplot2)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(shinythemes)

df=read.csv("ONLINE EDUCATION SYSTEM REVIEW.csv",stringsAsFactors = TRUE)

df$Gender<- as.factor(df$Gender)
df$Performance.in.online<- as.factor(df$Performance.in.online)
df$Do.elderly.people.monitor.you.<-as.factor(df$Do.elderly.people.monitor.you.)

mentor<- list("Yes","No")

Gender<-list("Male","Female")
Homelocation<- list("Urban","Rural")
Education<-list("School","Under Graduate", "Post Graduate")

Economicstatus<- list("Poor","Middle Class", "Rich")
FamilySize<-list("1","2", "3", "4", "5", "6", "7", "8", "9", "10", "11+") 

ui <- shinyUI(dashboardPage( skin = 'green',
                
                             dashboardHeader( title = "Online Education Dashboard", titleWidth = 700),
                             
                             dashboardSidebar(
                               sidebarMenu(
                                 id = "sidebar",
                                 
                                 #first menu item
                                 menuItem(text = "Home", tabName="home", icon = icon("graduation-cap")),
                                 
                                 #second menu item 
                                 menuItem(text = "Performance", tabName="a_tab", icon = icon("bar-chart")),
                                 
                                 #third menu item
                                 menuItem(text = "Box and Whisker Plots", tabName="chart", icon = icon("bar-chart")),
                                 
                                 #fourth menu item
                                 menuItem(text = "Demographics", tabName="demog", icon = icon("address-book")),
                                 
                                 #about data item
                                 menuItem(text = "About Data", tabName="about", icon = icon("table"))
                          
                               )
                               
                             ),
                             dashboardBody(
                               
                               #first performance tab
                               tabItems(
                                 tabItem('a_tab',
                                         titlePanel("Performance Online"), 
                                         sidebarLayout(
                                           
                                           sidebarPanel(
                                             p(" To understand if there is any relation between hours spent on social media and online performance , conditional whether there is any elderly monitering them."),
                                             p(" As in the violin plot that there is indeed certain negative relation between time spent on social media and online academic performance."),
                                             p("To further understand the conditionality effect of elderly monitoring and also gender causal effect we can use  interactive barplot"),
                                             br(),
                                             br(),
                                             selectInput("mentor","Having a mentor",choices=mentor),
                                             selectInput("gender","Gender",choices=Gender),
                                             actionButton("plot","plot")
                                             
                                           ),
                                           
                                           mainPanel(
                                             plotOutput("violin"),
                                             
                                             plotOutput("barPlot")
                                             
                                           )
                                         )
                                 ),
                                 #the home tab
                                 tabItem('home',
                                         mainPanel(align = 'Center',
                                                   h1("Online Education Shiny Dashboard"),
                                                   br(),
                                                   
                                                   br(),
                                                   img(src='landing photo.jpeg', width = 500, height = 300, align = 'Center'),
                                                   br(),
                                                   br(),
                                                   p("This dashboard is designed to understand the demographics of students, their performance in online courses, and the extracurricular activities- they're involved in \n
              In this dashboard we will explore the relationship between factors of student's lives and their impact on their educational outcomes. \n
              The Demographics tab elaborates on the diverse characteristics and background(s) of the students. It details whether they come from \n
              smaller or larger size families. It also reveals if students come from poor, middle class, or rich backgrounds and more. \n
              The Regression tab covers specific variables selected from the online education system review dataset. From the analyzed \n
              data it reveals the relationship between the selected variables. The About Data discusses where the dataset was retrieved \n
              and the details/summary surrounding the online education system review data. ", style =  "font-size:15px;")
                                                   
                                         )),
                                 #the about tab
                                 tabItem('about',
                                         mainPanel(align = 'Center',
                                                   # h1('Data', style = "font-size:80px;" ),
                                                   br(),
                                                   img(src='kaggle logo.png', width = 180, height = 60, align = 'Center'),
                                                   br(),
                                                   br(),
                                                   br(),
                                                   p("Our chosen dataset, is a csv file that consisted of 23 columns and 1033 rows. This dataset is focusing on reviewing the online education system, was retrieved from Kaggle.com. \n
                    Original purpose of the dataset was to explore the impact of online education based on the impact COVID-19 had on education. Since, having to move from traditional classroom environments, \n
                    that has provided vital roles in nurturing and molding the life of individuals, to virtual education. After analyzing the data we discovered that it can be used to review the satisfactory level of the learners. \n
                    It can also be used to investigate the demographics of the students, which gives us an inside viewpoint of the learners. \n
                    For example, to view their gender, level of education, age, their economic class, or to determine if they are from an urban or rural home location.\n
                    We can also determine the extracurricular activities, their interests, and performance in online classes.\n
                    One of the limitations of this dataset is the inablility to for it to be used for is to view their specific grades and the number of classes they're enrolled in.", style = "font-size:14px;"),
                                                   tags$a(href="https://www.kaggle.com/datasets/sujaradha/online-education-system-review?select=ONLINE+EDUCATION+SYSTEM+REVIEW.csv", "For the Data"),
                                                   br(),
                                                   actionButton('prev_five', 'Previous Cols'),
                                                   actionButton('next_five', 'Next Cols'),
                                                   DTOutput('tbl')
                                         )),
                                 
                                 
                                 #the regression tab
                                 
                                 tabItem('chart',
                                         titlePanel("Box and Whisker Plots"),
                                         sidebarLayout(
                                           sidebarPanel(
                                             
                                             
                                             selectInput("indepvar", label = h3("Explanatory variable"),
                                                         choices = list("Economic status" = 'Economic.status' ,
                                                                        'Satisifaction about online education'='Your.level.of.satisfaction.in.Online.Education',
                                                                        
                                                                        
                                                                        'Group studies'='Engaged.in.group.studies.'
                                                                        
                                                                        
                                                         ), selected = 1)
                                           ),
                                           
                                           mainPanel(
                                             plotOutput("scatterplot"), # Plot
                                             
                                             verbatimTextOutput("summary")) # Regression output
                                           
                                           
                                         )
                                         
                                 ),
                                 
                                 
                                 #demographics tab
                                 tabItem('demog',
                                         titlePanel("Demographics Barchart"),
                                         sidebarLayout(
                                           sidebarPanel(
                                             p(" To understand demographic data of students causal effect we can use  interactive barplot"),
                                             br(),
                                             
                                             selectInput("Gender","Gender",choices= Gender),
                                             selectInput("hometown","Home location",choices=Homelocation),
                                             selectInput("economic_status","Economic status",choices=Economicstatus,selected = "Middle Class"), 
                                             selectInput("education_level","Level of education",choices=Education,selected = "Under Graduate"),
                                             #selectInput("choose","Demographic variable",choices = variables,multiple = TRUE),
                                             actionButton("action","Plot")
                                             
                                             #actionButtion("twoplot","twovariable")
                                           ),
                                           # Show a plot
                                           mainPanel(
                                             h5(strong("Click Plot to see visual")),
                                             plotOutput("dPlot"),
                                             p("* Certain combinations are unavailable due to the shallowness of the data.", style =  "font-size:11px;")
                                           )
                                           
                                         )
                                         
                                 )
                               )
                             )
)
)
