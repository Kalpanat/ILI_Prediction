## ui.R ##

library("shinydashboard")
sidebar <- dashboardSidebar(
  sidebarMenu(
    
    menuItem("Flu Positivity", icon = icon("th"), tabName = "FluP",
             badgeLabel = "new", badgeColor = "green"),
    
    menuItem("Flu Severity", icon = icon("th"), tabName = "FluS",
             badgeLabel = "new", badgeColor = "green"),
    
    menuItem("ILI Severity", icon = icon("th"), tabName = "IliS",
             badgeLabel = "new", badgeColor = "green"),
    menuItem("HeatMap", tabName = "hm", icon = icon("hm"))
  )
  
  
)

#Package LibPath Version Priority Depends Imports LinkingTo Suggests Enhances License
#License_is_FOSS License_restricts_use OS_type Archs MD5sum NeedsCompilation Built

body <- dashboardBody(
  tabItems(
    
    tabItem(tabName = "FluP",
            h2("Flu Positivity"),
            plotOutput("graphPlot"),
            plotOutput("lmPlot"),
            textOutput("valueP1"),
            textOutput("valueP2"),
            textOutput("valueP3")
            
            
            
            
    ),
    
    tabItem(tabName = "FluS",
            h2("Flu Severity"),
            plotOutput("gPlot2"),
            
            plotOutput("gPlot3"),
            plotOutput("gPlot4"),
            textOutput("valueP5"),
            textOutput("valueP4"),
            textOutput("valueP6")
            
            
    ),
    
    tabItem(tabName = "IliS",
            h2("ILI Severity"),
            # plotOutput("gPlot10"),
            plotOutput("gPlot11"),
            plotOutput("gPlot12"),
            plotOutput("gPlot13"),
            textOutput("valueP7"),
            textOutput("valueP8"),
            textOutput("valueP9")
            
            
            
    ),
    tabItem(tabName = "hm",
            h2("Heat Map across Regions of United States"),
            plotOutput("histPlot")
    )
    
  )
)

# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "Flu Prediction"),
  sidebar,
  body,
  skin = "purple"
)