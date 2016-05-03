library("shinydashboard")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Flu Positivity", icon = icon("th"), tabName = "FluP",
             badgeLabel = "new", badgeColor = "green"),
    menuSubItem("SVM" , tabName = "svm1"),
    menuSubItem("Decision Tree",tabName = "Decision1"),
    menuSubItem("Linear Regression",tabName = "Regr1"),
    
    menuItem("Flu Severity", icon = icon("th"), tabName = "FluS",
             badgeLabel = "new", badgeColor = "green"),
    menuSubItem("SVM" , tabName = "svm2"),
    menuSubItem("Decision Tree",tabName = "Decision2"),
    menuSubItem("Linear Regression",tabName = "Regr2"),
    
    menuItem("ILI Severity", icon = icon("th"), tabName = "IliS",
             badgeLabel = "new", badgeColor = "green"),
    menuSubItem("SVM" , tabName = "svm3"),
    menuSubItem("Decision Tree",tabName = "Decision3"),
    menuSubItem("Linear Regression",tabName = "Regr3"),
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
            plotOutput("lmPlot")
    ),
    
    tabItem(tabName = "FluS",
            h2("Flu Severity"),
            plotOutput("gPlot2"),
            
            plotOutput("gPlot3"),
            plotOutput("gPlot4")
            
    ),
    
    tabItem(tabName = "IliS",
            h2("ILI Severity"),
            plotOutput("gPlot10"),
            plotOutput("gPlot11"),
            plotOutput("gPlot12"),
            plotOutput("gPlot13")
            
    ),
    tabItem(tabName = "hm",
            h2("flu1"),
            plotOutput("histPlot")
    ),
    tabItem(tabName = "svm1",
            box(title = "SVM", 
                width = 10,
                height = "200px",
                status = "success", 
                solidHeader = TRUE, 
                collapsible = TRUE,
                textOutput("valueP1")
            )
    ),
    
    tabItem(tabName = "Decision1",
            box(title = "Decision Tree", 
                width = 10,
                height = "200px",
                status = "success", 
                solidHeader = TRUE, 
                collapsible = TRUE,
                textOutput("valueP2")
            )
    ),
    tabItem(tabName = "Regr1",
            box(title = "Linear Regression",
                width = 10,
                height = "200px",
                status = "success", 
                solidHeader = TRUE, 
                collapsible = TRUE,
                textOutput("valueP3")
            )
            
    ),
    tabItem(tabName = "svm2",
            box(title = "SVM", 
                width = 10,
                height = "200px",
                status = "success", 
                solidHeader = TRUE, 
                collapsible = TRUE,
                textOutput("valueP5")
            )
    ),
    tabItem(tabName = "Decision2",
            box(title = "Decision Tree", 
                width = 10,
                height = "200px",
                status = "success", 
                solidHeader = TRUE, 
                collapsible = TRUE,
                textOutput("valueP4")
            )
    ),
    
    tabItem(tabName = "Regr2",
            box(title = "Linear Regression",
                width = 10,
                height = "200px",
                status = "success", 
                solidHeader = TRUE, 
                collapsible = TRUE,
                textOutput("valueP6")
            )
            
    ),
    tabItem(tabName = "svm3",
            box(title = "SVM", 
                width = 10,
                height = "200px",
                status = "success", 
                solidHeader = TRUE, 
                collapsible = TRUE,
                textOutput("valueP7")
            )
    ),
    tabItem(tabName = "Decision3",
            box(title = "Decision Tree", 
                width = 10,
                height = "200px",
                status = "success", 
                solidHeader = TRUE, 
                collapsible = TRUE,
                textOutput("valueP8")
            )
    ),
    
    tabItem(tabName = "Regr3",
            box(title = "Linear Regression",
                width = 10,
                height = "200px",
                status = "success", 
                solidHeader = TRUE, 
                collapsible = TRUE,
                textOutput("valueP9")
            )
            
    )
  )
)

# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "Flu Prediction"),
  sidebar,
  body
)