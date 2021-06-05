header <- dashboardHeader(title = "Campus Recruitment")

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem(text = "Analysis", 
                 tabName = "analysis", 
                 icon = icon("dollar-sign")),
        menuItem(text = "Prediction", 
                 tabName = "pred", 
                 icon = icon("chart-line")),
        menuItem(text = "Dataset", 
                 tabName = "data", 
                 icon = icon("database"))
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "analysis", 
                h1("Campus Recruitment's Analysis", align = "center"),
                fluidRow(
                    tabBox(id="tabchart1", width = 12, height = "1200px",
                           tabPanel("Plot",
                                    fluidRow(column(3,
                                                    selectInput(inputId = "spec", 
                                                                label = "Specialisation",
                                                                choices = unique(p_status$specialisation))),
                                             column(3,
                                                    selectInput(inputId = "numvar", 
                                                                label = "Choose Y Axis", 
                                                                choices = p_status %>%
                                                                    select(hsc_s, degree_t) %>% 
                                                                    colnames()
                                                    )))
                                    ,
                                    plotlyOutput(outputId = "plot_composition")
                           ),
                           tabPanel("Table",
                                    selectInput(inputId = "spec1", 
                                                label = "Specialisation",
                                                choices = unique(p_status$specialisation)),
                                    box(background = "light-blue",
                                        width = 6,
                                        selectInput(inputId = "var", 
                                                    label = "Choose Variable", 
                                                    choices = p_status %>% 
                                                        select(c(ssc_p,hsc_p,degree_p,etest_p,mba_p)) %>% 
                                                        colnames()
                                        ),
                                        tableOutput(outputId = "mean_hscs")),
                                    
                                    box(background = "navy",
                                        width = 6,
                                        selectInput(inputId = "var2", 
                                                    label = "Choose Variable", 
                                                    choices = p_status %>% 
                                                        select(c(ssc_p,hsc_p,degree_p,etest_p,mba_p)) %>% 
                                                        colnames()
                                        ),
                                        tableOutput(outputId = "mean_degree_t"))
                    )
                ))),
        tabItem(tabName = "pred", 
                h1("Recruitment Prediction", align = "center"),
                fluidRow(column(4,
                                selectInput(inputId = "gen", 
                                            label = "Gender",
                                            choices = unique(p_status$gender))),
                         column(4,
                                numericInput(inputId = "ssc_p", 
                                             label = "Secondary Education percentage", 
                                             value = 60,
                                             min = 10, 
                                             max = 100, 
                                             step = 0.1)),
                         column(4,
                                selectInput(inputId = "ssc_b", 
                                            label = "Board of Education",
                                            choices = unique(p_status$ssc_b)))),
                fluidRow(column(4,
                                selectInput(inputId = "hsc_b", 
                                            label = "Higher Board of Education",
                                            choices = unique(p_status$hsc_b))),
                         column(5,
                                numericInput(inputId = "hsc_p", 
                                             label = "Higher Secondary Education percentage", 
                                             value = 60,
                                             min = 10, 
                                             max = 100, 
                                             step = 0.1))),
                fluidRow(column(4,
                                selectInput(inputId = "hsc_s", 
                                            label = "Specialization in Higher Secondary Education",
                                            choices = unique(p_status$hsc_s))),
                         column(4,
                                numericInput(inputId = "degree_p", 
                                             label = "Degree Percentage", 
                                             value = 60,
                                             min = 10, 
                                             max = 100, 
                                             step = 0.1))),
                fluidRow(column(4,
                                selectInput(inputId = "degree_t", 
                                            label = "Under Graduation(Degree type)",
                                            choices = unique(p_status$degree_t))),
                         column(4,
                                numericInput(inputId = "etest_p", 
                                             label = "Employability test percentage", 
                                             value = 60,
                                             min = 10, 
                                             max = 100, 
                                             step = 0.1)),
                         column(4,
                                selectInput(inputId = "workex", 
                                            label = "Work Experience",
                                            choices = unique(p_status$workex)))),
                fluidRow(column(4,
                                selectInput(inputId = "specialisation", 
                                            label = "Post Graduation(MBA)",
                                            choices = unique(p_status$specialisation))),
                         column(4,
                                numericInput(inputId = "mba_p", 
                                             label = "MBA percentage", 
                                             value = 60,
                                             min = 10, 
                                             max = 100, 
                                             step = 0.1))),
                valueBoxOutput("prediction")
                
        ),
        tabItem(tabName = "data", 
                dataTableOutput(outputId = "data_workers"))
    ))


dashboardPage(
    header = header,
    body = body,
    sidebar = sidebar, 
    skin = "blue"
)
