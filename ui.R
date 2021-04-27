ui <- function(request){
    navbarPage(
    title = "TestR", 
               theme = light, selected = "Overview",
             tags$head(
               tags$link(rel = "stylesheet", type = "text/css", href = "my_style.css")
             ),
    tabPanel(
      title = "Overview",
        div(class = "card-deck",
                 bs4_card_tabset(
                   div(class = "card-deck",
                        bs4_card_in(textOutput("nq"), "Total"),
                        bs4_card_in(textOutput("nqe"), "Easy"),
                        bs4_card_in(textOutput("nqm"), "Medium"),
                        bs4_card_in(textOutput("nqh"), "Hard"),
                        bs4_card_in(textOutput("nqc"), "Chapters")
                   ),
                 "Question base stats"),
                 bs4_card_tabset(
                   fluidRow(#align = "center",
                     column(3,
                            shinyDirButton("choose_dir", 
                                           "Choose directory", 
                                           "Choose the folder containing the question base files")
                     ),
                     column(3,
                            div(
                              class = "custom-control custom-switch", 
                              tags$input(
                                id = "dark_mode", type = "checkbox", class = "custom-control-input",
                                onclick = HTML("Shiny.setInputValue('dark_mode', document.getElementById('dark_mode').value);")
                              ),
                              tags$label(
                                "Dark mode", `for` = "dark_mode", class = "custom-control-label"
                              )
                            )
                     )
                   ), "Options"
                 )
        ),
        br(),
        div(class = "card-deck",
          #column(3,
          bs4_card(#"sad",
                   plotOutput("date_change"),
                   "Change in number of questions one month from the last addition"
          ),
                 bs4_card(
                   plotOutput("level_bars"),
                   "Distribution of questions depending on their level"
                 ),
            bs4_card_tabset(
              tabsetPanel(
                tabPanel("Sum", plotOutput("chapter_level_bars")),
                tabPanel("Easy", plotOutput("chapter_level_bars_easy")),
                tabPanel("Medium", plotOutput("chapter_level_bars_medium")),
                tabPanel("Hard", plotOutput("chapter_level_bars_hard"))
              ),
              "Distribution of questions depending on chapter-level combination"
            )
        )
      ),    
    tabPanel(
      title = "Generate exam", 
      sidebarLayout(
        sidebarPanel(
          use_bs_tooltip(),
          div(class = "card-deck",
              bs4_card_in(textOutput("test_easy"), "Easy"),
              bs4_card_in(textOutput("test_medium"), "Medium"),
              bs4_card_in(textOutput("test_hard"), "Hard"),
              bs4_card_in(textOutput("test_total"), "Total")
          ),
          br(),
          textInput("course", "Course name", value = "Demo name"),
          textInput("test_date", "Test date", value = Sys.Date()),
          fluidRow(
            column(5, offset = 1,
              fluidRow(
                numericInput("questions_version", "Choose questions", value = 1),
                div(class = "divs", icon("info-circle", class = "liclass") %>%
                      bs_embed_tooltip(title = "Sets a seed for random choice of questions", placement = "right"))
              )
            ),
            column(5, offset = 1,
              fluidRow(
                numericInput("choices_version", "Choose group", value = 1),
                div(class = "divs", icon("info-circle", class = "liclass") %>%
                      bs_embed_tooltip(title = "Creates groups by permuting the choices order 
                                       (value of 1 = group A, value of 2 = group B, ...)", placement = "right"))
                )
            )
          ),
          column(12,
                 div(
                 checkboxInput("checkbox", "Add correct answers and chosen questions per chapter", value = FALSE),
                 class = "qb")),
          div(class = "qb",
          downloadButton("generate_test", "Generate test"),
          div(class = "divider"),
          downloadButton("generate_base", "Generate question base"))
        ),
        mainPanel(
          fluidRow(
            useShinyalert(),
            column(6,
              bs4_card(
              DT::dataTableOutput("nchosen", width = "100%"),
              "Choose questions for the exam"
              )
            ),
            column(6,
              bs4_card(
              DT::dataTableOutput("n_list", width = "100%"),
              "Total number of questions per chapter-level subgroup"
              )
            )
          )
         )
    )
  ),
  tabPanel(
    title = "Manual exam changes",
    fluidRow(
      column(6,
             fluidRow(
               column(12, selectizeInput("q_search2", "Search questions", choices = NULL, selected = NULL, width = "100%"))
             ),
             fluidRow(
               column(4, selectInput("change_chapter2", "Choose chapter", choices = NULL, selected = NULL)),
               column(4, selectInput("change_level2", "Question level", choices = NULL, selected = NULL)),
               column(4, selectInput("change_correct2", "Correct answer", choices = NULL, selected = NULL))
             ),
             fluidRow(
               column(12, textInput(inputId = "change_question2", label = "Change question", width = "100%"))  
             ),
             map2(list("a_isp2", "b_isp2", "c_isp2", "d_isp2", "e_isp2"), list("a)", "b)", "c)", "d)", "e)"), map_choices_ui),
             fluidRow(
               useShinyalert(), 
               column(6, actionButton("swap_questions", "Swap questions")),
               column(6, downloadButton("generate_test_manual", "Generate test"))
             )
      ),
      column(6,
             bs4_card(column(12, HTML(manual_exam_change_text), style = "text-align: justify;"), "Instructions"),
             br(),
             div(class = "card p-3", 
                    selectizeInput("q_numbers", "Question numbers", 
                                   choices = NULL, selected = NULL, multiple = TRUE, width = "100%")
             )
             #,textOutput("si")
             #,textOutput("si2")
      )
    )
  ),
  tabPanel(
    title = "Add questions",
    fluidRow(
      column(6,
             fluidRow(
               column(4,selectizeInput("chapter", "Choose chapter", choices = NULL, options = list(create = TRUE))),
               column(4, selectInput("level", "Question level", c("Easy", "Medium", "Hard"))),
               column(4, selectInput("correct_answer", "Correct answer", c("a", "b", "c", "d", "e")))
             ),
             fluidRow(
               column(12, textInput("question", "Question", width = "100%"))  
             ),
             map2(list("a", "b", "c", "d", "e"), list("a)", "b)", "c)", "d)", "e)"), map_choices_ui),
             fluidRow(
               useShinyalert(),
               column(6, actionButton("add_question", "Add question")),
               column(6, actionButton("delete_question", "Delete last question"))
             ),
             style = "font-size:100%"
      ),
      column(6, DT::dataTableOutput('n_list2', width = "85%"), style = "font-size:95%", align = "center")
    )
  ),
  tabPanel(
    title = "Change or remove questions",
    fluidRow(
      column(6,
        fluidRow(
          column(12, selectizeInput("q_search", "Search questions", choices = NULL, selected = NULL, width = "100%"))
          ),
        fluidRow(
          column(6, verbatimTextOutput("added")),
          column(6, verbatimTextOutput("changed"))
        ),
        fluidRow(
          column(3, selectInput("change_chapter", "Choose chapter", choices = NULL, selected = NULL)),
          column(3, selectInput("change_level", "Question level", choices = NULL, selected = NULL)),
          column(3, selectInput("change_correct", "Correct answer", choices = NULL, selected = NULL))
        ),
        fluidRow(
          column(12, textInput(inputId = "change_question", label = "Change question", width = "100%"))  
        ),
        map2(list("a_isp", "b_isp", "c_isp", "d_isp", "e_isp"), list("a)", "b)", "c)", "d)", "e)"), map_choices_ui),
        fluidRow(
          useShinyalert(), 
          column(6, actionButton("change_question_button", "Change question")
          ),
          column(6, actionButton("remove_question_button", "Delete question"))
        )
      ),
      column(6, DT::dataTableOutput('n_list3', width = "85%"), style = "font-size:95%", align = "center")
    )
  ),
  navbarMenu("Help",
    tabPanel("About and overview", div(class = "card-deck",
                                       div(
                                         class = "card",
                                         div(class = "card-header bg-primary", "About"),
                                         div(class = "card-body d-flex",
                                             column(4, offset = 0, img(src="icon_hex.png", style = "height: auto; width: 70%; object-position: center center;")),
                                             column(8, offset = 0, HTML(about_text), style = "text-align: justify;")
                                         )
                                       ),
                                       bs4_card(column(12, HTML(overview_text), style = "text-align: justify;"), "Overview")
    )),
    tabPanel("Generate and alter the exam", div(class = "card-deck",
                                                bs4_card(column(12, HTML(generate_exam_text), style = "text-align: justify;"), "Generate exam"),
                                                bs4_card(column(12, HTML(manual_exam_change_text), style = "text-align: justify;"), "Manual exam change")
    )),
    tabPanel("Add, change or delete questions", div(class = "card-deck",
                                                    bs4_card(column(12, HTML(add_questions_text), style = "text-align: justify;"), "Add questions"),
                                                    bs4_card(column(12, HTML(change_delete_text), style = "text-align: justify;"), "Change or delete questions")
    ))
   )
  
  )
}
