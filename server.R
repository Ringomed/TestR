server <- function(input, output, session) {
  questions <- reactiveVal(questions)
  choices <- reactiveVal(choices)
  
  qa <- reactive({questions() %>%
      left_join(choices(), by = c("Chapter", "Question_number")) %>%
      as_tibble()})
  
  n_list <- reactive({
          questions() %>%
            group_by(Chapter, Level) %>%
            summarise(N = n()) %>%
            spread(Level, N, drop = FALSE) %>%
            replace_na(list(Easy = 0, Medium = 0, Hard = 0)) %>%
            relocate(Hard, .after = Medium)
    })
  
  n_chosen <- reactiveValues(n_chosen = n_chosen)
  
  shinyDirChoose(input, "choose_dir", roots=c(wd='.'), filetypes=c('csv'), allowDirCreate = TRUE)
  
  chosen_dir <- reactive({parseDirPath(c(wd = "."), input$choose_dir)})
  
  observe({
    if(!is.integer(input$choose_dir)){
      
      #create a question base if one is not present in the chosen directory
      if(!"questions.csv" %in% list.files(chosen_dir())) {
        
          questions <- tibble(
            Chapter = character(),
            Question_number = numeric(),
            Question = character(),
            Answer = character(),
            Level = factor(levels = c("Easy", "Medium", "Hard"))
            , Added = as.Date(c())
            , Changed = as.Date(c())
          )
          
        n_chosen2 <- tibble(
            Chapter = character(),
            Easy = numeric(),
            Medium = numeric(),
            Hard = numeric()
        )
          
        n_chosen$n_chosen <- n_chosen2
        
        write_excel_csv(questions, paste0(chosen_dir(), "/questions.csv"))
        questions(questions)
  
      }else{
        #if there is already a question base present, upload it into R
        questions <- read_csv(paste0(chosen_dir(), "/questions.csv"),
                              col_types = cols(
                                Chapter = col_character(),
                                Question_number = col_number(),
                                Question = col_character(),
                                Answer = col_character(),
                                Level = col_factor(c("Easy", "Medium", "Hard"))
                                , Added = col_date()
                                , Changed = col_date()
                              ),
                              locale = locale(encoding = "UTF-8")
        )
        
        n_chosen2 <- tibble(
          Chapter = unique(questions$Chapter),
          Easy = 0,
          Medium = 0,
          Hard = 0
        )
        
        n_chosen$n_chosen <- n_chosen2
        
        questions(questions)
      }
  
  
      if(!"choices.csv" %in% list.files(chosen_dir())) {
  
          choices <- tibble(
            Chapter = character(),
            Question_number = numeric(),
            Possible_answers = character(),
            Answer_number = numeric()
          )
        
        write_csv(choices, paste0(chosen_dir(), "/choices.csv"))
        choices(choices)
  
      }else{
        choices <- read_csv(paste0(chosen_dir(), "/choices.csv"),
                            col_types = cols(
                              Chapter = col_character(),
                              Question_number = col_number(),
                              Possible_answers = col_character(),
                              Answer_number = col_number()
                            ),
                            locale = locale(encoding = "UTF-8")
        )
        
        choices(choices)
      }
    }

  })
  
  #change to dark theme is switch is turned on
  observe({
    session$setCurrentTheme(
      if (isTRUE(input$dark_mode)) dark else light
    )
  })
  
  
  observe({
      output$nq <- renderText({nrow(questions())})
      output$nqe <- renderText({nrow(questions() %>% filter(Level == "Easy"))})
      output$nqm <- renderText({nrow(questions() %>% filter(Level == "Medium"))})
      output$nqh <- renderText({nrow(questions() %>% filter(Level == "Hard"))})
      output$nqc <- renderText({questions()$Chapter %>% unique %>% length})
      
      output$test_easy <- renderText({sum(n_chosen$n_chosen$Easy)})
      output$test_medium <- renderText({sum(n_chosen$n_chosen$Medium)})
      output$test_hard <- renderText({sum(n_chosen$n_chosen$Hard)})
      output$test_total <- renderText({sum(n_chosen$n_chosen$Easy) + 
                                       sum(n_chosen$n_chosen$Medium) +
                                       sum(n_chosen$n_chosen$Hard)})
  })
  
  output$level_bars <- renderPlot({
      questions() %>%
        group_by(Level) %>%
        summarise(N = n()) %>%
        mutate(prop = 100*N/sum(N)) %>%
        ggplot(aes(fct_reorder(Level, prop, .desc = TRUE), prop, fill = Level)) +
        geom_col(show.legend = FALSE, width = 0.75) +
        labs(x = "Level", y = "Proportion of questions (%)") +
        mt
  })
  
  output$chapter_level_bars <- renderPlot({
   questions() %>%
      mutate(Chapter = as_factor(Chapter)) %>%
      count(Chapter, Level) %>%
      ggplot(aes(fct_reorder(Chapter, n, .desc = FALSE, .fun = sum), n, fill = Level)) +
      geom_col(width = 0.8) +
      coord_flip() +
      labs(x = "Chapter", y = "Number of questions") + 
      mt
  })
  
  output$date_change <- renderPlot({
    questions() %>%
    count(Added) %>%
    filter(Added >= (max(Added) - 30)) %>%
    ggplot(aes(x = Added, y = as.integer(cumsum(n)))) +
      geom_line(col = "steelblue", lwd = 2) +
      geom_point(size = 3, col = "darkorange1") +
      labs(x = "Addition date", y = "Total number of questions") +
      theme(axis.text.x = element_text(angle = 10, hjust = 1)) +
      scale_x_date(#limit = c(Sys.Date() - 30, Sys.Date()),
                   date_breaks = "5 days",date_labels = "%b %d") +
      mt
  })

  
  output$chapter_level_bars_easy <- renderPlot({questions_bars(questions(), "Easy")})
  output$chapter_level_bars_medium <- renderPlot({questions_bars(questions(), "Medium")})
  output$chapter_level_bars_hard <- renderPlot({questions_bars(questions(), "Hard")})
  
  output$nchosen <- DT::renderDataTable({
    
    DT::datatable(
      n_chosen[["n_chosen"]],
      editable = TRUE,
      options = list(
        paging = FALSE,
        pageLength = 30,
        searching = FALSE,
        fixedColumns = TRUE,
        autoWidth = TRUE,
        ordering = FALSE,
        dom = "Blfrtip",
        lengthMenu = list( c(5, 10, 20, -1) # declare values
                           , c(5, 10, 20, "All") # declare titles
        ),
        class = "display",
        #change the column names color in response to the dark mode switch
        initComplete = if(isTRUE(input$dark_mode)){
          JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'color': '#fff'});",
            "}")
        }
      )) %>%
      #change the background and font colors in response to the dark mode switch
      formatStyle(1:5, color = ifelse(isTRUE(input$dark_mode), "#ccc4c4", "black"), 
                  background = ifelse(isTRUE(input$dark_mode), "#222222", ""), target = "row")
  })
  
  
  proxy_n_chosen <- dataTableProxy("nchosen")
  
  #edit cells in a datatable
  observeEvent(input$nchosen_cell_edit, {
    info <- input$nchosen_cell_edit
    i <- info$row
    j <- info$col
    v <- info$value
    if(v <= n_list()[[i, j]]){
      n_chosen[["n_chosen"]][[i, j]] <- coerceValue(v, n_chosen[["n_chosen"]][[i, j]])
      replaceData(proxy_n_chosen, n_chosen[["n_chosen"]], resetPaging = FALSE, rownames = FALSE)
    }else{
      shinyalert("Warning", 
                 paste0("The chosen number of questions exceeds the available number in the question base!\n 
                        The current number of questions for this group in the question base is ", n_list()[[i, j]], "!"), type = "warning")
      #uncomment if you want to refresh the whole page in response to a wrong user input
      #delay(3000, shinyjs::js$refresh())
    }
  })
  
  n_list_table <- reactive({
    DT::datatable(n_list(), editable = FALSE,
                  options = list(paging = FALSE, pageLength = 30, searching = FALSE, fixedColumns = TRUE, autoWidth = TRUE,ordering = FALSE, 
                                 lengthMenu = list( c(5, 10, 20, -1) # declare values
                                                    , c(5, 10, 20, "All")), # declare titles
                                 class = "display",
                                 initComplete = if(isTRUE(input$dark_mode)){
                                   JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'color': '#fff'});",
                                   "}")
                                   }
                                   )) %>%
      #when number of questions in a certain subgroup is 0, color it red
      formatStyle(c("Easy", "Medium", "Hard"), color = styleEqual(c(0), c("red"))) %>%
      #switch the background color in response to the dark mode switch
      formatStyle(1:5, color = ifelse(isTRUE(input$dark_mode), "#ccc4c4", "black"), 
                  background = ifelse(isTRUE(input$dark_mode), "#222222", ""), target = "row")
  })
  
  output$n_list <- DT::renderDataTable({n_list_table()})
  output$n_list2 <- DT::renderDataTable({n_list_table()})
  output$n_list3 <- DT::renderDataTable({n_list_table()})
  
  
  output$generate_base <- downloadHandler(
    
    filename = "Test base.html",
    
    content = function(file) {
      tempReport <- file.path(tempdir(), "base_html.Rmd")
      file.copy("base_html.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(qa = qa(), n_list = n_list())
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      
    }
    
  )
  
  output$generate_test <- downloadHandler(
    
    filename = "Test.html",
    
    
    content = function(file) {
      
      tempReport <- file.path(tempdir(), "test_html.Rmd")
      file.copy("test_html.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(#n_chosen = n_chosen[["n_chosen"]],
                     q = questions(),
                     n_chosen = n_chosen[["n_chosen"]],
                     q_all = q_all$q_all,
                     qa = qa(),
                     add_correct = input$checkbox,
                     course = input$course,
                     questions_version = input$questions_version,
                     choices_version = input$choices_version,
                     test_date = input$test_date
      )
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      
    }
    
  )
  
  
  ##ADD QUESTIONS
  
  #refresh the list of chapters
  observe({
    updateSelectizeInput(session, 'chapter', choices = n_list()$Chapter, server = TRUE)
  })
  
  observeEvent(input$add_question,{ 
    if(!is.integer(input$choose_dir))
    {
      if(!(input$question %in% questions()$Question)){
        #question to add
        question_add <- tibble(Chapter = input$chapter,
                               Question_number = ifelse(nrow(questions()) > 0, last(questions()$Question_number) + 1, 1),
                               Question = input$question,
                               Answer = input$correct_answer,
                               Level = factor(input$level, levels = c("Easy", "Medium", "Hard"))
                               , Added = Sys.Date()
                               , Changed = Sys.Date()
                               )
        
        #updating the n_chosen list
        if(!(input$chapter %in% questions()$Chapter)){
          n_chosen[["n_chosen"]] <- rbind(n_chosen[["n_chosen"]], tibble(Chapter = input$chapter, Easy = 0, Medium = 0, Hard = 0))
        }
        
        #updating the questions table
        questions(rbind(questions(), question_add))
        
        #choices to add
        choices_add <- tibble(Chapter = input$chapter,
                              Question_number = ifelse(nrow(choices()) > 0, last(choices()$Question_number) + 1, 1),
                              Possible_answers = c(input$a, input$b, input$c, input$d, input$e),
                              Answer_number = c(1:5))
        
        #updating the choices table
        choices(bind_rows(choices(), choices_add))
        
        #making the changes in the question database
        write_excel_csv(questions(), paste0(chosen_dir(), "/questions.csv"))
        write_excel_csv(choices(), paste0(chosen_dir(), "/choices.csv"))
        shinyalert("The question base has been updated", "The question has been added successfully!", type = "success")
      }else{
        shinyalert("Warning", "An identical question already exists!", type = "warning")
      }
    }else{
      shinyalert("Warning", "You haven't chosen the question base folder!", type = "warning")
    }
  }
  )
  
  ##DELETING THE LAST QUESTION
  
  observeEvent(input$delete_question,{
    if(!is.integer(input$choose_dir)){
      #updating the n_chosen list for a scenario in which all questions from a chapter have been removed
      q_list_remove <- questions() %>% filter(Question_number != last(Question_number))
      
      if(!(last(questions()$Chapter) %in% q_list_remove$Chapter)){
        n_chosen[["n_chosen"]] <- n_chosen[["n_chosen"]] %>% filter(Chapter != last(questions()$Chapter))
      }
          
      #removing the last question and corresponding choices
      questions(q_list_remove)
      
      choices(choices() %>% filter(Question_number != last(Question_number)))
      
      #making the changes in the question database
        write_excel_csv(questions(), paste0(chosen_dir(), "/questions.csv"))
        write_excel_csv(choices(), paste0(chosen_dir(), "/choices.csv"))
        shinyalert("Base refreshed", "The last question has been removed!", type = "info")     
    }else{
      shinyalert("Warning", "You haven't chosen the question base folder!", type = "warning")
    }
  }
  )
  
  ##CHANGE OR DELETE QUESTIONS
  
  #update the questions list
  observe({
    updateSelectizeInput(session, 'q_search', choices = questions()$Question, server = TRUE)
  })

  #fill the boxes depending on the chosen question
  observe({
    updateTextInput(session, "change_question", value = questions() %>% filter(Question == input$q_search) %>% .$Question)

    #filling the possible answers
    walk2( list("a_isp", "b_isp", "c_isp", "d_isp", "e_isp"), list(1,2,3,4,5),
          function(id_name, id_number)
          {
            updateTextInput(session, id_name, value = qa() %>% filter(Question == input$q_search, Answer_number == id_number) %>% .$Possible_answers)
          }
    )
    
    updateSelectInput(session, "change_chapter", 
                      choices = questions()[["Chapter"]] %>% unique %>% sort,
                      selected = (questions() %>% filter(Question == input$q_search))[["Chapter"]])
    
    updateSelectInput(session, "change_level", 
                      choices = c("Easy", "Medium", "Hard"),
                      selected = (questions() %>% filter(Question == input$q_search))[["Level"]])
    
    updateSelectInput(session, "change_correct", 
                      choices = letters[1:5],
                      selected = (questions() %>% filter(Question == input$q_search))[["Answer"]])
  })
  
  #filling the added and last change date
  output$added <- renderText({paste0("Added: ", as.character(questions() %>% filter(Question == input$q_search) %>% .$Added))})
  output$changed <- renderText({paste0("Last change: ", as.character(questions() %>% filter(Question == input$q_search) %>% .$Changed))})
  
  #changing a question that is already in the question base
  observeEvent(input$change_question_button,{
    if(!is.integer(input$choose_dir)){
      #update questions
      q_copy <- questions()
      
      q_chosen_number <- q_copy$Question_number[q_copy$Question == input$q_search]
      
      change_question_parameters <- function(parameter, input_id){
        q_copy[[parameter]][q_copy$Question == input$q_search] <<- input[[input_id]]
      }
  
      walk2(list("Question", "Chapter", "Level", "Answer"),
           list("change_question", "change_chapter", "change_level", "change_correct"),
           change_question_parameters)
      
      q_copy[["Changed"]][q_copy$Question == input$q_search] <- Sys.Date()
      
      questions(q_copy)
  
      #update choices
      c_copy <- choices()
  
      change_choices_parameters <- function(parameter_value, input_id){
        c_copy$Possible_answers[c_copy$Question_number == q_chosen_number][[parameter_value]] <<- input[[input_id]]
      }
  
      walk2(list(1,2,3,4,5),
           list("a_isp", "b_isp", "c_isp", "d_isp", "e_isp"),
           change_choices_parameters)
      
      choices(c_copy)
      
      #making the changes in the question base
        write_excel_csv(questions(), paste0(chosen_dir(), "/questions.csv"))
        write_excel_csv(choices(), paste0(chosen_dir(), "/choices.csv"))
        shinyalert("Question base is refreshed.", "The chosen question has been updated successfully!", type = "info")     
    }else{
      shinyalert("Warning", "You haven't chosen the question base folder!", type = "warning")
    }
  })

  #deleting the currently selected question from the question base
  observeEvent(input$remove_question_button,{
    if(!is.integer(input$choose_dir)){
      q_copy <- questions()
      c_copy <- choices()
      q_chosen_number <- q_copy$Question_number[q_copy$Question == input$q_search]
      
      #remove the selected question
      q_copy <- q_copy %>% filter(Question_number != q_chosen_number)
      
      #go through the questions and modify the question numbers accordingly
      q_copy <- q_copy %>% mutate(Question_number = case_when(
        Question_number < q_chosen_number ~ Question_number,
        Question_number > q_chosen_number ~ Question_number - 1)
        )
      
      #update questions list
      questions(q_copy)
      
      #remove the selected question choices
      c_copy <- c_copy %>% filter(Question_number != q_chosen_number)
      
      #go through the choices and modify the question numbers accordingly
      c_copy <- c_copy %>% mutate(Question_number = case_when(
        Question_number < q_chosen_number ~ Question_number,
        Question_number > q_chosen_number ~ Question_number - 1)
      )
      
      #update choices list
      choices(c_copy)

      write_excel_csv(questions(), paste0(chosen_dir(), "/questions.csv"))
      write_excel_csv(choices(), paste0(chosen_dir(), "/choices.csv"))
      shinyalert("Question base is refreshed.", "The chosen question has been deleted successfully!", type = "info")     
    }else{
      shinyalert("Warning", "You haven't chosen the question base folder!", type = "warning")
    }

  }
  )
  
  ##MANUAL EXAM CHANGE
  
  #helper functions
  choose_questions <- function(n, chapters, level)
  {
    x <- questions() %>%
      filter(Chapter == chapters, Level == level) %>%
      .$Question_number
    
    #first if condition needed because sample interprets one positive integer as sequence 1:integer
    #n condition needed to prevent question being generated with n = 0
    if(length(x) == 1 & n != 0)
    {
      x
    }else{
      if(is.integer(input$questions_version)){
        set.seed(input$questions_version)
      }else{
        set.seed(1)
      }
      sample(x, n, replace = FALSE)
    }
    
  }
  
  horizontal_row_bind_list <- function(x,y)
  {
    z <- list()
    
    map(1:length(x), 
        function(i){
          z[[i]] <- c(x[[i]], y[[i]])
        }
    )
    
  }
  
  #display chosen questions if exam is generated
  q_all <- reactiveValues(q_all = c())
  observe({
      if(nrow(questions()) > 0)
      {
        n_chosen$n_chosen
        q_easy <- reactive({map2(n_chosen$n_chosen$Easy, n_chosen$n_chosen$Chapter, choose_questions, level = "Easy")})
        q_medium <- reactive({map2(n_chosen$n_chosen$Medium, n_chosen$n_chosen$Chapter, choose_questions, level = "Medium")})
        q_hard <- reactive({map2(n_chosen$n_chosen$Hard, n_chosen$n_chosen$Chapter, choose_questions, level = "Hard")})
    
        q_all$q_all <- reduce(list(q_easy(), q_medium(), q_hard()), horizontal_row_bind_list) %>% unlist
      }
    })
  
    observe({
      n_chosen$n_chosen
      #if else to account for the situation where there is no question base in the folder at first 
      if(nrow(questions()) > 0){
        updateSelectizeInput(session, "q_numbers", choices = seq_along(q_all$q_all), selected = seq_along(q_all$q_all), 
                           server = TRUE, options = list(#plugins = list('remove_button'),
                                                         render = I("{item: function(item, escape) {return '<div class=\"item\" onclick=\"Shiny.onInputChange(\\\'q_numbers_click\\\', \\\'' + escape(item.value) + '\\\')\">' + escape(item.value) + '</div>';}}")
                                                         )
                           )
      }else{
      updateSelectizeInput(session, "q_numbers", choices = NULL, selected = NULL,
                           server = TRUE, options = list(#plugins = list('remove_button'),
                             render = I("{item: function(item, escape) {return '<div class=\"item\" onclick=\"Shiny.onInputChange(\\\'q_numbers_click\\\', \\\'' + escape(item.value) + '\\\')\">' + escape(item.value) + '</div>';}}")
                           )
      )
        
        q_all$q_all <- c()
      }
    })

    
   
  
  # output$si <- renderText({input$q_numbers})
  # output$si2 <- renderText({q_all$q_all})
  
  #update the questions list
  observe({
    if(!is.null(input$q_numbers_click) & length(q_all$q_all) > 0){
    compare_question <- questions() %>% filter(Question_number == q_all$q_all[as.integer(input$q_numbers_click)])
    chosen_group <- questions() %>% 
      filter(Chapter == compare_question$Chapter, Level == compare_question$Level,
             !(Question_number %in% q_all$q_all[-as.integer(input$q_numbers_click)])) %>% 
      .$Question
    updateSelectizeInput(session, 'q_search2', choices = chosen_group, selected = compare_question$Question, server = TRUE)
    }
  })
  
  #fill the boxes depending on the chosen question
  observe({
    updateTextInput(session, "change_question2", value = questions() %>% filter(Question == input$q_search2) %>% .$Question)
    
    #filling the possible answers
    walk2( list("a_isp2", "b_isp2", "c_isp2", "d_isp2", "e_isp2"), list(1,2,3,4,5),
           function(id_name, id_number)
           {
             updateTextInput(session, id_name, value = qa() %>% filter(Question == input$q_search2, Answer_number == id_number) %>% .$Possible_answers)
           }
    )
    #filling the chapter, level and correct answer
    walk2(list("change_chapter2", "change_level2", "change_correct2"),
          list("Chapter", "Level", "Answer"),
          function(id_name, parameter)
          {
            updateSelectInput(session, id_name, 
                              choices = questions()[[parameter]] %>% unique %>% sort,
                              selected = (questions() %>% filter(Question == input$q_search2))[[parameter]])
          }
    )
    
  })
  
  observeEvent(input$swap_questions, {
    if(input$swap_questions > 0)
    q_all$q_all[as.integer(input$q_numbers_click)] <- questions() %>% filter(Question == input$change_question2) %>% .$Question_number
  }, ignoreInit = TRUE)
  
  output$generate_test_manual <- downloadHandler(
    
    filename = "Test.html",
    
    
    content = function(file) {
      
      tempReport <- file.path(tempdir(), "test_html.Rmd")
      file.copy("test_html.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(#n_chosen = n_chosen[["n_chosen"]],
        q = questions(),
        n_chosen = n_chosen[["n_chosen"]],
        q_all = q_all$q_all,
        qa = qa(),
        add_correct = input$checkbox,
        course = input$course,
        questions_version = input$questions_version,
        choices_version = input$choices_version,
        test_date = input$test_date
      )
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      
    }
    
  )
  
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
  
}
