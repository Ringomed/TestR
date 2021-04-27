library(shiny)
library(DT)
library(knitr)
library(rmarkdown)
library(kableExtra)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(ggplot2)
library(ggthemes)
library(plotly)
library(rlang)
library(shinyjs)
library(shinyalert)
library(shinythemes)
library(pander)
library(shinydashboard)
#library(V8)
library(bslib)
library(thematic)
library(forcats)
library(shinyFiles)
library(shinyBS)
library(bsplus)
#library(stringi)
library(pagedown)

ggplot2::theme_set(ggplot2::theme_minimal())
thematic::thematic_shiny()

light <- bs_theme(version = 4, bootswatch = "flatly", primary = "#4272A2", secondary = "#DBE9EE")#"#2F88C4")
dark <- bs_theme(version = 4, bootswatch = "flatly", 
                 primary = "#5A869C", secondary = "#8FACBA",#"#4272A2",
                 bg = "#222222", fg = "#FFFFFF")

mt <- theme(
  axis.text = element_text(size = rel(1.1)),
  axis.title = element_text(size = rel(1.15)),
  axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), angle = 90),
  axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
  legend.text = element_text(size = 12),
  legend.title = element_text(size = 12),
  legend.position = "bottom"
)

questions_bars <- function(data, level, bars_fill = "#DBE9EE"){
  data %>%
    filter(Level == level) %>%
    mutate(Chapter = as_factor(Chapter)) %>%
    count(Chapter) %>%
    ggplot(aes(fct_reorder(Chapter, n, .desc = FALSE, .fun = sum), n)) +
    geom_col(fill = bars_fill, width = 0.8) +
    coord_flip() +
    labs(x = "Chapter", y = "Number of questions") +
    mt
}

my_theme <- function(bs = 12)
{
  theme_minimal(base_size = bs, base_family = "") %+replace%
    theme(
          axis.text = element_text(size = rel(1)),
          axis.title = element_text(size = rel(1.2)),
          axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), angle = 90),
          axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))
          #, plot.background = element_blank()
          #,plot.margin = margin(2, 2, 2, 2, "cm")
          )
}

bs4_card <- function(body, title) {
  div(
    class = "card",
    div(class = "card-header bg-primary", title),
    div(class = "card-body d-flex justify-content-center", body)
  )
}


map_choices_ui <- function(q_id, q_answers, col_wd = 12, answ_wd = "85%") 
{
  fluidRow(
    column(col_wd,
           textInput(
             q_id, 
             q_answers,
             width = answ_wd
           )
    )
  )
}

bs4_card_tabset <- function(body, title) {
  div(
    class = "card",
    div(class = "card-header bg-primary", title),
    div(class = "tabset_body", body),
  )
}

bs4_card_in <- function(body, title) {
  div(
    class = "card",
    div(class = "card-header bg-secondary text-center", title),
    div(class = "tabset_body text-center", body),
  )
}

questions <- tibble(
  Chapter = c(rep("01_Demo chapter", 3), rep("02_Demo chapter", 3), rep("03_Demo chapter", 2)),
  Question_number = 1:8,
  Question = paste0("Demo question ", 1:8),
  Answer = "c",
  Level = rep_len(c("Easy", "Medium", "Hard"), 8)
  , Added = c(rep(Sys.Date() - 25, 3), rep(Sys.Date() - 12, 2), rep(Sys.Date() - 7, 2), Sys.Date())
  , Changed = c(rep(Sys.Date() - 3, 3), rep(Sys.Date() - 2, 2), rep(Sys.Date() - 7, 2), Sys.Date())
)

choices <- tibble(
  Chapter = c(rep("01_Demo chapter", 15), rep("02_Demo chapter", 15), rep("03_Demo chapter", 10)),
  Question_number = map(1:8, function(x) c(rep(x, 5))) %>% unlist,
  Possible_answers = rep(paste0("Demo choice ", 1:5), 8),
  Answer_number = rep(1:5, 8)
)

n_chosen <- tibble(
  Chapter = unique(questions$Chapter),
  Easy = c(1, 1, 0),
  Medium = c(1, 1, 1),
  Hard = c(0, 1, 0)
)

about_text <- "TestR is a test generating app created by <a href='http://www.mefst.unist.hr/nastava/katedre/medicinska-fizika-i-biofizika-631/nastavnici-1047/zvonimir-boban-mag-phys/7389' target='_blank'>Zvonimir Boban</a>
, a physicist and a teaching/research assistant at the University of Split School of Medicine. It was created out of a desire to have a free and customizable 
software for quick generation of exams combined with an interest in all things related to R. It enables the user to create and alter a test base 
of multiple choice questions and generate tests using R Markdown.<br> The material for the demo question bases was mostly taken from 
<a href='https://www.usefultrivia.com/index.html' target='_blank'>Useful Trivia</a>. The hex sticker for the app was
created using the <a href='https://connect.thinkr.fr/hexmake/' target='_blank'>Hexmake Shiny app</a>."

overview_text <- "This tab gives an overview of the number and type of questions in the question base. The default base choice
is a demo question base. In order to select a different database, click on the <em>Choose directory</em> button and select the
folder from the dropdown list. The question base consists of two .csv files - questions.csv and choices.csv - so the chosen folder
should contain those files. If they are not present, they will be created, i.e. an empty question base will be created. The options 
card also contains the <em>Dark mode</em> switch which changes the theme of the app to a darker one, creating a more eye-friendly
surrounding. This is very beneficial in situations where you have to insert a lot of new questions in the question base.<br>
Aside from the numerical summaries of the question base, there are also three types of graphical summaries as well. Those
inform the user about the rate of question additions in the last month, the distribution of questions in terms of their difficulties and the number
of questions depending on the chapter/difficulty combination."

generate_exam_text <- "The interface of the tab is dominated by two tables. The right one gives insight into the number of questions for a
given chapter-difficulty combination. Subgroups with zero questions are coloured red. The left one is editable and allows the user to choose the number of question in the 
exam from each subgroup. If the user specifies a number larger than the total number in that subgroup, a message will pop up
warning them of that fact. If not changed directly, the subgroup number will revert back to its previous value upon the
next edit of any of the table values.<br>
The sidebar menu contains a summary of number of questions in the test, a set of options that affect the content of the test,
and download buttons which generate the test or the complete question base upon being clicked. Most of the options are self-explanatory, 
but two options stand out - <em>Choose questions</em> and <em>Choose group</em>. Namely, the questions are being chosen 'randomly' given
the number of questions in each subgroup and the seed for the question sampler. The <em>Choose questions</em> cell sets 
this seed. The <em>Choose group</em> cell is filled with a positive integer indicating the test group (1 = Group A, 2 = Group B, ...).
All test groups created this way have the same question selection, but the order of choices is different in order to
prevent cheating.<br>
The test and the question base are rendered as HTML documents, but they can easily be turned to a pdf format by opening them
in a Google Chrome (or Chromium on Linux) browser and running the print menu. From there, instead of choosing a printer device, 
you can choose the 'Save as PDF' option."

manual_exam_change_text <- "To use this tab, you first have to choose the number of question from the chapter-level
subgroup in the editable table from the <em>Generate exam</em> tab. Once you do that, on the right side of this tab, the <em>Question numbers</em>
field should be filled up with a sequence of numbers ranging from 1 to the total number of questions in the test. By clicking
on any of those numbers, the corresponding question will be displayed on the left. If you want to swap this question for another, you
can use the search bar in the top left corner. The search bar choices contain the current question along with
all questions from the same chapter-level subgroup that are not already in the test. For example, if a subgroup has 7 questions
in total, and you choose to place six questions in the test from that subgroup, after clicking on one of those questions, the search bar
will contain two choices - the currently clicked question and the one that isn't already in the test (6/7 are already in the test,
so only one remains). To confirm the swap, click on the <em>Swap questions</em> button. If you are satisfied with the new
composition of the test you can again generate it using the <em>Generate test</em> button"

add_questions_text <- "Here you can add new questions to the question base by filling up all the related fields. Alongside regular 
text and HTML elements (you can for example create lists using li HTML elements), mathematical expressions are also supported through Latex syntax (tested on Google Chrome). Once you 
fill all the necessary information, you can write the question to the base by clicking on the <em>Add question</em> button. If the question base 
doesn't already contain a question from a certain chapter, you can manually add a new chapter as a choice in the dropdown list by typing the new chapter
name and pressing enter on the keyboard.<br>
If you have made a mistake you can delete the last added question using the <em>Delete last question</em> button.<br>
The table on the right side will refresh with every change to the question base. If the order of questions in the test matters, the
chapter names should have a number prefix (e.g. 01_Chapter name 1, 02_Chapter name 2)"

change_delete_text <- "The user interface of this tab is very similar to the interface of the previous tab. The difference is that 
this tab is not meant to add new questions, but to alter or delete previously existing one. To find a specific question, use 
the search bar in the top left corner. Once you select a question, the remaining fields will be filled up accordingly. Now
you can make the changes and save them using the <em>Change question</em> button ot delete the whole question by clicking 
on the <em>Delete question</em> button."