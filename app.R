# pacman::p_load(devtools, shiny, shiny.semantic, semantic.dashboard, tidyverse, DT,
#                RSQLite, dbplyr, R6, shinyjs, shinytoastr, shinyuser, pairwiseR)


# devtools::install_github("systats/shinyuser")
# install.packages("V8")

n_mp <- 20 #nrow(pairwiseR::mp)
n_comp_pro_user <- 10
par_anal <- 2

library(shiny)
library(shiny.semantic)
library(shinyjs)
library(semantic.dashboard)
library(tidyverse)
library(shinytoastr)
devtools::load_all()

# library(dplyr)
# library(stringr)
# library(purrr)
# library(jsonlite)
# library(R6)
# library(RSQLite)
# library(V8)

library(shinyuser)
library(googlesheets4)
sheets_deauth()

#' get_pair_matrix
#' @export
ui <- shiny.semantic::semanticPage(
    shiny::tags$head(
        shiny::tags$link(rel="stylesheet", href="styles/main.css")
    ),
    # dashboardHeader(
    #     inverted = T, 
    #     shinyuser::manager_ui("manager")
    # ),
    shinytoastr::useToastr(),
    shinyjs::useShinyjs(),
    div(class = "ui text container",
        vignette_ui("action"),
        br(),
        div(class="ui progress", id = "pro",
            div(class="bar",
                div(class="progress")
            )
        )
    )
)

server <- function(input, output, session){
    
    #' inerheritance from previous version
    party <- "all"
    .GlobalEnv$next_pageid_1 <- NULL
    .GlobalEnv$next_pageid_2 <- NULL
    
    ### User authentification
    user_sheet <- "https://docs.google.com/spreadsheets/d/1ZbSSxaMuf0fV5_2exz69ahOMZH46bNwlkXSKyOjYD5w/edit?usp=sharing"
    user <- callModule(login_server, "user", user_sheet)
    
    ### User managment
    callModule(shinyuser::manager_server, "manager", user)
    ### Authorized content
    output$authorized <- renderUI({ 
        print(user())
        if(user()$status == 1){
            ui 
        } else { 
            shinyuser::login_ui("user", "", signin = T, recover = F, label_login = "User", label_pw = "Passwort")
        } 
    })
    
    observe({
        req(user())
        action()
        value <- pairwiseR::init_db(user = user()$username, path = "data/mp.db")   %>%
            dplyr::tbl("com") %>%
            dplyr::as_tibble() %>%
            dplyr::filter(user == user()$username) %>%
            dplyr::filter(type == "user") %>%
            nrow
        #value <- nrow(get_already(con, user())) #%/% 2
        shinyjs::runjs(glue::glue("$('#pro').progress({value: <value %/% 2>, total: <n_comp_pro_user>});", .open = "<", .close = ">"))
    })
    
    
    
    log <- reactiveValues(state = 0)
    logstate <- reactive({ log$state })
    
    pair <- reactive({
        
        req(user())
        if(user()$status == 0) return(NULL)
        
        logstate()
        con <- pairwiseR::init_db(user = user()$username, path = "data/mp.db")
        
        new_pair <- pairwiseR::get_new_pair(user = user()$username,
                                            con = con,
                                            pair_mp = get_pair_matrix(n_mp = n_mp),
                                            pageid_1  = .GlobalEnv$next_pageid_1, 
                                            pageid_2  = .GlobalEnv$next_pageid_2 )
        return(new_pair)
    })
    
    action <- callModule(vignette_server, "action", pair, user = user()$username)
    
    observe({
        req(action())
        req(user())
        
        con <- pairwiseR::init_db(user = user()$username, path = "data/mp.db")
        pair <- isolate(pair())
        
        if(con %>% get_already(user()$user) %>% filter(type == "user") %>% nrow %>% magrittr::is_weakly_greater_than(n_comp_pro_user*2)){
            shinytoastr::toastr_success("Danke für Ihre Teilnahme an unserer Umfrage. Wir werden uns bald melden, um den Vergütungsdetails zu regeln.")
        }
        if(is.null(pair$pageid_1)){
            shinytoastr::toastr_success("Danke für Ihre Teilnahme an unserer Umfrage. Wir werden uns bald melden, um den Vergütungsdetails zu regeln.")
        }
        
        if(!is.null(pair$pageid_1)){
            cli::cli_alert_success("\n{user()$user} ~ {action()}")
        }
        DBI::dbDisconnect(con) 
    })
    
    observeEvent(action(), {
        
        if(!is.null(.GlobalEnv$next_pageid_1)){.GlobalEnv$next_pageid_1 <- NULL}
        if(!is.null(.GlobalEnv$next_pageid_2)){.GlobalEnv$next_pageid_2 <- NULL}
        
        con <- pairwiseR::init_db(user = user()$username, path = "data/mp.db") #, force = T
        
        if(!is.null(pair()$pageid_1)){
            if(stringr::str_detect(action(), "ignore")){
                if(stringr::str_detect(action(), "a")){
                    add_dont_know(user = user()$username, pageid = pair()$pageid_1, name = pair()$name_1, con = con)
                    .GlobalEnv$next_pageid_2 <- pair()$pageid_2
                } else if(stringr::str_detect(action(), "b")){
                    add_dont_know(user = user()$username, pageid = pair()$pageid_2, name = pair()$name_2, con = con)
                    .GlobalEnv$next_pageid_1 <- pair()$pageid_1
                } else {
                    add_dont_know(user = user()$username, pageid = pair()$pageid_1, name = pair()$name_1, con = con)
                    add_dont_know(user = user()$username, pageid = pair()$pageid_2, name = pair()$name_2, con = con)
                }
            }
            
            
            if(stringr::str_detect(action(), "^(a|b)b?$")){
                
                if(action() == "a") outcome <- 1
                if(action() == "b") outcome <- -1
                if(action() == "ab") outcome <- 0
                
                add_comparison(user = user()$username,
                               pageid_1 = pair()$pageid_1,
                               pageid_2 = pair()$pageid_2,
                               name_1 = pair()$name_1,
                               pair()$name_2,
                               par_anal = par_anal,
                               more_left = outcome,
                               time = !!lubridate::now(),
                               con = con
                )
            }
        }
        log$state <- 1
        log$state <- 0
    })
}

shinyApp(shinyuser::meta_ui(), server)


