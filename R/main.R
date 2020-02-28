#' init_db
#' @export

init_db <- function(user = NA, path = NA){
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  
  if(length(setdiff(c("com", "dk"), DBI::dbListTables(con))) != 0){
    con %>% DBI::dbWriteTable("com", tibble::tibble(user = user, pageid_1 = NA, pageid_2 = NA, more_left = NA, time = NA, type = NA), overwrite = T)
    con %>% DBI::dbWriteTable("dk", tibble::tibble(user = user, pageid = NA, name = NA, time = NA), overwrite = T)
  }
  
  existing_users <- con %>% dplyr::tbl("com") %>% dplyr::pull(user) %>% unique
  if(!user %in% existing_users){
    con %>% DBI::dbWriteTable("com", tibble::tibble(user = user, pageid_1 = NA, pageid_2 = NA, more_left = NA, time = NA, type = NA), append = T)
    con %>% DBI::dbWriteTable("dk", tibble::tibble(user = user, pageid = NA, name = NA, time =), append = T)
  }
  
  return(con)
}

#' add_user 
#' @export
add_user <- function(user, password){
  new <- shinyuser::user$new("data/users")
  new$username <- user
  new$password <- password
  new$load("root1")
  new$update()
}

#' get_pair_matrix
#' @export

get_pair_matrix <- function(n_mp = NULL){
  if(!is.null(n_mp)){
    tmp_mp <- pairwiseR::mp %>%
      arrange(desc(traffic)) %>%
      slice(1:!!n_mp)
  } else {
    tmp_mp <- pairwiseR::mp
  }
  
  pair_mps <- tidyr::expand_grid(pageid_1 = tmp_mp$pageid, pageid_2 = tmp_mp$pageid) %>%
    dplyr::filter(pageid_1 != pageid_2) %>%
    dplyr::left_join(dplyr::select(pairwiseR::mp, pageid_1 = pageid, name_1 = name), by = "pageid_1") %>%
    dplyr::left_join(dplyr::select(pairwiseR::mp, pageid_2 = pageid, name_2 = name), by = "pageid_2")
  
  return(pair_mps)
  
}


#' get_dk
#' @export
get_dk <- function(con, user){
  con %>%
    dplyr::tbl("dk") %>%
    dplyr::collect() %>%
    dplyr::filter(user == {{user}})
}

#' get_already
#' @export
get_already <- function(con, user){
  con %>%
    dplyr::tbl("com") %>%
    dplyr::collect(.) %>%
    dplyr::filter(user == {{user}})
}

#' get_new_pair
#' @export

get_new_pair <- function(user = NA, con = NA, pair_mp = NULL, pageid_1 = NULL, pageid_2 = NULL){
  
  dk <- con %>% pairwiseR::get_dk({{user}})
  already <- con %>% pairwiseR::get_already({{user}}) %>%
    dplyr::filter_at(dplyr::vars(dplyr::contains("pageid")), ~!.x %in% dk$pageid)
  message("\n\n")
  cli::cli_alert_info("Compared manually: {nrow(filter(already, type == 'user')) %/% 2}")
  cli::cli_alert_info("Compared analytically: {nrow(filter(already, type == 'auto')) %/% 2}")
  
  tmp_mp <- pair_mp %>%
    dplyr::filter_at(dplyr::vars(dplyr::contains("pageid")), ~!.x %in% dk$pageid)
  
  cli::cli_alert_info("To compare: {(nrow(tmp_mp) - nrow(already)) %/% 2}")
  
  to_include <- pairwiseR::mp %>%
    dplyr::filter(!pageid %in% dk$pageid) %>%
    dplyr::arrange(dplyr::desc(traffic)) %>%
    dplyr::slice(1:(nrow(already) %/% 2 + 2 )) %>%
    dplyr::pull(pageid)
  
  new_pairs <- tmp_mp %>%
    dplyr::anti_join(already, by = c("pageid_1", "pageid_2")) %>%
    dplyr::filter(pageid_1 %in% to_include) %>%
    dplyr::filter(pageid_2 %in% to_include) %>%
    dplyr::mutate(party_1 = "", party_2 = "")
  
  if(nrow(new_pairs) == 0){
    cli::cli_alert_success("All pairs have been coded")
    return(NULL)
  }
  
  if(!is.null(pageid_1) ){
    if(pageid_1 %in% new_pairs$pageid_1){
      new_pairs <- new_pairs %>%
        filter(pageid_1 == !!pageid_1)
    }
  } else {
    if(!is.null(pageid_2) ){
      if(pageid_2 %in% new_pairs$pageid_2){
        new_pairs <- new_pairs %>%
          filter(pageid_2 == !!pageid_2)
      }
    }
  }
  
  
  return(dplyr::sample_n(new_pairs, 1))
}



#' add_dont_know
#' @export

add_dont_know <- function(user = NA, pageid = NA, name = NA, con = NULL){
  cli::cli_alert_danger("Ignored: {name}")
  con %>% DBI::dbWriteTable("dk", tibble::tibble(user, pageid, name, time = as.numeric(lubridate::now())), append = T)
}


#' add_comparison
#' @export

add_comparison <- function(user = NA, pageid_1 = NA, pageid_2 = NA, name_1 = NA, name_2 = NA,
                           more_left = NA, time = NA, con = NULL, par_anal = 3){
  
  if(more_left == 1){
    cli::cli_alert_success("{name_1} ({pageid_1}) > {name_2} ({pageid_2})")
  } else if (more_left == -1 ) {
    cli::cli_alert_success("{name_1} ({pageid_1}) < {name_2} ({pageid_2})")
  } else {
    cli::cli_alert_success("{name_1} ({pageid_1}) = {name_2} ({pageid_2})")
  }
  
  out_1 <- tibble::tibble(user, pageid_1, pageid_2, more_left, time = {{time}}, type = "user")
  out_2 <- tibble::tibble(user, pageid_2 = pageid_1, pageid_1 = {{pageid_2}}, more_left = -more_left, time = {{time}}, type = "user")
  out <- dplyr::bind_rows(out_1, out_2)
  con %>% DBI::dbWriteTable("com", out, append = T)
  
  solved <- con %>% 
    get_already(user = {{user}}) %>%
    pairwiseR::get_analytically_solved(quiet = F, par = {{par_anal}}) %>%
    dplyr::mutate(user = user, time = {{time}}, type = "auto")
  
  
  con %>% DBI::dbWriteTable("com", solved, append = T)
  
  
}

#' get_analytically_solved
#' @export

get_analytically_solved <- function(already, quiet = T, par = 3){
  
  # if(!quiet){message("Analytical param: ", par)}
  
  inp <- already %>%
    dplyr::filter(more_left != 0) %>%
    dplyr::mutate(ideo = case_when(
      more_left == -1 ~ "right", 
      more_left == 1 ~ "left")
    ) %>%
    dplyr::select(dplyr::contains("pageid"), ideo) %>%
    dplyr::group_by(pageid_1, ideo) %>%
    dplyr::filter(n() >= {{par}}) %>%
    dplyr::ungroup() %>%
    unique %>%
    dplyr::group_by(pageid_1, ideo) %>%
    dplyr::summarise(pageid_2 = list(pageid_2)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(id_cols = pageid_1, names_from = ideo, values_from = pageid_2)
  
  
  if(nrow(inp) <= 1 | !"left" %in% colnames(inp) | !"right" %in% colnames(inp) ){return(tibble::tibble())}
  out <- tidyr::expand_grid(dplyr::select(inp, pageid_1, left), 
                            dplyr::select(inp, pageid_2 = pageid_1, right)) %>%
    dplyr::filter(pageid_1 != pageid_2) %>%
    dplyr::filter_at(vars(left, right), ~!map_lgl(.x, is.null)) %>%
    dplyr::mutate(n_inter = purrr::map2_dbl(left, right, ~length(intersect(.x, .y)))) %>%
    dplyr::filter(n_inter >= {{par}}) %>%
    dplyr::select(contains("pageid")) %>%
    dplyr::mutate(more_left = -1) %>%
    dplyr::anti_join(already, by = c("pageid_1", "pageid_2"))
  
  if(!quiet){cli::cli_alert_success("Analytically solved: {nrow(out) %/% 2}")}
  sym_out <- tibble::tibble(pageid_1 = out$pageid_2, 
                            pageid_2 = out$pageid_1, 
                            more_left = -out$more_left) 
  
  
  return(dplyr::bind_rows(out, sym_out))
  
}

#' simule_ranking
#' @export
simule_ranking <- function(pageid){
  ranking <- tibble::tibble(pageid) %>%
    dplyr::sample_n(n()) %>%
    dplyr::mutate(rank = 1:n())
  
  tidyr::expand_grid(pageid_1 = pageid, pageid_2 = pageid) %>%
    dplyr::filter(pageid_1 != pageid_2) %>%
    dplyr::left_join(ranking, by = c("pageid_1" = "pageid")) %>%
    dplyr::rename(rank_1 = rank) %>%
    dplyr::left_join(ranking, by = c("pageid_2" = "pageid")) %>%
    dplyr::rename(rank_2 = rank) %>%
    dplyr::mutate(more_left = ifelse(rank_1 > rank_2, 1, -1)) %>%
    dplyr::select(-contains("rank"))
  
}

#' remove_last_action
#' @export
remove_last_action <- function(con, user){
  
  com_time <- con %>%
    dplyr::tbl("com") %>%
    dplyr::collect() %>%
    dplyr::filter(user == !!user) %>%
    mutate(time = round(time, 0)) %>%
    # arrange(-time)
    dplyr::filter(time == max(time, na.rm = T))
  
  
  dk_time <- con %>%
    dplyr::tbl("dk") %>%
    dplyr::collect() %>%
    dplyr::filter(user == !!user) %>%
    dplyr::filter(time == max(time, na.rm = T)) 
  
  if(nrow(dk_time) == 0 & nrow(com_time) == 0){
    cli::cli_alert_info("No action to cancel")
    return("No action to cancel")
  }
  
  if(max(com_time$time, na.rm = T) > max(dk_time$time, na.rm = T)){
    
    out <- glue::glue("Cancelling comparison between {pairwiseR::mp$name[ pairwiseR::mp$pageid == com_time$pageid_1[1]]} and {pairwiseR::mp$name[ pairwiseR::mp$pageid == com_time$pageid_2[1]]}")
    cli::cli_alert_warning("Cancelling {com_time$pageid_1[1]} | {com_time$pageid_2[1]}")
    
    com_time %>%
      split(1:nrow(.)) %>% 
      purrr::walk(~{
        
        query <- glue::glue_sql("DELETE FROM com
                          WHERE pageid_1 = {.x$pageid_1} AND pageid_2 = {.x$pageid_2};", .con = con)
        RSQLite::dbExecute(con, query)
      })
    
    return(out)
    
  } else {
    
    out <- glue::glue("Cancel ignoring {pairwiseR::mp$name[ pairwiseR::mp$pageid == dk_time$pageid]}")
    cli::cli_alert_warning("Cancel ignoring {dk_time$pageid}")
    
    dk_time %>%
      split(1:nrow(.)) %>% 
      purrr::walk(~{
        query <- glue::glue_sql("DELETE FROM dk
                          WHERE pageid = {.x$pageid};", .con = con)
        RSQLite::dbExecute(con, query)
      })
    
    return(out)
  }
}

#' add_user_db
#' @export

# add_user_db <- function(user, password, signed_in, role, debug = F){
#   con <- dbConnect(RSQLite::SQLite(), "data/user.db")
#   
#   existing_users <- con %>% dplyr::tbl("users") %>% dplyr::pull(user)
#   if(debug){print(existing_users)}
#   if(user %in% existing_users){
#     message("User already exists")
#   } else {
#     new_user <- tibble(user, password, signed_in, role)
#     dbWriteTable(con, "users", new_user, append = T)
#     message(glue::glue("User <{ user }> with password <{ password }> was created"))
#     
#   }
#   dbDisconnect(con)
# }