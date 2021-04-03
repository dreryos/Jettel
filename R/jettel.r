#Required packages
#usethis::use_package("httr")
#usethis::use_package("jsonlite")
#devtools::document()

#Basic variables
base_url <- "https://trefle.io/api/v1/"

#' Check Token
#'
#' This function checks if user have provided token, if no site for obtaining token show up
#' @param token User token, string, default null.
#' @keywords token
#' @export
#' @examples
#' isToken()
isToken <- function(token = 0){
    if(token == 0){
        print("Please get your own API key at: https://trefle.io/profile", call = F)
        return(F)
    }else{
        return(T)
    }
}

#' Check of accessibility of server
#'
#' This function checks if Treffle API server is accessible
#' @inheritParams isToken
#' @keywords server
#' @export
#' @examples
#' checkStatus()
checkStatus <- function(token = 0){
    res <- httr::GET(paste(base_url, "kingdoms", '?', 'token', '=', token, sep=''))
    if(httr::status_code(res) != 200){
        print(paste("There\' an error:", httr::http_status(httr::status_code(res))$message))
    }else{        
        return(T)
    }
    
}

#' Get all kingdoms
#'
#' This function retrieves kingdoms
#' @inheritParams isToken
#' @return A dataframe of kingdoms
#' @keywords search kingdom
#' @export
#' @examples
#' search.kingdom("token", "Plantae")
search.kingdom <- function(token = 0){
    isToken = F
    checkStatus = F

    if(isToken(token) == T) {
        print("Querying...")
        if(checkStatus(token) == T){
            response <- httr::GET(paste(base_url, "kingdoms/", "?", "token", "=", token, sep=""))
            print("Processing...")
            response_text <- httr::content(response, as = 'text', encoding = 'UTF-8')
            response_json <- jsonlite::fromJSON(response_text, flatten = T)
            response_df <- as.data.frame(response_json)
            #print(paste(length(response_df$data.scientific_name), "plants were found.", sep = " "))
            #print(response_df$data.scientific_name)
            return(response_df)
        }
    }
}

#' Get all plants which satisfy given search criteria
#'
#' This function search plant which satisfies given query
#' @inheritParams isToken
#' @param plant User query, string, default null.
#' @return A dataframe with plants which satisfy given search criteria
#' @keywords search plant
#' @export
#' @examples
#' search.plant("token", "coconut")
search.plant <- function(token = 0, plant = 0){
    isToken = F
    checkStatus = F

    if(isToken(token) == T) {
        print("Querying...")
        if(checkStatus(token) == T){
            response <- httr::GET(paste(base_url, "plants/", "search", "?", "token", "=", token, "&q=", plant, sep=""))
            print("Processing...")
            response_text <- httr::content(response, as = 'text', encoding = 'UTF-8')
            response_json <- jsonlite::fromJSON(response_text, flatten = T)
            response_df <- as.data.frame(response_json)
            #print(paste(length(response_df$data.scientific_name), "plants were found.", sep = " "))
            #print(response_df$data.scientific_name)
            return(response_df)
        }
    }
}

#' Get ID of only one plant selected form searched plants
#' 
#' This function search plant which satisfies given query and lets user choose one plant.
#' @inheritParams search.plant
#' @return A ID of selected plant, from list all plants which satisfy given search criteria
#' @keywords search plant
#' @export
#' @examples
#' retrieve.plant("token", "coconut")
retrieve.plant <- function(token = 0, plant = 0){
    i <- 1
    possibilities <- search.plant(token, plant)
    print(paste(length(possibilities$data.scientific_name), "plants were found.", sep = " "))
    poss <- 1:length(possibilities$data.scientific_name)
    while (i <= length(possibilities$data.scientific_name)){
        print(paste(i, possibilities$data.scientific_name[i], sep = ": "))
        i <- i + 1
    }
    user_sel <- readline("Please select plant with given number: ")
    if(!(user_sel %in% poss)){
        print("Invalid option selected")
    }else{
        return(possibilities$data.id)
    }
}

#' Frontend GUI for this package
#' 
#' Simple GUI for this package created using tcltk
#' @inheritParams 
#' @keywords GUI
#' @export
#' @examples
#' jettelGUI()
jettel.gui <- function(){
    search.query <- tcltk::tclVar("")
    key <- tcltk::tclVar("")

    key.gui <- function(){
       key.root <- tcltk::tktoplevel(gui)
       key.eneter.frame <- 
       key.enter <- tcltk::tkentry(key.root, textvariable = key)
       key.but <- tcltk::tkbutton(key.root, text="Enter")
       tcltk::tkpack(key.enter, key.but, side = "left")
    }

    key.check <- function(token){
        if(token == ""){
            return("Enter API key!")
        }else{
            return("Key is OK")
        }
    }
    gui <- tcltk::tktoplevel()
    gui.wel <- tcltk::tklabel(gui, text = "Welcome to Trefle search")    
    key.check.frame <- tcltk::tkframe(gui)    
    key.check.lab <- tcltk::tkmessage(key.check.frame, text = key.check(tcltk::tclvalue(key)))
    key.check.but <- tcltk::tkbutton(key.check.frame, text="Enter key", command = key.gui)
    search.frame <- tcltk::tkframe(gui)
    search.lab <- tcltk::tklabel(search.frame, text = "Enter search query:")
    search.entry <- tcltk::tkentry(search.frame, textvariable = search.query)
    search.button <- tcltk::tkbutton(search.frame, text = "Push", command = function(){print(as.character(tcltk::tclvalue(search.query)))})
    tcltk::tkpack(gui.wel, key.check.frame, search.frame, side = "top")
    tcltk::tkpack(key.check.lab, key.check.but, side = "left")
    tcltk::tkpack(search.lab, search.entry, search.button, side = "left")
}