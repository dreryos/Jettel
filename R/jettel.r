#Required packages
#usethis::use_package("httr")
#usethis::use_package("jsonlite")
#devtools::document()

#Basic variables
base_url <- "https://trefle.io/api/v1/"
library(geojsonio)
geo <- geojsonio::geojson_read("https://raw.githubusercontent.com/tdwg/wgsrpd/master/geojson/level3.geojson",  what = "sp")

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
#' search.kingdom("token")
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
search.plant <- function(token = 0, plant = ""){
    isToken = F
    checkStatus = F

    if(isToken(token) == T) {
        print("Querying...")
        if(checkStatus(token) == T){
            response <- httr::GET(paste(base_url, "plants/", "search", "?", "token", "=", token, "&q=", plant, sep=""))
            print("Processing...")
            response_text <- httr::content(response, as = 'text', encoding = 'UTF-8')
            response_json <- jsonlite::fromJSON(response_text, flatten = T)   
            print(response_json)         
            if(response_json$meta$total != 0){
                response_df <- as.data.frame(response_json)
                return(response_df)
            }else{
                return("Blank")
            }
        }
    }
}

#' Get all species which satisfy given search criteria
#'
#' This function search species which satisfies given query
#' @inheritParams isToken
#' @param species User query, string, default null.
#' @param page Number of page, integer, default 1. 
#' @return A dataframe with species which satisfy given search criteria
#' @keywords search plant
#' @export
#' @examples
#' search.plant("token", "coconut")
search.species <- function(token = 0, species = "", page = 1){
    isToken = F
    checkStatus = F

    if(isToken(token) == T) {
        print("Querying...")
        if(checkStatus(token) == T){
            response <- httr::GET(paste(base_url, "species/", "search", "?", "token", "=", token, "&q=", species, sep="", "&page=", page))
            print("Processing...")
            response_text <- httr::content(response, as = 'text', encoding = 'UTF-8')
            response_json <- jsonlite::fromJSON(response_text, flatten = T)
            if(is.null(response_json$error) != TRUE){
                tcltk::tkmessageBox(icon = "warning", message = response_json$message, type = "ok")
            }else{
                if(response_json$meta$total != 0){
                    response_df <- as.data.frame(response_json)
                    return(response_df)
                }else{
                    return("Blank")
            }}
        }
    }
}

#' Get dataframe of only one plant selected form searched plants
#' 
#' This function search plant which satisfies given query and lets user choose one plant.
#' @inheritParams search.plant
#' @return A dataframe of selected plant, from list all plants which satisfy given search criteria
#' @keywords search plant
#' @export
#' @examples
#' retrieve.plant("token", "coconut")
retrieve.plant <- function(token = 0, plant = ""){
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
        print("Processing...")
        sel_plant_resp <- httr::GET(paste(base_url, "plants/", possibilities[user_sel, "data.id"], "?", "token", "=", token, sep=""))
        sel_plant_resp_text <- httr::content(sel_plant_resp, as = 'text', encoding = 'UTF-8')
        sel_plant_resp_json <- jsonlite::fromJSON(sel_plant_resp_text, flatten = T)
        return(sel_plant_resp_json)
    }
}

#' Get dataframe of only one species selected form searched plants
#' 
#' This function search plant which satisfies given query and lets user choose one plant.
#' @inheritParams isToken
#' @param species User query, string, default null.
#' @return A dataframe of selected species, from list all plants which satisfy given search criteria
#' @keywords search species
#' @export
#' @examples
#' retrieve.species("token", "coconut")
retrieve.species <- function(token = 0, species = ""){
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
        print("Processing...")
        sel_plant_resp <- httr::GET(paste(base_url, "species/", possibilities[user_sel, "data.id"], "?", "token", "=", token, sep=""))
        sel_plant_resp_text <- httr::content(sel_plant_resp, as = 'text', encoding = 'UTF-8')
        sel_plant_resp_json <- jsonlite::fromJSON(sel_plant_resp_text, flatten = T)
        return(sel_plant_resp_json)
    }
}

#' Get JSON list of species by given ID
#' 
#' This function return list of data of given species
#' @inheritParams isToken
#' @param id ID of species, integer, default null.
#' @return List of data of given species
#' @keywords data species
#' @export
#' @examples
#' get.species.by.id("token", 000001)
get.species.by.id <- function(token = 0, id = 0){
    print("Querying...")
    sel_plant_resp <- httr::GET(paste(base_url, "species/", id, "?", "token", "=", token, sep=""))
    print("Processing...")
    sel_plant_resp_text <- httr::content(sel_plant_resp, as = 'text', encoding = 'UTF-8')
    sel_plant_resp_json <- jsonlite::fromJSON(sel_plant_resp_text, flatten = T)
    return(sel_plant_resp_json)
}

#' Frontend GUI for this package
#' 
#' Simple GUI for this package created using tcltk
#' @keywords GUI
#' @export
jettel.gui <- function(){
    search.query <- tcltk::tclVar("")
    key <- tcltk::tclVar("")
    key.status <- tcltk::tclVar("")
    pageW <- tcltk::tclVar("1")

    unknown.check <- function(term){
        if(is.null(term) == TRUE){
            return("Unknown")
        }else{
            return(term)
        }
    }

    create.image <- function(image, frame, name){
        if(is.null(image) != TRUE){
            image.url <- image
            z <- paste(tempfile(), ".jpg", sep = "")
            download.file(image.url,z,mode="wb")                                    
            jpgPIC <- jpeg::readJPEG(z)
            y <- sub(".jpg", ".png", z)
            png::writePNG(jpgPIC, target = y)
            file.remove(z)
            tcltk::tcl("image", "create", "photo", name, file = y, height = 600)
            file.remove(y)
            plant.image <- tcltk::ttklabel(frame, image = name, compound="image")          
            tcltk::tkpack(plant.image)
        }else{
            no.image <- tcltk::tklabel(frame, text = "There's no image of this entry")
            tcltk::tkpack(no.image)
        }
    }

    list.image <- function(images, parent){
        image.list.win <- tcltk::tktoplevel(parent)
        tcltk::tktitle(image.list.win) <- "Image select"
        image.list <- tcltk::tklistbox(image.list.win, listvariable = tcltk::tclVar(images$id), selectmode = "browse", height = 0, width = 0)
        image.open <- tcltk::tkbutton(image.list.win, text = "Open", command = function(){
            image.view <- tcltk::tktoplevel(image.list.win)
            tcltk::tktitle(image.view) <- "Image viewer"
            create.image(image = images$image_url[images$id == tcltk::tclvalue(tcltk::tkXselection.get())], frame = image.view, name = tcltk::tclvalue(tcltk::tkXselection.get()))
            image.copy <- tcltk::tklabel(image.view, text = images$copyright[images$id == tcltk::tclvalue(tcltk::tkXselection.get())])
            tcltk::tkpack(image.copy, side = "bottom")
        })
        tcltk::tkpack(image.list, image.open)
    }

    key.gui <- function(){
       key.root <- tcltk::tktoplevel(gui)
       tcltk::tktitle(key.root) <- "Enter Trefle API key"
       key.lab <- tcltk::tklabel(key.root, text = "Enter your API key")
       key.obtain.frame <- tcltk::tkframe(key.root)
       key.obtain.lab <- tcltk::tklabel(key.obtain.frame, text = "Key can be obtained:")
       key.obtain.but <- tcltk::tkbutton(key.obtain.frame, text = "here", relief = "flat", fg="#0900ff" , command = function(){
           utils::browseURL("https://trefle.io/users/sign_up")
       })
       key.enter.frame <- tcltk::tkframe(key.root)
       key.enter <- tcltk::tkentry(key.enter.frame, textvariable = key)
       key.but <- tcltk::tkbutton(key.enter.frame, text="Enter", command = function(){
           if(tcltk::tclvalue(key) != ""){
            #unblocks Search button and hide Enter key button
            if(tcltk::tclvalue(key.check(tcltk::tclvalue(key))) == "Key is OK"){
                    key.status <- key.check(tcltk::tclvalue(key))
                    tcltk::tkpack.forget(key.check.lab, key.check.but, search.button)
                    key.check.lab <- tcltk::tkmessage(key.check.frame, text = tcltk::tclvalue(key.status))
                    search.button <- tcltk::tkbutton(search.frame, text = "Search", command = function(){
                        if(tcltk::tclvalue(search.query) != ""){
                            response <- search.species(tcltk::tclvalue(key), tcltk::tclvalue(search.query), as.numeric(tcltk::tclvalue(pageW)))
                            if(response != "Blank"){
                                #creates window for choosing a entry
                                resp.win <- tcltk::tktoplevel(gui)
                                tcltk::tktitle(resp.win) <- "Found plants at Trefle"
                                resp.lab <- tcltk::tklabel(resp.win, text = "Select entry by double-click")
                                list <- tcltk::tklistbox(resp.win, listvariable = tcltk::tclVar(response$data.scientific_name), selectmode = "single", height = 0)
                                tcltk::tkbind(list, '<Double-Button>', function(){
                                    #displays selected entry
                                    entry.win <- tcltk::tktoplevel(resp.win)
                                    sel.plant <- get.species.by.id(token = tcltk::tclvalue(key), id = response$data.id[response$data.scientific_name == tcltk::tclvalue(tcltk::tkXselection.get())])
                                    tcltk::tktitle(entry.win) <- paste("Detail of", sel.plant$data$scientific_name)
                                    #names of plant
                                    entry.label <- tcltk::tklabel(entry.win, text = sel.plant$data$scientific_name, font = (size = 25))
                                    entry.common_name <- tcltk::tklabel(entry.win, text = unknown.check(sel.plant$data$common_name))
                                    #medium section frame
                                    entry.medium.frame <- tcltk::tkframe(entry.win)
                                    #info frame
                                    entry.info.frame <- tcltk::tkframe(entry.medium.frame)
                                    #IDs in database
                                    entry.id.frame <- tcltk::tkframe(entry.info.frame)
                                    entry.id <- tcltk::tklabel(entry.id.frame, text = paste("ID: ", sel.plant$data$id))
                                    entry.slug <- tcltk::tklabel(entry.id.frame, text = sel.plant$data$slug)
                                    tcltk::tkpack(entry.id, entry.slug, side = "left")
                                    #plant bibliography
                                    entry.bib <- tcltk::tklabel(entry.info.frame, text = paste("In: ", unknown.check(sel.plant$data$bibliography), " by: ", unknown.check(sel.plant$data$author)))
                                    #plant family
                                    entry.family.frame <- tcltk::tkframe(entry.info.frame)
                                    entry.family.science <- tcltk::tklabel(entry.family.frame, text = unknown.check(sel.plant$data$family))
                                    entry.family.common <- tcltk::tklabel(entry.family.frame, text = unknown.check(sel.plant$data$family_common_name))
                                    tcltk::tkpack(entry.family.science, entry.family.common, side = "left")
                                    #plant synonyms
                                    entry.syn.frame <- tcltk::tkframe(entry.info.frame)
                                    entry.syn.lab <- tcltk::tklabel(entry.syn.frame, text = paste("List of synonyms (", length(sel.plant$data$synonyms[,"name"]) ,") (might be scrollable)", sep =""))
                                    entry.syn <- tcltk::tklistbox(entry.syn.frame, listvariable = tcltk::tclVar(unknown.check(unlist(sel.plant$data$synonyms[,"name"]))), height = 3, selectmode = "browse", width = 0)
                                    #entry.syn.src <- tcltk::ttkscrollbar(entry.syn.frame)
                                    #tcltk::tkconfigure(entry.syn, yscrollcommand = tcltk::tkset(entry.syn.src))
                                    #tcltk::tkconfigure(entry.syn.src, command = tcltk::tkyview(entry.syn))
                                    tcltk::tkpack(entry.syn.lab, entry.syn, side = "top")
                                    tcltk::tkpack(entry.id.frame, entry.bib, entry.family.frame, entry.syn.frame)
                                    #Distribution map
                                    entry.distribution.frame <- tcltk::tkframe(entry.medium.frame)
                                    entry.distribution.but <- tcltk::tkbutton(entry.distribution.frame, text = "Map of distribution", command = function(){
                                        if(is.null(unlist(sel.plant$data$distribution)) != TRUE){                                            
                                            file <- tempfile(fileext = ".png")
                                            png(file, width = 900)
                                            plot(geo)
                                            if(is.null(unlist(sel.plant$data$distribution$native)) != TRUE){
                                                for(n in sel.plant$data$distribution$native){
                                                    plot(geo[geo@data$LEVEL3_NAM == n, ], col = "red", add = T)
                                                }}
                                            if(is.null(unlist(sel.plant$data$distribution$introduced)) != TRUE){
                                                for(i in sel.plant$data$distribution$introduced){
                                                plot(geo[geo@data$LEVEL3_NAM == i, ], col = "yellow", add = T)
                                            }}
                                            legend(x = "topright", legend = c("Native", "Introduced"), fill = c("red", "yellow"))
                                            dev.off()
                                            map <- tcltk::tktoplevel(entry.win)
                                            tcltk::tktitle(map) <- paste("Map of",sel.plant$data$scientific_name)
                                            tcltk::tcl("image", "create", "photo", "imageMap", file = file)
                                            file.remove(file)
                                            map.image <- tcltk::ttklabel(map, image = "imageMap", compound="image")
                                            map.text.frame <- tcltk::tkframe(map)
                                            text.native.frame <- tcltk::tkframe(map.text.frame)
                                            text.introduced.frame <- tcltk::tkframe(map.text.frame)
                                            if(is.null(unlist(sel.plant$data$distribution$native)) != TRUE){
                                                text.native <- tcltk::tklabel(text.native.frame, text = "Native locations:")
                                                list.native <- tcltk::tklistbox(text.native.frame, listvariable = tcltk::tclVar(unlist(sel.plant$data$distribution$native)))
                                                tcltk::tkpack(text.native, list.native)}
                                            if(is.null(unlist(sel.plant$data$distribution$introduced)) != TRUE){
                                                text.introduced <- tcltk::tklabel(text.introduced.frame, text = "Introduced locations:")
                                                list.introduced <- tcltk::tklistbox(text.introduced.frame, listvariable = tcltk::tclVar(unlist(sel.plant$data$distribution$introduced)))
                                                tcltk::tkpack(text.introduced, list.introduced)}
                                            tcltk::tkpack(text.native.frame, text.introduced.frame, side = "left")
                                            tcltk::tkpack(map.image, map.text.frame)
                                        }else{
                                            tcltk::tkmessageBox(icon = "warning", message = paste("Distribution of", sel.plant$data$scientific_name, "is unknown", type = "ok"))
                                        }                                    
                                    })
                                    observations <- tcltk::tklabel(entry.distribution.frame, text = paste("Observations :", unknown.check(sel.plant$data$observations)))
                                    tcltk::tkpack(observations, entry.distribution.but)
                                    tcltk::tkpack(entry.info.frame, entry.distribution.frame, side = "left")
                                    #partial images of plant
                                    dis.but <- function(list){
                                        if(is.null(list) == TRUE){
                                            return("disabled")
                                        }else{
                                            return("normal")
                                        }
                                    }
                                    entry.parts.frame <- tcltk::tkframe(entry.win)
                                    entry.fruit.image <- tcltk::tkbutton(entry.parts.frame, text = "Fruit", command = function(){list.image(parent = entry.win, images = sel.plant$data$images$fruit)}, state = dis.but(sel.plant$data$images$fruit))
                                    entry.bark.image <- tcltk::tkbutton(entry.parts.frame, text = "Bark", command = function(){list.image(parent = entry.win, images = sel.plant$data$images$bark)}, state = dis.but(sel.plant$data$images$bark))
                                    entry.other.image <- tcltk::tkbutton(entry.parts.frame, text = "Other", command = function(){list.image(parent = entry.win, images = sel.plant$data$images$other)}, state = dis.but(sel.plant$data$images$other))
                                    entry.flower.image <- tcltk::tkbutton(entry.parts.frame, text = "Flower", command = function(){list.image(parent = entry.win, images = sel.plant$data$images$flower)}, state = dis.but(sel.plant$data$images$flower))
                                    entry.leaf.image <- tcltk::tkbutton(entry.parts.frame, text = "Leaf", command = function(){list.image(parent = entry.win, images = sel.plant$data$images$leaf)}, state = dis.but(sel.plant$data$images$leaf))
                                    entry.habit.image <- tcltk::tkbutton(entry.parts.frame, text = "Habit", command = function(){list.image(parent = entry.win, images = sel.plant$data$images$habit)}, state = dis.but(sel.plant$data$images$habit))
                                    tcltk::tkpack(entry.fruit.image, entry.bark.image, entry.other.image, entry.flower.image, entry.leaf.image, entry.habit.image, padx = c(1.25, 1.25), pady = c(2,0), side = "left")
                                    #main image of plant
                                    entry.image.frame <- tcltk::tkframe(entry.win)                                    
                                    create.image(image = sel.plant$data$image_url, frame = entry.image.frame, name = "Image")                                    
                                    tcltk::tkpack(entry.label, entry.common_name, entry.medium.frame, entry.parts.frame, entry.image.frame)
                                })
                                tcltk::tkpack(resp.lab,list)
                            }else{
                                tcltk::tkmessageBox(icon = "warning", message = "Nothing found", type = "ok")
                            }
                        }else{
                            tcltk::tkmessageBox(icon = "warning", message = "Search field can't be empty!", type = "ok")
                        }
                    }, state = "active")
                tcltk::tkpack(key.check.lab, search.button, side = "left", padx = c(5,5), pady = c(0,5))
                tcltk::tkdestroy(key.root)
            }           
           }else{
                tcltk::tkmessageBox(icon = "error", message = "Key field can't be empty!", type = "ok")
           }})
       tcltk::tkpack(key.lab, key.obtain.frame, key.enter.frame)
       tcltk::tkpack(key.obtain.lab, key.obtain.but, side = "left")
       tcltk::tkpack(key.enter, key.but, side = "left", padx = c(5,5), pady = c(0,5))
    }

    #checks the API key
    key.check <- function(token){
        if(token == ""){
            return(tcltk::tclVar("Enter API key!"))
        }
        if(token != ""){
            res <- httr::GET(paste(base_url, "kingdoms", '?', 'token', '=', token, sep=''))
            if(httr::status_code(res) != 200){
                if(httr::status_code(res) == 401){
                    tcltk::tkmessageBox(icon = "error", message = "You've entered invalid API key")
                }else{
                    tcltk::tkmessageBox(icon = "error", message = paste("There\'s an error:", httr::http_status(httr::status_code(res))$message))
                }
            }else{        
                return(tcltk::tclVar("Key is OK"))
            }            
        }
    }

    #Start gui
    gui <- tcltk::tktoplevel()
    tcltk::tktitle(gui) <- "Trefle API R GUI"
    gui.wel <- tcltk::tklabel(gui, text = "Welcome to Trefle search")    
    key.check.frame <- tcltk::tkframe(gui)    
    key.check.lab <- tcltk::tkmessage(key.check.frame, text = tcltk::tclvalue(key.check(tcltk::tclvalue(key))))
    key.check.but <- tcltk::tkbutton(key.check.frame, text="Enter key", command = key.gui)
    search.frame <- tcltk::tkframe(gui)
    search.lab <- tcltk::tklabel(search.frame, text = "Enter search query:")
    search.entry <- tcltk::tkentry(search.frame, textvariable = search.query)
    search.button <- tcltk::tkbutton(search.frame, text = "Search", state = "disabled")
    page.frame <- tcltk::tkframe(gui)
    page.lab <- tcltk::tklabel(page.frame, text = "Page :")
    page.entry <- tcltk::tkentry(page.frame, textvariable = pageW, width = 3)    
    tcltk::tkpack(gui.wel, key.check.frame, search.frame, page.frame, side = "top")
    tcltk::tkpack(key.check.lab, key.check.but, side = "left")
    tcltk::tkpack(search.lab, search.entry, search.button, side = "left", padx = c(5,5), pady = c(0,5))
    tcltk::tkpack(page.lab, page.entry, side = "left", pady = c(0,5))
}