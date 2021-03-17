#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(httr)
library(httpcache)
library(jsonlite)
library(rvest)
library(stringr)
library(shinyjs)

# Getting values from Pokemon dataset

pokemon <- NULL
r <- GET("https://pokeapi.co/api/v2/pokemon/?limit=1118")
json <- content(r, as = "text")
temp <- fromJSON(json, flatten=TRUE)$results %>% head(127)

## Get height values of all pokemon

getHeight <- function(link) {
    r2 <- GET(link)
    stop_for_status(r2)
    json2 <- content(r2, as = "text")
    temp2 <- fromJSON(json2, flatten=TRUE)$height %>% as.numeric()
    return(temp2)
}

## Get image link for pokemon

getImage <- function(name) {
    temp3 <- read_html(str_glue('https://bulbapedia.bulbagarden.net/wiki/{name}_(Pok%C3%A9mon)', name=name)) %>%
        html_node('.image') %>%
        html_node('img') %>%
        html_attr("src")
    return(temp3)
}

pokemon <- temp[!grepl("-", temp$name),] %>%
    rowwise() %>%
    mutate(height = getHeight(url)) %>%
    ungroup()


# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("What Gen 1 Pokemon is as tall as you?"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            useShinyjs(),
            selectInput('units', 'Units of Measurement', c('Imperial', 'Metric')),
            textInput('feet', 'Feet', placeholder = "5"),
            textInput('inches', "Inches", placeholder = "7"),
            textInput('cm', "Centimeters", placeholder = "170")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           fillRow(imageOutput("image1"),htmlOutput("image2"), width = '300px', height = '250px'),
           div(htmlOutput("height"), style="font-size: 2em")
        )
    )
)



# Define server logic
server <- function(input, output, session) {

    observeEvent(input$units, {
        if (input$units == 'Imperial') {
            shinyjs::hideElement(id="cm")
            shinyjs::show(id="feet")
            shinyjs::show(id="inches")

            updateTextInput(session, "cm", value = "")
        } else {
            shinyjs::hide(id="feet")
            shinyjs::hide(id="inches")
            shinyjs::show(id="cm")

            updateTextInput(session, "feet", value = "")
            updateTextInput(session, "inches", value = "")
        }
    })

    userHeightCm <- reactive({
        if(input$units == 'Imperial') {
            if (input$inches == "") {
                (as.numeric(input$feet)*12) * 2.54
            } else if (input$feet == "") {
                (as.numeric(input$inches)) * 2.54
            } else {
                ( (as.numeric(input$feet)*12) + as.numeric(input$inches) ) * 2.54
            }
        } else {
            userHeightCm = as.numeric(input$cm)
        }
    })

    selectedPokemon <- reactive({
        pokemon %>% filter(abs(height - (as.numeric(userHeightCm()/10))) == min(abs(height - (as.numeric(userHeightCm()/10))))) %>% sample_n(size=1)
    })

    output$height <- renderText({
        if (input$feet != "" | input$inches != "" | input$cm != "") {
            if (!is.na(as.numeric(paste(input$feet, input$inches, input$cm, sep="")))) {
                str_glue("You are at similar height with <strong>{name}</strong> who is <strong>{feet}'{inches}\" ({cm}cm)</strong>",
                         name = str_to_title(selectedPokemon()$name),
                         feet = (as.numeric(selectedPokemon()$height)*10/2.54)%/%12 %>% round(0),
                         inches = (as.numeric(selectedPokemon()$height)*10/2.54)%%12 %>% round(0),
                         cm = as.numeric(selectedPokemon()$height)*10
                )
            } else {
                "Input is invalid. Please make sure you are using numeric values only."
            }
        }
    })

    output$image1 <- renderImage({
        if (input$feet != "" | input$inches != "" | input$cm != "") {
            if (!is.na(as.numeric(paste(input$feet, input$inches, input$cm, sep="")))) {
                filename <- normalizePath(file.path('person.jpg'))
                list(src=filename,
                     alt="Your height",
                     style="height: 250px")
            } else {
                filename <- normalizePath(file.path('person.jpg'))
                list(src=filename,
                     alt="Your height",
                     style="height: 0px")
            }

        } else {
            filename <- normalizePath(file.path('person.jpg'))
            list(src=filename,
                 alt="Your height",
                 style="height: 0px")
        }
    }, deleteFile = FALSE)

    output$image2 <- renderText({
        if (input$feet != "" | input$inches != "" | input$cm != "") {
            if (!is.na(as.numeric(paste(input$feet, input$inches, input$cm, sep="")))) {
                str_glue('<img src="{link}">', link = getImage(selectedPokemon()$name))
            }
        }

    })
}

# Run the application
shinyApp(ui = ui, server = server)
