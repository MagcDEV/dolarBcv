#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
library(rvest)
library(selectr)
library(xml2)
library(jsonlite)
library(tidyverse)
library(ggthemes)
library(gridExtra)

#download.file("http://www.bcv.org.ve", destfile = "bdv.html")

scrappedurl <- read_html("http://www.bcv.org.ve")

pricebcv <- html_text(html_node(scrappedurl, "div#dolar"))

dolarBcv <- str_squish(str_replace_all(pricebcv, "[\n\t]" , ""))

#download.file("http://www.bcv.org.ve/estadisticas/indice-de-inversion", destfile = "bdvIdi.html") # agregar = ?page=1 para el resto de la tabla

scrappedurl <- read_html("http://www.bcv.org.ve/estadisticas/indice-de-inversion")

tablaIdi <- html_table(html_node(scrappedurl, "table"), dec = "," )

names(tablaIdi) <- c("fecha", "tipo_de_cambio", "idi")

tablaIdi$tipo_de_cambio <- gsub("\\.", "", tablaIdi$tipo_de_cambio)
tablaIdi$tipo_de_cambio <- gsub(",", ".", tablaIdi$tipo_de_cambio)

tablaIdi$idi <- gsub("\\.", "", tablaIdi$idi)
tablaIdi$idi <- gsub(",", ".", tablaIdi$idi)

tablaIdi$tipo_de_cambio <- as.numeric(tablaIdi$tipo_de_cambio)
tablaIdi$idi <- as.numeric(tablaIdi$idi)

tablaIdi$fecha <- as.Date(tablaIdi$fecha, "%d-%m-%Y")

tablaIdi <- tablaIdi %>% mutate(pct_change = (idi /lead(idi) - 1) * 100)

yadioRate <- read_json("https://api.yadio.io/rates")[1]

#download.file("https://rates.airtm.com", destfile = "airtm.html")

scrappedurl <- read_html("https://rates.airtm.com")

priceAitm <- as.numeric(html_text(html_node(scrappedurl, "span.rate--general")))


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    #titlePanel("Tipo de Cambio Bs/USD"),
    titlePanel(title=div(img(src="https://encrypted-tbn0.gstatic.com/images?q=tbn%3AANd9GcSgmZe9ET6AU4wRNEoNn_MMPbCFouPwUaPH9yvU4T6Yj378KpTh&usqp=CAU", height='60px',width='50px'), "Tipo de Cambio")),
    h3(Sys.Date()),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            
            img(src="data:image/jpeg;base64,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", height='50px',width='40px'),
            h3("BCV"),
            strong(h3(dolarBcv)),
            img(src="data:image/png;base64,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", height='50px',width='40px'),
            h3("Yadio"),
            strong(h3("Bs/USD", format(as.numeric(yadioRate), scientific = FALSE, big.mark = ","))),
            img(src="data:image/png;base64,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", height='50px',width='40px'),
            h3("AirTm"),
            strong(h3("Bs/USD", format(priceAitm, scientific = FALSE, big.mark = ","))),
            br(),
            sliderInput("bins",
                        "Numero de Dias:",
                        min = 7,
                        max = 50,
                        value = 50)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            
            plotOutput("distPlot")
        ),
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        
        
        grid.arrange(ggplot(tablaIdi[1:input$bins,], aes(fecha, tipo_de_cambio)) +
                         theme_wsj() +
                         geom_area(fill = alpha("#1380A1", .2)) +
                         geom_line(colour = "red", size = 1) +
                         labs(title="Bs/USD", subtitle = "TC BCV Bs/$") +
                         scale_x_date(date_breaks = "1 week") +
                         theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)),ggplot(tablaIdi[1:input$bins,], aes(fecha, pct_change)) +
                         theme_wsj() +
                         geom_area(fill = alpha("#1380A1", .2)) +
                         geom_line(colour = "red", size = 1) +
                         labs(title="IDI BCV", subtitle = "IDI BCV %") +
                         scale_x_date(date_breaks = "1 week") +
                         theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)))
        
    }, height = 600)
}

# Run the application 
shinyApp(ui = ui, server = server)