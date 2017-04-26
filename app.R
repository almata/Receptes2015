#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

rm(list = ls())

importData <- function() {
  # Importació i filtrat de columnes
  df <- read.csv("Receptes2015.csv", sep = ";", stringsAsFactors = FALSE, header = FALSE)
  df <- df[-c(1, 2, 8, 9, 10, 11, 12, 13)]
  colnames(df) <- c("Residencia", "Sexe", "Edat", "CodiATC", "GrupATC", "Receptes")
  
  # Netejar dades
  sanitizeThousands <- function(x) {
    ifelse(x == as.integer(x), x, x * 1000)
  }
  df$Receptes <- sapply(df$Receptes, sanitizeThousands)
  
  # Esborrar files poc significatives
  df <- df[-which(df$Residencia == "Altres"),]
  df <- df[-which(df$Residencia == "Sense especificar"),]
  df <- df[-which(df$GrupATC == "Sense especificar"),]
  
  # Afegir factors
  df$Residencia <- as.factor(df$Residencia)
  df$Sexe <- as.factor(df$Sexe)
  df$Edat <- as.factor(df$Edat)
  df$CodiATC <- as.factor(df$CodiATC)
  df$GrupATC <- as.factor(df$GrupATC)
  levels(df$Edat)[levels(df$Edat) == "85 anys o m\x8es"] <- "85 anys o més"
  levels(df$Residencia) <- c("Pirineu", "Barcelona", "Camp Tarr.", "Cat.Cent.", "Girona", "Lleida", "Terr.Ebre")

  df
}

df <- importData()

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Receptes facturades al Servei Català de la Salut el 2015"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("groupvar", 
                  "Agrupar per:",
                  choices = c("Sexe", "Residència", "Edat", "Tipus de fàrmac"),
                  selected = c("Sexe"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("barPlot"),
      tableOutput("table")
    )
  )
)

# Define server logic required to draw a barplot
server <- function(input, output) {
  
  output$barPlot <- renderPlot({
    groupvar <- input$groupvar
    groupvar <- ifelse (groupvar == "Residència", "Residencia", groupvar)
    groupvar <- ifelse (groupvar == "Tipus de fàrmac", "CodiATC", groupvar)
    
    data1 <- aggregate(df$Receptes ~ df[[which(dimnames(df)[[2]] == groupvar)]], FUN = sum)
    data2 <- t(data.matrix(data1[-1])) / 1000000 
    colnames(data2) <- levels(df[[which(dimnames(df)[[2]] == groupvar)]])
    xcolors <- hsv((length(data2[1,]):1)/length(data2[1,]), 0.6, 0.8)
    
    barplot(data2[1,], col = xcolors, border = "white", ylab = "(en milions)")
  })
  
  output$table <- renderTable({
    if (input$groupvar == "Tipus de fàrmac") {
      xlegend <- unique(df[-c(1, 2, 3, 6)])
      colnames(xlegend) <- c("Codi", "Descripció")
      xlegend
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

