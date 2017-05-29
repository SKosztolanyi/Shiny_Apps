library(shiny)
library(plotly)
library(openxlsx)

# Tables containing data
mzdy_subset <- read.csv("data/Wages_Subset_Ordered.csv", sep = ";", header = T, stringsAsFactors = F)
mzdy_subset <- mzdy_subset[order(mzdy_subset$Povolani),]
Pohlavi <- as.data.frame(read.xlsx(xlsxFile = "data/CR_2016_Mzdy.xlsx", sheet = "Pohlavi"))
Vzdelani <- as.data.frame(read.xlsx(xlsxFile = "data/CR_2016_Mzdy.xlsx", sheet = "Vzdelani"))
Vekove_kategorie <-  as.data.frame(read.xlsx(xlsxFile = "data/CR_2016_Mzdy.xlsx", sheet = "Vekove_Kategorie"))
Region <-  as.data.frame(read.xlsx(xlsxFile = "data/CR_2016_Mzdy.xlsx", sheet = "Region"))

# Tables for Mzdova_Kalkulacka
vek_mod <- as.data.frame(read.xlsx(xlsxFile = "data/CR_2016_Mzdy.xlsx", sheet = "Vekovy_Modifikator"))
pohlavi_mod <- as.data.frame(read.xlsx(xlsxFile = "data/CR_2016_Mzdy.xlsx", sheet = "Pohlavny_Modifikator"))
zkusenosti_mod <- as.data.frame(read.xlsx(xlsxFile = "data/CR_2016_Mzdy.xlsx", sheet = "Skusenostny_Modifikator"))
vzdelani_mod <- as.data.frame(read.xlsx(xlsxFile = "data/CR_2016_Mzdy.xlsx", sheet = "Vzdelanostny_Modifikator"))
mesto_mod <- as.data.frame(read.xlsx(xlsxFile = "data/CR_2016_Mzdy.xlsx", sheet = "Mestsky_Modifikator"))


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Wages in Year 2016 for Czech Republic in CZK"),
  
  sidebarPanel(
    conditionalPanel(condition = "input.conditionedPanels==5",
                     selectInput("ZvolenaPozice", "Pracovni pozice", choices = mzdy_subset$Povolani, 
                                 selected = "Herci"),
                     selectInput("ZvolenyVek", "Vas vek", choices = vek_mod$Vek, selected = "23-27"),
                     selectInput("ZvolenePohlavi", "Vase pohlavi", choices = pohlavi_mod$Pohlavie, selected = "Zena"),
                     selectInput("ZvoleneVzdelani", "Nejvyssi dosazene vzdelani", 
                                 choices = vzdelani_mod$Vzdelani, selected = "S maturitou a vyssi odborne"),
                     selectInput("ZvoleneZkusenosti", "Delka zkusenosti v oboru", 
                                 choices = zkusenosti_mod$Prax_v_oboru, selected = "1-2"),
                     selectInput("ZvoleneMesto", "Mesto vykonu prace (nebo sidlo zamestnavatele)", choices = mesto_mod$Mesto, selected = "Brno_Ostrava"),
                     submitButton("submit")
                     #actionButton("goButton", "Hotovo"),
                     #p("Kliknete na Hotovo aby sa projevili zmeny.")
    ),
    conditionalPanel(condition="input.conditionedPanels==1",
                     selectInput("Zamestnani", "Vyberte si zamestnani", choices = mzdy_subset$Povolani, selected = "Herci"),
                     h5("Spodni cara pod krabickou urcuje hranici 10 percentil"),
                     h5("Spodni cast krabicky urcuje 25 percentil"),
                     h5("Cara vevnitr krabicky urcuje median"),
                     h5("Vrchni cast krabicky urcuje 75 percentil"),
                     h5("Vrchni cara nad krabickou urcuje 90 percentil")
    ),
    conditionalPanel(condition="input.conditionedPanels==2",
                     h3("Rozdily v celkove mzde podle pohlavi"),
                     h5("Spodni cara pod krabickou urcuje hranici 10 percentil"),
                     h5("Spodni cast krabicky urcuje 25 percentil"),
                     h5("Cara vevnitr krabicky urcuje median"),
                     h5("Vrchni cast krabicky urcuje 75 percentil"),
                     h5("Vrchni cara nad krabickou urcuje 90 percentil")
    ),
    conditionalPanel(condition="input.conditionedPanels==3",
                     h3("Rozdily v celkove mzde podle vzdelani"),
                     h5("Spodni cara pod krabickou urcuje hranici 10 percentil"),
                     h5("Spodni cast krabicky urcuje 25 percentil"),
                     h5("Cara vevnitr krabicky urcuje median"),
                     h5("Vrchni cast krabicky urcuje 75 percentil"),
                     h5("Vrchni cara nad krabickou urcuje 90 percentil")
    ),
    conditionalPanel(condition="input.conditionedPanels==4",
                     h3("Rozdily v celkove mzde podle veku"),
                     h5("Spodni cara pod krabickou urcuje hranici 10 percentil"),
                     h5("Spodni cast krabicky urcuje 25 percentil"),
                     h5("Cara vevnitr krabicky urcuje median"),
                     h5("Vrchni cast krabicky urcuje 75 percentil"),
                     h5("Vrchni cara nad krabickou urcuje 90 percentil")
    ),
    conditionalPanel(condition="input.conditionedPanels==6",
                     h3("Rozdily v celkove mzde podle regionu"),
                     h5("Spodni cara pod krabickou urcuje hranici 10 percentil"),
                     h5("Spodni cast krabicky urcuje 25 percentil"),
                     h5("Cara vevnitr krabicky urcuje median"),
                     h5("Vrchni cast krabicky urcuje 75 percentil"),
                     h5("Vrchni cara nad krabickou urcuje 90 percentil")
    )
    
  ),
  
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Pohlavi", plotlyOutput("Pohlavi"), value = 2),
        tabPanel("Vzdelani", plotlyOutput("Vzdelani"), value = 3),
        tabPanel("Vek", plotlyOutput("Vek"), value = 4),
        tabPanel("Region", plotlyOutput("Region"), value = 6),
        tabPanel("Vyber_Povolani", plotlyOutput("VybratePovolani"), value = 1),
        tabPanel("Mzdova_Kalkulacka", plotlyOutput("Mzdova_Kalkulacka"), value = 5) # instead of plotly
        , id = "conditionedPanels"
      )
    )
  )
)