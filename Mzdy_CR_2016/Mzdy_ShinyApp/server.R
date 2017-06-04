library(shiny)
library(plotly)
library(openxlsx)

# 1 Defining functions for plots

# 1.1 works well for tables with same values at the same positions
create_plotly_boxplots <- function(df, type) {
  num_rows <- nrow(df)
  plp <- plot_ly()
  for (rw in 1:num_rows) {
    y = c(df[rw,3], df[rw, 4], df[rw, 4], df[rw, 5], df[rw, 6], df[rw, 6], df[rw, 7])
    name = as.character(df[rw, 1])
    plp <- add_trace(plp, y = c(df[rw,3], df[rw, 4], df[rw, 4], df[rw, 5], df[rw, 6], df[rw, 6], df[rw, 7]), name = as.character(df[rw, 1]), type = type)
  }
  return(plp)
}

# 1.2 This function is for creating boxplots for employment type
vyber_povolani_plotly_boxplot <- function(df, type) {
      num_rows <- nrow(df)
      plp <- plot_ly()
      for (rw in 1:num_rows) {
            y = c(df[rw,3], df[rw, 4], df[rw, 4], df[rw, 5], df[rw, 6], df[rw, 6], df[rw, 7])
            plp <- add_trace(plp, y = c(df[rw,3], df[rw, 4], df[rw, 4], df[rw, 5], df[rw, 6], df[rw, 6], df[rw, 7])
                             , name = as.character(df[rw, "Dataset"])
                             , type = type) %>%
                  layout(title = df[rw, "Povolani"])
      }
      return(plp)
}

# 1.3 These functions are for creating boxplots with calculated wage based on selected parameters
calculate_wage <- function(df, job, points_sum) {
      wage <- df$Kc_mes_prumer[df$Povolani == job]
      if (points_sum == 0) {wage = df$Kc_mes_1_decil[df$Povolani == job] * 0.85}
      if (points_sum == 1) {wage = df$Kc_mes_1_decil[df$Povolani == job] * 0.92}
      if (points_sum == 2) {wage = df$Kc_mes_1_decil[df$Povolani == job] * 1}
      # this apply function applies min function to a matrix that chooses row of values with smaller values
      if (points_sum == 3) {wage = apply(cbind(df$Kc_mes_1_decil[df$Povolani == job] * 1.1, df$Kc_mes_1_kvartil[df$Povolani == job] * 0.85), 1, min)}
      if (points_sum == 4) {wage = apply(cbind(df$Kc_mes_1_decil[df$Povolani == job] * 1.1, df$Kc_mes_1_kvartil[df$Povolani == job] * 0.85), 1, max)}
      if (points_sum == 5) {wage = df$Kc_mes_1_kvartil[df$Povolani == job] * 0.92}
      if (points_sum == 6) {wage = df$Kc_mes_1_kvartil[df$Povolani == job] * 1}
      if (points_sum == 7) {wage = apply(cbind(df$Kc_mes_1_kvartil[df$Povolani == job] * 1.1, df$Kc_mes_median[df$Povolani == job] * 0.85), 1, min)}
      if (points_sum == 8) {wage = apply(cbind(df$Kc_mes_1_kvartil[df$Povolani == job] * 1.1, df$Kc_mes_median[df$Povolani == job] * 0.85), 1, max)}
      if (points_sum == 9) {wage = df$Kc_mes_median[df$Povolani == job] * 0.95}
      if (points_sum == 10) {wage = df$Kc_mes_median[df$Povolani == job] * 1}
      if (points_sum == 11) {wage = apply(cbind(df$Kc_mes_prumer[df$Povolani == job] * 1, df$Kc_mes_median[df$Povolani == job] * 1.1), 1, min)}
      if (points_sum == 12) {wage = apply(cbind(df$Kc_mes_prumer[df$Povolani == job] * 1, df$Kc_mes_median[df$Povolani == job] * 1.1), 1, max)}
      if (points_sum == 13) {wage = apply(cbind(df$Kc_mes_prumer[df$Povolani == job] * 1.1, df$Kc_mes_3_kvartil[df$Povolani == job] * 0.9), 1, min)}
      if (points_sum == 14) {wage = apply(cbind(df$Kc_mes_prumer[df$Povolani == job] * 1.1, df$Kc_mes_3_kvartil[df$Povolani == job] * 0.9), 1, max)}
      if (points_sum == 15) {wage = df$Kc_mes_3_kvartil[df$Povolani == job] * 1}
      if (points_sum == 16) {wage = df$Kc_mes_3_kvartil[df$Povolani == job] * 1.05}
      if (points_sum == 17) {wage = df$Kc_mes_3_kvartil[df$Povolani == job] * 1.1}
      if (points_sum == 18) {wage = apply(cbind(df$Kc_mes_3_kvartil[df$Povolani == job] * 1.15, df$Kc_mes_9_decil[df$Povolani == job] * 0.9), 1, min)}
      if (points_sum == 19) {wage = apply(cbind(df$Kc_mes_3_kvartil[df$Povolani == job] * 1.15, df$Kc_mes_9_decil[df$Povolani == job] * 0.9), 1, max)}
      if (points_sum == 20) {wage = df$Kc_mes_9_decil[df$Povolani == job] * 1}
      if (points_sum == 21) {wage = df$Kc_mes_9_decil[df$Povolani == job] * 1.05}
      if (points_sum == 22) {wage = df$Kc_mes_9_decil[df$Povolani == job] * 1.1}
      
      new_df <- data.frame(Dataset = df$Dataset[df$Povolani == job], Wage = wage)
      return(new_df)
}



vypocet_mzdy_plotly_boxplot <- function(df, wage_df, type) {
      num_rows <- nrow(df)
      plp <- plot_ly()
      for (rw in 1:num_rows) {
            y = c(df[rw,3], df[rw, 4], df[rw, 4], df[rw, 5], df[rw, 6], df[rw, 6], df[rw, 7])
            plp <- add_trace(plp, y = c(df[rw,3], df[rw, 4], df[rw, 4], df[rw, 5], df[rw, 6], df[rw, 6], df[rw, 7])
                             , x = as.character(df[rw, "Dataset"])
                             , name = as.character(df[rw, "Dataset"])
                             , type = type) %>%
                  layout(title = df[rw, "Povolani"])
      }
      plp <- add_trace(plp, y = wage_df$Wage
                       , x = as.character(wage_df$Dataset)
                       , name = "Odpovidajici mzda"
                       , type = "scatter"
                       , mode = "markers")
      
      return(plp)
}

# Define server logic
shinyServer(function(input, output) {
  
  mzdy_subset <- read.csv("data/Wages_Subset_Clean.csv", sep = ";", header = T, stringsAsFactors = F)
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
  
  output$Pohlavi <- renderPlotly({
    pohlavi_plp <- create_plotly_boxplots(Pohlavi, "box")
    pohlavi_plp
  })
    
  output$Vzdelani <- renderPlotly({
    vzdelani_plp <- create_plotly_boxplots(Vzdelani, "box")
    vzdelani_plp
  })
  
  output$Vek <- renderPlotly({
    vek_plp <- create_plotly_boxplots(Vekove_kategorie, "box")
    vek_plp
  })
  
  output$Region <- renderPlotly({
        vek_plp <- create_plotly_boxplots(Region, "box")
        vek_plp
  })
  
  output$VybratePovolani <- renderPlotly({
    zamestnani_plp <- vyber_povolani_plotly_boxplot(mzdy_subset[mzdy_subset$Povolani == input$Zamestnani,], "box")
    zamestnani_plp
  })
  
  output$Mzdova_Kalkulacka <- renderPlotly({
        input$Button

        ZvolenePovolani <- isolate(input$ZvolenaPozice)
        # ZvolenePovolani
        #ZvolenePovolani <- reactive({input$ZvolenaPozice})

        # soucet bodu podle modifikatoru a zvolenych parametru
        soucet_bodu <- sum(vek_mod$Body[vek_mod$Vek == isolate(input$ZvolenyVek)],
                           pohlavi_mod$Body[pohlavi_mod$Pohlavi == isolate(input$ZvolenePohlavi)],
                           zkusenosti_mod$Body[zkusenosti_mod$Prax_v_oboru == isolate(input$ZvoleneZkusenosti)],
                           vzdelani_mod$Body[vzdelani_mod$Vzdelani == isolate(input$ZvoleneVzdelani)],
                           mesto_mod$Body[mesto_mod$Mesto == isolate(input$ZvoleneMesto)])
        #as.character(soucet_bodu)
        
        # vypocet odhadovane mzdy pro verejnou a sokromou sferu podle zadanych parametru
        wage_df <- calculate_wage(mzdy_subset, ZvolenePovolani, soucet_bodu)
        #wage_df
        
        vypocet_mzdy_plotly_boxplot(mzdy_subset[mzdy_subset$Povolani == ZvolenePovolani,] , wage_df , "box")
  })
  
  output$Bezne_Povolani <- renderPlotly({
        mzdy_ordered <- mzdy_subset[order(-mzdy_subset$Pocet_zamestnancu_v_tis),]
        plt <- plot_ly(data = mzdy_ordered[1:20,]
                       , x = ~Kc_mes_prumer 
                       , y = ~Pocet_zamestnancu_v_tis
                       , type = "scatter"
                       , mode = "markers"
                       , color = ~Kc_mes_prumer
                       , marker = list(size = 15)
                       , text = ~paste(Povolani)
                       ) %>%
              layout(title = "20 Nejbeznejsich zamestnani v CR",
                     yaxis = list(range = c(0, 150000), title = "Pocet zamestnanych lidi na pozici v tis."),
                     xaxis = list(range = c(10000, 40000), title = "Prumerna mesicni odmena v tis. Kc"))
        plt
  })
  
  output$Placene_Povolani <- renderPlotly({
        mzdy_placene <- mzdy_subset[order(-mzdy_subset$Kc_mes_prumer),]
        plt <- plot_ly(data = mzdy_placene[1:20,]
                       , x = ~Kc_mes_prumer 
                       , y = ~Pocet_zamestnancu_v_tis
                       , type = "scatter"
                       , mode = "markers"
                       , color = ~Kc_mes_prumer
                       , marker = list(size = 15)
                       , text = ~paste(Povolani)
        ) %>%
              layout(title = "20 Nejlepe placenych zamestnani v CR",
                     yaxis = list(range = c(0, 25000), title = "Pocet zamestnanych lidi na pozici v tis."),
                     xaxis = list(range = c(20000, 170000), title = "Prumerna mesicni odmena v tis. Kc"))
        plt
  })
  
})
