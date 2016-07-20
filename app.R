#
# Combined App Code
#

#
# Global Code
# Grab the external file with the data
rm(list=ls())
df_countrydata              <- as.data.frame(unique(read.csv("utopia_population_data.csv")))
df_countrylist              <- as.data.frame(unique(read.csv("utopia_population_data.csv")[,1]))
colnames(df_countrylist)    <- c("Country")
v_countries                 <- setNames(as.character(df_countrylist$Country),df_countrylist$Country)
#rm(df_country)

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

# UI Part
ui <- fluidPage(
    
    # HEADER LINE
    headerPanel("Planet Utopia Population Overview (2015-01, 2016-06)"),
    
    # PANEL SECTION
    # send this stuff to the server
    sidebarPanel(
        wellPanel(
            h4("Selected Countries"),
            selectInput(inputId = "p_Country", 
                        label = NULL, 
                        choices = v_countries, 
                        selected = c('India','United States',
                                     'Canada', 'Australia',
                                     'Russia','Argentina',
                                     'Andorra'),
                        multiple = T
            )
        ),
        wellPanel(
            h4("Plot"),
            checkboxGroupInput(inputId = "p_RenderGeo", 
                               NULL, 
                               choices = 'World Map',
                               selected = 'World Map'
            ),
            checkboxGroupInput(inputId = "p_RenderPop", 
                               label = NULL, 
                               choices = 'Population History'
            ),
            checkboxGroupInput(inputId = "p_RenderProj", 
                               NULL, 
                               choices = 'Population Projection',
                               selected = 'Population Projection'
            )
        )
    ),
    
    # Get the stuff back from the server and out on the screen
    mainPanel(
        
        tabsetPanel(
           
            tabPanel("Statistics",
                 # Plot the charts depending on what is selected
                conditionalPanel(
                    condition = "input.p_RenderGeo == 'World Map'",
                    plotlyOutput('p_Geo')
                ),
                conditionalPanel(
                    condition = "input.p_RenderPop == 'Population History'",
                    plotOutput('p_Chart'),
                    br()
                ),    
                conditionalPanel(
                    condition = "input.p_RenderProj == 'Population Projection'",
                    plotOutput('p_Lm'),
                    br()
                )
            ),
            
            tabPanel("How to use the application", 
                     h3("Adding countries to the list"),
                     p("Hover with the mouse over the white box below the text 'Select Countries' and select
                       a white area on the right hand side."),
                     br(),
                     p("A list appears and you can either start typing the country you are looking for and
                        press the enter key or scroll down the list using the scrollbar and 
                       selecting one ore more countries by a mouse click"),
                     h3("Removing countries from the list"),
                     p("Hover with the mouse over the country you want to remove. Select it and press backspace or delete key"),
                     h3("Select chart types to plot"),
                     p("Check the statistical chart type you wish to plot. Uncheck if you wish to hide an item."),
                     h3("Application Source Code"),
                     helpText(a("app.R on GitHub",href="https://github.com/bjoernsteffens/CreatingDataProductsShiny/blob/master/app.R",target="_blank"))
            )
            
        )
        
    )
)

# Server Part
server <- function(input, output) {
    
    # Grab what is coming from the panel input
    output$p_Country    <- renderText({input$p_Country})
    output$p_RenderPop  <- renderText({input$p_RenderPop})
    output$p_RenderProj <- renderText({input$p_RenderProj})
    output$p_RenderGeo  <- renderText({input$p_RenderGeo})
    
    # Create the Line Chart
    output$p_Chart <- renderPlot({
        
        #Grab the countries
        #df_countrydata %>% filter(Country %in% c_list)
        #plot(df_countrydata$MONTH, df_countrydata$Births, data = df_countrydata[df_countrydata$COUNTRY == "United States",])
        
        #c_list <- c('India','United States', 'United Kingdom')
        c_list <- input$p_Country
        df_pData <- df_countrydata %>% filter(Country %in% c_list)
        g <- ggplot(df_pData, aes(x = Month, y = log10(Births), colour = Country, group = Country)) +
            geom_line() +
            geom_point(aes(colour = Country)) +
            ylab("Births per month (log scale)") +
            xlab("Month") +
            theme(legend.position = "bottom") +
            ggtitle("Historical Population Development for Selected Countries") +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            theme(panel.background = element_rect(fill = "lightblue")) +
            theme(strip.background = element_rect(fill = "lightblue")) +
            theme(panel.grid.minor = element_blank()) +
            theme(panel.grid.major = element_line(colour = "grey95")) +
            theme(axis.text.x = element_text(size=10,margin = margin(5,0,20,0))) +
            theme(axis.text.y = element_text(size=10,margin = margin(0,0,0,10))) +
            theme(axis.title.x = element_blank()) +
            theme(axis.title.y = element_text(size = 12,margin = margin(15,15,30,15))) 
       return(g)
    })
    
    # Create the linear model charts
    output$p_Lm <- renderPlot({
        c_list <- input$p_Country
        df_pData <- df_countrydata %>% filter(Country %in% c_list)
        g <- ggplot(df_pData, aes(x = Month, y = log10(Births), group = 1)) +
            geom_point() +
            facet_wrap(~ Country) +
            geom_smooth(method = "lm") +
            ggtitle("Population Trends with 95% Confidence Intervals") +
            ylab("Births per month (log scale)") +
            xlab("Month") +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            theme(panel.background = element_rect(fill = "lightblue")) +
            theme(strip.background = element_rect(fill = "lightblue")) +
            theme(panel.grid.minor = element_blank()) +
            theme(panel.grid.major = element_line(colour = "grey95")) +
            theme(axis.text.x = element_text(size=10,margin = margin(5,0,20,0))) +
            theme(axis.text.y = element_text(size=10,margin = margin(0,0,0,10))) +
            theme(axis.title.x = element_blank()) +
            theme(axis.title.y = element_text(size = 12,margin = margin(15,15,30,15))) 
        return (g)
    })
    
    # Create the World Map
    # https://plot.ly/r/choropleth-maps/
    # https://plot.ly/r/shiny-tutorial/
    output$p_Geo <- renderPlotly({
        
        c_list <- input$p_Country
        df_pData <- df_countrydata %>% filter(Country %in% c_list)
        df_Total <- df_pData %>% group_by(Country,CountryCode) %>% summarise(TotalBirths=sum(Births))
        
        l <- list(color = toRGB("grey"), width = 0.5)
        g <- list(showframe = FALSE,
                  showcoastlines = FALSE,
                  projection = list(type = 'Mercator'))
        
        plot_ly(df_Total, z = TotalBirths, text = Country,locations = CountryCode, type = 'choropleth',
                color = df_pData$Births, colors = 'Blues', marker = list(line = l),
                colorbar = list(tickprefix = '$', title = 'Births')) %>%
            layout(
                title = "Total Births on Planet Utopia",
                geo = g)
    })
}

shinyApp(ui = ui, server = server)
