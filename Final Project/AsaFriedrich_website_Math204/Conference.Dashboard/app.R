#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#



# install.packages("devtools")
# install.packages("nflreadr")
# install.packages("tidyverse")
# install.packages("ggplot2")
#install.packages("ggthemes")

library("nflreadr")
library("ggplot2")
library("tidyverse")
library("dplyr")
library(shiny)
library("ggthemes")

Draftday <- load_draft_picks(seasons = c(1980:2014))
Draftday <- subset(Draftday, select = -c(gsis_id,pfr_player_id,cfb_player_id,car_av,hof))
Draftday <- drop_na(Draftday, c(dr_av,w_av))

Draftday <- Draftday %>%
  group_by(category) %>%
  mutate(
    Group = case_when(
      category %in% c("RB", "WR", "QB", "TE") ~ "Skill",
      category %in% c("OL") ~ "O-Line",
      category %in% c("DB") ~ "Secondary",
      category %in% c("DL", "LB") ~ "D-Line",
      category %in% c("P", "K") ~ "SpecialT",
      TRUE ~ "Other"
    )
  )

Draftday <- Draftday %>%
  mutate(team = if_else(team == "OAK", "RAI", team))
Draftday <- Draftday %>%
  mutate(team = if_else(team == "STL", "RAM", team))
Draftday <- Draftday %>%
  mutate(team = if_else(team == "PHO", "ARI", team))

Draftday <- Draftday %>%
  group_by(team) %>%
  mutate(
    Division = case_when(
      team %in% c("BUF", "MIA", "NYJ", "NWE") ~ "AFC-EAST",
      team %in% c("BAL","CLE","PIT","CIN") ~ "AFC-NORTH",
      team %in% c("HOU","JAX","IND","TEN") ~ "AFC-SOUTH",
      team %in% c("KAN", "RAI","DEN","SDG") ~ "AFC-WEST",
      team %in% c("DAL", "PHI","NYG","WAS") ~ "NFC-EAST",
      team %in% c("DET","GNB","MIN","CHI") ~ "NFC-NORTH",
      team %in% c("TAM","NOR","ATL","CAR") ~ "NFC-SOUTH",
      team %in% c("SFO","RAM","SEA","ARI") ~ "NFC-WEST",
      TRUE ~ "Other"
    )
  )

team_colors <- c(
  "ARI" = "#97233F" ,
  "ATL" = "#A5ACAF",
  "BAL" = "#241773",
  "BUF" = "#00338D",
  "CAR" = "#0085CA",
  "CHI" = "#E94800",
  "CIN" = "#FB4F14",
  "CLE" = "#311D00",
  "DAL" = "#7F9695",
  "DEN" = "#002244",
  "DET" = "#0076B6",
  "GNB" = "#203731",
  "HOU" = "#A71930",
  "IND" = "#002C5F",
  "JAX" = "#9F792C",
  "KAN" = "#E31837",
  "RAI" = "#000000",
  "SDG" = "#FFC20E",
  "RAM" = "#003594",
  "MIA" = "#008E97",
  "MIN" = "#4F2683",
  "NWE" = "#C60C30",
  "NOR" = "#D3BC8D",
  "NYG" = "#0B2265",
  "NYJ" = "#125740",
  "PHI" = "#4CBB17",
  "PIT" = "#FFB612",
  "SFO" = "#AA0000",
  "SEA" = "#69BE28",
  "TAM" = "#D50A0A",
  "TEN" = "#A2AAAD",
  "WAS" = "#5A1414"
)

group_colors <- c("Skill" = "#FF7312", "O-Line" = "#DD5AE6", "D-Line" = "#69CEF5",
                  "Secondary" = "#7CF971", "SpecialT" = "#9A9A9A")

attach(Draftday)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NFL Draft Results by Division"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("divi",
                        "Divisions",
                        c("AFC East" = "AFC-EAST","AFC West" = "AFC-WEST","AFC South" = "AFC-SOUTH",
                          "AFC North"=  "AFC-NORTH", "NFC East" = "NFC-EAST","NFC West" = "NFC-WEST",
                          "NFC South" = "NFC-SOUTH","NFC North" = "NFC-NORTH")),
            sliderInput("round", 
                        "Draft Rounds",
                        min = 1, max = 12, value = 1, step = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          p("This line graph shows the average pick career value broken by team's in each division. 
            We can see the slight deviations between teams through the rounds. It also highlights some players
            who should be considered generational talents so we can see where they were taken in the draft."),
           plotOutput("smoothPlot"),
          br(),
          p("Then we can look at which sort of picks are made in each round by the divisional teams. See how it 
            changes as we move into the later stages of the draft. It is also important to note that before 1994 the NFl
            changed the amount of rounds a few times so when you go above round 7 the number of picks will vary drastically
            between teams."),
           plotOutput("histplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  

    output$smoothPlot <- renderPlot({
      Divi <- input$divi
      
      select_players <- Draftday[Division == Divi & w_av > 95, ]
      
      ggplot(Draftday[Division == Divi & w_av > 0, ], aes(
        x = pick,
        y = w_av,
        group = team,
        color = team
      )) +
        geom_smooth(se = FALSE) + 
        labs(x = "Pick #", y = "Career Average Value", color = "Team") +
        theme_stata() +
        geom_text(
          data = select_players,
          aes(label = pfr_player_name),
          hjust = -0.1,
          vjust = 0.7,
          color = "black",
          size = 3.5,
          position = position_dodge(0.9),
          check_overlap = TRUE
        )+
        geom_point(data = select_players ,alpha = 1)+
        scale_colour_manual(values = team_colors)
      

       
    })
    
    output$histplot <- renderPlot({
      Divi <- input$divi
      Rnd <- input$round
      
       
        ggplot(Draftday[Division == Divi & round == c(Rnd),], 
               aes(x = pick, fill = Group))+
          geom_histogram(bins = 6, colour = "black")+
          facet_wrap(~ team, ncol = 4)+
          theme(panel.spacing.x = unit(0.25,"lines"))+
          theme_bw()+
          ylab("Number of Picks")+
          xlab("Pick Number")+
          scale_fill_manual(values = group_colors)
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
