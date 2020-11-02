# load the required packages
library(tidyverse)
library(shiny)
library(choroplethr)
library(xfun)
library(choroplethrMaps)
library(dplyr)
library(shinydashboard)
library(data.table)

# Define server logic required to draw a map
server <- shinyServer(function(input, output) {
  
  get_state_ranks <- function(state, winner) {
    updated_df <- df
    updated_df[updated_df$region %in% state,"value"] <- winner
    return(updated_df)
  }
  
  dataInput <- reactive({

    state_list <- c()
    winner_list <- c()
    if (input$AL == "Trump") {
      state_list <- c(state_list,df[1,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$AL == "Biden") {
      state_list <- c(state_list,df[1,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$AK == "Trump") {
      state_list <- c(state_list,df[2,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$AK == "Biden") {
      state_list <- c(state_list,df[2,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$AZ == "Trump") {
      state_list <- c(state_list,df[3,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$AZ == "Biden") {
      state_list <- c(state_list,df[3,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$AR == "Trump") {
      state_list <- c(state_list,df[4,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$AR == "Biden") {
      state_list <- c(state_list,df[4,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$CA == "Trump") {
      state_list <- c(state_list,df[5,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$CA == "Biden") {
      state_list <- c(state_list,df[5,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$CO == "Trump") {
      state_list <- c(state_list,df[6,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$CO == "Biden") {
      state_list <- c(state_list,df[6,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$CT == "Trump") {
      state_list <- c(state_list,df[7,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$CT == "Biden") {
      state_list <- c(state_list,df[7,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$DE == "Trump") {
      state_list <- c(state_list,df[8,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$DE == "Biden") {
      state_list <- c(state_list,df[8,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$DC == "Trump") {
      state_list <- c(state_list,df[9,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$DC == "Biden") {
      state_list <- c(state_list,df[9,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$FL == "Trump") {
      state_list <- c(state_list,df[10,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$FL == "Biden") {
      state_list <- c(state_list,df[10,5])
      winner_list <- c(winner_list,"Biden")
    }    
    if (input$GA == "Trump") {
      state_list <- c(state_list,df[11,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$GA == "Biden") {
      state_list <- c(state_list,df[11,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$HI == "Trump") {
      state_list <- c(state_list,df[12,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$HI == "Biden") {
      state_list <- c(state_list,df[12,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$ID == "Trump") {
      state_list <- c(state_list,df[13,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$ID == "Biden") {
      state_list <- c(state_list,df[13,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$IL == "Trump") {
      state_list <- c(state_list,df[14,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$IL == "Biden") {
      state_list <- c(state_list,df[14,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$IN == "Trump") {
      state_list <- c(state_list,df[15,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$IN == "Biden") {
      state_list <- c(state_list,df[15,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$IA == "Trump") {
      state_list <- c(state_list,df[16,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$IA == "Biden") {
      state_list <- c(state_list,df[16,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$KS == "Trump") {
      state_list <- c(state_list,df[17,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$KS == "Biden") {
      state_list <- c(state_list,df[17,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$KY == "Trump") {
      state_list <- c(state_list,df[18,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$KY == "Biden") {
      state_list <- c(state_list,df[18,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$LA == "Trump") {
      state_list <- c(state_list,df[19,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$LA == "Biden") {
      state_list <- c(state_list,df[19,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$ME == "Trump") {
      state_list <- c(state_list,df[20,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$ME == "Biden") {
      state_list <- c(state_list,df[20,5])
      winner_list <- c(winner_list,"Biden")
    }    
    if (input$MD == "Trump") {
      state_list <- c(state_list,df[21,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$MD == "Biden") {
      state_list <- c(state_list,df[21,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$MA == "Trump") {
      state_list <- c(state_list,df[22,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$MA == "Biden") {
      state_list <- c(state_list,df[22,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$MI == "Trump") {
      state_list <- c(state_list,df[23,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$MI == "Biden") {
      state_list <- c(state_list,df[23,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$MN == "Trump") {
      state_list <- c(state_list,df[24,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$MN == "Biden") {
      state_list <- c(state_list,df[24,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$MS == "Trump") {
      state_list <- c(state_list,df[25,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$MS == "Biden") {
      state_list <- c(state_list,df[25,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$MO == "Trump") {
      state_list <- c(state_list,df[26,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$MO == "Biden") {
      state_list <- c(state_list,df[26,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$MT == "Trump") {
      state_list <- c(state_list,df[27,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$MT == "Biden") {
      state_list <- c(state_list,df[27,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$NE == "Trump") {
      state_list <- c(state_list,df[28,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$NE == "Biden") {
      state_list <- c(state_list,df[28,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$NV == "Trump") {
      state_list <- c(state_list,df[29,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$NV == "Biden") {
      state_list <- c(state_list,df[29,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$NH == "Trump") {
      state_list <- c(state_list,df[30,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$NH == "Biden") {
      state_list <- c(state_list,df[30,5])
      winner_list <- c(winner_list,"Biden")
    }    
    if (input$NJ == "Trump") {
      state_list <- c(state_list,df[31,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$NJ == "Biden") {
      state_list <- c(state_list,df[31,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$NM == "Trump") {
      state_list <- c(state_list,df[32,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$NM == "Biden") {
      state_list <- c(state_list,df[32,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$NY == "Trump") {
      state_list <- c(state_list,df[33,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$NY == "Biden") {
      state_list <- c(state_list,df[33,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$NC == "Trump") {
      state_list <- c(state_list,df[34,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$NC == "Biden") {
      state_list <- c(state_list,df[34,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$ND == "Trump") {
      state_list <- c(state_list,df[35,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$ND == "Biden") {
      state_list <- c(state_list,df[35,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$OH == "Trump") {
      state_list <- c(state_list,df[36,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$OH == "Biden") {
      state_list <- c(state_list,df[36,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$OK == "Trump") {
      state_list <- c(state_list,df[37,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$OK == "Biden") {
      state_list <- c(state_list,df[37,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$OR == "Trump") {
      state_list <- c(state_list,df[38,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$OR == "Biden") {
      state_list <- c(state_list,df[38,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$PA == "Trump") {
      state_list <- c(state_list,df[39,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$PA == "Biden") {
      state_list <- c(state_list,df[39,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$RI == "Trump") {
      state_list <- c(state_list,df[40,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$RI == "Biden") {
      state_list <- c(state_list,df[40,5])
      winner_list <- c(winner_list,"Biden")
    }    
    if (input$SC == "Trump") {
      state_list <- c(state_list,df[41,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$SC == "Biden") {
      state_list <- c(state_list,df[41,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$SD == "Trump") {
      state_list <- c(state_list,df[42,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$SD == "Biden") {
      state_list <- c(state_list,df[42,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$TN == "Trump") {
      state_list <- c(state_list,df[43,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$TN == "Biden") {
      state_list <- c(state_list,df[43,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$TX == "Trump") {
      state_list <- c(state_list,df[44,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$TX == "Biden") {
      state_list <- c(state_list,df[44,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$UT == "Trump") {
      state_list <- c(state_list,df[45,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$UT == "Biden") {
      state_list <- c(state_list,df[45,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$VT == "Trump") {
      state_list <- c(state_list,df[46,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$VT == "Biden") {
      state_list <- c(state_list,df[46,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$VA == "Trump") {
      state_list <- c(state_list,df[47,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$VA == "Biden") {
      state_list <- c(state_list,df[47,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$WA == "Trump") {
      state_list <- c(state_list,df[48,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$WA == "Biden") {
      state_list <- c(state_list,df[48,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$WV == "Trump") {
      state_list <- c(state_list,df[49,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$WV == "Biden") {
      state_list <- c(state_list,df[49,5])
      winner_list <- c(winner_list,"Biden")
    }
    if (input$WI == "Trump") {
      state_list <- c(state_list,df[50,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$WI == "Biden") {
      state_list <- c(state_list,df[50,5])
      winner_list <- c(winner_list,"Biden")
    }    
    if (input$WY == "Trump") {
      state_list <- c(state_list,df[51,5])
      winner_list <- c(winner_list,"Trump")
    } else if (input$WY == "Biden") {
      state_list <- c(state_list,df[51,5])
      winner_list <- c(winner_list,"Biden")
    }  
    
    winner_list <- factor(winner_list, levels = c("Undecided", "Biden", "Trump"))
    
    newDF <- get_state_ranks(state_list, winner_list)
    
    bs <- newDF[newDF$value == "Biden", "code"]
    ts <- newDF[newDF$value == "Trump", "code"]
    result_set <- update_prob( biden_states = bs , trump_states = ts, biden_scores_list = NULL )
    
    reactive_outputs <- list(newDF = newDF , result_set = result_set)
  })
  
  
  output$mapPlot <- renderPlot({
    reactive_outputs <- dataInput()
    newDF <- reactive_outputs$newDF
    
    choro = StateChoropleth$new(newDF[,c(5,4)])
      #choro$title = "2020 Election Results"
    choro$ggplot_scale = scale_fill_manual(name="", values=c("#E0E0E0","#799FFF", "#FF9794"), drop=FALSE)
    choro$render()
  }) 
  
  
  
## BIDEN WIN BOX 
  output$biden_win <- renderValueBox({
    
    reactive_outputs <- dataInput()
    result_set <- reactive_outputs$result_set
    biden_win_prob <- result_set$biden_prob
    
   valueBox(
      paste0( round(biden_win_prob*100, 1),"%")
      ,'Biden Win Probability'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "blue")
  }) 
  
## BIDEN EC BOX 
  output$biden_ec <- renderValueBox({
    
    reactive_outputs <- dataInput()
    newDF <- reactive_outputs$newDF
    presECs <- left_join(newDF[c(3,4)], ec, by = c("code" = "Code"))
    
    bidenECs <- sum(presECs[presECs$value=="Biden","EC"])
    
    valueBox(
      paste0( bidenECs )
      ,'Biden EC Votes'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "blue")
  })    
  

 ## TRUMP WIN BOX 
  output$trump_win <- renderValueBox({

    reactive_outputs <- dataInput()
    result_set <- reactive_outputs$result_set
    biden_win_prob <- result_set$biden_prob
    
    valueBox(
      paste0( round(100-biden_win_prob*100,1),
              "%")
      ,'Trump Win Probability'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "red")
  })

  ## BIDEN EC BOX 
  output$trump_ec <- renderValueBox({
    
    reactive_outputs <- dataInput()
    newDF <- reactive_outputs$newDF
    presECs <- left_join(newDF[c(3,4)], ec, by = c("code" = "Code"))
    
    trumpECs <- sum(presECs[presECs$value=="Trump","EC"])
    
    valueBox(
      paste0( trumpECs )
      ,'Trump EC Votes'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "red")
  })      
    
  
  output$data_table <- renderDataTable({
    
    reactive_outputs <- dataInput()
    newDF <- reactive_outputs$newDF
    result_set <- reactive_outputs$result_set
    state_res <- result_set$state_prob
    
    state_res_df <-  data.frame(state_res)
    state_res_df <-  data.frame("Code" = rownames(state_res_df), 
                                "Biden-Win-Prob" = format(round(state_res_df[,1],3), nsmall = 3),
                                "Trump-Win-Prob" = format(round(1-state_res_df[,1],3), nsmall = 3))
    state_res_df <- state_res_df[order(state_res_df$Code), ]
    ec_state_res_df <- left_join(state_res_df, ec, by = "Code")
    ec_state_res_df <- left_join(ec_state_res_df, newDF[c(3,4)], by = c("Code" = "code"))
    
    ec_state_res_df <- ec_state_res_df[,c(1,4,5,6,2,3)]
    names(ec_state_res_df) <- c("Code","State","Electoral College","Winner","Biden Win Prob","Trump Win Prob")
    
    # draw table
    datatable(ec_state_res_df, rownames = F, options = list(pageLength = 100))
  
  })
  
})


# Run the application 
# shinyApp(ui = ui, server = server)
