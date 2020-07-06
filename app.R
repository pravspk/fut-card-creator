library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(dplyr)
library(ggplot2)
library(htmlwidgets)
library(rvest)
library(magick)

db <- read.csv("./data/sofifa.csv")

headings = c("PACE","SHOOTING","PASSING","DRIBBLING","DEFENDING","PHYSICAL")
values = c(83,45,62,64,85,80)
values1 = list(c(79,86),c(44,40,52,53,42,43),c(59,36,52,75,76,46),c(63,38,83,77,55,84),c(82,83,87,86,85),c(79,72,86,77))

len1 <- length(values)
len2 <- max(lengths(values1))
default = 6

sy = 1.0  ##start pos y
sx = 0.25 ##start pos x
#lx = 0.05 #hori spacing
lx = 1/4 * ((1.0-sx) / len1)
#ly = 0.05 #vert spacing
ly = ifelse(len2 > default, 3/8 * ((sy-0.0)/(len2+2)), ifelse(len1>6,3/8 * ((sy-0.0)/(len2+2)),0.04))
#bh = 0.015  #plot height
bh = ifelse(len2 > default, 1/8 * ((sy-0.0)/(len2+2)), 0.01)
#bh = 2/3 * ((1.0-(3/16*1.0))/len2)
#bw = 0.120  #plot width
bw = 3/4 * ((1.0-sx) / len1)
#gh = 0.12  #gauge height
gh = ifelse(len2 > default, 3/5 * (2 * ((sy-0.0)/(len2+2))), 0.12)
#gw = 0.1  #gauge width
gw = 3/4 * ((1.0-sx) / len1)
sz = 12 ## font size
ts = ifelse(len2 > default, 3/8 * ((sy-0.0)/(len2+2)), ifelse(len1>6,3/8 * ((sy-0.0)/(len1+2)),0.04))
#ts = 0.05 ##text height


paper_bg_color = "white"
plot_bgcolor= "white"

gauge_border_color = "grey"
gauge_bgcolor = "#d4d4d4"
gauge_border_width = 0.3

#players <- c("AWFR","fDSXF","df","dfSD","WEF","WRG","WEDQ","WEFRG","ERTGB","ertg","rfg","weqa","rnhn","rioh","ierf")
players <- db$Player.Name

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(" "),
    sidebarPanel(
        selectizeInput(
            'country', 'Choose player:', choices = players,
            options = list(
                placeholder = 'Select player',
                onInitialize = I('function() { this.setValue(""); }')
            )
        ), width=2
    ),
    # Show a plot of the generated distribution
    mainPanel(
        #tabsetPanel(
            #tabPanel("Summary", dataTableOutput("dis")),
            #tabPanel("Plot", 
                     plotlyOutput("plotx")
            #)
        #)
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    output$plotx <- renderPlotly({
        validate(
            need(input$country, "Please select a player to get started!")
        )
        fig <- plot_ly(height = 500, width = 1050)
            ## main viz
            for(i in 1:len1){
                ## gauge meter
                fig <- fig %>% add_trace(
                type = "indicator",
                mode = "number+gauge",
                value = values[i],
                number = list(font=list(color=ifelse(values[i]>50,
                                           ifelse(values[i]>65,
                                                  ifelse(values[i]>80 ,
                                                         "forestgreen",
                                                         "limegreen"),
                                                  "orange"),
                                           "red"))),
                gauge = list(shape = "angular", 
                             bordercolor = gauge_border_color,
                             borderwidth = gauge_border_width,
                             bgcolor = gauge_bgcolor,
                             axis = list(range = list(0, 100),
                                         visible=F), 
                             bar = list(thickness = 1,
                                        color=ifelse(values[i]>50,
                                                     ifelse(values[i]>65,
                                                            ifelse(values[i]>80 ,
                                                                   "forestgreen",
                                                                   "limegreen"),
                                                            "orange"),
                                                     "red"))),
                domain = list(x = c(sx+(i-1)*bw+(i-1)*lx, sx+i*bw+(i-1)*lx),
                              y = c(sy-1*ly-gh, sy-1*ly)),
                title= list(text = headings[i],
                            align="center",
                            font=list(size=sz)))
                
                ## bar and names
                for(j in 1:length(values1[[i]])){
                    ## bars
                    fig <- fig %>% add_trace(
                    type = "indicator",
                    mode = "gauge",
                    value = values1[[i]][j],
                    gauge = list(shape = "bullet",
                                 bordercolor = gauge_border_color,
                                 borderwidth = gauge_border_width,
                                 bgcolor = gauge_bgcolor,
                                 axis = list(range = list(0, 100),visible=F),
                                 bar = list(thickness = 1, 
                                            color=ifelse(values1[[i]][j]>50, 
                                                         (ifelse(values1[[i]][j]>65,
                                                                 (ifelse(values1[[i]][j]>80 ,
                                                                         "forestgreen",
                                                                         "limegreen")),
                                                                 "orange")),
                                                         "red"))),
                    number = list(prefix="   ",
                                  font=list(size=14,
                                            color=ifelse(values1[[i]][j]>50, 
                                                         (ifelse(values1[[i]][j]>65,
                                                                 (ifelse(values1[[i]][j]>80 ,
                                                                         "forestgreen",
                                                                         "limegreen")),
                                                                 "orange")),
                                                         "red"))),
                    domain = list(x = c(sx+(i-1)*bw+(i-1)*lx, sx+i*bw+(i-1)*lx), 
                                  y = c(sy-1*ly-gh -j*ly-j*ts-j*bh, sy-1*ly-gh -j*ly-j*ts-(j-1)*bh)),
                    title= list(text = " ",
                                align="center",
                                font=list(size=10))) 
                    ## names above bars
                    fig <- fig %>% layout(
                        annotations = list(
                            x = sx+(i-1)*bw+(i-1)*lx, 
                            y = sy-1*ly-gh -j*ly-(j-1)*ts-(j-1)*bh, 
                            #text = paste("Shooting",values1[[i]][j],sep = "       "),
                            text = paste("Interceptions"),
                            showarrow = FALSE,
                            xanchor = "left",
                            yanchor = "top",
                            font = list(color = '#264E86',
                                        family = 'sans serif',
                                        size = ifelse(len2 > default, 72 / (len2), ifelse(len1>6, 72/len1,10)))
                    ))
                    
                    ## values besides names
                    fig <- fig %>% layout(
                        annotations = list(
                            x = (sx+(i-1)*bw+(i-1)*lx) + 1.0*bw, 
                            y = sy-1*ly-gh -j*ly-(j-1)*ts-(j-1)*bh, 
                            text = toString(values1[[i]][j]),
                            showarrow = FALSE,
                            xanchor = "right",
                            yanchor = "top",
                            font = list(color=ifelse(values1[[i]][j]>50, 
                                                     (ifelse(values1[[i]][j]>65,
                                                             (ifelse(values1[[i]][j]>80 ,
                                                                     "forestgreen",
                                                                     "limegreen")),
                                                             "orange")),
                                                     "red"),
                                        family = 'sans serif',
                                        size = ifelse(len2 > default,  72 / (len2), ifelse(len1>6, 72/len1,10)))
                        )
                    )
                }
            }
        fig <- fig %>% layout(autosize = F, margin = list(t=20,b=10,l=10,r=10),
                              paper_bgcolor = paper_bg_color,
                              plot_bgcolor = plot_bgcolor) %>% config(displayModeBar = FALSE)
        
        ## card template
        img <- image_read('https://cdn.shopify.com/s/files/1/2412/8291/products/CardsPlugGoldRare_518d0f05-7da7-43ab-869f-26ef60ad3f1d.png?v=1584368104')
        img <- image_scale(img, "194x259!")

        ## stats
        img <- image_annotate(img, "99", size = 14, color = "black", location = "+43+173")
        img <- image_annotate(img, "99", size = 14, color = "black", location = "+113+173")
        img <- image_annotate(img, "99", size = 14, color = "black", location = "+43+191")
        img <- image_annotate(img, "99", size = 14, color = "black", location = "+113+191")
        img <- image_annotate(img, "99", size = 14, color = "black", location = "+43+209")
        img <- image_annotate(img, "99", size = 14, color = "black", location = "+113+209")
        
        ###control name overflow in card##################
        namee <- paste0(db$Player.Name[db$Player.Name==input$country])
        if(nchar(namee)>17){
            pname1 <- strsplit(namee," ")
            pname <- pname1[[1]][length(pname1[[1]])]
        }
        else{
            pname <- namee      
        }
        ##################################################
        
        img <- image_annotate(img, toupper(pname), size = 14, color = "black", gravity = "North", location = "+0+148")
        
        img <- image_annotate(img, "99", size = 24, color = "black", location = "+43+40")
        img <- image_annotate(img, "CM", size = 10, color = "black", location = "+48+65")        
        
        #################################IMAGES
        # url = ""
        # page <- read_html(paste0("https://www.google.com.tr/search?q=","messi+transparent+image","&espv=2&biw=1366&bih=662&source=lnms&tbm=isch&sa=X&ved=0ahUKEwjCnJ6H2ITRAhWCQBoKHfQ5DUAQ_AUIBigB#tbm=isch&q=apple+logo+png"))
        # node <- page %>% html_nodes("img") %>% html_attr("src")
        # for(i in 1:length(node)){
        #     if(length(grep('.com/images', toString(node[[i]]))) == 1){
        #         url <- toString(node[[i]])
        #         print(url)
        #         break
        #     }
        # }
        #####################################
        
        ## image processing - player image
        playerimg <- image_read(paste0(db$picture[db$Player.Name==input$country]))
        playerimg <- image_scale(playerimg, "90x110!")
        #playerimg <- image_transparent(playerimg, "white", fuzz = 5)
        img1 <- image_composite(img, playerimg, offset = "+70+25")
        
        ## team flag
        teamflag <- image_read(paste0(db$Flag[db$Player.Name==input$country]))
        teamflag <- image_scale(teamflag, "25x15!") 
        img2 <- image_composite(img1, teamflag, offset = "+43+84")
        
        ## club logo
        clublogo <- image_read(paste0(db$Team_Image[db$Player.Name==input$country]))
        clublogo <- image_scale(clublogo, "20x25!") 
        img3 <- image_composite(img2, clublogo, offset = "+46+107")
        
        ## load image
        fig <- fig %>% layout(
            images = list(
                #source = base64enc::dataURI(file = '/home/pravs/myfile.png'),
                source = raster2uri(as.raster(img3)),
                x = 0, y = 0.3, 
                sizex = 0.2, sizey = 0.7,
                xref = "paper", yref = "paper", 
                xanchor = "left", yanchor = "bottom"
            ),
            margin = list(t = 50)
        )
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

