library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)

cancer <- read.table("./data/tidy/data_p.txt",fill = T, header = T)
cancer_c <- cancer %>% dplyr::select(-14,-4,-5,-10) %>% filter(Cancer.Sites != "Applicable" & Cancer.Sites != "Not" & Count != "Not" & Count != "Applicable") 
cancer_noall <- cancer_c %>% filter(Cancer.Sites.Code != 0 & Cancer.Sites.Code != "Breast-InSitu"  & Cancer.Sites.Code != "Breast-InSitu-Male" & Cancer.Sites.Code != "Breast-InSitu-Female")

a <- c(which(cancer_noall$Cancer.Sites.Code == "21041-21049")[1] : (which(cancer_noall$Cancer.Sites.Code == "21060")[1] - 1))
cancer_noall <- cancer_noall[-a,]
b <- c(which(cancer_noall$Cancer.Sites.Code == "21071")[1] : (which(cancer_noall$Cancer.Sites.Code == "21080")[1] - 1))
cancer_noall <- cancer_noall[-b,]
c <- c(which(cancer_noall$Cancer.Sites.Code == "33011")[1] : (which(cancer_noall$Cancer.Sites.Code == "33041-33042")[1] - 1))
cancer_noall <- cancer_noall[-c,]
d <- c(which(cancer_noall$Cancer.Sites.Code == "33041")[1] : (which(cancer_noall$Cancer.Sites.Code == "34000")[1] - 1))
cancer_noall <- cancer_noall[-d,]
e <- c(which(cancer_noall$Cancer.Sites.Code == "35013")[1] : (which(cancer_noall$Cancer.Sites.Code == "36010")[1] - 1))
cancer_noall <- cancer_noall[-e,]
cancer_noall$Race <- as.character(cancer_noall$Race)
cancer_noall[which(cancer_noall$Race == "American Indian or Alaska Native"),]$Race <- "AI-AN"
cancer_noall[which(cancer_noall$Race == "Asian or Pacific Islander"),]$Race <- "A-PI"
cancer_noall[which(cancer_noall$Race == "Black or African American"),]$Race <- "B-AA"
cancer_noall[which(cancer_noall$Race == "Other Races and Unknown combined"),]$Race <- "ORU"

labels <- c("20010-20100","21010-21130","22010-22060","23000", "24000", "25010-25020", "26000", "27010-27070","28010-28040", "29010-29040", "30000", "31010-31040", "32010-32020","33011-33042","34000","35011-35043", "36010", "36020", "37000")

cancer2_m <- cancer_noall[-which(cancer_noall$Cancer.Sites.Code %in% labels),]
cancer2 <- cancer2_m %>% filter(Count != "Suppressed")
cancer2$Count <- as.numeric(as.character(cancer2$Count))

shinyServer(function(input, output) {

  output$plot1 <- renderPlot({
    
    ifelse(input$gender == "Female", g <- "F", g <- "M")
    if(input$race == "AI-AN"){r <- "American Indian or Alaska Native"}
    if(input$race == "A-PI"){r <- "Asian or Pacific Islander"}
    if(input$race == "B-AA"){r <- "Black or African American"}
    if(input$race == "ORU"){r <- "Other Races and Unknown combined"}
    if(input$race == "White"){r <- "White"}
    
    if (input$gender != "All"){
      if(input$age != "All"){
        if (input$race != "All"){
          df <- unite(cancer2 %>% filter(Year == "2015"),"ars", c(4,6,7), sep = "_" ) 
          df <- df %>% dplyr::select(4,1,6) %>% tibble::rowid_to_column() %>% spread(Cancer.Sites, Count) %>% group_by(ars) %>% summarise_each(funs(sum(.,na.rm = T))) %>% dplyr::select(-2) %>% gather(Type, num, -ars)
          df$ars <- as.factor(df$ars)
          label <- paste(input$age,g,input$race, sep = "_")
          g8<- df %>%  group_by(ars)  %>% ungroup()%>% filter(num != 0) %>% filter(ars == label)
          ggplot(g8, aes(x = reorder(Type,num), y = num)) + 
            geom_bar(stat = "identity",position = "dodge") +
            xlab("" ) + ylab("Count") + 
            ggtitle("Incidence Count Distribution for Different Cancer Sites",subtitle = paste("-- For",input$age, input$gender,r, sep = " ")) + 
            scale_fill_discrete(name="Cancer Site") + theme(legend.position="None") + theme_grey(16)+coord_flip()
        }else{
        df <- unite(cancer2%>% filter(Year == "2015"),"age_sex", 4:5, sep = "_" ) 
        df <- df %>% dplyr::select(4,1,7) %>% tibble::rowid_to_column() %>% spread(Cancer.Sites, Count) %>% group_by(age_sex) %>% summarise_each(funs(sum(.,na.rm = T))) %>% dplyr::select(-2) %>% gather(Type, num, -age_sex)
        df$age_sex <- as.factor(df$age_sex)
        label <- paste(input$age,input$gender, sep = "_")
        g5<-  df %>%  group_by(age_sex) %>% ungroup()%>% filter(num != 0)%>% filter(age_sex == label)
        ggplot(g5, aes(x = reorder(Type,num), y = num)) + 
          geom_bar(stat = "identity",position = "dodge") +
          xlab("" ) + ylab("Count") + 
          ggtitle("Incidence Count Distribution for Different Cancer Sites",subtitle = paste("-- For",input$age, input$gender, sep = " ")) + 
          scale_fill_discrete(name="Cancer Site") + theme(legend.position="None") + theme_grey(16)+coord_flip()}
      }else{
      if (input$race != "All"){
        df <- unite(cancer2%>% filter(Year == "2015"),"sex_race", 6:7, sep = "_" ) 
        df <- df %>% dplyr::select(6,1,7) %>% tibble::rowid_to_column() %>% spread(Cancer.Sites, Count) %>% group_by(sex_race) %>% summarise_each(funs(sum(.,na.rm = T))) %>% dplyr::select(-2) %>% gather(Type, num, -sex_race)
        df$sex_race <- as.factor(df$sex_race)
        label <- paste(g,input$race, sep = "_")
        g6<-  df %>%  group_by(sex_race) %>% ungroup()%>% filter(num != 0)%>% filter(sex_race == label)
        ggplot(g6, aes(x = reorder(Type,num), y = num)) + 
          geom_bar(stat = "identity",position = "dodge") +
          xlab("" ) + ylab("Count") + 
          ggtitle("Incidence Count Distribution for Different Cancer Sites",subtitle = paste("-- For", input$gender,r, sep = " ")) + 
          scale_fill_discrete(name="Cancer Site") + theme(legend.position="None") + theme_grey(16)+coord_flip()
      }else{
      df <- cancer2 %>% filter(Year == "2015") %>% dplyr::select(5,1,8) %>% tibble::rowid_to_column() %>% spread(Cancer.Sites, Count) %>% group_by(Sex) %>% summarise_each(funs(sum(.,na.rm = T))) %>% dplyr::select(-2) %>% gather(Type, num, -Sex)
      df$Sex <- as.factor(df$Sex)
      g3 <- df %>%  group_by(Sex)  %>% ungroup() %>% filter(num != 0)%>% filter(Sex == input$gender)
      ggplot(g3, aes(x = reorder(Type,num), y = num)) + 
        geom_bar(stat = "identity",position = "dodge") +
        xlab("" ) + ylab("Count") + 
        ggtitle("Incidence Count Distribution for Different Cancer Sites",subtitle = paste("-- For", input$gender, sep = " ")) + 
        scale_fill_discrete(name="Cancer Site") + theme(legend.position="None") + theme_grey(16)+coord_flip()}
    }}else{
    if (input$age != "All"){
      if(input$race != "All"){
        df <- unite(cancer2%>% filter(Year == "2015"),"age_race", c(4,7), sep = "_" ) 
        df <- df %>% dplyr::select(4,1,7) %>% tibble::rowid_to_column() %>% spread(Cancer.Sites, Count) %>% group_by(age_race) %>% summarise_each(funs(sum(.,na.rm = T))) %>% dplyr::select(-2) %>% gather(Type, num, -age_race)
        df$age_race <- as.factor(df$age_race)
        label <- paste(input$age,input$race, sep = "_")
        g7<-  df %>%  group_by(age_race)  %>% ungroup()%>% filter(num != 0)%>% filter(age_race == label)
        ggplot(g7, aes(x = reorder(Type,num), y = num)) + 
          geom_bar(stat = "identity",position = "dodge") +
          xlab("" ) + ylab("Count") + 
          ggtitle("Incidence Count Distribution for Different Cancer Sites",subtitle = paste("-- For", input$age,r, sep = " ")) + 
          scale_fill_discrete(name="Cancer Site") + theme(legend.position="None") + theme_grey(16) +coord_flip()
      }else{
      df <- cancer2 %>%  filter(Year == "2015") %>%  dplyr::select(4,1,8) %>% tibble::rowid_to_column() %>% spread(Cancer.Sites, Count) %>% group_by(Age.Groups.Code) %>% summarise_each(funs(sum(.,na.rm = T))) %>% dplyr::select(-2) %>% gather(Type, num, -Age.Groups.Code)
      df$Age.Groups.Code <- as.factor(df$Age.Groups.Code)
      g2 <-  df %>%  group_by(Age.Groups.Code) %>% ungroup()  %>% filter(num != 0)%>% filter(Age.Groups.Code == input$age)
      ggplot(g2, aes(x = reorder(Type,num), y = num)) + 
        geom_bar(stat = "identity",position = "dodge") +
        xlab("" ) + ylab("Count") + 
        ggtitle("Incidence Count Distribution for Different Cancer Sites",subtitle = paste("-- For", input$age, sep = " ")) + 
        scale_fill_discrete(name="Cancer Site") + theme(legend.position="None") + theme_grey(16)+coord_flip()}
      
    }else{
    if(input$race != "All"){
      df <- cancer2 %>%  filter(Year == "2015") %>%  dplyr::select(7,1,8) %>% tibble::rowid_to_column() %>% spread(Cancer.Sites, Count) %>% group_by(Race) %>% summarise_each(funs(sum(.,na.rm = T))) %>% dplyr::select(-2) %>% gather(Type, num, -Race)
      df$Race <- as.factor(df$Race)
      g4 <- df %>%  group_by(Race)  %>% ungroup() %>% filter(num != 0) %>% filter(Race == input$race)
      ggplot(g4, aes(x = reorder(Type,num), y = num)) + 
        geom_bar(stat = "identity",position = "dodge") +
        xlab("" ) + ylab("Count") + 
        ggtitle("Incidence Count Distribution for Different Cancer Sites",subtitle = paste("-- For", r, sep = " ")) + 
        scale_fill_discrete(name="Cancer Site") + theme(legend.position="None") + theme_grey(16)+coord_flip()
    }else{
    df <- cancer2 %>%  dplyr::select(3,1,8) %>% tibble::rowid_to_column() %>% spread(Cancer.Sites, Count) %>% group_by(Year) %>% summarise_each(funs(sum(.,na.rm = T))) %>% dplyr::select(-2) %>% gather(Type, num, -Year)
    df$Year <- as.factor(df$Year)
    g1 <-  df %>%  group_by(Year)  %>% ungroup()%>% filter(num != 0) %>% filter(Year == "2015")
    ggplot(g1, aes(x = reorder(Type,num), y = num)) + 
      geom_bar(stat = "identity",position = "dodge") +
      xlab("" ) + ylab("Count") + 
      ggtitle("Incidence Count Distribution for Different Cancer Sites", subtitle = "-- For all") + 
      scale_fill_discrete(name="Cancer Site") + theme(legend.position="None") + theme_grey(16)+coord_flip()}}
    }
  })
  
  output$image1 <- renderImage({
    if(input$gender == "Female"){
      if(input$age == "20-24"){
              if(input$race == "AI-AN"){
                return(list(src = "",
                            width = 900,
                            height = 500, alt = "No recorded data"))
                  }
               if(input$race == "A-PI"){
                 return(list(src = "images/body(1)-4.jpg",
                             contentType = "image/jpg",
                             width = 900,
                             height = 500, alt = "Face"))
                   }
               if(input$race == "B-AA"){
                 return(list(src = "images/body(1)-6.jpg",
                             contentType = "image/jpg",
                             width = 900,
                             height = 500, alt = "Face"))
                   }
               if(input$race == "ORU"){
                 return(list(src = "images/body(1)-7.jpg",
                             contentType = "image/jpg",
                             width = 900,
                             height = 500, alt = "Face"))
                   }
               if(input$race == "White"){
                 return(list(src = "images/body(1)-8.jpg",
                             contentType = "image/jpg",
                             width = 900,
                             height = 500, alt = "Face"))
                   }
              if(input$race == "All"){
                return(list(src = "images/body(1)-33.jpg",
                            contentType = "image/jpg",
                            width = 900,
                            height = 500, alt = "Face"))
                   }
           }
      if(input$age == "25-29"){
               if(input$race == "AI-AN"){
                 return(list(src = "images/body(1)-10.jpg",
                             contentType = "image/jpg",
                             width = 900,
                             height = 500, alt = "Face"))
                  }
               if(input$race == "A-PI"){
                 return(list(src = "images/body(1)-9.jpg",
                             contentType = "image/jpg",
                             width = 900,
                             height = 500, alt = "Face"))
                  }
               if(input$race == "B-AA"){
                 return(list(src = "images/body(1)-11.jpg",
                             contentType = "image/jpg",
                             width = 900,
                             height = 500, alt = "Face"))
                 }
               if(input$race == "ORU"){
                 return(list(src = "images/body(1)-12.jpg",
                             contentType = "image/jpg",
                             width = 900,
                             height = 500, alt = "Face"))
                }
               if(input$race == "White"){
                 return(list(src = "images/body(1)-13.jpg",
                             contentType = "image/jpg",
                             width = 900,
                             height = 500, alt = "Face"))
                 }
               if(input$race == "All"){
                 return(list(src = "images/body(1)-34.jpg",
                             contentType = "image/jpg",
                             width = 900,
                             height = 500, alt = "Face"))
                 }
      }
      if(input$age == "All"){
        if(input$race == "AI-AN"){
          return(list(src = "images/body(1)-38.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
        if(input$race == "A-PI"){
          return(list(src = "images/body(1)-37.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
        if(input$race == "B-AA"){
          return(list(src = "images/body(1)-39.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
        if(input$race == "ORU"){
          return(list(src = "images/body(1)-40.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
        if(input$race == "White"){
          return(list(src = "images/body(1)-41.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
        if(input$race == "All"){
          return(list(src = "images/body(1)-24.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
      }
    }
    if(input$gender == "Male"){
      if(input$age == "20-24"){
        if(input$race == "AI-AN"){
          return(list(src = "",
                      width = 900,
                      height = 500, alt = "No recorded data"))
        }
        if(input$race == "A-PI"){
          return(list(src = "images/body(1)-14.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
        if(input$race == "B-AA"){
          return(list(src = "images/body(1)-16.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
        if(input$race == "ORU"){
          return(list(src = "images/body(1)-17.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
        if(input$race == "White"){
          return(list(src = "images/body(1)-18.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
        if(input$race == "All"){
          return(list(src = "images/body(1)-35.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
      }
      if(input$age == "25-29"){
        if(input$race == "AI-AN"){
          return(list(src = "images/body(1)-20.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
        if(input$race == "A-PI"){
          return(list(src = "images/body(1)-19.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
        if(input$race == "B-AA"){
          return(list(src = "images/body(1)-21.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
        if(input$race == "ORU"){
          return(list(src = "images/body(1)-22.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
        if(input$race == "White"){
          return(list(src = "images/body(1)-23.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
        if(input$race == "All"){
          return(list(src = "images/body(1)-36.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
      }
      if(input$age == "All"){
        if(input$race == "AI-AN"){
          return(list(src = "images/body(1)-43.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
        if(input$race == "A-PI"){
          return(list(src = "images/body(1)-42.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
        if(input$race == "B-AA"){
          return(list(src = "images/body(1)-44.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
        if(input$race == "ORU"){
          return(list(src = "images/body(1)-45.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
        if(input$race == "White"){
          return(list(src = "images/body(1)-46.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
        if(input$race == "All"){
          return(list(src = "images/body(1)-25.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
      }
    }
    if(input$gender == "All"){
      if(input$age == "20-24"){
        if(input$race == "AI-AN"){
          return(list(src = "",
                      width = 900,
                      height = 500, alt = "No recorded data"))
        }
        if(input$race == "A-PI"){
          return(list(src = "images/body(1)-47.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
        if(input$race == "B-AA"){
          return(list(src = "images/body(1)-49.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
        if(input$race == "ORU"){
          return(list(src = "images/body(1)-50.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
        if(input$race == "White"){
          return(list(src = "images/body(1)-51.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
        if(input$race == "All"){
          return(list(src = "images/body(1)-26.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
      }
      if(input$age == "25-29"){
        if(input$race == "AI-AN"){
          return(list(src = "images/body(1)-53.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
        if(input$race == "A-PI"){
          return(list(src = "images/body(1)-52.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
        if(input$race == "B-AA"){
          return(list(src = "images/body(1)-54.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
        if(input$race == "ORU"){
          return(list(src = "images/body(1)-55.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
        if(input$race == "White"){
          return(list(src = "images/body(1)-56.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
        if(input$race == "All"){
          return(list(src = "images/body(1)-27.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
      }
      if(input$age == "All"){
        if(input$race == "AI-AN"){
          return(list(src = "images/body(1)-29.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
        if(input$race == "A-PI"){
          return(list(src = "images/body(1)-28.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
        if(input$race == "B-AA"){
          return(list(src = "images/body(1)-30.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
        if(input$race == "ORU"){
          return(list(src = "images/body(1)-31.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
        if(input$race == "White"){
          return(list(src = "images/body(1)-32.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
        if(input$race == "All"){
          return(list(src = "images/body(1)-3.jpg",
                      contentType = "image/jpg",
                      width = 900,
                      height = 500, alt = "Face"))
        }
      }
    }
    }, deleteFile = FALSE)

})
