###############################################################################
###############################################################################
###############################################################################

# Parametry -------------------------------------------------------------------

nadpis <- "Survey on survey"
cislo_otazky <- 1:21


# Sankey Diagram --------------------------------------------------------------

df1 <- read.csv2("ICO.csv", fileEncoding="UTF-8-BOM")
#" df1 <- read.csv2("ICO - kopie.csv", fileEncoding="UTF-8-BOM")

df1 <- na.omit(df1)
df1 <- df1[,-c(2)]

# Musim vytvorim vsechny mozne kombinace aby byla ke kazde otazce cesta

## Vytvořím plochý data.frame jenvyberovych odpovedi
df2 <- data.frame(ICO = df1$ICO,
                  X1 = df1$X1,
                  
                  X3 = df1$X3,
                  X4 = df1$X4,
                  X5 = df1$X5,
                  X6 = df1$X6,
                  X7 = df1$X7,
                  X8_1 = df1$X8_výroba,
                  X8_2 = df1$X8_zam,
                  
                  X10 = df1$X10,
                  X11 = df1$X11,
                  X12 = df1$X12,
                  X13 = df1$X13,
                  X14 = df1$X14,
                  
                  X16 = df1$X16,
                  
                  X18 = df1$X18,
                  X19 = df1$X19
)

## Seskupím multiple otázky do jednoho vektoru
big_X2 <- c(df1$X2_1,
            df1$X2_2,
            df1$X2_3,
            df1$X2_4,
            df1$X2_5,
            df1$X2_6,
            df1$X2_7,
            df1$X2_8,
            df1$X2_9
            )
big_X9 <- c(df1$X9_1,
            df1$X9_2,
            df1$X9_3
            )
big_X15 <- c(df1$X15_1,
             df1$X15_2,
             df1$X15_3,
             df1$X15_4,
             df1$X15_5,
             df1$X15_6,
             df1$X15_7,
             df1$X15_8,
             df1$X15_9,
             df1$X15_10
             )
big_X17 <- c(df1$X17_1,
             df1$X17_2,
             df1$X17_3,
             df1$X17_4,
             df1$X17_5,
             df1$X17_6,
             df1$X17_7,
             df1$X17_8,
             df1$X17_9
             )

## Pak k nim přidám idenifkátor ICO
big_X2 <- cbind(ICO = df1$ICO, big_X2)
big_X9 <- cbind(ICO = df1$ICO, big_X9)
big_X15 <- cbind(ICO = df1$ICO, big_X15)
big_X17 <- cbind(ICO = df1$ICO, big_X17)

## A pak to začnu propojovat
df3 <- merge(df2, big_X2, by = "ICO")
df4 <- merge(df3, big_X9, by = "ICO")
df5 <- merge(df4, big_X15, by = "ICO")
df6 <- merge(df5, big_X17, by = "ICO")

## Nakonec odstraním ty kombinace odpovědí kde se v multiodpovědín nic nevybralo
df7 <- df6[-which(df6$big_X2 == 0),]
df8 <- df7[-which(df7$big_X9 == 0),]
df9 <- df8[-which(df8$big_X15 == 0),]
df10 <- df9[-which(df9$big_X17 == 0),]

## Úplně nakonec ještě odstraním identifikátor ICO a uspořádám tabulku

df <- data.frame(X1 = df10$X1,
                 X2 = df10$big_X2,
                 X3 = df10$X3,
                 X4 = df10$X4,
                 X5 = df10$X5,
                 X6 = df10$X6,
                 X7 = df10$X7,
                 X8_1 = df10$X8_1,
                 X8_2 = df10$X8_2,
                 X9 = df10$big_X9,
                 X10 = df10$X10,
                 X11 = df10$X11,
                 X12 = df10$X12,
                 X13 = df10$X13,
                 X14 = df10$X14,
                 X15 = df10$big_X15,
                 X16 = df10$X16,
                 X17 = df10$big_X17,
                 X18 = df10$X18,
                 X19 = df10$X19
)

dfx <- df[, cislo_otazky]


###############################################################################

library(dplyr)
library(tidyr)

links1 <-
        df %>%
        mutate(row = row_number()) %>%
        gather('column', 'source', -row) %>%
        mutate(column = match(column, names(df))) %>%
        group_by(row) %>%
        arrange(column) %>%
        mutate(target = lead(source)) %>%
        ungroup() %>%
        filter(!is.na(target))

links1 <- links1 %>% 
        group_by(source, target) %>% 
        summarise(value = n())
# ----

nodes1 <- data.frame(
        name = unique(c(links1$source, 
                        links1$target)
        )
)

links1$source <- match(links1$source, nodes1$name) - 1
links1$target <- match(links1$target, nodes1$name) - 1

nodes1$name <- sub('_[0-9]+$', '', nodes1$name)

# Graph -----------------------------------------------------------------------
library(networkD3)
library(htmlwidgets)


# sankeyNetwork(Links = links1, 
#               Nodes = nodes1, 
#               Source = 'source',
#               Target = 'target',
#               Value = 'value',
#               NodeID = 'name',
#               
#               # #  iterations = 0
#               #   fontSize = 10,
#              #    nodeWidth = 30
# ) 


library(plotly)
p3 <- plot_ly(
        type = "sankey",
        domain = c(
                x =  c(0,1),
                y =  c(0,1)
        ),
        # orientation = "h",
        valueformat = ".0f",
        
        node = list(
                label = nodes1$name,
                # color = json_data$data[[1]]$node$color,
                pad = 15,
                thickness = 15,
                line = list(
                        color = "black",
                        width = 0.5
                )
        ),
        
        link = list(
                source = links1$source,
                target = links1$target,
                value =  links1$value,
                # color =  json_data$data[[1]]$link$color,
                label = nodes1$name
        )
) %>% 
        layout(
                title = nadpis,
                font = list(
                        size = 00
                ),
                xaxis = list(showgrid = F,
                             zeroline = F,
                             fixedrange = F),
                yaxis = list(showgrid = F,
                             zeroline = F,
                             fixedrange = F)
                
        ) 

p3

###############################################################################
###############################################################################
###############################################################################
