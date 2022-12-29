library(ggplot2)
library(raster)
library(tidyverse)

setwd("D:/School/Transportation Network Analysis/My Project")
Nodes <- read.csv("Nodes.csv")
Links <- read.csv("Links.csv")
        
# Links$source[1], Links$target[1]

Links_Plot <- ggplot()
for (i in 1:nrow(Links)) {
        SRC_TAR <- c(Links$target[i], Links$source[i])
        Line <- Nodes %>% filter(Nodes$ID %in% SRC_TAR)
        
        Links$Distance[i] <- round(pointDistance(c(Line$x[1],Line$y[1]), c(Line$x[2],Line$y[2]), lonlat = F),1)
        Links$Label_x[i] <- mean(Line$x)
        Links$Label_Y[i] <- mean(Line$y)
        
        Links_Plot <- Links_Plot +geom_line(data = Line,aes(x = x, y = y))
}

Final_Network <- Links_Plot + geom_point(data = Nodes, aes(x = x, y = y ),colour = "red") +
        geom_text(data = Nodes, aes(x = x , y = y ,label = ID),colour = "blue",nudge_x = 0.75, nudge_y = 0)+
        geom_text(data = Links, aes(x = Label_x , y = Label_Y ,label = paste0(round(Distance,1),"m")),colour = "red",nudge_x = 0.75, nudge_y = 0,size = 3)+
        labs(x = "X", y = "Y",title = "Network For Carpooling")+
        theme(plot.title = element_text(hjust = 0.5))

# ggsave(plot = Final_Network,"Network.png")
Dijkstra  <- function(Nodes, Links, source.node, end.node){
        
        nodes <- pull(Nodes,ID)
        arcs <- as.matrix(Links[,c("source","target","Distance")])
        # source.node = 10
        # end.node = 22
        
        #Make a dataset has all the links as a two direction.
        arcs <- rbind(arcs, matrix(c(arcs[, 2], arcs[, 1], arcs[,3]), ncol = 3))
        
        #Source node
        tree.nodes <- nodes[source.node]
        #Generate zero weights for all nodes
        w <- rep(0, length(nodes))
        #Empty dataset that will has the minimum distance links
        tree.arcs <- matrix(ncol = 3)[-1, ]
        
        while (length(tree.nodes) < length(nodes)) {
                
                #All the links that has source node as origin or destination
                k <- which(arcs[, 1] %in% tree.nodes & arcs[, 2] %in% 
                                   nodes[-which(nodes %in% tree.nodes)])
                
                #Data of the links that has source node as origin or destination
                kArcs <- matrix(arcs[k, ], ncol = 3)
                #Minimum distance link connected to source node
                minW <- min(kArcs[, 3] + w[kArcs[, 1]])
                
                #Get the data of the link that has minimum distance
                min.arc <- matrix(kArcs[which(kArcs[, 3] + w[kArcs[,1]] == minW), ], ncol = 3)
                
                #Add minimum distance link to the dataset tree.arcs
                tree.arcs <- rbind(tree.arcs, min.arc[1, ])
                
                #Add the destination on the minimum distance link to the tree nodes
                tree.nodes <- c(tree.nodes, min.arc[1, 2])
                
                #Assign the minmium distance to the destination node 
                w[min.arc[1, 2]] <- w[min.arc[1, 2]] + minW
        }
        
        End_Node <- 0
        Path <- end.node
        while (!End_Node == source.node) {
                if (End_Node == 0) {
                        End_Node <- tree.arcs[which(tree.arcs[,2]==end.node),1]                
                }else {
                        End_Node <- tree.arcs[which(tree.arcs[,2]==End_Node),1]        
                }
                Path <- c(Path, End_Node)
        }
        Path <- Path[length(Path)[1]:1]
        output <- list(Path = Path, Distance = w[end.node], Tree_Arc = tree.arcs, Weight = w)
        return(output)
}

#Shortest path information from Dijkstra Algorithm
Path_and_Distance <- Dijkstra(Nodes = Nodes, Links = Links, source.node = 10, end.node = 1)

#Getting the data of the shortest path to plot it on the network 
ShortestPath <- Nodes[Nodes$ID %in% Path_and_Distance$Path,]

#Drawing the shortestpath with the network
ShortestPath_Plot <- Final_Network + geom_line(data = ShortestPath, aes(x,y),colour = "blue",size = 2)
# ggsave(plot = ShortestPath_Plot,"ShortestPath_10_1.png")

#Number of passengers and how many seats left.
First_Group_Passengers <- 2
Seats_Left <- 4 - First_Group_Passengers

#Pick up and drop off 
Destination <- 22
Origin <- 1

#Shortest path from origin to destination
Path_and_Distance_1 <- Dijkstra(Nodes = Nodes, Links = Links, source.node = Origin, end.node = Destination)
ShortestPath_1 <- Nodes[Nodes$ID %in% Path_and_Distance$Path,]
Nodes_O_D_1 <- Nodes[Nodes$ID %in% c(Origin,Destination),]

#Drawing the shortestpath with the network
ShortestPath_Plot_1 <- Final_Network + geom_line(data = ShortestPath, aes(x,y),colour = "blue",size = 2) +
        geom_point(data = Nodes_O_D_1, mapping = aes(x = x , y = y), size = 3, color = "Orange")
# ggsave(plot = ShortestPath_Plot_1,"ShortestPath_1_22.png")

#Requests
Requests_Data <- tibble(Requests_Node = c(15, 7, 18, 17), Number_Passengers = c(1, 1, 3, 1))
Current_Position <- 12
        
#Shortest path to all nodes from node 12 and to destination node 22
Path_and_Distance_2 <- Dijkstra(Nodes = Nodes, Links = Links, source.node = Current_Position, end.node = Destination)
Requests_Data$Distance_From_Current_Node <- Path_and_Distance_2$Weight[Requests_Data$Requests_Node]

#Drawing the requests, the driver current position, origin and destination for first group
ShortestPath_Plot_2 <- Final_Network + geom_line(data = ShortestPath, aes(x,y),colour = "blue",size = 2) +
        geom_point(data = Nodes_O_D_1, mapping = aes(x = x , y = y), size = 3, color = "Orange") +
        geom_point(data = Nodes[Current_Position,], mapping = aes(x = x , y = y), size = 3, color = "Green") +
        geom_point(data = Nodes[Requests_Data$Requests_Node,], mapping = aes(x = x , y = y), size = 3, color = "White")+
        labs(x = "X", y = "Y",title = "Requests On Network")
# ggsave(plot = ShortestPath_Plot_2,"Requests.png")

# First filter requests that has higher passengers than avilable seats and distance within 20m
Requests_Data <- Requests_Data %>% filter(Number_Passengers <= Seats_Left & Distance_From_Current_Node <20 )

#Filtering using third criteria
Distance_From_Request_To_Destination <- 1:nrow(Requests_Data) %>%  map(~ Dijkstra(Nodes = Nodes, Links = Links, source.node = Requests_Data$Requests_Node[.], end.node = Destination)[[4]][Destination]) 
Total_Distance_For_Each_Node <- 1:nrow(Requests_Data) %>% map_dbl(~Distance_From_Request_To_Destination[[.]][1] +Requests_Data$Distance_From_Current_Node[.] )
Accepted_Request <- which.min(Total_Distance_For_Each_Node)

#Accepted request data
Requests_Data <- Requests_Data[Accepted_Request,]

#Making final shortest path
Path_and_Distance_2 <- Dijkstra(Nodes = Nodes, Links = Links, source.node = Current_Position, end.node = Destination)
Current_Request_Destination <- c(Current_Position, Requests_Data$Requests_Node, Destination)
Final_Shortest_Path <- 1:2 %>% map(~ Dijkstra(Nodes = Nodes, Links = Links, source.node = Current_Request_Destination[.], end.node = Current_Request_Destination[.+1])[[1]]) %>% 
        unlist() %>% unique()
Final_Shortest_Path_Data <- Nodes[Final_Shortest_Path,] 

ShortestPath_Plot_3 <- Final_Network + geom_line(data = Final_Shortest_Path_Data, aes(x,y),colour = "blue",size = 2) +
        geom_point(data = Nodes[c(Current_Position, Destination),], mapping = aes(x = x , y = y), size = 3, color = "Orange")+
        labs(x = "X", y = "Y",title = "Final Shortest Path")

# ggsave(plot = ShortestPath_Plot_3,"Final_Shortest_Path.png",width = 8.62, height = 6.62)
        