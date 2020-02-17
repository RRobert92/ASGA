ncol<- ncol(Segments)

Segments_1 <- Segments %>% filter_at(vars(starts_with(Pole1)),
                                     any_vars(.>= 1))
Segments_1 <- Segments_1 %>% select("Segment ID",
                                    "length",
                                    "Node ID #1",
                                    "Node ID #2",
                                    "Point IDs")
nrow_1 <- seq(from = 1, to = nrow(Segments_1),by = 1)

Segments_2 <- Segments %>% filter_at(vars(starts_with(Pole2)),
                                     any_vars(.>= 1))
Segments_2 <- Segments_2 %>% select("Segment ID",
                                    "length",
                                    "Node ID #1",
                                    "Node ID #2",
                                    "Point IDs")
nrow_2 <- seq(from = 1, to = nrow(Segments_2),by = 1)

Pole1 <- Nodes %>% filter_at(vars(Pole1),
                             any_vars(.>=1))
Pole1 <- data.frame(X = c(Pole1 %>% select("X Coord")/10000),
                    Y = c(Pole1 %>% select("Y Coord")/10000),
                    Z = c(Pole1 %>% select("Z Coord")/10000))
Pole2 <- Nodes %>% filter_at(vars(Pole2),
                             any_vars(.>=1))
Pole2 <- data.frame(X = c(Pole2 %>% select("X Coord")/10000),
                    Y = c(Pole2 %>% select("Y Coord")/10000),
                    Z = c(Pole2 %>% select("Z Coord")/10000))

Nodes <- Nodes %>% select("Node ID", 
                          "X Coord",
                          "Y Coord",
                          "Z Coord")

Nodes[2:4] <- Nodes[2:4]/10000

Points <- Points %>% select("Point ID", 
                            "X Coord",
                            "Y Coord",
                            "Z Coord")
Points[2:4] <- Points[2:4]/10000