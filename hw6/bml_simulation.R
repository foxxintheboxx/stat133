#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.


source("bml_functions.r")

rCols = c("white", "red", "blue")
steps.large.gridlock <- bml.sim(200, 200, 0.4)
steps.large.free <- bml.sim(200, 200, 0.3)



#observe increase of density
# grids = array(0, dim = c(100,3,3))
# index = 1
# for (i in seq(5,20, by = 5)) {
#     r = 10 * i
#     c = 10 * i
#     inner.index = 1
#     for (p in seq(0.0,0.99, by = 0.01)) {
#         steps = 0
#         density = p
#         count = 0
#         for (j in 1:100) {
#             if (j %% 10 == 0) {
#                 print(j)
#             }
#             temp = bml.sim(r,c,p)
#             if (temp != 6000) {
#                 steps = steps + temp
#                 count = count + 1
#             }
#         }

#         grids[inner.index, 1, index] = density
#         if (count == 0) {
#             grids[inner.index, 2, index] = 6000
#             grids[inner.index, 3, index] = 0
#         } else {
#             grids[inner.index, 2, index] = steps/count
#             grids[inner.index, 3, index] = count/100
#         }
#         print("\n")
#         print(density)
#         print(grids[inner.index, 2, index])
#         print(grids[inner.index, 3, index])
        
#         inner.index = inner.index + 1
        
#     }
#     print(grids)
#     index = index + 1
    
# }

#grid.50 = data.frame(density= grids[,1,1],num.steps= grids[,2,1],p.gridlock= grids[,3,1] )
#grid.100 = data.frame(density= grids[,1,2],num.steps= grids[,2,2],p.gridlock= grids[,3,2] )
#grid.150 = data.frame(density= grids[,1,3],num.steps= grids[,2,3],p.gridlock= grids[,3,3] )


#save(grids[], file = "MedGrid.Rda")
load("LargeGrid.Rda")
load("MedGrid.Rda")
load("SmallGrid.Rda")

rCols = c("dark green", "green", "light blue", "orange", "red", "dark red")
col.key = c("0", "0 < p <= 25", "25 < p <= 50", "50 < p <= 75", "75 < p <= 99", "1")
plot(x=grid.50$density, y = log(grid.50$num.steps), xlab = "density of matrix", ylab = "log of Steps till Gridlock", main = "50 by 50 BML simulation")
points(x = grid.50[grid.50$p.gridlock == 0,]$density, y = log(grid.50[grid.50$p.gridlock == 0,]$num.steps), col = rCols[1], bg = rCols[1], pch = 21)
points(x = grid.50[grid.50$p.gridlock <= 0.25 & grid.50$p.gridlock > 0.00,]$density, y = log(grid.50[grid.50$p.gridlock <= 0.25 & grid.50$p.gridlock > 0.00,]$num.steps), col = rCols[2], bg = rCols[2], pch = 21)
points(x = grid.50[grid.50$p.gridlock <= 0.50 & grid.50$p.gridlock > 0.25,]$density, y = log(grid.50[grid.50$p.gridlock <= 0.50 & grid.50$p.gridlock > 0.25,]$num.steps), col = rCols[3], bg = rCols[3], pch = 21)
points(x = grid.50[grid.50$p.gridlock <= 0.75 & grid.50$p.gridlock > 0.5,]$density, y = log(grid.50[grid.50$p.gridlock <= 0.75 & grid.50$p.gridlock > 0.5,]$num.steps), col = rCols[4], bg = rCols[4], pch = 21)
points(x = grid.50[grid.50$p.gridlock <= 0.99 & grid.50$p.gridlock > 0.75,]$density, y = log(grid.50[grid.50$p.gridlock <= 0.99 & grid.50$p.gridlock > 0.75,]$num.steps), col = rCols[5], bg = rCols[5], pch = 21)
points(x = grid.50[grid.50$p.gridlock == 1,]$density, y = log(grid.50[grid.50$p.gridlock == 1,]$num.steps), col = rCols[6], bg = rCols[6], pch = 21)
abline(a = NULL, b = NULL, h = NULL, v = 0.32, col = "grey")
actual = max(grid.50[grid.50$p.gridlock == 0,]$density)
text(x = c(0.32, actual), y= c(4, 5), labels = c("expected 0.32", paste("observed", as.character(actual), sep = " ")),cex = 0.7, adj = c(1, -0.5))
abline(a = NULL, b = NULL, h = NULL, v = actual, col = "dark blue")
legend(legend = col.key, fill = rCols, "topright", title = "probability of Gridlock")

plot(x=grid.100$density, y = log(grid.100$num.steps), xlab = "density of matrix", ylab = "log of Steps till Gridlock", main = "100 by 100 BML simulation")
points(x = grid.100[grid.100$p.gridlock == 0,]$density, y = log(grid.100[grid.100$p.gridlock == 0,]$num.steps), col = rCols[1], bg = rCols[1], pch = 21)
points(x = grid.100[grid.100$p.gridlock <= 0.25 & grid.100$p.gridlock > 0.00,]$density, y = log(grid.100[grid.100$p.gridlock <= 0.25 & grid.100$p.gridlock > 0.00,]$num.steps), col = rCols[2], bg = rCols[2], pch = 21)
points(x = grid.100[grid.100$p.gridlock <= 0.50 & grid.100$p.gridlock > 0.25,]$density, y = log(grid.100[grid.100$p.gridlock <= 0.50 & grid.100$p.gridlock > 0.25,]$num.steps), col = rCols[3], bg = rCols[3], pch = 21)
points(x = grid.100[grid.100$p.gridlock <= 0.75 & grid.100$p.gridlock > 0.5,]$density, y = log(grid.100[grid.100$p.gridlock <= 0.75 & grid.100$p.gridlock > 0.5,]$num.steps), col = rCols[4], bg = rCols[4], pch = 21)
points(x = grid.100[grid.100$p.gridlock <= 0.99 & grid.100$p.gridlock > 0.75,]$density, y = log(grid.100[grid.100$p.gridlock <= 0.99 & grid.100$p.gridlock > 0.75,]$num.steps), col = rCols[5], bg = rCols[5], pch = 21)
points(x = grid.100[grid.100$p.gridlock == 1,]$density, y = log(grid.100[grid.100$p.gridlock == 1,]$num.steps), col = rCols[6], bg = rCols[6], pch = 21)
abline(a = NULL, b = NULL, h = NULL, v = 0.32, col = "grey")
actual = max(grid.100[grid.100$p.gridlock == 0,]$density)
text(x = c(0.32, actual), y= c(4, 5), labels = c("expected 0.32", paste("observed", as.character(actual), sep = " ")),cex = 0.7, adj = c(1, -0.5))
abline(a = NULL, b = NULL, h = NULL, v = actual, col = "dark blue")
legend(legend = col.key, fill = rCols, "topright", title = "probability of Gridlock")


plot(x=grid.150$density, y = log(grid.150$num.steps), xlab = "density of matrix", ylab = "log of Steps till Gridlock", main = "150 by 150 BML simulation")
points(x = grid.150[grid.150$p.gridlock == 0,]$density, y = log(grid.150[grid.150$p.gridlock == 0,]$num.steps), col = rCols[1], bg = rCols[1], pch = 21)
points(x = grid.150[grid.150$p.gridlock <= 0.25 & grid.150$p.gridlock > 0.00,]$density, y = log(grid.150[grid.150$p.gridlock <= 0.25 & grid.150$p.gridlock > 0.00,]$num.steps), col = rCols[2], bg = rCols[2], pch = 21)
points(x = grid.150[grid.150$p.gridlock <= 0.50 & grid.150$p.gridlock > 0.25,]$density, y = log(grid.150[grid.150$p.gridlock <= 0.50 & grid.150$p.gridlock > 0.25,]$num.steps), col = rCols[3], bg = rCols[3], pch = 21)
points(x = grid.150[grid.150$p.gridlock <= 0.75 & grid.150$p.gridlock > 0.5,]$density, y = log(grid.150[grid.150$p.gridlock <= 0.75 & grid.150$p.gridlock > 0.5,]$num.steps), col = rCols[4], bg = rCols[4], pch = 21)
points(x = grid.150[grid.150$p.gridlock <= 0.99 & grid.150$p.gridlock > 0.75,]$density, y = log(grid.150[grid.150$p.gridlock <= 0.99 & grid.150$p.gridlock > 0.75,]$num.steps), col = rCols[5], bg = rCols[5], pch = 21)
points(x = grid.150[grid.150$p.gridlock == 1,]$density, y = log(grid.150[grid.150$p.gridlock == 1,]$num.steps), col = rCols[6], bg = rCols[6], pch = 21)
abline(a = NULL, b = NULL, h = NULL, v = 0.32, col = "grey")
actual = max(grid.150[grid.150$p.gridlock == 0,]$density)
text(x = c(0.32, actual), y= c(4, 5), labels = c("expected 0.32", paste("observed", as.character(actual), sep = " ")),cex = 0.7, adj = c(1, -0.5))
abline(a = NULL, b = NULL, h = NULL, v = actual, col = "dark blue")
legend(legend = col.key, fill = rCols, "topright", title = "probability of Gridlock")





