# Script for digitizing my triangle plot hybrid/parental type classifications
# Written by Amanda Meuser, Sept 2023

pdf("triangle_plot_boundaries.pdf", height = 9.4, width = 9.4)
  plot.new() 
  axis(1, at = seq(0, 1, by = 0.25))
  axis(2, at = seq(0, 1, by = 0.25))
  box()
  arrows(0,0,0.5,1, length=0, col="gray") # Triangle left
  arrows(0.5,1,1,0, length=0, col="gray") # Triangle right
  arrows(0,0,1,0, length=0, col="gray") # Triangle bottom
  arrows(0.375,0.75,0.625,0.75, length=0, col="gray") # F1 bottom
  arrows(0.1,0,0.1,0.2, length=0, col="gray") # P1 right
  arrows(0.9,0,0.9,0.2, length=0, col="gray") # P2 left
  arrows(0.1875,0.375,0.3125,0.375, length=0, col="gray") #BC1P1 bottom
  arrows(0.3125,0.375,0.3125,0.625, length=0, col="gray") #BC1P1 left
  arrows(0.6875,0.375,0.8125,0.375, length=0, col="gray") #BC1P2 bottom
  arrows(0.6875,0.375,0.6875,0.625, length=0, col="gray") #BC1P2 left
  arrows(0.375,0.25,0.375,0.625, length=0, col="gray") # F2/F3 left
  arrows(0.625,0.25,0.625,0.625, length=0, col="gray") # F2/F3 right
  arrows(0.375,0.375,0.625,0.375, length=0, col="gray") # F2/F3 middle
  arrows(0.375,0.25,0.625,0.25, length=0, col="gray") # F3 bottom
  arrows(0.375,0.625,0.625,0.625, length=0, col="gray") # F2 top
  mtext("F1", side=3, cex=1.5, outer=T, line=-12, adj = 0.52)
  mtext("F2", side=3, cex=1.5, outer=T, line=-23.5, adj = 0.52)
  mtext("F3", side=3, cex=1.5, outer=T, line=-30, adj = 0.52)
  mtext("Other", side=3, cex=1.5, outer=T, line=-37, adj = 0.52)
  mtext("P1", side=3, cex=1.5, outer=T, line=-39, adj = 0.16)
  mtext("P2", side=3, cex=1.5, outer=T, line=-39, adj = 0.88)
  mtext("BC1P1", side=3, cex=1.3, outer=T, line=-27, adj = 0.31)
  mtext("BC1P2", side=3, cex=1.3, outer=T, line=-27, adj = 0.735)
  mtext("Admixture Proportion (q)", side=3, cex=1.5, outer=T, line = -46)
  mtext("Inter-source Ancestry (Q)", side=2, cex=1.5, outer=T, line = -1.75)
dev.off()
