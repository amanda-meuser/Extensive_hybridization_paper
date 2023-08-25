# colour palette for master's thesis plots

library(RColorBrewer)
# get info on package
?RColorBrewer()
display.brewer.all(colorblindFriendly = T)

cols <- brewer.pal(12, "Paired")
color <- brewer.pal(9, "Greys")
colorbrown <- brewer.pal(11, "BrBG")
set1 <- brewer.pal(8, "Set1")
colorpink <- brewer.pal(11, "PiYG")
set3 <- brewer.pal(12, "Set3")
set3

# dark colours for parentals and light colours for hybrids w creek chub
fishy_col <- c("#8E0152", #CC?
               #"#8DD3C7",
               "#A6CEE3", #HHCxCC
               "#1F78B4", #HHC
               "#B2DF8A", #RCxCC
               "#33A02C", #RC
               "#FB9A99", #RFSxCC
               "#E31A1C", #RFS
               "#FDBF6F", #CSxCC
               "#FF7F00", #CS
               "#CAB2D6", #LNDxCC
               "#6A3D9A", #LND
               "#FFFF99", #SSxCC
               "#FFFF33", #SS
               "#8C510A", #CSRxCC
               "#543005", #CSR
               "#969696", #BNDxCC
               "#000000") #BND


parentals <- c("#8E0152", #CC?
               "#1F78B4", #HHC
               "#33A02C", #RC
               "#E31A1C", #RFS
               "#FF7F00", #CS
               "#6A3D9A", #LND
               "#FFFF33", #SS
               "#543005", #CSR
               "#000000") #BND

# in order for k=9
colour <- c("#8E0152", "#6A3D9A", "#FFFF33", "#E31A1C", "#543005", "#FF7F00", "#33A02C", "#000000", "#1F78B4") 



pink <- brewer.pal(8, "PuRd")
pink[4]
green <- brewer.pal(10, "BrBG")
green[8]

parentals2 <- c("#A6CEE3", #CC
                "#35978F", #HHC
                "#B2DF8A", #RC
                "#FB9A99", #RFS
                "#FDBF6F", #CS
                "#CAB2D6", #LND
                "#E5DF60", #SS
                "#8C510A", #CSR
                "#DF65B0", #BND
                "#000000", #v11,v12
                "#969696", #v7
                "#8E0152") #hybrid

               
# in order for k=12
colour <- c("#FDBF6F","#8C510A","#35978F","#CAB2D6","#E5DF60","#B2DF8A","#969696","#FB9A99","#A6CEE3","#DF65B0","#000000","#000000")

colour <- c("#A6CEE3","#FDBF6F","#DF65B0","#B2DF8A","#35978F","#FB9A99","#CAB2D6","#8C510A","#969696","#FFFF99","#000000","#000000")

