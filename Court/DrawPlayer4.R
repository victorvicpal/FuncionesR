DrawPlayer4 <- function(player,season,pv=0.01)
{
    
    load('Pxy3.Rdata')
    A <- Pxy3(player,season,pv)
    plot.title <- paste0(player,' // season = ',season,sep='')
    library('Rmisc')
    
    library(ggplot2)
    library(jpeg)
    library(grid)
    #SQL <- paste("select player, x ,y , if(result='made',1,0) as result, if(x<3 or x>47,1,if(y>(sqrt((23.71*23.71)-(x-25)*(x-25))+5.25),1,0)) as triple,if(y<=10 and x>=17 and x<=33,1,0) as zone1,if(y<=19 and y>10 and x>=17 and x<=33,1,0) as zone2,if(y>19 and (y>=((13.75/(-8))*(x-25)+5.25)) and (y>=((13.75/8)*(x-25)+5.25)) and y<=(sqrt((23.71*23.71)-(x-25)*(x-25))+5.25),1,0) as zone3,if(x<17 and x>=3 and (y<((13.75/(-8))*(x-25)+5.25)) and y<=(sqrt((23.71*23.71)-(x-25)*(x-25))+5.25),1,0) as zone4,if(x>33 and x<=47 and (y<((13.75/8)*(x-25)+5.25)) and y<=(sqrt((23.71*23.71)-(x-25)*(x-25))+5.25),1,0) as zone5 from nba.play_by_play where etype='shot' and y>3 and game_id in (select id from nba.game where season='",season,"') and player='",player,"';", sep="")
    SQL <- paste("select a.x, a.y, ifnull(b.ma,0)/(ifnull(b.ma,0)+ifnull(a.mi,0)) as p, ifnull(b.ma,0)+ifnull(a.mi,0) as n from (select x,y, count(player) as mi from nba.play_by_play where game_id in (select id from nba.game where season='",season,"')  and player='",player,"' and (y>=4 and y<40) and result='missed' group by x,y) a left join (select x,y, count(player) as ma from nba.play_by_play where game_id in (select id from nba.game where season='",season,"')  and player='",player,"' and (y>=4 and y<40) and result='made' group by x,y) b on (a.y=b.y and a.x=b.x);", sep="")
    M <- dbGetQuery(con,SQL)
    nt <- sum()

    #myurl <- "http://www.carpinteriakapitel.es/images/stories/virtuemart/product/white6.jpg"
    myurl <- "http://cdn.shopify.com/s/files/1/0241/9661/products/ParquetWhiteWood_web.jpg?v=1389726128"
    z <- tempfile()
    download.file(myurl,z,mode="wb")
    img <- readJPEG(z)
    file.remove(z)
    
    
    ggplot(data=data.frame(x=1,y=2),aes(x,y))+
    annotation_custom(rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), -Inf, Inf, -Inf, Inf) +
    #geom_polygon(fill = "#939393",data=data.frame(x=c(3,3,25+c(-22000:(-1)/1000,1:22000/1000),47,47),y=c(4,169/12,5.25+sqrt(23.75^2-c(-22000:(-1)/1000,1:22000/1000)^2),169/12,4)),aes(x=x,y=y))+
    #geom_polygon(fill = "#808080",data=data.frame(x=c(17,17,33,33,17),y=c(4,19,19,4,4)))+
        ###Tile + Contour
        geom_tile(aes(x=x,y=y,fill=p,alpha=0.8), data = A, size=0.1) +
        scale_fill_gradient2(low="#2771ff",mid="#F7FE00", high="#FF0D00",space = "Lab",midpoint = 0.5,limits=c(0,1)) +
        #stat_contour(aes(z=p),data=A,bins = 15, binwidth=0.5,linetype = 3,alpha=0.5) +
        #geom_point(data=M,aes(x,y,size=factor(result),alpha=0.5)) +
        
        ### Contour
        #stat_contour(aes(x=x,y=y,z=p,colour = ..level..),data=A,bins = 15, binwidth=10, size = 2) +
        #scale_colour_gradient(low = "#378aff", high = "red") +
        ### Contour areas
        #stat_contour(aes(x=x,y=y,z=p,fill=..level..),data=A,geom="polygon") +
        #scale_fill_gradient(low = "yellow", high = "red") +
geom_path(size=1.5,colour = "#ffffff",data=data.frame(x=c(0,50,50,0,0),y=c(4,4,40,40,4)))+           
           ###solid FT semicircle above FT line:
geom_path(size=1.5,colour = "#ffffff",data=data.frame(x=(25+c(-6000:(-1)/1000,1:6000/1000)),y=c(19+sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y))+
   ###dashed FT semicircle below FT line:
geom_path(size=1.5,colour = "#ffffff",data=data.frame(x=(25+c(-6000:(-1)/1000,1:6000/1000)),y=c(19-sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y),linetype='dashed')+
   ###key:
geom_path(size=1.5,colour = "#ffffff",data=data.frame(x=c(17,17,33,33,17),y=c(4,19,19,4,4)))+
   ###box inside the key:
geom_path(size=1.5,colour = "#ffffff",data=data.frame(x=c(19,19,31,31,19),y=c(4,19,19,4,4)))+
   ###restricted area semicircle:
geom_path(size=1.5,colour = "#ffffff",data=data.frame(x=(25+c(-4000:(-1)/1000,1:4000/1000)),y=c(5.25+sqrt(4^2-c(-4000:(-1)/1000,1:4000/1000)^2))),aes(x=x,y=y))+
   ###rim:
geom_path(size=1.5,colour = "#ffffff",data=data.frame(x=(25+c(-750:(-1)/1000,1:750/1000,750:1/1000,-1:-750/1000)),y=c(c(5.25+sqrt(0.75^2-c(-750:(-1)/1000,1:750/1000)^2)),c(5.25-sqrt(0.75^2-c(750:1/1000,-1:-750/1000)^2)))),aes(x=x,y=y))+
   ###backboard:
geom_path(size=1.5,colour = "#ffffff",data=data.frame(x=c(22,28),y=c(4,4)),lineend='butt')+
   ###three-point line:
geom_path(size=1.5,colour = "#ffffff",data=data.frame(x=c(3,3,25+c(-22000:(-1)/1000,1:22000/1000),47,47,3),y=c(4,169/12,5.25+sqrt(23.75^2-c(-22000:(-1)/1000,1:22000/1000)^2),169/12,4,4)),aes(x=x,y=y))+
        #scale_y_continuous(limits=c(min(A$y)-3,max(A$y)+3), expand=c(0,0))+
        #ggtitle(player) +
        #theme(panel.background = element_rect(fill = "#666666"),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        geom_point(data=M,aes(x,y,size=p*n,alpha=0.8)) +
        scale_size_continuous(range = c(3, 6)) +
        scale_y_continuous(limits=c(4, 40))+
        ggtitle(bquote(atop(.(plot.title), "")))+
        coord_fixed()


}