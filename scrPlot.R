graph=jsonlite::fromJSON("graph.json")


class(graph$nodes)

library(ggplot2)
ggplot(graph$nodes)+geom_point(aes(x=attributes$x,y=attributes$y,size=attributes$`nansi-indegree`,color=factor(attributes$`nansi-louvain`)))+
  scale_size_area("",guide="none",max_size = 3)+
  scale_color_brewer(palette="Set3",guide="none")+
  theme_void()


library(dplyr)

graph$nodes |> group_by(attributes$`nansi-louvain`) |> top_n(3,attributes$`nansi-indegree`) |>
  transmute(indeg=attributes$`nansi-indegree`,com=attributes$`nansi-louvain`,lab= attributes$`label`) |> arrange(com) |> View()


mapstodon = graph$nodes |> filter(attributes$label=="mapstodon.space")
labels = graph$nodes |> 
  transmute(label=attributes$label,x=attributes$x,y=attributes$y,indeg=attributes$`nansi-indegree`) |> 
  arrange(desc(indeg)) |> 
  filter(indeg>=20,!is.na(label))

ggplot(graph$nodes)+geom_point(aes(x=attributes$x,y=attributes$y,size=attributes$`nansi-indegree`,color=factor(attributes$`nansi-louvain`)))+
  geom_point(data=mapstodon,aes(x=attributes$x,y=attributes$y,size=5*attributes$`nansi-indegree`),shape=1,color="red")+
  geom_text(data=labels,aes(x=x,y=y,label=label),size=1.5)+
  scale_size_area("",guide="none",max_size = 3)+
  scale_color_brewer(palette="Set3",guide="none")+
  theme_void()



