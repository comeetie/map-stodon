library(httr)
library(dplyr)
library(stringr)


peers = jsonlite:::fromJSON("https://mapstodon.space/api/v1/instance/peers")
peers= c(peers,"mapstodon.space")
Links=list()
Lang=list()
for(peer in peers){
  ur <- paste0("https://",peer,"/api/v1/timelines/public?limit=25")
  next_url = ur;
  instances = c();
  languages = c();
  for(pages in 1:20){
    try({
      response = GET(next_url)
      link = headers(response)$link
      min_id = link |> str_extract("max_id=[0123456789]+") |> str_remove("max_id=")
      next_url = paste0(ur,"&max_id=",min_id)
      print(next_url)
      jsonRespText <- content(response, "text") 
      toots <- jsonlite::fromJSON(jsonRespText)
      instances = c(instances,toots$account$url |> str_remove("/@.*") |> str_remove("https://"))
      languages = c(languages,toots$language)
    })
    Sys.sleep(0.2)
  }
  try({
    peers_links=table(instances)
    Links[[peer]]=data.frame(instance=peer,from=names(peers_links),nb_toots=as.numeric(peers_links))
    peers_langs=table(languages)
    Lang[[peer]]=data.frame(instance=peer,from=names(peers_langs),nb_toots=as.numeric(peers_langs))
  })
}


seems_ok=sapply(Links,\(df){sum(df$nb_toots)})>400
sum(seems_ok)/length(peers)
Links_clean=Links[seems_ok]
Lang_clean=Lang[seems_ok]

Links.df = do.call(rbind,Links_clean)
Lang.df = do.call(rbind,Lang_clean)

library(readr)
write_csv(Links.df,"Links_raw.csv")                                
write_csv(Lang.df,"Langs_raw.csv")

# some cleaning
instances = unique(Links.df$instance)
Links.cl = Links.df |> mutate(from=str_remove(from,"/.*")) |>
  group_by(instance,from)|>
  summarise(nb_toots=sum(nb_toots)) |>
  filter(from %in% instances) |>
  ungroup()

Links.probs=  Links.cl |> 
  add_count(instance,wt = nb_toots) |>
  mutate(p=nb_toots/n) |>
  arrange(desc(p)) 

# get instances average prob
Nblink = sum(Links.cl$nb_toots)
Links.probs.average = Links.cl |>
  count(from,wt=nb_toots,name="nb_toots_total") |>
  mutate(paverage=nb_toots_total/Nblink) |>
  arrange(desc(paverage)) 

# some filtering + biner
Links.filtered = Links.probs |> left_join(Links.probs.average,by="from") |>
  mutate(logratio=log(p/paverage)) |>
  filter(logratio>log(5),nb_toots>10)

write_csv(Links.filtered,"Links_filtered.csv")


# individuals
library(rtoot)



token = auth_setup("mapstodon.space", "user")

directory = get_instance_directory("mapstodon.space",local = TRUE,limit=500)
attr(directory,"headers")

folowers = get_account_followers("109302019656613226",token=token,limit=200)
attr(folowers,"headers")

folowing = get_account_following("109302019656613226",token=token,limit=200)
