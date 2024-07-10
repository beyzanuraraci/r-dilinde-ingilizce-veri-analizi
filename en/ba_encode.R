
#YAZILIM DILLERIYLE DUYGU VE EMOJI ANALIZI :INGILIZCE PERSPEKTIF

install.packages("tuber")
install.packages("magrittr")
install.packages("purrr")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("promises")
install.packages("httpuv")
install.packages("readr")
install.packages("readxl")
install.packages("stringi")
install.packages("stringr")
install.packages("tm")
install.packages("pander")
install.packages("RCurl")
install.packages("wordcloud")
install.packages("ROAuth")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("tidytext")
install.packages("RColorBrewer")
install.packages("ggthemes")
install.packages("ggpubr")
install.packages("formattable")
install.packages("psych")
install.packages("GGally")
install.packages("rstatix")
install.packages("sentimentr")
install.packages("webshot")
install.packages("htmlwidgets")
install.packages("syuzhet")
install.packages("skimr")
install.packages("janitor")
install.packages("openssl")
install.packages("writexl")
install.packages("stopwords")
install.packages("devtools")
install.packages("tibble")
install.packages("rmarkdown")
install.packages("ggstance")
install.packages("pastecs")
install.packages("kableExtra")
install.packages("citation")
install.packages("lubridate")

library(tuber) #Youtube videolarinda yayinlanan videolar icin kullanilir.
library(magrittr) #Fonksiyonlari %>% operatoru ile birbirine baglar.
library(purrr) #D0slevler ve vektorlerle calismak icin eksiksiz ve tutarli bir arac seti saglayan paket.
library(dplyr) #Veri manipC<lasyon islemini yapar.
library(tidyverse) # veri setlerini dC<zenli, anlasilir ve etkili bir sekilde islemeyi saDlayan pakettir.
library(httpuv) #HTTP ve webSocket isteklerini islemek icin dC<sC<k seviyeli soket ve protokol destegi saglayan paket.
library(readr) # verileri okumayi kolaylastirir.
library(readxl) #Excel dosyalarini okur ve R'a yukleme yapar.
library(stringi) #Hizli ve tasinabilir karakter dizisi isleme tesisleri paketi.
library(stringr) #Karakter yapili veriler icin kullanilan paket.
library(tm) #Metin madenciliginde kullanilan paket.
library(pander) # tablolari ve cerceveleri goruntulemek icin tasarlanmistir.
library(wordcloud) #Kelime bulutu icin kullanilan pakettir.
library(ROAuth) # OAuth 1.0 protokolunu uygulamak icin kullanilan bir pakettir.
library(ggplot2) #Verileri gorsellestirmek icin kullanilan paket.
library(tidytext) #DC<zenli veri ilkelerini kullanmak ve bircok metin madenciligi gorevini yerine getirmek icin kullanilir.
library(RColorBrewer) #Kelime bulutunun renklendirilmesi icin kullanilir.
library(ggthemes) #grafiklerinizi daha cekici ve profesyonel gorunumlu hale getirmenize yardimci olur.
library(ggpubr) #ggplot2 ile birlikte kullanilir.
library(GGally) #cesitli grafik araclarini bir araya getiren bir pakettir. 
library(skimr) #veri cercevelerini ve veri setlerini ozetlemek ve kesfetmek icin kullanilan bir pakettir.
library(janitor) #veri setlerini temizlemeye yarar.
library(writexl) #Elde edilen verilerin excel formatinda disa aktarilmasi icin kullanilan pakettir.
library(stopwords) #Veri dosyasindan atilacak kelimeleri cikarmaya yarar. 
library(ggstance) #grafikler olusturmaya yarar.
library(xlsx) #Excel  dosyalarini okumak, yazmak ve bicimlendirmek icin kullanilan pakettir.
library(plyr)
library(ggeasy)
library(RCurl)
library(psych)
library(formattable)
library(rstatix)
library(sentimentr)
library(webshot)
library(htmlwidgets)
library(syuzhet)
library(skimr)
library(janitor)
library(openssl)
library(writexl)
library(stopwords)
library(devtools)
library(tibble)
library(rmarkdown)
library(ggstance)
library(pastecs)
library(kableExtra)
library(citation)
library(lubridate)



#API'LER ILE YOUTUBE VIDEOSUNDAN VERI CEKIYORUZ
client_id <- "********" 
client_secret <-"*********"
yt_oauth(client_id, client_secret, token ='')




#VIDEO URLDEKI '=' IBARESINDEN SONRAKI KISMI ALIYORUZ

#YORUMLARI CEKIYORUZ
A1<- get_all_comments(video_id = "DGrszAeMZJI")

B1 <- get_all_comments(video_id = "W-d-t5mtSZM")

C1<- get_all_comments(video_id = "4lcwTGA7MZw")

D1<- get_all_comments(video_id = "YhaPN9_lHPw")



#VERILERIMIZI BIRLESTIRIYORUZ
en_data <- bind_rows(A1,B1,C1,D1 )

#ISIMIZE YARAMAYACAK SUTUNLARI SILIYORUZ
en_data<-en_data[-1:-3]
en_data<-en_data[-2:-13]

#VERIYI BILGISAYARA KAYDET
write_xlsx(en_data,"C:/Users/beyza/Desktop/dataen.xlsx")

#EMOJILERDEN ONCE KELIMELERLE VE DUYGU ANALIZINI HALLEDELIM
en_data1<-en_data

#VERI TEMIZLEME ISLEMI BASLASIN

#RakamlarD1n temizlenmesi
en_data1$textOriginal <- removeNumbers(en_data1$textOriginal)

#URL link temizleme
en_data1$textOriginal <- str_replace_all(en_data1$textOriginal, "http[^[:space:]]*","")

#Tum harflerin kuculmesi
en_data1$textOriginal <- str_to_lower(en_data1$textOriginal,"tr")

#hastag ve @ kaldD1rD1lmasD1
en_data1$textOriginal <- str_replace_all(en_data1$textOriginal, "#\\S+","")
en_data1$textOriginal <- str_replace_all(en_data1$textOriginal, "@\\S+","")

#noktlama isaretleri temizlenmesi
en_data1$textOriginal <- str_replace_all(en_data1$textOriginal, "[[:punct:][:blank:]]+", " ")

#ASCII formatD1na uymayan karakterlerin temizlenmesi
en_data1$textOriginal <- str_replace_all (en_data1$textOriginal, "[<].*[>]", " ") 
en_data1$textOriginal<- gsub ("\uFFFD", "", en_data1$textOriginal, fixed = TRUE)
en_data1$textOriginal <- gsub("\n","", en_data1$textOriginal, fixed = TRUE)

#Alfabetik olmayan karakterlerin temizlenmesi--emojileri siliyor
en_data1$textOriginal <- str_replace_all(en_data1$textOriginal, "[^[:alnum:]]"," ")
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

#stopwordsler
library(stopwords)

stopwords::stopwords("en",source = "stopwords-iso")

liste<- stopwords::stopwords("en",source = "stopwords-iso")

kelime_liste = c( "'ll","'tis","'twas","'ve","10","39","a","a's","able","ableabout","about","above","abroad","abst","accordance","according","accordingly",
                  "across","act","actually","ad","added","adj","adopted","ae","af","affected","affecting","affects","after","afterwards","ag","again",
                  "against","ago","ah","ahead","ai","ain't","aint","al","all","allow","allows","almost","alone","along","alongside","already","also",
                  "although","always","am","amid","amidst","among","amongst","amoungst","amount","an","and","announce","another","any","anybody",
                  "anyhow","anymore","anyone","anything","anyway","anyways","anywhere","ao","apart","apparently","appear","appreciate","appropriate",
                  "approximately","aq","ar","are","area","areas","aren","aren't","arent","arise","around","arpa","as","aside","ask","asked","asking",
                  "asks","associated","at","au","auth","available","aw","away","awfully","az","b","ba","back","backed","backing","backs","backward",
                  "backwards","bb","bd","be","became","because","become","becomes","becoming","been","before","beforehand","began","begin","beginning",
                  "beginnings","begins","behind","being","beings","believe","below","beside","besides","best","better","between","beyond","bf","bg","bh",
                  "bi","big","bill","billion","biol","bj","bm","bn","bo","both","bottom","br","brief","briefly","bs","bt","but","buy","bv","bw","by","bz",
                  "c","c'mon","c's","ca","call","came","can","can't","cannot","cant","caption","case","cases","cause","causes","cc","cd","certain",
                  "certainly","cf","cg","ch","changes","ci","ck","cl","clear","clearly","click","cm","cmon","cn","co","co.","com","come","comes","computer",
                  "con","concerning","consequently","consider","considering","contain","containing","contains","copy","corresponding","could","could've",
                  "couldn","couldn't","couldnt","course","cr","cry","cs","cu","currently","cv","cx","cy","cz","d","dare","daren't","darent","date","de",
                  "dear","definitely","describe","described","despite","detail","did","didn","didn't","didnt","differ","different","differently","directly",
                  "dj","dk","dm","do","does","doesn","doesn't","doesnt","doing","don","don't","done","dont","doubtful","down","downed","downing","downs",
                  "downwards","due","during","dz","e","each","early","ec","ed","edu","ee","effect","eg","eh","eight","eighty","either","eleven","else",
                  "elsewhere","empty","end","ended","ending","ends","enough","entirely","er","es","especially","et","et-al","etc","even","evenly","ever",
                  "evermore","every","everybody","everyone","everything","everywhere","ex","exactly","example","except","f","face","faces","fact","facts",
                  "fairly","far","farther","felt","few","fewer","ff","fi","fifteen","fifth","fifty","fify","fill","find","finds","fire","first","five","fix",
                  "fj","fk","fm","fo","followed","following","follows","for","forever","former","formerly","forth","forty","forward","found","four","fr","free",
                  "from","front","full","fully","further","furthered","furthering","furthermore","furthers","fx","g","ga","gave","gb","gd","ge","general",
                  "generally","get","gets","getting","gf","gg","gh","gi","give","given","gives","giving","gl","gm","gmt","gn","go","goes","going","gone",
                  "good","goods","got","gotten","gov","gp","gq","gr","great","greater","greatest","greetings","group","grouped","grouping","groups","gs",
                  "gt","gu","gw","gy","h","had","hadn't","hadnt","half","happens","hardly","has","hasn","hasn't","hasnt","have","haven","haven't","havent",
                  "having","he","he'd","he'll","he's","hed","hell","hello","help","hence","her","here","here's","hereafter","hereby","herein","heres","hereupon",
                  "hers","herself","herse???","hes","hi","hid","high","higher","highest","him","himself","himse???","his","hither","hk","hm","hn","home","homepage",
                  "hopefully","how","how'd","how'll","how's","howbeit","however","hr","ht","htm","html","http","hu","hundred","i","i'd","i'll","i'm","i've","i.e.",
                  "id","ie","if","ignored","ii","il","ill","im","immediate","immediately","importance","important","in","inasmuch","inc","inc.","indeed","index",
                  "indicate","indicated","indicates","information","inner","inside","insofar","instead","int","interest","interested","interesting","interests",
                  "into","invention","inward","io","iq","ir","is","isn","isn't","isnt","it","it'd","it'll","it's","itd","itll","its","itself","itse???","ive","j",
                  "je","jm","jo","join","jp","just","k","ke","keep","keeps","kept","keys","kg","kh","ki","kind","km","kn","knew","know","known","knows","kp","kr",
                  "kw","ky","kz","l","la","large","largely","last","lately","later","latest","latter","latterly","lb","lc","least","length","less","lest","let",
                  "let's","lets","li","like","liked","likely","likewise","line","little","lk","ll","long","longer","longest","look","looking","looks","low","lower",
                  "lr","ls","lt","ltd","lu","lv","ly","m","ma","made","mainly","make","makes","making","man","many","may","maybe","mayn't","maynt","mc","md","me",
                  "mean","means","meantime","meanwhile","member","members","men","merely","mg","mh","microsoft","might","might've","mightn't","mightnt","mil","mill",
                  "million","mine","minus","miss","mk","ml","mm","mn","mo","more","moreover","most","mostly","move","mp","mq","mr","mrs","ms","msie","mt","mu","much","mug",
                  "must","must've","mustn't","mustnt","mv","mw","mx","my","myself","myse???","mz","n","na","name","namely","nay","nc","nd","ne","near","nearly","necessarily",
                  "necessary","need","needed","needing","needn't","neednt","needs","neither","net","netscape","never","neverf","neverless","nevertheless","new","newer",
                  "newest","next","nf","ng","ni","nine","ninety","nl","no","no-one","nobody","non","none","nonetheless","noone","nor","normally","nos","not","noted",
                  "nothing","notwithstanding","novel","now","nowhere","np","nr","nu","null","number","numbers","nz","o","obtain","obtained","obviously","of","off",
                  "often","oh","ok","okay","old","older","oldest","om","omitted","on","once","one","one's","ones","only","onto","open","opened","opening","opens",
                  "opposite","or","ord","order","ordered","ordering","orders","org","other","others","otherwise","ought","oughtn't","oughtnt","our","ours","ourselves",
                  "out","outside","over","overall","owing","own","p","pa","page","pages","part","parted","particular","particularly","parting","parts","past","pe",
                  "per","perhaps","pf","pg","ph","pk","pl","place","placed","places","please","plus","pm","pmid","pn","point","pointed","pointing","points","poorly",
                  "possible","possibly","potentially","pp","pr","predominantly","present","presented","presenting","presents","presumably","previously","primarily",
                  "probably","problem","problems","promptly","proud","provided","provides","pt","put","puts","pw","py","q","qa","que","quickly","quite","qv","r","ran",
                  "rather","rd","re","readily","really","reasonably","recent","recently","ref","refs","regarding","regardless","regards","related","relatively",
                  "research","reserved","respectively","resulted","resulting","results","right","ring","ro","room","rooms","round","ru","run","rw","s","sa","said",
                  "same","saw","say","saying","says","sb","sc","sd","se","sec","second","secondly","seconds","section","see","seeing","seem","seemed","seeming",
                  "seems","seen","sees","self","selves","sensible","sent","serious","seriously","seven","seventy","several","sg","sh","shall","shan't","shant",
                  "she","she'd","she'll","she's","shed","shell","shes","should","should've","shouldn","shouldn't","shouldnt","show","showed","showing","shown",
                  "showns","shows","si","side","sides","significant","significantly","similar","similarly","since","sincere","site","six","sixty","sj","sk","sl",
                  "slightly","sm","small","smaller","smallest","sn","so","some","somebody","someday","somehow","someone","somethan","something","sometime","sometimes",
                  "somewhat","somewhere","soon","sorry","specifically","specified","specify","specifying","sr","st","state","states","still","stop","strongly","su",
                  "sub","substantially","successfully","such","sufficiently","suggest","sup","sure","sv","sy","system","sz","t","t's","take","taken","taking","tc",
                  "td","tell","ten","tends","test","text","tf","tg","th","than","thank","thanks","thanx","that","that'll","that's","that've","thatll","thats","thatve",
                  "the","their","theirs","them","themselves","then","thence","there","there'd","there'll","there're","there's","there've","thereafter","thereby",
                  "thered","therefore","therein","therell","thereof","therere","theres","thereto","thereupon","thereve","these","they","they'd","they'll","they're",
                  "they've","theyd","theyll","theyre","theyve","thick","thin","thing","things","think","thinks","third","thirty","this","thorough","thoroughly",
                  "those","thou","though","thoughh","thought","thoughts","thousand","three","throug","through","throughout","thru","thus","til","till","tip","tis",
                  "tj","tk","tm","tn","to","today","together","too","took","top","toward","towards","tp","tr","tried","tries","trillion","truly","try","trying","ts",
                  "tt","turn","turned","turning","turns","tv","tw","twas","twelve","twenty","twice","two","tz","u","ua","ug","uk","um","un","under","underneath","undoing",
                  "unfortunately","unless","unlike","unlikely","until","unto","up","upon","ups","upwards","us","use","used","useful","usefully","usefulness","uses","using",
                  "usually","uucp","uy","uz","v","va","value","various","vc","ve","versus","very","vg","vi","via","viz","vn","vol","vols","vs","vu","w","want","wanted",
                  "wanting","wants","was","wasn","wasn't","wasnt","way","ways","we","we'd","we'll","we're","we've","web","webpage","website","wed","welcome","well","wells",
                  "went","were","weren","weren't","werent","weve","wf","what","what'd","what'll","what's","what've","whatever","whatll","whats","whatve","when","when'd",
                  "when'll","when's","whence","whenever","where","where'd","where'll","where's","whereafter","whereas","whereby","wherein","wheres","whereupon","wherever",
                  "whether","which","whichever","while","whilst","whim","whither","who","who'd","who'll","who's","whod","whoever","whole","wholl","whom","whomever","whos",
                  "whose","why","why'd","why'll","why's","widely","width","will","willing","wish","with","within","without","won","won't","wonder","wont","words","work",
                  "worked","working","works","world","would","would've","wouldn","wouldn't","wouldnt","ws","www","x","y","ye","year","years","yes","yet","you","you'd",
                  "you'll","you're","you've","youd","youll","young","younger","youngest","your","youre","yours","yourself","yourselves","youve","yt","yu","z","za","zero","zm","zr",
                  "lol","??"
)

#ISTENMEYEN KELIMELERIMIZI CIKARIYORUZ
en_data1$textOriginal <- removeWords(en_data1$textOriginal, kelime_liste)
en_data1$textOriginal <- removeWords(en_data1$textOriginal, liste)

#TEMIZLEDIGIMIZ VERIYI KAYDEDELIM
write_xlsx(en_data1,"C:/Users/beyza/Desktop/en_data1.xlsx")

#TEMIZ VERI R AKTARALIM
clean_data<- read_xlsx(file.choose())

c_1<- data.frame(text=clean_data$textOriginal)



en_cdata1<- c_1%>% mutate(linenumber=row_number()) %>% unnest_tokens(word,text)
en_cdata1

#BU ASAMADA "PLYR" KUTUPHANESI KULLANILAMMALI

# Kelime frekanslar??n?? hesaplama ve filtreleme
word_frekans <- en_cdata1 %>%
  count(word, sort = TRUE) %>%
  filter(n > 1) %>%
  mutate(word = reorder(word, n)) %>%
  filter(!is.na(word)) %>%
  filter(n > 0)

#top20
word_frekans_top20 <- word_frekans %>%
  top_n(20, n)

# KELIME FREKANS GORSELLESTIRME
ggplot(word_frekans_top20, aes(x = word, y = n, fill = word)) +
  geom_col() +
  coord_flip() +
  labs(title = "En Yuksek Frekansa Sahip 20 Kelime", x = "Kelime", y = "Frekans") +
  theme_minimal()


# KELIME BULUTU ICIN GEREKLI PAKETLER
library(wordcloud)
library(RColorBrewer)
install.packages("wordcloud2")
library(wordcloud2)


# KELIME BULUTU OLUSTURALIM
wordcloud(word_frekans$word,
          min.freq = 1,max.words = 100,
          colors = brewer.pal(1,"Dark2"),
          random.color = T,
          random.order = F)

#DUYGU ANAL??Z??


# Duygu s??zl??????n?? y??kleme
sentiments <- get_sentiments("bing")

# Metin verisini kelimelere ay??rma
tidy_comments <- en_cdata1 %>%
  unnest_tokens(word, word)

# Kelimeleri duygu s??zl?????? ile birle??tirme
sentiment_comments <- tidy_comments %>%
  inner_join(sentiments, by = "word")

# Duygular?? sayma
sentiment_summary <- sentiment_comments %>%
  count(sentiment)

print(sentiment_summary)


# Duygu analizi sonu??lar??n?? g??rselle??tirme
ggplot(sentiment_summary, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  xlab("Duygu") +
  ylab("Kelime Say??s??") +
  ggtitle("Duygu Analizi Sonu??lar??") +
  theme_minimal()

#EMOJI ANALIZI

en_data2<-en_data
#YORUMLARDA HANG?? EMOJ?? KA?? KERE KULLANILMI??  
en_emoji_sayisi <- table(unlist(str_extract_all(en_data2$textOriginal, "[\\x{1F600}-\\x{1F64F}]|[\\x{1F300}-\\x{1F5FF}]|[\\x{1F680}-\\x{1F6FF}]")))
en_emojiler<- as.matrix(en_emoji_sayisi)

names(en_emojiler) <- "Emoji_1"

# YORUMLARDAKI EMOJI SAYISI
en_emoji_sayisi <- table(unlist(str_extract_all(en_data2$textOriginal, "[\\x{1F600}-\\x{1F64F}]|[\\x{1F300}-\\x{1F5FF}]|[\\x{1F680}-\\x{1F6FF}]")))

# TOPLAM KULLANILAN EMOJI SAYISI
en_toplam_emoji_sayisi <- sum(en_emoji_sayisi)

# TOPLAM EMOJI SAYISI YAZDIRALIM
print(en_toplam_emoji_sayisi)


# FONKSIYONU TANIMLAYALIM
en_extract_emojis <- function(text) {
  # Text'in NULL veya NA olmad??????ndan emin olun
  if (is.null(text) || is.na(text)) return(NA_character_)
  
  # UNICODE ARALIGINA GORE EMOJI SECIMI
  en_emojis <- str_extract_all(text, "[\\x{1F600}-\\x{1F64F}]|[\\x{1F300}-\\x{1F5FF}]|[\\x{1F680}-\\x{1F6FF}]|[\\x{1F700}-\\x{1F77F}]|[\\x{1F780}-\\x{1F7FF}]|[\\x{1F800}-\\x{1F8FF}]|[\\x{1F900}-\\x{1F9FF}]|[\\x{1FA00}-\\x{1FA6F}]|[\\x{1FA70}-\\x{1FAFF}]|[\\x{2600}-\\x{26FF}]|[\\x{2700}-\\x{27BF}]|[\\x{2300}-\\x{23FF}]|[\\x{2B50}-\\x{2B50}]|[\\x{1F004}-\\x{1F004}]|[\\x{1F0CF}-\\x{1F0CF}]|[\\x{1F18E}-\\x{1F18E}]|[\\x{1F191}-\\x{1F19A}]|[\\x{1F201}-\\x{1F201}]|[\\x{1F21A}-\\x{1F21A}]|[\\x{1F22F}-\\x{1F23A}]|[\\x{1F250}-\\x{1F251}]|[\\x{3297}-\\x{3297}]|[\\x{3299}-\\x{3299}]")
  return(unlist(en_emojis))
}


#METIN VERISINDEKI EMOJILERI AYIKLAMA
en_emo_emojis <- en_data2 %>%
  mutate(en_emojiler = map(textOriginal, en_extract_emojis))

#DATA FRAME CEVIRME ISLEMI
en_emoji_df<- data.frame(emoji = names(en_emoji_sayisi), frequency = as.numeric(en_emoji_sayisi))

#ILK 30 EMOJI BUL
top_30_emojiler_en<- en_emoji_df %>%
  arrange(desc(frequency)) %>%
  head(30)

#CUBUK GRAFIGI
ggplot(data = top_30_emojiler_en, aes(x = factor(emoji, levels = emoji), y = frequency)) +
  geom_bar(fill="pink", stat = "identity") +
  geom_text(aes(label = frequency), vjust = -0.5, color = "red", size = 4) +  # Frekans say??lae??
  labs(title = "En Cok Kullan??lan 30 Emoji", x = "Emoji", y = "Kullan??m Say??s??") +
  scale_x_discrete(labels = top_30_emojiler_en$emoji) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(family = "EmojiOne", size = 25, color = "purple4"),
        axis.text.y = element_text(family = "EmojiOne", size = 20, color = "brown"),
        plot.title = element_text(color = "black", size = 25),
        axis.title.x = element_text(color = "black", size = 20),
        axis.title.y = element_text(color = "black", size = 20)) +
  coord_cartesian(ylim = c(0, max(top_30_emojiler_en$frequency) * 1.1))



# Emojifont paketinden EmojiOne fontunu indiriyoruz
load.emojifont(font = "EmojiOne.ttf")

# KELIME BULUTU
wordcloud(words = en_emoji_df$emoji,
          freq = en_emoji_df$frequency,
          scale = c(6, 0.2),
          colors = brewer.pal(5, "Dark2"),
          family = "EmojiOne")


#EGER SIZE KISMINI 2 YAPARSANIZ ILK SIRADAKI EMOJIYI ALMIYOR
wordcloud2(data= top_30_emojiler_en,
           size=1,
           backgroundColor = "pink",
           shape = "circle")
