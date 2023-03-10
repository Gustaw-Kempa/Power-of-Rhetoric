rm(list = ls())

library(rvest)
library(dplyr)
library(lubridate)
library(stringr)
library(googleLanguageR)
gl_auth("/Users/gustawkempa/Desktop/Studia/Master/Master.Data/master-377713-d6f77365d167.json")




# "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"
links <- c('https://web.archive.org/web/20201024034506/http://www.governo.it/it/articolo/coronavirus-dichiarazione-del-presidente-conte/14297',
           'https://web.archive.org/web/20201031130153/http://www.governo.it/it/articolo/dichiarazioni-del-presidente-conte/14361',
           'https://web.archive.org/web/20201031130153/http://www.governo.it/it/articolo/dichiarazioni-del-presidente-conte/14361',
           'https://web.archive.org/web/20201031130724/http://www.governo.it/it/articolo/covid-19-informativa-del-presidente-conte-al-senato/14500',
           'https://web.archive.org/web/20200926194019/http://www.governo.it/it/articolo/covid-19-informativa-del-presidente-conte-alla-camera/14550',
           'https://web.archive.org/web/20201030131712/http://www.governo.it/it/articolo/nuova-fase-legate-allemergenza-epidemiologica-da-covid-19-informativa-del-presidente-conte',
           'https://web.archive.org/web/20201026163449/http://www.governo.it/it/articolo/video-messaggio-del-presidente-conte-al-summit-globale-oil-su-covid-19-e-mondo-del-lavoro',
           'https://web.archive.org/web/20201027070748/http://www.governo.it/it/articolo/progettiamo-il-rilancio-l-intervento-introduttivo-del-presidente-conte/14746',
           'https://web.archive.org/web/20210101091333/http://www.governo.it/it/articolo/progettiamo-il-rilancio-lintervento-introduttivo-del-presidente-conte-della-quarta-giornata',
           'https://web.archive.org/web/20201026163213/http://www.governo.it/it/articolo/intervento-del-presidente-conte-alla-resentazione-del-soer-2020/14672',
           'https://web.archive.org/web/20201124172555/http://www.governo.it/it/articolo/intervento-del-presidente-conte-allinaugurazione-del-nuovo-ponte-autostradale-di-genova',
           'https://web.archive.org/web/20210127123655/http://www.governo.it/it/articolo/comunicazioni-del-presidente-conte-parlamento-intervento-al-senato/14992',
           'https://web.archive.org/web/20201129201743/http://www.governo.it/it/articolo/messaggio-del-presidente-conte-losservatorio-riparte-litalia/15260',
           'https://web.archive.org/web/20201204055814/http://www.governo.it/it/articolo/dpcm-18-ottobre-2020-informativa-del-presidente-conte-alla-camera/15483',
           'https://web.archive.org/web/20201125194541/http://www.governo.it/it/articolo/intervento-del-presidente-conte-allassemblea-annuale-dell-ania/15460',
           'https://web.archive.org/web/20201025173632/http://www.governo.it/it/articolo/senato-le-comunicazioni-del-presidente-conte-vista-del-consiglio-europeo-del-15-e-16',
           'https://web.archive.org/web/20201025173550/http://www.governo.it/it/articolo/conte-videoconferenza-al-festival-dello-sviluppo-sostenibile-2020/15355',
           'https://web.archive.org/web/20201129201743/http://www.governo.it/it/articolo/messaggio-del-presidente-conte-losservatorio-riparte-litalia/15260',
           'https://web.archive.org/web/20201126211622/http://www.governo.it/it/articolo/misure-lemergenza-covid-19-le-comunicazioni-di-conte-alla-camera-dei-deputati/15602',
           'https://web.archive.org/web/20210126211956/http://www.governo.it/it/articolo/intervento-di-conte-alla-presentazione-del-rapporto-svimez-2020/15772',
           'https://web.archive.org/web/20210210085014/http://www.governo.it/it/articolo/comunicazioni-al-senato-della-repubblica-replica-del-presidente-conte/16085',
           'https://web.archive.org/web/20210121211834/http://www.governo.it/it/articolo/intervento-del-presidente-conte-al-allevento-al-convegno-next-generation-italia/15906',
           'https://web.archive.org/web/20210121210015/http://www.governo.it/it/articolo/intervento-del-presidente-conte-al-rome-investment-forum-2020/15890',
           #Change of government - how do we approach that?
           'https://web.archive.org/web/20210924025546/https://www.governo.it/it/articolo/intervento-del-presidente-draghi-alliniziativa-sud-progetti-ripartire/16445',
           'https://web.archive.org/web/20220117094329/https://www.governo.it/it/articolo/intervento-al-centro-vaccinale-anti-covid-di-fiumicino/16393',
           'https://web.archive.org/web/20211023045102/https://www.governo.it/it/articolo/intervento-del-presidente-draghi-alla-conferenza-verso-una-strategia-nazionale-sulla-parit',
           'https://web.archive.org/web/20211026121128/https://www.governo.it/it/articolo/le-comunicazioni-del-presidente-draghi-al-senato/16225',
           'https://web.archive.org/web/20220209083019/https://www.governo.it/it/articolo/videomessaggio-del-presidente-draghi-alla-consegna-delle-borse-di-studio-della-fondazione',
           'https://web.archive.org/web/20210924021332/https://www.governo.it/it/articolo/draghi-emilia-romagna/16967',
           'https://web.archive.org/web/20210925070714/https://www.governo.it/it/articolo/onu-intervento-del-presidente-draghi-al-global-covid-19-summit/17958'
)

# REMEMBER TO EXCLUDE Q&AS!!!!!
res <- tibble( titles = character(), dates = character(), speeches_IT = character(), links = character())

for (i in 1:length(links)) {
 res_temp <- matrix("", ncol = 4, nrow =1)
  res_temp[,1] <- links[i]
  page <- read_html(res_temp[,1])
 res_temp[,2] <- page %>% html_nodes('.title_large') %>% html_text()
 res_temp[,3] <-  page %>% html_nodes('.h6') %>% html_text()
  speech <- page %>% html_nodes('.body_intervista p') %>% html_text()
  res_temp[,4] <- paste(speech[-1], collapse = " ")
  res <- rbind(res, res_temp)
}

data_IT <- res
colnames(data_IT) <- c("links", "titles", "dates", "speeches_IT")


# manually adding unscrapable data
data_IT$speeches_IT[13] <- "Un saluto a tutte le Autorit?? presenti, un saluto al professor Luigi Balestra quale Presidente del Comitato di indirizzo Osservatorio ???Riparte l???Italia???. ?? per me un piacere intervenire, anche se a distanza, a questo incontro da cui sono certo scaturiranno idee, proposte, opinioni e spunti di riflessione certamente preziosi anche per il nostro lavoro, nell???interesse del Paese, dei cittadini e delle imprese.
Con l???Osservatorio economico e sociale ???Riparte l???Italia??? vi siete posti l???obiettivo di offrire un contributo, di indicare delle direttrici per migliorare la nostra vita collettiva ???oltre l???emergenza???, come avete precisato. Io penso che queste iniziative siano necessarie per dare vita a stimoli, a impulsi di cui abbiamo bisogno per ripartire insieme.
Veniamo da mesi molto difficili, abbiamo combattuto e ancora stiamo affrontando una pandemia inattesa, sconosciuta. Negli scorsi giorni, autorevoli testate ??? penso a Bloomberg, al Financial Times ??? hanno riconosciuto all???Italia di aver gestito meglio di altri Paesi l???emergenza, tenendo sotto controllo l???epidemia. Da Presidente del Consiglio, lasciatemelo dire, sono orgoglioso di questi riconoscimenti. Dobbiamo tutti essere fieri del comportamento esemplare dimostrato da tutta la Comunit?? italiana.
Lo spirito di solidariet?? e il senso di comunit?? che ci hanno unito nei mesi pi?? difficili della pandemia oggi non vanno dispersi.
Non dobbiamo disunirci e sfilacciarci nella fase cruciale della ricostruzione. Credo che oltre alla sfida della resilienza possiamo vincere anche quella della ripartenza. ?? un impegno che prendiamo davanti alle generazioni future, non c????? quindi sfida pi?? importante.
Il confronto continuo fra Stato, attori economici e sociali, cittadini e comunit?? tutta ci aiuta senz???altro a focalizzare con maggior decisione in che modo utilizzare la storica occasione dei 209 miliardi ottenuti in sede europea per far procedere speditamente il progetto di una ???nuova Italia???, con la spinta del Recovery Plan.
Molte delle parole d???ordine del ???manifesto??? del vostro Osservatorio sono le stesse poste alla base del lavoro che stiamo mettendo in campo in questi mesi: sviluppo, efficienza, sostenibilit??, innovazione, solidariet??.
Sul fronte economico la pandemia ha spinto verso un maggior protagonismo dello Stato in tutto il mondo. Ed ?? un dovere ??? ritengo ??? dello Stato fare uno sforzo per proteggere cittadini, famiglie, imprese, e accompagnarle verso la crescita e verso uno sviluppo sostenibile. Nei mesi scorsi ci siamo mossi per creare le condizioni affinch?? gli investimenti si traducano realmente e rapidamente da ???freddi numeri??? in bilancio in opere, cantieri, interventi che migliorino efficacemente, realmente la vita dei nostri cittadini e dei territori. Va in questo senso anche, in questa direzione il decreto Semplificazioni: lo considero un acceleratore per lo sviluppo per una maggiore efficienza della macchina amministrativa, che ci consentir?? ??? insieme ad altri interventi che abbiamo in cantiere ??? di non disperdere risorse ed energie nei rivoli della burocrazia in un momento cos?? cruciale. In questi giorni firmer?? un decreto per individuare i cantieri che avranno un percorso accelerato e i relativi commissari.
Ma non pu?? essere ???velocit????? l???unica parola d???ordine della crescita e dello sviluppo di cui abbiamo bisogno per ripartire. Questo percorso deve essere anche sostenibile. E sostenibile innanzitutto a livello ambientale. Con il superbonus edilizia al 110%, ad esempio, diventa concreta la possibilit?? di produrre lavoro e occupazione nel settore dell???edilizia perseguendo per?? l???obiettivo dell???efficientamento energetico e dell???adeguamento sismico delle abitazioni. Intendiamo estendere questo strumento anche oltre il 2021. Con il Recovery Plan almeno il 37% delle risorse disponibili riguarder?? investimenti green: quindi transizione energetica in settori strategici come l???automotive, il potenziamento della rete idrica e il contrasto al dissesto idrogeologico, l???efficientamento energetico degli edifici pubblici.
Altrettanto importante ?? la sostenibilit?? sociale: non c????? crescita se qualcuno rimane indietro e senza protezioni, se non c????? la stella polare della solidariet?? e dell???inclusione a indicarci la strada giusta. Qualcuno ha classificato alcuni provvedimenti emergenziali del Governo come una ???pioggia??? di bonus, di sussidi. In realt?? erano l???ombrello con cui riparare le categorie pi?? esposte alla tempesta. Questo ci impone di ragionare per il futuro su un welfare che metta sempre al centro la persona, la sua dignit??, il diritto per i giovani ad avere gli strumenti per non cedere alla cultura dell???odio e della violenza e per abbracciare la speranza, il sogno di realizzarsi, il diritto di poter costruire una famiglia.
Parlando di giovani e di futuro non possiamo ignorare che l???Italia riparte solo se riparte la scuola: siamo in giorni cruciali, in queste prime settimane l???anno scolastico ?? ripreso in maniera ordinata, nel rispetto delle regole, simbolo anche questo ??? potremmo dire ??? di un???Italia che si rialza e riprende a correre. Contemporaneamente, per??, su scuola, universit?? e ricerca il nostro Paese non pu?? accontentarsi della situazione pre pandemia, dobbiamo programmare e preparare da subito un nuovo slancio. Intendiamo trasformare tutte le classi italiane in luoghi innovativi per l???apprendimento; collegare le scuole superiori alle professioni del futuro, dando agli istituti gli strumenti e i mezzi per creare sin da subito ???lavoratori digitali???. Investiamo su poli tematici di ricerca in ambiti come il fin-tech e agri-tech, promuoviamo le industrie strategiche nel settore aerospaziale. ?? nostro obiettivo anche sostenere interventi per la digitalizzazione della Sanit??, per la telemedicina.
L???innovazione dovr?? rimanere in cima a questa agenda per la ripresa. L???Italia del boom economico trov?? nell???Autostrada del Sole la risorsa e il collegamento capace di unire il Paese e favorirne la crescita. Oggi acceleriamo sul fronte dell???autostrada del nostro futuro, un'infrastruttura digitale unica in banda ultralarga capace di proiettare velocemente il nostro Paese in avanti, recuperando il terreno perduto.
Investire sulla rete unica significa fornire agli studenti la possibilit?? di accedere a tutte le informazioni in maniera semplice e rapida; alla scuola di digitalizzarsi e rispondere alle aspettative dei nostri ragazzi. Significa anche rafforzare un rapporto diretto e veloce fra i cittadini e i servizi pubblici e privati; aprire la strada a nuove occasioni di sviluppo nelle aree depresse d???Italia dove i dati potranno finalmente viaggiare alla velocit?? degli obiettivi delle imprese innovative e dei sogni dei nostri giovani che sono troppo spesso costretti ad emigrare.
Nell???augurarvi buon lavoro intendo fare un auspicio per il nostro Paese: ripartire significa ritrovare la fiducia nel fatto che l???Italia ha un potenziale enorme. Dobbiamo rialzarci per accelerare, non per stare in piedi come prima. All???Italia non manca nulla per farcela, l???Italia non ha motivi per accontentarsi, l???Italia pu??, deve osare. Buon lavoro."


data_IT$speeches_IT[18] <- "Un saluto a tutte le Autorit?? presenti, un saluto al professor Luigi Balestra quale Presidente del Comitato di indirizzo Osservatorio ???Riparte l???Italia???. ?? per me un piacere intervenire, anche se a distanza, a questo incontro da cui sono certo scaturiranno idee, proposte, opinioni e spunti di riflessione certamente preziosi anche per il nostro lavoro, nell???interesse del Paese, dei cittadini e delle imprese.
Con l???Osservatorio economico e sociale ???Riparte l???Italia??? vi siete posti l???obiettivo di offrire un contributo, di indicare delle direttrici per migliorare la nostra vita collettiva ???oltre l???emergenza???, come avete precisato. Io penso che queste iniziative siano necessarie per dare vita a stimoli, a impulsi di cui abbiamo bisogno per ripartire insieme.
Veniamo da mesi molto difficili, abbiamo combattuto e ancora stiamo affrontando una pandemia inattesa, sconosciuta. Negli scorsi giorni, autorevoli testate ??? penso a Bloomberg, al Financial Times ??? hanno riconosciuto all???Italia di aver gestito meglio di altri Paesi l???emergenza, tenendo sotto controllo l???epidemia. Da Presidente del Consiglio, lasciatemelo dire, sono orgoglioso di questi riconoscimenti. Dobbiamo tutti essere fieri del comportamento esemplare dimostrato da tutta la Comunit?? italiana.
Lo spirito di solidariet?? e il senso di comunit?? che ci hanno unito nei mesi pi?? difficili della pandemia oggi non vanno dispersi.
Non dobbiamo disunirci e sfilacciarci nella fase cruciale della ricostruzione. Credo che oltre alla sfida della resilienza possiamo vincere anche quella della ripartenza. ?? un impegno che prendiamo davanti alle generazioni future, non c????? quindi sfida pi?? importante.
Il confronto continuo fra Stato, attori economici e sociali, cittadini e comunit?? tutta ci aiuta senz???altro a focalizzare con maggior decisione in che modo utilizzare la storica occasione dei 209 miliardi ottenuti in sede europea per far procedere speditamente il progetto di una ???nuova Italia???, con la spinta del Recovery Plan.
Molte delle parole d???ordine del ???manifesto??? del vostro Osservatorio sono le stesse poste alla base del lavoro che stiamo mettendo in campo in questi mesi: sviluppo, efficienza, sostenibilit??, innovazione, solidariet??.
Sul fronte economico la pandemia ha spinto verso un maggior protagonismo dello Stato in tutto il mondo. Ed ?? un dovere ??? ritengo ??? dello Stato fare uno sforzo per proteggere cittadini, famiglie, imprese, e accompagnarle verso la crescita e verso uno sviluppo sostenibile. Nei mesi scorsi ci siamo mossi per creare le condizioni affinch?? gli investimenti si traducano realmente e rapidamente da ???freddi numeri??? in bilancio in opere, cantieri, interventi che migliorino efficacemente, realmente la vita dei nostri cittadini e dei territori. Va in questo senso anche, in questa direzione il decreto Semplificazioni: lo considero un acceleratore per lo sviluppo per una maggiore efficienza della macchina amministrativa, che ci consentir?? ??? insieme ad altri interventi che abbiamo in cantiere ??? di non disperdere risorse ed energie nei rivoli della burocrazia in un momento cos?? cruciale. In questi giorni firmer?? un decreto per individuare i cantieri che avranno un percorso accelerato e i relativi commissari.
Ma non pu?? essere ???velocit????? l???unica parola d???ordine della crescita e dello sviluppo di cui abbiamo bisogno per ripartire. Questo percorso deve essere anche sostenibile. E sostenibile innanzitutto a livello ambientale. Con il superbonus edilizia al 110%, ad esempio, diventa concreta la possibilit?? di produrre lavoro e occupazione nel settore dell???edilizia perseguendo per?? l???obiettivo dell???efficientamento energetico e dell???adeguamento sismico delle abitazioni. Intendiamo estendere questo strumento anche oltre il 2021. Con il Recovery Plan almeno il 37% delle risorse disponibili riguarder?? investimenti green: quindi transizione energetica in settori strategici come l???automotive, il potenziamento della rete idrica e il contrasto al dissesto idrogeologico, l???efficientamento energetico degli edifici pubblici.
Altrettanto importante ?? la sostenibilit?? sociale: non c????? crescita se qualcuno rimane indietro e senza protezioni, se non c????? la stella polare della solidariet?? e dell???inclusione a indicarci la strada giusta. Qualcuno ha classificato alcuni provvedimenti emergenziali del Governo come una ???pioggia??? di bonus, di sussidi. In realt?? erano l???ombrello con cui riparare le categorie pi?? esposte alla tempesta. Questo ci impone di ragionare per il futuro su un welfare che metta sempre al centro la persona, la sua dignit??, il diritto per i giovani ad avere gli strumenti per non cedere alla cultura dell???odio e della violenza e per abbracciare la speranza, il sogno di realizzarsi, il diritto di poter costruire una famiglia.
Parlando di giovani e di futuro non possiamo ignorare che l???Italia riparte solo se riparte la scuola: siamo in giorni cruciali, in queste prime settimane l???anno scolastico ?? ripreso in maniera ordinata, nel rispetto delle regole, simbolo anche questo ??? potremmo dire ??? di un???Italia che si rialza e riprende a correre. Contemporaneamente, per??, su scuola, universit?? e ricerca il nostro Paese non pu?? accontentarsi della situazione pre pandemia, dobbiamo programmare e preparare da subito un nuovo slancio. Intendiamo trasformare tutte le classi italiane in luoghi innovativi per l???apprendimento; collegare le scuole superiori alle professioni del futuro, dando agli istituti gli strumenti e i mezzi per creare sin da subito ???lavoratori digitali???. Investiamo su poli tematici di ricerca in ambiti come il fin-tech e agri-tech, promuoviamo le industrie strategiche nel settore aerospaziale. ?? nostro obiettivo anche sostenere interventi per la digitalizzazione della Sanit??, per la telemedicina.
L???innovazione dovr?? rimanere in cima a questa agenda per la ripresa. L???Italia del boom economico trov?? nell???Autostrada del Sole la risorsa e il collegamento capace di unire il Paese e favorirne la crescita. Oggi acceleriamo sul fronte dell???autostrada del nostro futuro, un'infrastruttura digitale unica in banda ultralarga capace di proiettare velocemente il nostro Paese in avanti, recuperando il terreno perduto.
Investire sulla rete unica significa fornire agli studenti la possibilit?? di accedere a tutte le informazioni in maniera semplice e rapida; alla scuola di digitalizzarsi e rispondere alle aspettative dei nostri ragazzi. Significa anche rafforzare un rapporto diretto e veloce fra i cittadini e i servizi pubblici e privati; aprire la strada a nuove occasioni di sviluppo nelle aree depresse d???Italia dove i dati potranno finalmente viaggiare alla velocit?? degli obiettivi delle imprese innovative e dei sogni dei nostri giovani che sono troppo spesso costretti ad emigrare.
Nell???augurarvi buon lavoro intendo fare un auspicio per il nostro Paese: ripartire significa ritrovare la fiducia nel fatto che l???Italia ha un potenziale enorme. Dobbiamo rialzarci per accelerare, non per stare in piedi come prima. All???Italia non manca nulla per farcela, l???Italia non ha motivi per accontentarsi, l???Italia pu??, deve osare. Buon lavoro."

data_IT$speeches_IT[26] <- "Ci troviamo tutti di fronte, in questi giorni, a un nuovo peggioramento dell???emergenza sanitaria. Ognuno deve fare la propria parte nel contenere la diffusione del virus. Ma soprattutto il governo deve fare la sua. Anzi deve cercare ogni giorno di fare di pi??.
La pandemia non ?? ancora sconfitta ma si intravede, con l'accelerazione del piano dei vaccini, una via d'uscita non lontana. Voglio cogliere questa occasione per mandare a tutti un segnale vero di fiducia. Anche in noi stessi.
Ringrazio, ancora una volta, i cittadini per la loro disciplina, la loro infinita pazienza, soprattutto coloro che soffrono le conseguenze anche economiche della pandemia. Ringrazio gli studenti, le famiglie e gli insegnanti che sopportano il peso della chiusura delle scuole. Ringrazio gli operatori sanitari, le forze dell'ordine, le forze armate, la Protezione Civile e tanti altri lavoratori in prima linea per la loro incessante opera. Sono anche questi esempi di responsabilit?? civica e professionale, di cittadinanza italiana attiva che impongono al governo di moltiplicare ogni sforzo. Siamo solo all'inizio.
Il nostro compito - e mi riferisco a tutti i livelli istituzionali - ?? quello di salvaguardare con ogni mezzo la vita degli italiani e permettere al pi?? presto un ritorno alla normalit??. Ogni vita conta. Non perdere un attimo, non lasciare nulla di intentato, compiere scelte meditate, ma rapide. Non voglio promettere nulla che non sia veramente realizzabile. Le mie preoccupazioni sono le vostre preoccupazioni. Il mio pensiero costante ?? diretto a rendere efficace ed efficiente l'azione dell'esecutivo nel tutelare la salute, sostenere chi ?? in difficolt??, favorire la ripresa economica, accelerare le riforme.
Il 10 marzo di un anno fa l'Italia si chiudeva diventando per la prima volta, una grande zona rossa. Un nostro concittadino su venti ?? stato contagiato ??? secondo i dati ufficiali che, come ?? noto, sottostimano la diffusione del virus. Mai avremmo pensato che un anno dopo ci saremmo trovati a fronteggiare un'emergenza analoga e che il conto ufficiale delle vittime si sarebbe avvicinato alla terribile soglia dei centomila morti. Dobbiamo al rispetto della memoria dei tanti cittadini che hanno perso la vita il dovere del nostro impegno. Nel piano di vaccinazioni, che nei prossimi giorni sar?? decisamente potenziato, si privilegeranno le persone pi?? fragili e le categorie a rischio. Aspettare il proprio turno ?? un modo anche per tutelare la salute dei nostri concittadini pi?? deboli.
Questo non ?? il momento di dividerci o di riaffermare le nostre identit??. Ma ?? il momento di dare una risposta alle tante persone che soffrono per la crisi economica, che rischiano di perdere il posto di lavoro, di combattere le disuguaglianze. In un solo anno il numero di italiani che vivono in una situazione di povert?? assoluta ?? aumentato di oltre un milione, mentre si sono acuite altre disparit??, prima fra tutte quella tra donne e uomini.
?? anche per questo che oggi, Giornata Internazionale della Donna, voglio che il mio saluto accompagni la presentazione della Strategia Nazionale per la parit?? di genere, elaborata dalla Ministra Elena Bonetti, a conclusione di un lavoro che ha visto partecipi personalit?? a cui va il mio caloroso ringraziamento.
A fronte dell???esempio di molte italiane eccezionali in tutti i campi, anche nella normalit?? familiare, abbiamo molto, moltissimo da fare per portare il livello e la qualit?? della parit?? di genere alle medie europee.
La mobilitazione delle energie femminili, un non solo simbolico riconoscimento della funzione e del talento delle donne, sono essenziali per la costruzione del futuro della nostra nazione.
Azioni mirate e profonde riforme sono necessarie per coinvolgere pienamente le donne nella vita economica, sociale e istituzionale del Paese. Ma dobbiamo prima di tutto cambiare noi stessi nella quotidianit?? della vita familiare.
Lo Stato e gli enti territoriali dovranno assistere le famiglie, specie le pi?? giovani, anche quando questa fase di emergenza sar?? terminata. Gli strumenti che dobbiamo impiegare sono vari: penso tra gli altri ai congedi parentali; penso al numero dei posti negli asili nido, che ci vede inferiori agli obiettivi europei, e sulla loro distribuzione territoriale che va resa ben pi?? equa di quanto non sia oggi. Tutto ci?? ?? obiettivo di questo governo.
Non voglio qui ripetere le bellissime parole di oggi del Presidente della Repubblica sulla condizione femminile. Voi sapete bene quanto sia dolorosa. Sul femminicidio e su ogni forma di violenza di genere, sono da condividere le proposte della Commissione parlamentare d???inchiesta. Oggi, per le vittime dei troppi femminicidi e anche come reazione prodotta dalla pandemia, sembra formarsi una nuova consapevolezza che trova un???opportunit?? straordinaria nel programma Next Generation EU per diventare realt?? nell???azione di governo, del mio governo. Tra i vari criteri che verranno usati per valutare i progetti del Piano Nazionale di Ripresa e Resilienza ci sar?? anche il loro contributo alla parit?? di genere.
?? con questo spirito di fiducia nel nostro, nel vostro futuro e con l'impegno di questo governo a conquistarsela, che vi auguro buon 8 marzo."




topics_EN <- gl_translate(t_string = data_IT$titles, target = "en",
                                  format = "text", model = "nmt")[1]
speeches_EN <- gl_translate(t_string = data_IT$speeches_IT, target = "en",
                                    format = "text", model = "nmt")[1]


months <- sub(".*,\\s(\\d{1,2})\\s(\\w+)\\s\\d{4}$", "\\2", data_IT$dates)
months <- gl_translate(t_string = months, target = "en",
                       format = "text", model = "nmt")[1]


# data_IT$dates <- 


data_IT$dates <-sub(".*,\\s(\\d{1,2})\\s(\\w+)\\s(\\d{4})$", "\\1 \\3", data_IT$dates)
data_IT$dates<- paste(months$translatedText,data_IT$dates )
data_IT$dates <- mdy(data_IT$dates)



data <- cbind(data_IT,topics_EN ,speeches_EN)
colnames(data) <- c("links", "topics", "dates", "speeches_IT", "topics_EN", "speeches_EN")
write.csv(data, '/Users/gustawkempa/Desktop/Studia/Master/data/speeches/speeches_IT_translated.csv')
