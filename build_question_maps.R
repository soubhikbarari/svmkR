## Create a mapping from common SurveyMonkey question bank 
## questions to variables stored in targets.

library(tidyverse)

qmap <- data.frame()

# AGE ---------------------------------------------------------------------
message("x AGE")

qbank <- c("What is your( current)+ age")

## Age 2 ----
labels <- c("age2", "Age 2")
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice=paste0("(", paste0(paste0("^", 18:44,"$"), collapse="|"), ")"), variable_name=labels, variable_level="18 - 44"))
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice=paste0("(", paste0(paste0("^", 45:100,"$"), collapse="|"), ")"), variable_name=labels, variable_level="45 and up"))

## Age 6 ----
labels <- c("age6", "Age 6")
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice=paste0("(", paste0(paste0("^", 18:24,"$"), collapse="|"), ")"), variable_name=labels, variable_level="18 - 24"))
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice=paste0("(", paste0(paste0("^", 25:34,"$"), collapse="|"), ")"), variable_name=labels, variable_level="25 - 34"))
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice=paste0("(", paste0(paste0("^", 35:44,"$"), collapse="|"), ")"), variable_name=labels, variable_level="35 - 44"))
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice=paste0("(", paste0(paste0("^", 45:54,"$"), collapse="|"), ")"), variable_name=labels, variable_level="45 - 54"))
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice=paste0("(", paste0(paste0("^", 55:64,"$"), collapse="|"), ")"), variable_name=labels, variable_level="55 - 64"))
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice=paste0("(", paste0(paste0("^", 65:100,"$"), collapse="|"), ")"), variable_name=labels, variable_level="65 and up"))

# RACE --------------------------------------------------------------------
message("x RACE")

qbank <- c("(What is your race|^Are you\\:|Which racial group do you most identify with)")

## Race 2 ----
labels <- c("race2","Race 2","white","nonwhite")
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice="^White", variable_name=labels, variable_level="White"))
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice="[^(White)]", variable_name=labels, variable_level="Non-white"))

## Race 4 ----
labels <- c("race4","Race 4")
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice="^White", variable_name=labels, variable_level="White"))
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice="(^Black|African American)", variable_name=labels, variable_level="Black"))
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice="(^Hispanic|Latino)", variable_name=labels, variable_level="Hispanic"))
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice="(Asian|Middle Eastern|Other|Another|Native|Pacific)", variable_name=labels, variable_level="Other"))
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice="[^(White|Black|African American|Hispanic|Latino)]", variable_name=labels, variable_level="Other"))

## Race 5 ----
labels <- c("Are you:","race5","Race 5")
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice="^White", variable_name=labels, variable_level="White"))
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice="(^Black|African American)", variable_name=labels, variable_level="Black"))
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice="(^Hispanic|Latino)", variable_name=labels, variable_level="Hispanic"))
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice="(Asian)", variable_name=labels, variable_level="Asian"))
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice="[^(White|Black|African American|Hispanic|Latino|Asian)]", variable_name=labels, variable_level="Other"))

# GENDER ------------------------------------------------------------------
message("x GENDER")

qbank <- c("(Which gender best describes you|What is your gender|What best describes your gender|Gender\\: how do you identify)")
  
## Gender 2 ----
labels <- c("gender2","Gender 2","Gender")
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice="^Male", variable_name=labels, variable_level="Male"))
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice="^Female", variable_name=labels, variable_level="Female"))

# EDUC --------------------------------------------------------------------
message("x EDUC")

qbank <- c("What is the highest level of school or level of education( that)? you have (completed|attained)")
  
## Educ 4 ----
labels <- c("educ4", "Education 4")
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice="Did Not Complete High School", variable_name=labels, variable_level="High school or less"))
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice="Graduated High School", variable_name=labels, variable_level="High school or less"))
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice="GED", variable_name=labels, variable_level="High school or less"))
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice="G\\.E\\.D", variable_name=labels, variable_level="High school or less"))
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice="^High School", variable_name=labels, variable_level="High school or less"))

qmap <- rbind(qmap, crossing(question_text=qbank, question_choice="^Associate.*", variable_name=labels, variable_level="Some college"))
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice="Some college", variable_name=labels, variable_level="Some college"))

qmap <- rbind(qmap, crossing(question_text=qbank, question_choice="^Bachelor", variable_name=labels, variable_level="College or more"))
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice="College (graduate|degree)", variable_name=labels, variable_level="College or more"))

qmap <- rbind(qmap, crossing(question_text=qbank, question_choice="(Master|PHD|Ph\\.D|professional|J\\.D|M\\.D)", variable_name=labels, variable_level="Post graduate degree"))

## Educ 3 ----
labels <- c("educ3", "Education 3", "Education 3 College")
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice="Did Not Complete High School", variable_name=labels, variable_level="HS or less/some college"))
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice="Graduated High School", variable_name=labels, variable_level="HS or less/some college"))
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice="GED", variable_name=labels, variable_level="HS or less/some college"))
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice="G\\.E\\.D", variable_name=labels, variable_level="HS or less/some college"))
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice="^High School", variable_name=labels, variable_level="HS or less/some college"))
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice="^Associate.*", variable_name=labels, variable_level="HS or less/some college"))
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice="Some college", variable_name=labels, variable_level="HS or less/some college"))

qmap <- rbind(qmap, crossing(question_text=qbank, question_choice="^Bachelor", variable_name=labels, variable_level="College degree"))
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice="College (graduate|degree)", variable_name=labels, variable_level="College degree"))

qmap <- rbind(qmap, crossing(question_text=qbank, question_choice="(Master|PHD|Ph\\.D|professional|J\\.D|M\\.D)", variable_name=labels, variable_level="Post graduate degree"))

# INCOME ------------------------------------------------------------------
message("x INCOME")

qbank <- c("(What is your total household income|My total family income last year was)")

# TODO

# GEO ---------------------------------------------------------------------
message("x GEO")

## State ----
labels <- c("State","st")
### direct question
qbank <- c("What state( or U.S. territory)? do you (reside|live)")

qmap <- rbind(qmap, 
              crossing(data.frame(question_choice=state.name, variable_level=state.abb),
                       crossing(question_text=qbank, variable_name=labels)))
### by zip code
qbank <- c("In what ZIP code is your home located?.*")
qmap <- rbind(qmap, crossing(variable_level="Alabama", question_choice="(^350|^351|^352|^354|^355|^356|^357|^358|^359|^360|^361|^362|^363|^364|^365|^366|^367|^368|^369|^388)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Alaska", question_choice="(^995|^996|^997|^998|^999)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Arizona", question_choice="(^845|^850|^851|^852|^853|^855|^856|^857|^859|^860|^863|^864|^865|^873|^890)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Arkansas", question_choice="(^380|^657|^716|^717|^718|^719|^720|^721|^722|^723|^724|^725|^726|^727|^728|^729)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="California", question_choice="(^894|^900|^902|^903|^904|^905|^906|^907|^908|^910|^911|^912|^913|^914|^915|^916|^917|^918|^919|^920|^921|^922|^923|^924|^925|^926|^927|^928|^930|^931|^932|^933|^934|^935|^936|^937|^939|^940|^941|^943|^944|^945|^946|^947|^948|^949|^950|^951|^952|^953|^954|^955|^956|^957|^958|^959|^960|^961|^976)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Colorado", question_choice="(^800|^801|^802|^803|^804|^805|^806|^807|^808|^809|^810|^811|^812|^813|^814|^815|^816|^820)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Connecticut", question_choice="(^060|^061|^062|^063|^064|^065|^066|^067|^068|^069)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Delaware", question_choice="(^197|^198|^199)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="District of columbia", question_choice="(^200|^203)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Florida", question_choice="(^320|^321|^322|^323|^324|^325|^326|^327|^328|^329|^330|^331|^333|^334|^335|^336|^337|^338|^339|^341|^342|^344|^346|^347|^349)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Georgia", question_choice="(^300|^301|^302|^303|^304|^305|^306|^307|^308|^309|^310|^312|^313|^314|^315|^316|^317|^318|^319)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Hawaii", question_choice="(^967|^968)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Idaho", question_choice="(^831|^832|^833|^834|^835|^836|^837|^838|^979|^991)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Illinois", question_choice="(^527|^600|^601|^602|^603|^604|^605|^606|^607|^608|^609|^610|^611|^612|^613|^614|^615|^616|^617|^618|^619|^620|^622|^623|^624|^625|^626|^627|^628|^629)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Indiana", question_choice="(^460|^461|^462|^463|^464|^465|^466|^467|^468|^469|^470|^471|^472|^473|^474|^475|^476|^477|^478|^479)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Iowa", question_choice="(^500|^501|^502|^503|^504|^505|^506|^507|^508|^510|^511|^512|^513|^514|^515|^516|^520|^521|^522|^523|^524|^525|^526|^527|^528|^559|^560|^561)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Kansas", question_choice="(^660|^661|^662|^664|^665|^666|^667|^668|^669|^670|^671|^672|^673|^674|^675|^676|^677|^678|^679)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Kentucky", question_choice="(^380|^400|^401|^402|^403|^404|^405|^406|^407|^408|^409|^410|^411|^412|^413|^414|^415|^416|^417|^418|^420|^421|^422|^423|^424|^425|^426|^427)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Louisiana", question_choice="(^700|^701|^703|^704|^705|^706|^707|^708|^710|^711|^712|^713|^714|^717)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Maine", question_choice="(^039|^040|^041|^042|^043|^044|^045|^046|^047|^048|^049)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Maryland", question_choice="(^206|^207|^208|^209|^210|^211|^212|^214|^215|^216|^217|^218|^219)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Massachusetts", question_choice="(^010|^011|^012|^013|^014|^015|^016|^017|^018|^019|^020|^021|^022|^023|^024|^025|^026|^027)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Michigan", question_choice="(^480|^481|^482|^483|^484|^485|^486|^487|^488|^489|^490|^491|^492|^493|^494|^495|^496|^497|^498|^499)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Minnesota", question_choice="(^550|^551|^553|^554|^556|^557|^558|^559|^560|^561|^562|^563|^564|^565|^566|^567|^582)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Mississippi", question_choice="(^386|^387|^388|^389|^390|^391|^392|^393|^394|^395|^396|^397)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Missouri", question_choice="(^516|^526|^630|^631|^633|^634|^635|^636|^637|^638|^639|^640|^641|^644|^645|^646|^647|^648|^650|^651|^652|^653|^654|^655|^656|^657|^658|^726)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Montana", question_choice="(^590|^591|^592|^593|^594|^595|^596|^597|^598|^599)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Nebraska", question_choice="(^680|^681|^683|^684|^685|^686|^687|^688|^689|^690|^691|^692|^693|^820)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Nevada", question_choice="(^890|^891|^893|^894|^895|^897|^898)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="New hampshire", question_choice="(^030|^031|^032|^033|^034|^035|^036|^037|^038)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="New jersey", question_choice="(^070|^071|^072|^073|^074|^075|^076|^077|^078|^079|^080|^081|^082|^083|^084|^085|^086|^087|^088|^089|^100)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="New mexico", question_choice="(^799|^855|^865|^870|^871|^873|^874|^875|^877|^878|^879|^880|^881|^882|^883|^884|^885)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="New york", question_choice="(^063|^100|^101|^102|^103|^104|^105|^106|^107|^108|^109|^110|^111|^112|^113|^114|^115|^116|^117|^118|^119|^120|^121|^122|^123|^124|^125|^126|^127|^128|^129|^130|^131|^132|^133|^134|^135|^136|^137|^138|^139|^140|^141|^142|^143|^144|^145|^146|^147|^148|^149)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="North carolina", question_choice="(^270|^271|^272|^273|^274|^275|^276|^277|^278|^279|^280|^281|^282|^283|^284|^285|^286|^287|^288|^289)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="North dakota", question_choice="(^576|^580|^581|^582|^583|^584|^585|^586|^587|^588|^592)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Ohio", question_choice="(^430|^431|^432|^433|^434|^435|^436|^437|^438|^439|^440|^441|^442|^443|^444|^445|^446|^447|^448|^449|^450|^451|^452|^453|^454|^455|^456|^457|^458)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Oklahoma", question_choice="(^679|^730|^731|^734|^735|^736|^737|^738|^739|^740|^741|^743|^744|^745|^746|^747|^748|^749|^790)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Oregon", question_choice="(^970|^971|^972|^973|^974|^975|^976|^977|^978|^979|^993)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Pennsylvania", question_choice="(^150|^151|^152|^153|^154|^155|^156|^157|^158|^159|^160|^161|^162|^163|^164|^165|^166|^167|^168|^169|^170|^171|^172|^173|^174|^175|^176|^177|^178|^179|^180|^181|^182|^183|^184|^185|^186|^187|^188|^189|^190|^191|^192|^193|^194|^195|^196)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Rhode island", question_choice="(^028|^029)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="South carolina", question_choice="(^290|^291|^292|^293|^294|^295|^296|^297|^298|^299)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="South dakota", question_choice="(^562|^570|^571|^572|^573|^574|^575|^576|^577|^693)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Tennessee", question_choice="(^370|^371|^372|^373|^374|^376|^377|^378|^379|^380|^381|^382|^383|^384|^385|^422)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Texas", question_choice="(^739|^750|^751|^752|^754|^755|^756|^757|^758|^759|^760|^761|^762|^763|^764|^765|^766|^767|^768|^769|^770|^772|^773|^774|^775|^776|^777|^778|^779|^780|^781|^782|^783|^784|^785|^786|^787|^788|^789|^790|^791|^792|^793|^794|^795|^796|^797|^798|^799|^884|^885)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Utah", question_choice="(^840|^841|^843|^844|^845|^846|^847|^860|^865)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Vermont", question_choice="(^050|^051|^052|^053|^054|^056|^057|^058|^059)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Virginia", question_choice="(^201|^220|^221|^222|^223|^224|^225|^226|^227|^228|^229|^230|^231|^232|^233|^234|^235|^236|^237|^238|^239|^240|^241|^242|^243|^244|^245|^246|^376)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Washington", question_choice="(^980|^981|^982|^983|^984|^985|^986|^988|^989|^990|^991|^992|^993|^994)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="West virginia", question_choice="(^247|^248|^249|^250|^251|^252|^253|^254|^255|^256|^257|^258|^259|^260|^261|^262|^263|^264|^265|^266|^267|^268)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Wisconsin", question_choice="(^530|^531|^532|^534|^535|^537|^538|^539|^540|^541|^542|^543|^544|^545|^546|^547|^548|^549)", variable_name=labels, question_text=qbank))
qmap <- rbind(qmap, crossing(variable_level="Wyoming", question_choice="(^820|^821|^822|^823|^824|^825|^826|^827|^828|^829|^830|^831)", variable_name=labels, question_text=qbank))

## Region ----
labels <- c("State 4", "region")
### by state
qbank <- c("What state( or U.S. territory)? do you (reside|live)")
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice=c("(New Hampshire|Vermont|Maine|Massachusetts|Rhode Island|Connecticut|New York|New Jersey|Pennsylvania)"), variable_name=labels, variable_level="Northeast"))
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice=c("(Wisconsin|Illinois|Michigan|Indiana|Ohio|North Dakota|South Dakota|Nebraska|Kansas|Minnesota|Iowa|Missouri)"), variable_name=labels, variable_level="Midwest"))
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice=c("(Delaware|Maryland|D\\.C|DC|District of Columbia|West Virginia|Virginia|North Carolina|South Carolina|Georgia|Florida|Kentucky|Tennessee|Mississippi|Alabama|Oklahoma|Texas|Arkansas|Louisiana)"), variable_name=labels, variable_level="South"))
qmap <- rbind(qmap, crossing(question_text=qbank, question_choice=c("(Montana|Idaho|Wyoming|Nevada|Utah|Colorado|Arizona|New Mexico|Washington|Oregon|California|Alaska|Hawaii)"), variable_name=labels, variable_level="West"))

### by zip code
qbank <- c("In what ZIP code is your home located?.*")
qmap <- rbind(qmap, crossing(question_text=qbank, variable_level="Northeast", question_choice=c("(^060|^061|^062|^063|^064|^065|^066|^067|^068|^069|^039|^040|^041|^042|^043|^044|^045|^046|^047|^048|^049|^010|^011|^012|^013|^014|^015|^016|^017|^018|^019|^020|^021|^022|^023|^024|^025|^026|^027|^030|^031|^032|^033|^034|^035|^036|^037|^038|^070|^071|^072|^073|^074|^075|^076|^077|^078|^079|^080|^081|^082|^083|^084|^085|^086|^087|^088|^089|^100|^101|^102|^103|^104|^105|^106|^107|^108|^109|^110|^111|^112|^113|^114|^115|^116|^117|^118|^119|^120|^121|^122|^123|^124|^125|^126|^127|^128|^129|^130|^131|^132|^133|^134|^135|^136|^137|^138|^139|^140|^141|^142|^143|^144|^145|^146|^147|^148|^149|^150|^151|^152|^153|^154|^155|^156|^157|^158|^159|^160|^161|^162|^163|^164|^165|^166|^167|^168|^169|^170|^171|^172|^173|^174|^175|^176|^177|^178|^179|^180|^181|^182|^183|^184|^185|^186|^187|^188|^189|^190|^191|^192|^193|^194|^195|^196|^028|^029|^050|^051|^052|^053|^054|^056|^057|^058|^059)"), variable_name=labels))
qmap <- rbind(qmap, crossing(question_text=qbank, variable_level="Midwest", question_choice=c("(^527|^600|^601|^602|^603|^604|^605|^606|^607|^608|^609|^610|^611|^612|^613|^614|^615|^616|^617|^618|^619|^620|^622|^623|^624|^625|^626|^627|^628|^629|^460|^461|^462|^463|^464|^465|^466|^467|^468|^469|^470|^471|^472|^473|^474|^475|^476|^477|^478|^479|^500|^501|^502|^503|^504|^505|^506|^507|^508|^510|^511|^512|^513|^514|^515|^516|^520|^521|^522|^523|^524|^525|^526|^528|^559|^560|^561|^660|^661|^662|^664|^665|^666|^667|^668|^669|^670|^671|^672|^673|^674|^675|^676|^677|^678|^679|^480|^481|^482|^483|^484|^485|^486|^487|^488|^489|^490|^491|^492|^493|^494|^495|^496|^497|^498|^499|^550|^551|^553|^554|^556|^557|^558|^562|^563|^564|^565|^566|^567|^582|^630|^631|^633|^634|^635|^636|^637|^638|^639|^640|^641|^644|^645|^646|^647|^648|^650|^651|^652|^653|^654|^655|^656|^657|^658|^726|^680|^681|^683|^684|^685|^686|^687|^688|^689|^690|^691|^692|^693|^820|^576|^580|^581|^583|^584|^585|^586|^587|^588|^592|^430|^431|^432|^433|^434|^435|^436|^437|^438|^439|^440|^441|^442|^443|^444|^445|^446|^447|^448|^449|^450|^451|^452|^453|^454|^455|^456|^457|^458|^570|^571|^572|^573|^574|^575|^577|^530|^531|^532|^534|^535|^537|^538|^539|^540|^541|^542|^543|^544|^545|^546|^547|^548|^549)"), variable_name=labels))
qmap <- rbind(qmap, crossing(question_text=qbank, variable_level="South", question_choice=c("(^350|^351|^352|^354|^355|^356|^357|^358|^359|^360|^361|^362|^363|^364|^365|^366|^367|^368|^369|^388|^380|^657|^716|^717|^718|^719|^720|^721|^722|^723|^724|^725|^726|^727|^728|^729|^197|^198|^199|^200|^203|^320|^321|^322|^323|^324|^325|^326|^327|^328|^329|^330|^331|^333|^334|^335|^336|^337|^338|^339|^341|^342|^344|^346|^347|^349|^300|^301|^302|^303|^304|^305|^306|^307|^308|^309|^310|^312|^313|^314|^315|^316|^317|^318|^319|^400|^401|^402|^403|^404|^405|^406|^407|^408|^409|^410|^411|^412|^413|^414|^415|^416|^417|^418|^420|^421|^422|^423|^424|^425|^426|^427|^700|^701|^703|^704|^705|^706|^707|^708|^710|^711|^712|^713|^714|^206|^207|^208|^209|^210|^211|^212|^214|^215|^216|^217|^218|^219|^386|^387|^389|^390|^391|^392|^393|^394|^395|^396|^397|^270|^271|^272|^273|^274|^275|^276|^277|^278|^279|^280|^281|^282|^283|^284|^285|^286|^287|^288|^289|^679|^730|^731|^734|^735|^736|^737|^738|^739|^740|^741|^743|^744|^745|^746|^747|^748|^749|^790|^290|^291|^292|^293|^294|^295|^296|^297|^298|^299|^370|^371|^372|^373|^374|^376|^377|^378|^379|^381|^382|^383|^384|^385|^750|^751|^752|^754|^755|^756|^757|^758|^759|^760|^761|^762|^763|^764|^765|^766|^767|^768|^769|^770|^772|^773|^774|^775|^776|^777|^778|^779|^780|^781|^782|^783|^784|^785|^786|^787|^788|^789|^791|^792|^793|^794|^795|^796|^797|^798|^799|^884|^885|^201|^220|^221|^222|^223|^224|^225|^226|^227|^228|^229|^230|^231|^232|^233|^234|^235|^236|^237|^238|^239|^240|^241|^242|^243|^244|^245|^246|^247|^248|^249|^250|^251|^252|^253|^254|^255|^256|^257|^258|^259|^260|^261|^262|^263|^264|^265|^266|^267|^268)"), variable_name=labels))
qmap <- rbind(qmap, crossing(question_text=qbank, variable_level="West", question_choice=c("(^995|^996|^997|^998|^999|^845|^850|^851|^852|^853|^855|^856|^857|^859|^860|^863|^864|^865|^873|^890|^894|^900|^902|^903|^904|^905|^906|^907|^908|^910|^911|^912|^913|^914|^915|^916|^917|^918|^919|^920|^921|^922|^923|^924|^925|^926|^927|^928|^930|^931|^932|^933|^934|^935|^936|^937|^939|^940|^941|^943|^944|^945|^946|^947|^948|^949|^950|^951|^952|^953|^954|^955|^956|^957|^958|^959|^960|^961|^976|^800|^801|^802|^803|^804|^805|^806|^807|^808|^809|^810|^811|^812|^813|^814|^815|^816|^820|^967|^968|^831|^832|^833|^834|^835|^836|^837|^838|^979|^991|^590|^591|^592|^593|^594|^595|^596|^597|^598|^599|^891|^893|^895|^897|^898|^799|^870|^871|^874|^875|^877|^878|^879|^880|^881|^882|^883|^884|^885|^970|^971|^972|^973|^974|^975|^977|^978|^993|^840|^841|^843|^844|^846|^847|^980|^981|^982|^983|^984|^985|^986|^988|^989|^990|^992|^994|^821|^822|^823|^824|^825|^826|^827|^828|^829|^830)"), variable_name=labels))

## Division ----
labels <- c("stdivision", "Division", "State Division", "State 9", "state9")
### by state
qmap <- rbind(qmap, crossing(question_text=qbank, variable_level="East North Central", question_choice=c("(illinois|indiana|michigan|ohio|wisconsin)"), variable_name=labels))
qmap <- rbind(qmap, crossing(question_text=qbank, variable_level="East South Central", question_choice=c("(alabama|kentucky|mississippi|tennessee)"), variable_name=labels))
qmap <- rbind(qmap, crossing(question_text=qbank, variable_level="Middle Atlantic", question_choice=c("(new jersey|new york|pennsylvania)"), variable_name=labels))
qmap <- rbind(qmap, crossing(question_text=qbank, variable_level="Mountain", question_choice=c("(arizona|colorado|idaho|montana|nevada|new mexico|utah|wyoming)"), variable_name=labels))
qmap <- rbind(qmap, crossing(question_text=qbank, variable_level="New England", question_choice=c("(connecticut|maine|massachusetts|new hampshire|rhode island|vermont)"), variable_name=labels))
qmap <- rbind(qmap, crossing(question_text=qbank, variable_level="Pacific", question_choice=c("(alaska|california|hawaii|oregon|washington)"), variable_name=labels))
qmap <- rbind(qmap, crossing(question_text=qbank, variable_level="South Atlantic", question_choice=c("(delaware|district of columbia|florida|georgia|maryland|north carolina|south carolina|virginia|west virginia)"), variable_name=labels))
qmap <- rbind(qmap, crossing(question_text=qbank, variable_level="West North Central", question_choice=c("(iowa|kansas|minnesota|missouri|nebraska|north dakota|south dakota)"), variable_name=labels))
qmap <- rbind(qmap, crossing(question_text=qbank, variable_level="West South Central", question_choice=c("(arkansas|louisiana|oklahoma|texas)"), variable_name=labels))

### by zip code
qmap <- rbind(qmap, crossing(question_text=qbank, variable_level="East North Central", question_choice=c("(^527|^600|^601|^602|^603|^604|^605|^606|^607|^608|^609|^610|^611|^612|^613|^614|^615|^616|^617|^618|^619|^620|^622|^623|^624|^625|^626|^627|^628|^629|^460|^461|^462|^463|^464|^465|^466|^467|^468|^469|^470|^471|^472|^473|^474|^475|^476|^477|^478|^479|^480|^481|^482|^483|^484|^485|^486|^487|^488|^489|^490|^491|^492|^493|^494|^495|^496|^497|^498|^499|^430|^431|^432|^433|^434|^435|^436|^437|^438|^439|^440|^441|^442|^443|^444|^445|^446|^447|^448|^449|^450|^451|^452|^453|^454|^455|^456|^457|^458|^530|^531|^532|^534|^535|^537|^538|^539|^540|^541|^542|^543|^544|^545|^546|^547|^548|^549)"), variable_name=labels))
qmap <- rbind(qmap, crossing(question_text=qbank, variable_level="East South Central", question_choice=c("(^350|^351|^352|^354|^355|^356|^357|^358|^359|^360|^361|^362|^363|^364|^365|^366|^367|^368|^369|^388|^380|^400|^401|^402|^403|^404|^405|^406|^407|^408|^409|^410|^411|^412|^413|^414|^415|^416|^417|^418|^420|^421|^422|^423|^424|^425|^426|^427|^386|^387|^389|^390|^391|^392|^393|^394|^395|^396|^397|^370|^371|^372|^373|^374|^376|^377|^378|^379|^381|^382|^383|^384|^385)"), variable_name=labels))
qmap <- rbind(qmap, crossing(question_text=qbank, variable_level="Middle Atlantic", question_choice=c("(^070|^071|^072|^073|^074|^075|^076|^077|^078|^079|^080|^081|^082|^083|^084|^085|^086|^087|^088|^089|^100|^063|^101|^102|^103|^104|^105|^106|^107|^108|^109|^110|^111|^112|^113|^114|^115|^116|^117|^118|^119|^120|^121|^122|^123|^124|^125|^126|^127|^128|^129|^130|^131|^132|^133|^134|^135|^136|^137|^138|^139|^140|^141|^142|^143|^144|^145|^146|^147|^148|^149|^150|^151|^152|^153|^154|^155|^156|^157|^158|^159|^160|^161|^162|^163|^164|^165|^166|^167|^168|^169|^170|^171|^172|^173|^174|^175|^176|^177|^178|^179|^180|^181|^182|^183|^184|^185|^186|^187|^188|^189|^190|^191|^192|^193|^194|^195|^196)"), variable_name=labels))
qmap <- rbind(qmap, crossing(question_text=qbank, variable_level="Mountain", question_choice=c("(^845|^850|^851|^852|^853|^855|^856|^857|^859|^860|^863|^864|^865|^873|^890|^800|^801|^802|^803|^804|^805|^806|^807|^808|^809|^810|^811|^812|^813|^814|^815|^816|^820|^831|^832|^833|^834|^835|^836|^837|^838|^979|^991|^590|^591|^592|^593|^594|^595|^596|^597|^598|^599|^891|^893|^894|^895|^897|^898|^799|^870|^871|^874|^875|^877|^878|^879|^880|^881|^882|^883|^884|^885|^840|^841|^843|^844|^846|^847|^821|^822|^823|^824|^825|^826|^827|^828|^829|^830)"), variable_name=labels))
qmap <- rbind(qmap, crossing(question_text=qbank, variable_level="New England", question_choice=c("(^060|^061|^062|^063|^064|^065|^066|^067|^068|^069|^039|^040|^041|^042|^043|^044|^045|^046|^047|^048|^049|^010|^011|^012|^013|^014|^015|^016|^017|^018|^019|^020|^021|^022|^023|^024|^025|^026|^027|^030|^031|^032|^033|^034|^035|^036|^037|^038|^028|^029|^050|^051|^052|^053|^054|^056|^057|^058|^059)"), variable_name=labels))
qmap <- rbind(qmap, crossing(question_text=qbank, variable_level="Pacific", question_choice=c("(^995|^996|^997|^998|^999|^894|^900|^902|^903|^904|^905|^906|^907|^908|^910|^911|^912|^913|^914|^915|^916|^917|^918|^919|^920|^921|^922|^923|^924|^925|^926|^927|^928|^930|^931|^932|^933|^934|^935|^936|^937|^939|^940|^941|^943|^944|^945|^946|^947|^948|^949|^950|^951|^952|^953|^954|^955|^956|^957|^958|^959|^960|^961|^976|^967|^968|^970|^971|^972|^973|^974|^975|^977|^978|^979|^993|^980|^981|^982|^983|^984|^985|^986|^988|^989|^990|^991|^992|^994)"), variable_name=labels))
qmap <- rbind(qmap, crossing(question_text=qbank, variable_level="South Atlantic", question_choice=c("(^197|^198|^199|^200|^203|^320|^321|^322|^323|^324|^325|^326|^327|^328|^329|^330|^331|^333|^334|^335|^336|^337|^338|^339|^341|^342|^344|^346|^347|^349|^300|^301|^302|^303|^304|^305|^306|^307|^308|^309|^310|^312|^313|^314|^315|^316|^317|^318|^319|^206|^207|^208|^209|^210|^211|^212|^214|^215|^216|^217|^218|^219|^270|^271|^272|^273|^274|^275|^276|^277|^278|^279|^280|^281|^282|^283|^284|^285|^286|^287|^288|^289|^290|^291|^292|^293|^294|^295|^296|^297|^298|^299|^201|^220|^221|^222|^223|^224|^225|^226|^227|^228|^229|^230|^231|^232|^233|^234|^235|^236|^237|^238|^239|^240|^241|^242|^243|^244|^245|^246|^376|^247|^248|^249|^250|^251|^252|^253|^254|^255|^256|^257|^258|^259|^260|^261|^262|^263|^264|^265|^266|^267|^268)"), variable_name=labels))
qmap <- rbind(qmap, crossing(question_text=qbank, variable_level="West North Central", question_choice=c("(^500|^501|^502|^503|^504|^505|^506|^507|^508|^510|^511|^512|^513|^514|^515|^516|^520|^521|^522|^523|^524|^525|^526|^527|^528|^559|^560|^561|^660|^661|^662|^664|^665|^666|^667|^668|^669|^670|^671|^672|^673|^674|^675|^676|^677|^678|^679|^550|^551|^553|^554|^556|^557|^558|^562|^563|^564|^565|^566|^567|^582|^630|^631|^633|^634|^635|^636|^637|^638|^639|^640|^641|^644|^645|^646|^647|^648|^650|^651|^652|^653|^654|^655|^656|^657|^658|^726|^680|^681|^683|^684|^685|^686|^687|^688|^689|^690|^691|^692|^693|^820|^576|^580|^581|^583|^584|^585|^586|^587|^588|^592|^570|^571|^572|^573|^574|^575|^577)"), variable_name=labels))
qmap <- rbind(qmap, crossing(question_text=qbank, variable_level="West South Central", question_choice=c("(^380|^657|^716|^717|^718|^719|^720|^721|^722|^723|^724|^725|^726|^727|^728|^729|^700|^701|^703|^704|^705|^706|^707|^708|^710|^711|^712|^713|^714|^679|^730|^731|^734|^735|^736|^737|^738|^739|^740|^741|^743|^744|^745|^746|^747|^748|^749|^790|^750|^751|^752|^754|^755|^756|^757|^758|^759|^760|^761|^762|^763|^764|^765|^766|^767|^768|^769|^770|^772|^773|^774|^775|^776|^777|^778|^779|^780|^781|^782|^783|^784|^785|^786|^787|^788|^789|^791|^792|^793|^794|^795|^796|^797|^798|^799|^884|^885)"), variable_name=labels))

# Save --------------------------------------------------------------------

write_csv(qmap, file = "./inst/extdata/qmap.txt")
