# dane z mapami - wczytanie gotowych
load("mapy/mapy.RData")

library(dplyr)
library(ggplot2)

# dane z wynikami - wczytanie z XLS
library(XLConnect)
# Sejm
wb <- loadWorkbook("wyniki/sejm_gminy.xls")
wyniki.sejm.gminy <- readWorksheet(wb, sheet="2015-gl-lis-gm")
wb <- loadWorkbook("wyniki/sejm_powiaty.xls")
wyniki.sejm.powiaty <- readWorksheet(wb, sheet="2015-gl-lis-pow")
wb <- loadWorkbook("wyniki/sejm_wojewodztwa.xls")
wyniki.sejm.wojewodztwa <- readWorksheet(wb, sheet="2015-gl-lis-woj")
# Senat
wb <- loadWorkbook("wyniki/senat_gminy.xls")
wyniki.senat.gminy <- readWorksheet(wb, sheet="2015-senat-gm")
wb <- loadWorkbook("wyniki/senat_powiaty.xls")
wyniki.senat.powiaty <- readWorksheet(wb, sheet="2015-senat-pow")
wb <- loadWorkbook("wyniki/senat_wojewodztwa.xls")
wyniki.senat.wojewodztwa <- readWorksheet(wb, sheet="2015-senat-woj")
rm(wb)


# mapki

# PiS - wojewodztwa

# wybieramy odpowiednie dane + zmiana nazw kolumn
pis.wojewodztwa <- select(wyniki.sejm.wojewodztwa, TERYT, Liczba.wyborców, Głosy.ważne, X1...Komitet.Wyborczy.Prawo.i.Sprawiedliwość)
names(pis.wojewodztwa) <- c("TERYT", "Wyborcow", "Wazne", "Glosy")
# liczymy % głosów na PiS wśród ważnych
pis.wojewodztwa <- mutate(pis.wojewodztwa, procent=100*Glosy/Wazne)
# łączymy mapę z danymi
pis.wojewodztwa <- left_join(mapa.wojewodztwa, pis.wojewodztwa, by=c("kod_TERYT" = "TERYT"))
# rysujemy mapę
pis.wojewodztwa.mapa <- mapa_tlo +
		geom_polygon(data=pis.wojewodztwa,
					 aes(x=long, y=lat, group=group, fill=procent),
					 color = "black",
					 size = 0.2,
					 alpha = 0.7) +
	scale_fill_gradient(low='green', high='red')
print(pis.wojewodztwa.mapa)


# PiS - powiaty

# wybieramy odpowiednie dane + zmiana nazw kolumn
pis.powiaty <- select(wyniki.sejm.powiaty, TERYT, Liczba.wyborców, Głosy.ważne, X1...Komitet.Wyborczy.Prawo.i.Sprawiedliwość)
names(pis.powiaty) <- c("TERYT", "Wyborcow", "Wazne", "Glosy")
# liczymy % głosów na PiS wśród ważnych
pis.powiaty <- mutate(pis.powiaty, procent=100*Glosy/Wazne)
# łączymy mapę z danymi
pis.powiaty <- left_join(mapa.powiaty, pis.powiaty, by=c("kod_TERYT" = "TERYT"))
# rysujemy mapę
pis.powiaty.mapa <- mapa_tlo +
	geom_polygon(data=pis.powiaty,
					 aes(x=long, y=lat, group=group, fill=procent),
					 color = "black",
					 size = 0.2,
					 alpha = 0.7) +
	scale_fill_gradient(low='green', high='red')
print(pis.powiaty.mapa)



# ktora partia wygrala w powiecie?
# tutaj tylko PO i PiS
kto_wygral <- select(wyniki.sejm.powiaty, TERYT, Głosy.ważne,
							X1...Komitet.Wyborczy.Prawo.i.Sprawiedliwość,
							X2...Komitet.Wyborczy.Platforma.Obywatelska.RP,
							X3...Komitet.Wyborczy.Partia.Razem,
							X4...Komitet.Wyborczy.KORWiN,
							X5...Komitet.Wyborczy.Polskie.Stronnictwo.Ludowe,
							X6...Koalicyjny.Komitet.Wyborczy.Zjednoczona.Lewica.SLD.TR.PPS.UP.Zieloni,
							X7...Komitet.Wyborczy.Wyborców..Kukiz.15.,
							X8...Komitet.Wyborczy.Nowoczesna.Ryszarda.Petru)
# procenty dla wszystkich
kto_wygral <- mutate(kto_wygral,PO=100*X2...Komitet.Wyborczy.Platforma.Obywatelska.RP/Głosy.ważne, 
										PiS=100*X1...Komitet.Wyborczy.Prawo.i.Sprawiedliwość/Głosy.ważne, 
										Razem=100*X3...Komitet.Wyborczy.Partia.Razem/Głosy.ważne, 
										KORWiN=100*X4...Komitet.Wyborczy.KORWiN/Głosy.ważne, 
										PSL=100*X5...Komitet.Wyborczy.Polskie.Stronnictwo.Ludowe/Głosy.ważne, 
										ZLew=100*X6...Koalicyjny.Komitet.Wyborczy.Zjednoczona.Lewica.SLD.TR.PPS.UP.Zieloni/Głosy.ważne, 
										Kukiz15=100*X7...Komitet.Wyborczy.Wyborców..Kukiz.15./Głosy.ważne, 
										Nowoczesna=100*X8...Komitet.Wyborczy.Nowoczesna.Ryszarda.Petru/Głosy.ważne)

# tylko te istotne kolumy
kto_wygral <- select(kto_wygral, TERYT, PO, PiS, Razem, KORWiN, PSL, ZLew, Kukiz15, Nowoczesna)

# na chwile, żeby nie liczyć maxów z TERYT
row.names(kto_wygral)<-kto_wygral$TERYT
kto_wygral$TERYT <- NULL

# dodanie kolumne maxy z nazwa kolumny gdzie jest max
kto_wygral$wygrany <- apply(kto_wygral,1,function(x) names(kto_wygral)[which(x==max(x))])

# dodaj z powrotem kolumne TERYT
kto_wygral$TERYT <- row.names(kto_wygral)

# złącz dane z mapą
kto_wygral.powiaty <- left_join(mapa.powiaty, kto_wygral, by=c("kod_TERYT" = "TERYT"))

# rysujemy mapę
kto_wygral.mapa <- mapa_tlo +
	geom_polygon(data=kto_wygral.powiaty,
					 aes(x=long, y=lat, group=group, fill=wygrany),
					 color = 'grey',
					 size = 0.1,
					 alpha = 0.7) +
	scale_fill_discrete(name="Wygrany w powiatach")
print(kto_wygral.mapa)




# ktora partia wygrala w gminach?
kto_wygral <- select(wyniki.sejm.gminy, TERYT, Głosy.ważne,
							X1...Komitet.Wyborczy.Prawo.i.Sprawiedliwość,
							X2...Komitet.Wyborczy.Platforma.Obywatelska.RP,
							X3...Komitet.Wyborczy.Partia.Razem,
							X4...Komitet.Wyborczy.KORWiN,
							X5...Komitet.Wyborczy.Polskie.Stronnictwo.Ludowe,
							X6...Koalicyjny.Komitet.Wyborczy.Zjednoczona.Lewica.SLD.TR.PPS.UP.Zieloni,
							X7...Komitet.Wyborczy.Wyborców..Kukiz.15.,
							X8...Komitet.Wyborczy.Nowoczesna.Ryszarda.Petru)
# procenty dla wszystkich
kto_wygral <- mutate(kto_wygral,PO=100*X2...Komitet.Wyborczy.Platforma.Obywatelska.RP/Głosy.ważne)
kto_wygral <- mutate(kto_wygral,PiS=100*X1...Komitet.Wyborczy.Prawo.i.Sprawiedliwość/Głosy.ważne)
kto_wygral <- mutate(kto_wygral,Razem=100*X3...Komitet.Wyborczy.Partia.Razem/Głosy.ważne)
kto_wygral <- mutate(kto_wygral,KORWiN=100*X4...Komitet.Wyborczy.KORWiN/Głosy.ważne)
kto_wygral <- mutate(kto_wygral,PSL=100*X5...Komitet.Wyborczy.Polskie.Stronnictwo.Ludowe/Głosy.ważne)
kto_wygral <- mutate(kto_wygral,ZLew=100*X6...Koalicyjny.Komitet.Wyborczy.Zjednoczona.Lewica.SLD.TR.PPS.UP.Zieloni/Głosy.ważne)
kto_wygral <- mutate(kto_wygral,Kukiz15=100*X7...Komitet.Wyborczy.Wyborców..Kukiz.15./Głosy.ważne)
kto_wygral <- mutate(kto_wygral,Nowoczesna=100*X8...Komitet.Wyborczy.Nowoczesna.Ryszarda.Petru/Głosy.ważne)

# tylko te istotne kolumy
kto_wygral <- select(kto_wygral, TERYT, PO, PiS, Razem, KORWiN, PSL, ZLew, Kukiz15, Nowoczesna)

# na chwile, żeby nie liczyć maxów z TERYT
row.names(kto_wygral)<-kto_wygral$TERYT
kto_wygral$TERYT <- NULL

# dodanie kolumne maxy z nazwa kolumny gdzie jest max
kto_wygral$wygrany <- apply(kto_wygral,1,function(x) names(kto_wygral)[which(x==max(x))])

# dodaj z powrotem kolumne TERYT
kto_wygral$TERYT <- row.names(kto_wygral)

# złącz dane z mapą
kto_wygral.gminy <- left_join(mapa.gminy, kto_wygral, by=c("kod_TERYT" = "TERYT"))

# rysujemy mapę
kto_wygral.mapa <- mapa_tlo +
	geom_polygon(data=kto_wygral.gminy,
					 aes(x=long, y=lat, group=group, fill=wygrany),
					 color = 'grey',
					 size = 0.1,
					 alpha = 0.7) +
	scale_fill_discrete(name="Wygrany w powiatach")
print(kto_wygral.mapa)
