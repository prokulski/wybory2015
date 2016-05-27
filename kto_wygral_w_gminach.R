# dane z mapami - wczytanie gotowych
load("mapy/mapy.RData")

library(dplyr)
library(ggplot2)

# dane z wynikami - wczytanie z XLS
library(XLConnect)
# Sejm
wb <- loadWorkbook("wyniki/sejm_powiaty.xls")
wyniki.sejm.powiaty <- readWorksheet(wb, sheet="2015-gl-lis-pow")



# ktora partia wygrala w gminach?
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
	ggplot() +
	geom_polygon(data=kto_wygral.powiaty,
					 aes(x=long, y=lat, group=group, fill=wygrany),
					 color = 'grey',
					 size = 0.1,
					 alpha = 0.7) +
	scale_fill_discrete(name="Wygrany w powiatach")
print(kto_wygral.mapa)



# wynik partii
wyniki.powiaty.PO <- select(kto_wygral.powiaty, lat, long, group, PO)
wyniki.powiaty.PiS <- select(kto_wygral.powiaty, lat, long, group, PiS)
wyniki.powiaty.Razem <- select(kto_wygral.powiaty, lat, long, group, Razem)
wyniki.powiaty.KORWiN <- select(kto_wygral.powiaty, lat, long, group, KORWiN)
wyniki.powiaty.PSL <- select(kto_wygral.powiaty, lat, long, group, PSL)
wyniki.powiaty.ZLew <- select(kto_wygral.powiaty, lat, long, group, ZLew)
wyniki.powiaty.Kukiz15 <- select(kto_wygral.powiaty, lat, long, group, Kukiz15)
wyniki.powiaty.Nowoczesna <- select(kto_wygral.powiaty, lat, long, group, Nowoczesna)

# rysujemy mapę
wyniki.mapa.KORWiN <- mapa_tlo +
	geom_polygon(data=wyniki.powiaty.KORWiN,
					 aes(x=long, y=lat, group=group, fill=KORWiN),
					 color = 'grey',
					 size = 0.1,
					 alpha = 0.7) +
	scale_fill_gradient(low='green', high='red')
print(wyniki.mapa.KORWiN)



# robimy pdfa
pdf("wyniki.pdf")
print(wyniki.mapa.PiS)
print(wyniki.mapa.PO)
print(wyniki.mapa.Kukiz)
print(wyniki.mapa.Nowoczesna)
print(wyniki.mapa.PSL)
print(wyniki.mapa.ZLew)
print(wyniki.mapa.Razem)
print(wyniki.mapa.KORWiN)
dev.off()


