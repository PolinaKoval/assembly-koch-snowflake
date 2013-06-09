;Jakub Pilch
;Informatyka IEiT


data segment
        tabl db 127 dup (?)             ;tablica 127 elementów po 1 bajcie (max input)
        licz dw 1                       ;licznik przeladowanych danych
        argz dw 1                       ;licznik argrumentow (oddzielonych spacjami/tabulatorem)
        lent dw 4       dup     (?)     ;miara dlugosci argumentow (oraz adresy ich poczatkow)
                                        ;tablica postaci: [adres][dlugosc][adres][dlugosc]...
        flaga dw 1                      ;flaga oczekiwania na argument.
		iternum dw 1					;liczba operacji l-systemu.
		len dw 1						;dlugosc pojedynczego odcinka krzywej.
		mulkeeper dw 1					;pomocniczo przechowuje mnoznik podczas konwersji liczb.
        lsystab db 7170	dup	(?)			;tablica l-systemu.
		lsyscpy	db 7170 dup (?)			;tablica pomocnicza.
		lsis	db	'-', 'F', '+', '+', 'F', '-', 'F'	;"dopisek" do kazdego F.
		rad	db	10	dup (0)				;kat zgodnie z ktorym przemieszcza sie zolw.
		x	db	10	dup (0)
		y	db	10	dup (0)
		sc 	dw	1						;pomocniczo do przenoszenia z FPU do rejestrow CPU.
		three dw 1						;dzielnik dla pi/3.
		fif dw 50						;polozenie startowe.
		hun dw 50
		
		komu1 db "Blad danych: zla liczba argumentow (oczekiwano 2).$"
		komu2 db "Blad: zbyt duza liczba iteracji l-systemu.$"
		komu3 db "Zerowa liczba len - brak pikseli do wyrysowania.$"
		komu4 db "Proste o tak duzej dlugosci len nie mieszcza sie na ekranie.$"
				
data ends
;-------------------------------

stack segment stack
        dw      1024 dup (?)           	;stos 256 * 2 bajty
top     dw      ?                       ;wierzcholek stosu
stack ends

;------------------------

code segment
	.386		;dopuszczenie instrukcji 386.
        ;==============================LOADER===========================================
		
         LOADER proc                            ;laduje argumenty do tabl
                                                ;seg tablicy wejsciowej w ES
                                                ;seg danych w DS
                push ax                         ;ladowanie rejestrow na stos
                push bx                         ;celem przywrocenia ich na koncu
                push cx                         ;procedury
                push dx
                mov di,offset tabl              ;początek docelowej tablicy
                mov si,82h                      ;w 81h jest spacja
                mov ch,byte ptr es:[80h]        ;licznik argumentow z wiersza polecen
                ;-----------------------------------------------
                cmp ch,0d                       ;jesli wywolano bez argumentow
                JE  stoper                      ;skocz na koniec
                cmp ch,1d                       ;jesli wywolano z jednym argumentem
                JE  stoper                      ;czyli spacja, skocz na koniec
                ;-----------------------------------------------
                mov ax,0                        ;zerowanie licznika przeniesionych danych
                mov ds:[licz],ax                ;licznik przeniesionych arg. wyzerowany
                mov ax,0                        ;poczatkowa ilosc argumentow.
                mov ds:[argz],ax                ;licznik arg. oddzielonych bialym znakiem =0.
                mov dx,1d
                mov ds:[flaga],dx               ;flaga oczekiwania=TRUE.
                mov bx,offset lent              ;offset tablicy dlugosci argumentow do BX.
                dec bx                          ;oczekujemy "przed" tablica na pierwszy arg.
                ;-----------------------------------------------
        whspace:
                cmp ch,1d                       ;czy juz nie ma czego przenosic?
                JE  stoper                      ;jeśli tak, skocz do stoper
                mov ah,es:[si]                  ;przenosimy porownywany el. do ah
                cmp ah,20h                      ;czy wczytany znak to spacja?
                JNE whtab                       ;jesli nie, skocz do whtab.

                inc si                          ;jestli tak to przesun offset wejscia
                dec ch                          ;zmniejsz licznik arg. do przeniesienia
                mov dx,1d
                mov ds:[flaga],dx               ;flaga oczekiwania=TRUE.
                JMP whspace                     ;sprawdzamy od nowa
                ;-----------------------------------------------
        whtab:
                cmp ah,9h                       ;czy wczytany znak to tabulacja?
                JNE finish                      ;jesli nie, skocz do finish

                inc si                          ;jesli tak, przesun offset wejscia
                dec ch                          ;zmniejsz licznik arg. do przeniesienia
                mov dx,1d
                mov ds:[flaga],dx  				;flaga oczekiwania=TRUE.
                JMP whspace                     ;i skok do sprawdzania spacji
                ;-----------------------------------------------
        finish:

                mov ds:[di],ah                  ;przerzut do tablicy docelowej
                inc si                          ;przesuwamy offset wejscia
                dec ch                          ;zmniejszamy licznik arg. do przeniesienia
                inc ds:[licz]                   ;zwiekszamy licznik przeniesionych argumentow

				cmp ds:[flaga],1d               ;czy oczekiwano na argument?
				JNE conti                       ;jesli nie, kontynuuj przeladowanie.
				mov dx,0d                       ;jesli tak:
				mov ds:[flaga],dx               ;flaga oczekiwania=FALSE.
				inc ds:[argz]                   ;zwieksz licznik argumentow
				mov dx,2d
                cmp ds:[argz],dx                ;porownanie z limitem argumentow
                JA argerr                   	;jesli przekroczony, skok do komuniaktu o bledzie.
                inc bx                      	;jesli nie, przesuwamy sie w tablicy [lent].
                mov [bx],di             		;jako poczatek kolejnego argumentu: aktualne polozenie w [tabl]
                inc bx                      	;przesuwamy sie do licznika dlugosci kolejnego argumentu[lent]

            conti:
                inc di                          ;przesuwamy sie w tablicy docelowej
                mov ax,1d
                add ds:[bx],ax                  	;zwiekszamy licznik dlugosci argumentu.
                JMP whspace                 	;i sprawdzamy kolejny znak

                ;-------------Powrot do programu----------------------
                stoper:
                pop dx                  ;przywracamy rejestry
                pop cx
                pop bx
                pop ax
                ret                     ;powrot do programu
        LOADER endp

		;===============================CHECKUP==============================================

		CHECKUP proc                        ;procedura CHECKUP sprawdza poprawnosc
				push ax                 	;danych przeniesionych do tablicy [tabl]
				push bx                     ;przez procedure LOADER oraz oblicza argumenty liczbowe
				push cx                     ;na podstawie argumentow wejsciowych.
				push dx

			;-------------Sprawdzenie ilosci argumentow---------------
				cmp ds:[argz],2d        	;sprawdzenie, czy wywolano z wlasciwa iloscia argumentow
				JNE argerr                  ;jesli nie 2 argumenty, blad.
			;-------------Sprawdzenie rozmiaru licznikow-----------
			call ITERNUMGETVALUE			;okreslenie wartosci iternum oraz len
			call LENGETVALUE				;z argumentow wejsciowych.
			
			cmp ds:[iternum],4				;czy podana liczba iteracji l-systemu nie jest
			JA sizeerr						;zbyt duza? Jesli tak - blad.
			
			cmp ds:[len],1					;jezeli dlugosc len jest mniejsza od 1 piksela
			JB lenerr						;skok do komunikatu.
			
			cmp ds:[len],100				;za dlugie linie nie spowoduja wyrysowania
			JA lenerr2						;zadnego sensownego obrazu.
			;-------------Powrot do programu----------------------
			getback:
				pop dx                      ;przywracamy rejestry
				pop cx
				pop bx
				pop ax
				ret                     	;powrot do programu
		CHECKUP endp
		
	;=========================ITERNUMGETVALUE=======================================
	
	ITERNUMGETVALUE proc
			push ax							;procedura konwertuje pierwszy argument wejsciowy
			push bx							;do liczby iternum.
			push cx
			push dx
			;--------------------------------
			mov di,offset tabl				;offset pierwszego argumentu do DI.
			mov cx,ds:[lent+1]				;dlugosc pierwszego argumentu do CX.
			xor ch,ch
			add di,cx						;ustawiamy sie na koncu (czytamy od najmlodszych bitow).
			
			mov ds:[mulkeeper],1d			;nasz poczatkowy mnoznik.
			mov ds:[iternum],0d				;inicjalizacja.
		unpacki:
			dec di							;przesuniecie w tablicy.
			xor bx,bx
			mov bl,ds:[di]					;pobranie znaku z tablicy do BX.
			cmp bx,65d						;klasyfikacja znaku, czy jest mniejszy od 'A'?
			JB decimalsub					;jesli tak, jest z zakresu 0-9.
			sub bx,55d						;jesli nie, jest z zakresu 'A' - 'F'.
			JMP multiplier
		decimalsub:
			sub bx,48d
		multiplier:
			xor ax,ax
			mov ax,ds:[mulkeeper]			;aktualny mnoznik do AX.
			mul bx							;DX:AX = AX * BX
			add ds:[iternum],ax				;dodanie obliczonej liczby do zmiennej iternum.
			mov bx,ds:[mulkeeper]			;mnoznik = mnoznik * 10.
			mov ax,10d
			mul bx
			mov ds:[mulkeeper],ax			;zapisanie nowego mnoznika.
			loop unpacki					;powtarzane dla wszystkich znakow wejsciowych.
			;------------------------------------
			pop dx
			pop cx
			pop bx
			pop ax
			ret
	ITERNUMGETVALUE endp
	
	;===============================LENGETVALUE======================================
	
	LENGETVALUE proc
			push ax							;procedura konwertuje drugi argument wejsciowy
			push bx							;do liczby len.
			push cx
			push dx
			;--------------------------------
			mov bx,ds:[lent+2]				;adres drugiego argumentu do BX.
			xor bh,bh
			mov di,bx
			mov cx,ds:[lent+3]				;dlugosc drugiego argumentu do CX.
			xor ch,ch
			add di,cx						;ustawiamy sie na koncu (czytamy od najmlodszych bitow).
			
			mov ds:[mulkeeper],1d			;nasz poczatkowy mnoznik.
			mov ds:[len],0d					;inicjalizacja.
		unpack:
			dec di							;przesuniecie w tablicy.
			xor bx,bx
			mov bl,ds:[di]					;pobranie znaku z tablicy do BX.
			cmp bx,65d						;klasyfikacja znaku, czy jest mniejszy od 'A'?
			JB decimalsub					;jesli tak, jest z zakresu 0-9.
			sub bx,55d						;jesli nie, jest z zakresu 'A' - 'F'.
			JMP multiplier
		decimalsub:
			sub bx,48d
		multiplier:
			xor ax,ax
			mov ax,ds:[mulkeeper]			;aktualny mnoznik do AX.
			mul bx							;DX:AX = AX * BX
			add ds:[len],ax					;dodanie obliczonej liczby do zmiennej len.
			mov bx,ds:[mulkeeper]			;mnoznik = mnoznik * 10.
			mov ax,10d
			mul bx
			mov ds:[mulkeeper],ax			;zapisanie nowego mnoznika.
			loop unpack						;powtarzane dla wszystkich znakow wejsciowych.
			;------------------------------------
			pop dx
			pop cx
			pop bx
			pop ax
			ret
	LENGETVALUE endp
	
	;==================================LSYSTEM======================================
	
	LSYSTEM proc
			push ax						;procedura oblicza instrukcje l-systemu
			push bx						;po iternum iteracjach i zapisuje wynik
			push cx						;do tablicy lsystab.
			push dx
			;------------------------------
			mov cx,ds:[iternum]			;licznik iteracji do CX.
			cmp cx,0d
			JE endsis2					;jezeli 0 iteracji, skocz do endsis2.
			
			mov si,offset lsyscpy		;offset tablicy pomocniczej z l-systemem do SI.
			
			mov al,'F'
			mov ds:[si],al				;zapis poczatkowego l-systemu.
			mov ds:[si+3],al
			mov ds:[si+6],al
			mov al,'+'
			mov ds:[si+1],al
			mov ds:[si+2],al
			mov ds:[si+4],al
			mov ds:[si+5],al			;l-system: F++F++F
			mov al,0d
			mov ds:[si+7],al			;oznaczenie konca aktualnej tablicy.
			
		iterate:
			mov di,offset lsystab		;offset tablicy z l-systemem do DI.
			mov si,offset lsyscpy		;offset tablicy pomocniczej z l-systemem do SI.
			fullsis:
				mov al,ds:[si]				;przeniesienie pojedynczego znaku do docelowej tablicy.
				mov ds:[di],al
				mov ah,'F'
				cmp ds:[di],ah				;czy przeniesiono znak 'F'?
				JE pluslsis					;jesli tak, skocz do pluslsis.
				mov ah,0
				cmp ds:[si],ah				;czy przeniesiono ostatni znak?
				JE endsis					;jesli tak, skocz do endsis.
				inc di
				inc si						;przesuniecie w obu tablicach.
				JMP fullsis					;powtarzaj od fullsis.
			pluslsis:
				call LSISADD				;dodaj znaki rozwiniecia do docelowego l-systemu.
				inc si
				inc di
				JMP fullsis					;powtarzaj od fullsis.
			endsis:
				call TABCOPY				;przenies dane z tabicy docelowej do pomocniczej.
				loop iterate				;powtarzaj iteracje.
				JMP endsis3					;koniec.
				
		endsis2:
			call STARTLSYS					;jezeli 0 iteracji, zapisz poczatkowy l-system.
			;--------------------------------
		endsis3:
			pop dx
			pop cx
			pop bx
			pop ax
			ret
	LSYSTEM endp
	
	;================================STARTLSYS=======================================
	STARTLSYS proc
			push ax						;procedura zapisuje poczatkowy l-system do
			push bx						;lsystab.
			push cx
			push dx
			;---------------------------
			mov di,offset lsystab
			mov al,'F'
			mov ds:[di],al				;zapis poczatkowego l-systemu.
			mov ds:[di+3],al
			mov ds:[di+6],al
			mov al,'+'
			mov ds:[di+1],al
			mov ds:[di+2],al
			mov ds:[di+4],al
			mov ds:[di+5],al			;l-system: F++F++F
			mov al,0d
			mov ds:[di+7],al			;oznaczenie konca aktualnej tablicy.
			;--------------------------
			pop dx
			pop cx
			pop bx
			pop ax
			ret
	STARTLSYS endp
	;=================================LSISADD=========================================
	
	LSISADD proc
			push ax							;procedura rozszerza F do F-F++F-F.
			push bx							;AKTUALNY OFFSET W DOCELOWEJ TABLICY W DI.
			push cx
			push dx
			
			mov bx,offset lsis				;offset dodawanego ciagu do BX.
			dec bx							;oczekiwanie przed tablica.
			mov cx,7d						;7 znakow do dodania.
		addition:
			inc di
			inc bx							;przesuniecie w obu tablicach.
			mov al,ds:[bx]
			mov ds:[di],al					;jeden znak z lsis do wyjsciowego l-systemu.
			loop addition					;powtorzenie dla wszystkich znakow lsis.
			
			pop dx
			pop cx
			pop bx
			pop ax
			ret
	LSISADD endp
	
	;================================TABCOPY====================================
	
	TABCOPY proc
			push ax						;procedura przekopiowuje zawartosc docelowej tablicy
			push bx						;do pomocniczej tablicy.
			push cx
			push dx
			;------------------------
			mov di,offset lsystab		;offset tablicy z l-systemem do DI.
			mov si,offset lsyscpy		;offset tablicy pomocniczej z l-systemem do SI.
			dec di
			dec si						;oczekiwanie przed tablicami.
		copier:
			inc di
			inc si
			mov al,ds:[di]
			mov ds:[si],al				;przenies pojedynczy znak z tablicy glownej do pomocniczej.
			mov ah,0d
			cmp ds:[si],ah				;czy przeniesiono ostatni znak?
			JNE copier					;jesli nie, przenos dalej.
			;-------------------------
			pop dx
			pop cx
			pop bx
			pop ax
			ret
	TABCOPY endp
	
	;===================================DRAWER==================================
	
	DRAWER proc
		push ax									;procedura rysuje linie wg. zadanego kata.
		push bx									;parametry wrzucone jednorazowo na FPU pozostaja tam.
		push cx									;st(0) = x, st(1) = y,
		push dx									;st(2) = rad, st(3) = pi/3.
		;---------------------------------
		
		mov cx,ds:[len]							;len pixeli do wyrysowania.
		fld st(2)								;kopia rad na wierzch.
		fsincos
		fxch									;zamien cos i sin.
		fxch st(3)								;y na wierzch.
		fxch									;cos na wierzch, x w st(1)
		fxch st(2)								;uzyskujemy od gory x,y,cos,sin,rad,pi/3.
	draw:
		push cx
		fist word ptr ds:[sc]					;wspolrzedna x do CX.
		mov cx,ds:[sc]
		fxch									;teraz x jest u gory.
		fist word ptr ds:[sc]					;wspolrzedna y do DX.
		mov dx,ds:[sc]
		
		cmp cx,320d					;nie rysuj poza ekranem.
		JA noscreen
		cmp dx,200d
		JA noscreen
		
		mov al,12
		mov ah,0Ch
		int 10h						;zapalenie pixela o wspolrzednych (x,y).
	noscreen:
		pop cx
		
		fadd st,st(3)					;y = y + sin(fi)
		fxch
		fadd st,st(2)					;x = x + cos(fi)
		loop draw						;powtarzaj len razy.
		
		fxch st(2)
		fistp word ptr ds:[sc]			;zrzut cosinusa oraz sinusa.
		fxch st(2)						;na FPU: x,y,rad,pi/3.
		fistp word ptr ds:[sc]
			
		;----------------------
		pop dx
		pop cx
		pop bx
		pop ax
		ret
	DRAWER endp
	;===============================SNOWFLAKE====================================
	
	SNOWFLAKE proc
			push ax						;procedura ustawia dane i wywoluje odpowiednie metody
			push bx						;w celu narysowania Krzywej Kocha.
			push cx
			push dx
			
			mov di,offset lsystab		;offset tablicy z l-systemem do di.
			mov ds:[three],3d
			fldpi
			fidiv ds:[three]			;na FPU pi/3.
			
			fld tbyte ptr ds:[rad]		;ustawienie stosu rejestrow FPU.
			fld tbyte ptr ds:[y]
			fiadd ds:[hun]
			fld tbyte ptr ds:[x]
			fiadd ds:[fif]
		flake:
			mov ah,ds:[di]
			cmp ah,0d
			JE noflake						;jezeli przeczytano caly l-system, skocz do noflake.
			call CHANGLE					;zmiana kata.
			inc di							;przesuniecie w tablicy l-systemu.
			JMP flake						;powtarzaj od flake.
		noflake:
			pop dx
			pop cx
			pop bx
			pop ax
			ret
	SNOWFLAKE endp
	
	;==============================CHANGLE======================================
	
	CHANGLE proc
			push ax						;procedura odczytuje kod lsystemu z lsystab
			push bx						;i wykonuje stosowny ruch
			push cx						;(zmiana kata lub rysowanie lini).
			push dx
			
			mov ah,ds:[di]
			cmp ah,'+'					;jezeli wczytano plus
			JE angleplus				;skocz do angleplus.
			cmp ah,'-'					;jezeli ani plus ani minus
			JNE run						;skocz do run.
			JMP angleminus				;w przeciwnym razie, skok do angleminus.
		angleplus:
			fxch st(2)					;rad na wierzch.
			fadd st,st(3)				;dodaj pi/3 do rad.
			fxch st(2)					;przywroc oryginalny porzadek x,y,rad,pi/3.
			JMP nochange				;koniec.
		angleminus:
			fxch st(2)					;rad na wierzch.
			fsub st,st(3)				;odejmij pi/3 od rad.
			fxch st(2)					;przywroc oryginalny porzadek x,y,rad,pi/3.
			JMP nochange				;koniec.
		
		run:
			call DRAWER
		
		nochange:
			pop dx
			pop cx
			pop bx
			pop ax
			ret
	CHANGLE endp
	
	;===============================================================================
              

        START:
        mov bx,ds
										;Program Segment Prefix do BX
        mov es,bx                     	;przenosimy segment do ES dla procedury
        mov ax,seg data                 ;segment danych przeladowany
        mov ds,ax						;do DS
		;----------------------------------------------------
		mov ax,seg stack				;inicjalizacja stosu
		mov ss,ax
		mov sp,offset top
		;----------------------------------------------------
		call LOADER           			;procedura loader-zaladuje arg. do tabl.
		call CHECKUP					;sprawdzenie argumentow, konwersja liczb iternum i len.
		call LSYSTEM					;zapis l-systemu do lsystab.
		mov ax,13						;wejscie do trybu graficznego.
		int 10h
		finit							;inicjalizacja FPU.
		call SNOWFLAKE					;rysowanie fraktala Krzywej Kocha.
		
		xor ax,ax						;oczekiwanie na znak.
		int 16h
		mov ax,3						;wyjscie z trybu graficznego.
		int 10h
        mov ah,4ch                    	;zakonczenie programu.
        int 21h
		
        ;****************BLAD ilosci argumentow*************************
argerr:
        mov dx,offset komu1             ;napis komunikatu do rejestru DX
        mov ah,9                        ;przerwanie nr 9 wypisuje lancuch zakonczony $
        int 21h                         ;komunikat o zlej liczbie argumentow
        mov ah,4ch                      ;i zakonczenie programu.
        int 21h
        
		;****************BLAD rozmiaru liczb*************************
sizeerr:
        mov dx,offset komu2            	;napis komunikatu do rejestru DX
        mov ah,9                        ;przerwanie nr 9 wypisuje lancuch zakonczony $
        int 21h                         ;komunikat o zbyt duzym rozmiarze iternum.
        mov ah,4ch                      ;i zakonczenie programu.
        int 21h
		
		;***************BLAD dlugosci len*****************************
lenerr:
		mov dx,offset komu3            	;napis komunikatu do rejestru DX
        mov ah,9                        ;przerwanie nr 9 wypisuje lancuch zakonczony $
        int 21h                         ;komunikat o zerowej dlugosci len.
        mov ah,4ch                      ;i zakonczenie programu.
        int 21h
		
		;***************BLAD dlugosci len 2*****************************
lenerr2:
		mov dx,offset komu4            	;napis komunikatu do rejestru DX
        mov ah,9                        ;przerwanie nr 9 wypisuje lancuch zakonczony $
        int 21h                         ;komunikat o zerowej dlugosci len.
        mov ah,4ch                      ;i zakonczenie programu.
        int 21h
code ends
end START