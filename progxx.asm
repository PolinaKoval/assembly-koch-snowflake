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
                
		komu1 db "Blad danych: zla liczba argumentow (oczekiwano 2).$"
		komu2 db "Blad: zbyt duza liczba iteracji l-systemu.$"
				
data ends
;-------------------------------

stack segment stack
        dw      256 dup (?)           	;stos 256 * 2 bajty
top     dw      ?                       ;wierzcholek stosu
stack ends

;------------------------

code segment
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
			
			cmp ds:[iternum],10				;czy podana liczba iteracji l-systemu nie jest
			JA sizeerr						;zbyt duza? Jesli tak - blad.
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
		call CHECKUP
		
		;========TESTY TESTY TESTY================
	
		mov dx,ds:[iternum]
		add dx,48
		mov ah,2
		int 21h
		
		mov dx,ds:[len]
		add dx,48
		mov ah,2
		int 21h
		;==========TESTY TESTY TESTY==============

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
code ends
end START