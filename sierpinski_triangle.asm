;Jakub Kołoczek
;ostatnie podejście do projektu z asemblerków
;oby było czystą przyjemnością
;program rysujący Trójkąt Sierpińskiego

.387

data1 segment

maxposition_x	equ 27fh
maxposition_y	equ 1dfh
color			equ 05h


noargs		db "Brak argumentow. Powinienes podac dwa argumenty.",10,13,"$"
toosmall	db "Za malo argumnetow. Powinienes podac dwa argumenty.",10,13,"$"
toomuch		db "Za duzo argumentow. Powinienes podac dwa argumenty.",10,13,"$"
bad0		db "Niepoprawny pierwszy argument. Pierwszy argument powinien byc jednocyfrowy.",10,13,"$"
bad1		db "Niepoprawny drugi argument. Drugi argument powinien byc liczba calkowita o wartosci maksymalnie rownej 200.",10,13,"$"
;-----------TEKSTY POMOCNICZNE WYKORZYSTYWANE DO SPRAWDZENIA CZY KOD ODPOWIEDNIO DZIAŁA----------------------
great					db "Wprowadzone przez Ciebie dane sa poprawne.",10,13,"$"
textdrawtriangle		db "textdrawtriangle",10,13,"$"
textlsystemA			db "textlsystemA",10,13,"$"
textlsystemB			db "textlsystemB",10,13,"$"
textcoprocessorstart	db "textcoprocessorstart",10,13,"$"
textdrawline			db "textdrawline",10,13,"$"
textminus				db "textminus",10,13,"$"
textplus				db "textplus",10,13,"$"
textsincos				db "textsincos",10,13,"$"
textdeletesincos		db "textdeletesincos",10,13,"$"
textdrawpixel			db "textdrawpixel",10,13,"$"
;------------------------------------------------------------------------------------------------------------

args		db 128 dup (?) 					;tablica w której będą przechowywane dane wejściowe
numofargs	db 0h 							;liczba argumentów
counter		db 0h							;licznik argumentów
len			db ? ,10,13,"$"					;dlugosc rysowanej kreski
iternum		db ? ,10,13,"$"					;liczba iteracji
delta		word 3ch						;60 stopni
radian		word 0b4h						;180 stopni

position_x 	word 0h
position_y	word 1dfh

data1 ends


code1 segment
assume cs:code1,ds:data1,ss:stack1

start:		mov ax,seg noargs				;przygotowanie danych
			mov ds,ax
			
			mov ax, seg top1				;przygotowanie stosu
			mov ss,ax
			mov sp, offset top1

			call readargs
			call writeargs					;pomocnicze przy parsowaniu // DO WYŁĄCZENIA PODCZAS ODDAWANIA ZADANIA
			call checkargs
			call drawtriangle
			
			
endofprog:	mov ah,4ch 						;funkcja powrotu do DOSu
			int 21h
			
;------------------------PROCEDURA WCZYTUJĄCA ARGUMENTY Z WIERSZA POLECEŃ-----------------------------------
	
readargs proc
			push ax 						;wrzucenie kolejnych rejestrów na stos by nie stracić ich wartości
			push bx
			push cx
			push dx
			push di
			push si
			
			xor ax,ax 						;zerowanie rejestrów
			xor bx,bx
			xor cx,cx
			xor dx,dx
											; int21 62h // pobieranie adresu PSP (info o długości danych itd.) // SPRÓBOWAĆ TO TAK OGARNĄĆ 
			mov al,byte ptr es:[80h] 		;pobranie dlugosci podanych argumentow //z jakiegoś powodu wczytuje o jeden więcej // liczy spację po nazwie programu
			mov cl,al 						;cl-licznik znaków, przeniesienie wartosci do rejestru licznikowego
			cmp cl,0h 						;porównanie warości, zmiana flag
			je exit							;skok jeśli równe zero czyli brak argumentów, ZF=1

				;w przeciwnym wypadku łapię argumenty do tablicy

			mov ax,81h 						;ustawienie adresu początkowego argumentu-1
			mov si,ax 						;si adres pobierania znaku, będziemy iterować
			mov di,offset args 				;przypisanie adresu początkowego,gdzie będę kopiował znaku
			
			xor ax,ax 						;zerowanie
			mov ah,0h 						;ah-flaga czy wpisywać dolara //0-nie, 1-tak
			mov dl,24h 						;znak separatora $

				;wszytuje kolejne znaki z wiersza poleceń i sprawdza czy nie są znakami białymi, jeśli nie to zapisuje w tablicy argmentów
			
readchar:	inc si							;zwiększenie adresu z którego wczytujemy znaki
			dec cl							;zmniejszenie licznika znaków
			cmp cl,0h 						;czy licznik doszedł do zera
			je endreadchar 					;skok jeśli równe, ZF=1
			mov al,byte ptr es:[si] 		;pobranie znaku z adresu
			cmp al,21h 						;poniżej 21h są znaki sterujące czyli białe znaki
			jb ifinsertdolar 				;skok jeśli mniejsze cf=1
			cmp al,7fh						;sprawdzenie dla DELETE
			jz	ifinsertdolar				;skok jeśli równe
			mov ah,01h 						;znaleźliśmy znak więc zmieniamy flagę wstawiania dolara
			mov byte ptr ds:[di],al 		;zapisujemy znak w tablicy
			inc di 							;zwiększamy pozycję w tablicy
			jmp readchar 					;pętelka

				;skaczemy tutaj jeśli wczytaliśmy biały znak i na podstawie flagi określamy czy wstawiać dolara czy nie
			
ifinsertdolar: 
			cmp ah,0h 						;sprawdza flagę
			je readchar						;jeśli flaga równa 0 to nie było przed tym znakiem niebiałego znaku więc po prostu pomijamy
			mov byte ptr ds:[di],dl 		;zapisujemy dolara, którego wcześniej zapisaliśmy do dl
			mov ah,ds:[counter] 			;jeśli był znak niebiały to zwiększamy licznik argumentów
			inc ah
			mov ds:[counter],ah
			mov ah,0h 						;zmiana flagi by kolejny biały znak się nie zapisał
			inc di 							;zwiększanie pozycji w tablicy zapisywanych znaków
			jmp readchar

				;po wczytaniu ostatniego znaku sprawdzamy czy trzeba wstawić za nim dolara
			
endreadchar:cmp ah,0h 						;czy flaga równa
			je exit 						;jeśli zero, czyli wcześniej był biały znak to zapisujemy liczbę arg
			mov byte ptr ds:[di],dl 		;jeśli był znak niebiały to dopisujemy na koniec dolara
			mov ah,ds:[counter] 				;dodanie ostatniego argumentu
			inc ah
			mov ds:[counter], ah

				;zakończenie procedury, zapisanie liczby argumentów oraz odzyskanie rejestrów
			
exit:		mov ah,[counter]
			mov [numofargs],ah 				;zapisanie liczby argumentow
			pop si 							;przywracanie pierwotnych wartosci rejestru
			pop di
			pop dx
			pop cx
			pop bx
			pop ax
			ret 				
readargs endp

;------------------------PROCEDURA WYPISUJĄCA WCZYTANE ARGUMENTY Z WIERSZA POLECEŃ-----------------------------------

writeargs proc
			push ax
			push bx
			push cx
			push dx
			
			xor ax,ax
			xor bx,bx
			xor cx,cx
			xor dx,dx
			
			mov bl,[numofargs] 				;"globalny dana", liczba argumentów
			cmp bl,0h 						;czy równa zero?
			je eexit    					;jeśli tak to skos do exit

write:		mov ax,cx
			call adress						;wywołanie procedury odnajdującej adres argumentu
			mov dx,ax 						;adres argumentu
			mov ah,09h 						;funkcja wypisująca ds:dx do dolara
			int 21h
			
			mov ah,02h 						;wyświetla znak z dl
			mov dl,0ah 						;znak nowej linii
			int 21h
			mov dl,0dh						;znak powrotu karetki na początek linii
			int 21h
				
			inc cx 							;zwiększa licznik wypisanych argumentów
			cmp cx,bx  						;porównuje z liczbą argumentów
			jne write
			
eexit:		pop dx
			pop cx
			pop bx
			pop ax
			ret
			
writeargs endp

;------------------------PROCEDURA ODNAJDUJĄCA ADRES ARGUMENTU-----------------------------------

adress proc
			push bx
			push cx
			push dx
			push si	
			
			xor bx,bx
			xor cx,cx
			xor dx,dx
			
			mov cx,ax 						;numer szukanego argumentu
			cmp cx,0h
			lea si,args 					;znajduje adres początku tablicy z argumentami
			je exit0 						;jeśli cx=0 to szukany był pierwszy argument więc agres mamy

				;itereuje po tablicy z zapisanymi znakami wejściowymi by znaleźć kolejne argumenty, które oddzielone są dolarami
			
ifnotdolar:	
			mov al, byte ptr ds:[si]		;wczytuje znak z tablicy z argumentami
			inc si							;zwiększenie przesunięcia o 1 w tablicy
			cmp al,24h 						;znak dolara
			jne ifnotdolar					;skok jeśli wczytany znak nie jest dolarem
			dec cx							;zmniejsza licznik argumentu
			cmp cx,0h						;sprawdzenie czy jest to interesujący nas argument
			jne ifnotdolar					;jeśli nierówne zero to wczytuje kolejny argumnet z tablica
			
exit0:		mov ax,si						;przekazanie adresu w rejestrze ax
			pop si
			pop dx
			pop cx
			pop bx
			ret
adress endp
			
;------------------------PROCEDURA SPRAWDZAJĄCA POPRAWNOŚĆ ARGUMENTÓW-----------------------------------

checkargs proc
			push ax
			push bx
			push cx
			push dx
			push di
			push si
			
			xor ax,ax
			xor bx,bx
			xor cx,cx
			xor dx,dx
			
			mov al,[numofargs]				;wczytanie liczby argumenów
		
				;sprawdzanie liczby argumentów

			cmp al, 0h						
			je lackargs						;skacze jeśli równe 0
			cmp al, 2h
			jb givemore						;skacze jeśli mniejsze od 2
			ja giveless						;skacze jeśli większe od 2
			
				;sprawdzanie poprawności pierwszego argumentu // długość + znaki
			
			mov di,offset args				;ustawienie przesunięcie początku tablicy z danymi
			mov al, byte ptr ds:[di+1]		;w tym miejscu powinien być dolar by pierwszy argument miał długość jeden
			cmp al,24h
			jne error0						;jeśli nie równe, czyli znak nie jest dolarem to błąd
			mov al, byte ptr ds:[di]		;wczytanie pierwszego znaku
			cmp al,30h						
			jb error0						;skacze jeśli mniejsze od 30h // kod ascii 0
			cmp al,39h						
			ja error0						;skacze jesli wieksze od 39h // kod ascii 9
			sub al,30h
			mov byte ptr ds:[iternum],al
			inc di							;przesunięcie na znak dolara
			mov si,di						;skopiowanie przesunięcia (zatrzymanego na znaku dolara) w razie potrzeby
			
				;sprawdzanie długości drugiego argumentu			

secondarg1:	inc di							;przesunięcie na kolejny znak
			inc cl							;zwiększenie licznika znaków
			mov bl, byte ptr ds:[di]		;wczytanie kolejnego znaku
			cmp bl,24h						;sprawdzenie czy jest dolarem
			jne secondarg1
			dec cl                          ;zmniejszenie licznika, bo został wczytany znak dolara
			cmp cl,03h						;sprawdzenie czy dlugos poprawna
			ja	error1

				;sprawdzanie poprawnści znaków w drugim argumencie
				
secondarg2:	dec di							
			mov al,byte ptr ds:[di]
			cmp al,24h						;sprawdzenie czy jest dolarem
			je next
			cmp al,30h						
			jb error1						;skacze jeśli mniejsze od 30h // kod ascii 0
			cmp al,39h						
			ja error1						;skacze jesli wieksze od 39h // kod ascii 9
			jmp secondarg2
			
			
next:		xor al,al						
			mov dl,10
toint:		inc di
			mul dl							;mnożenie
			mov bl, byte ptr ds:[di]
			sub bl,30h						;zamiana znaku na liczbę
			add al,bl
			loop toint
				
			cmp al,200						;sprawdzenie czy mniejszy od 201
			ja error1
			jmp good
			
lackargs:	mov dx,offset noargs			;wczytuje przesunięcie odpowiedniego komentarza // tak samo poniżej
			jmp theend						;skok bezwarunkowy
			
givemore:	mov dx,offset toosmall
			jmp theend

giveless:	mov dx,offset toomuch
			jmp theend

error0:		mov dx,offset bad0
			jmp theend
			
error1:		mov dx,offset bad1
			jmp theend
			
				;pomocnicze wypisanie, że argumenty są poprawne
			
good:		mov byte ptr ds:[len],al
			mov dx,offset great
			mov ah,9h 						;funkcja wypisywania
			int 21h
			
exit3:		pop si
			pop di
			pop dx
			pop cx
			pop bx
			pop ax
			ret	

				;jeśli był błąd to kończymy całkowicie program
			
theend:		mov ah,9h 						;funkcja wypisywania
			int 21h
			jmp endofprog					;skok do etykiety kończącej cały program, a nie tą procedurę // wcześniej wystąpił błąd
		
checkargs endp

;------------------------PROCEDURA RYSUJĄCA TRÓJKĄT-----------------------------------

drawtriangle proc
			push ax
			push bx
			push cx
			push dx
			push di
			push si
			
			xor ax,ax
			xor bx,bx
			xor cx,cx
			xor dx,dx
			
graphicsmodestart:			
			mov al,12h						;ah=0h funkcja włączająca tryb video, al - wybór trybu video, AL=c
			int 10h							;12h - graficzny, 640x480 w 16/256tys. kolorach, segment 0A000
			
			xor ax,ax
			mov al,byte ptr ds:[len]
			cmp al,0h						;jeśli długość boku 0, to nie będziemy nic rysować więc koprocesor niepotrzebny
			jz textmodeback
			
			mov al,ds:[iternum]				;sprawdzam czy iternum jest parzyste, jeśli tak to zmieniam pozycję startową na 0
			mov bl,02h
			div bl
			cmp ah,00h
			jz loadpositionY0
			jmp reloaditernum
			
loadpositionY0:
			mov ax,00h
			mov ds:[position_y],ax

reloaditernum:			
			mov al,ds:[iternum]
			
			
			call coprocessorstart			;włączenie koprocesora
			
			mov al,ds:[iternum]
			cmp al,0h						;jeśli 0 zagłębień to rysujemy tylko jedną linię
			jz onlyoneline	
			
			dec al
			call lsystemA
			jmp textmodeback
			
onlyoneline:call drawline			
			
textmodeback:
			mov ah,0h						;oczekiwanie na wciśnięcie klawisza i odczyt znaku, zwraca w al-kod ascii znaku, ah-kod klawisza
			int 16h							;int 16h przerwania związane z klawiaturą
			
			mov al,03h						;AL=03h: CGA, tekstowy kolorowy 80x25 		
			mov ah,0h						;ah=0h funkcja włączająca tryb video	
			int 10h							;przerwania sterujące grafiką
			
			;mov dx,offset textdrawtriangle
			;mov ah,9h 						;funkcja wypisywania
			;int 21h
			
			pop di
			pop si
			pop dx
			pop cx
			pop bx
			pop ax
			ret	
drawtriangle endp

;------------------------REKURENCYJNE TWORZENIE NAPISU W L-SYSTEMIE DLA LITERY A-----------------------------------

lsystemA proc
			push bx
			push cx
			push dx
			push di
			push si
			push ax
					
			pop ax
			xor bx,bx
			xor cx,cx
			xor dx,dx
			
			push ax							;odłożenie licznika zejść na stos by potem go ściągnąć ze stosu by się zgadzał przy wracaniu
			cmp al,0h						
			ja lsystemAdeep					;jeśli zostało zero zejść to od razu rysujemy
			
lsystemAdraw:
			call drawline					;rysowanie trójkąta B-A-B, A i B oznaczają rysowanie lini, minusy to obrót żółwia o kąt zgodny z kierunkiem ruchu wskazówek zegara
			call minus
			call drawline
			call minus
			call drawline
			jmp exitA
			
			
lsystemAdeep:
			dec al
			call lsystemB					;wywołanie kolejnych zejść rekursywnych dla A -> B-A-B
			call minus
			call lsystemA
			call minus
			call lsystemB
					
			;mov dx,offset textlsystemA
			;mov ah,9h 						;funkcja wypisywania
			;int 21h
			
exitA:		pop ax							;ściągamy licznik potrzebny do poprawnych powrotów
			pop si
			pop di
			pop dx
			pop cx
			pop bx
			ret	
lsystemA endp

;------------------------REKURENCYJNE TWORZENIE NAPISU W L-SYSTEMIE DLA LITERY B-----------------------------------

lsystemB proc
			push bx
			push cx
			push dx
			push di
			push si
						
			;push ax
			
			;mov dx,offset textlsystemB
			;mov ah,9h 						;funkcja wypisywania
			;int 21h
			
			;pop ax
			
			xor bx,bx
			xor cx,cx
			xor dx,dx
			
			push ax							;odłożenie licznika zejść na stos by potem go ściągnąć ze stosu by się zgadzał przy wracaniu
			cmp al,0h						
			ja lsystemBdeep					;jeśli zostało zero zejść to od razu rysujemy
			
lsystemBdraw:
			call drawline					;rysowanie trójkąta A+B+A, A i B oznaczają rysowanie lini, plusy to obrót żółwia o kąt przeciwny do kierunku ruchu wskazówek zegara
			call plus
			call drawline
			call plus
			call drawline
			jmp exitB

lsystemBdeep:			
			dec al
			call lsystemA					;wywołanie kolejnych zejść rekursywnych dla B -> A+B+A
			call plus
			call lsystemB
			call plus
			call lsystemA
			
exitB:		pop ax							;ściągamy licznik potrzebny do poprawnych powrotów
			pop si
			pop di
			pop dx
			pop cx
			pop bx
			ret	
lsystemB endp

;------------------------PROCEDURA PRZYGOTOWUJĄCA KOPROCESOR-----------------------------------

coprocessorstart proc
			push ax
			push bx
			push cx
			push dx
			push di
			push si
			
			finit							;inicjalizowanie fp
			fild word ptr radian			;załadowanie liczb na stos
			fild word ptr delta
			fdiv st(0),st(1)				;dzieli st(0) przez st(1), wynik w st(0)
			fldpi							;ładuje pi do st(0)
			fmul st(0),st(1)				;mnożenie st(0) i st(1), wynik w st(0)
			;fxch 							;zamiana st1 z st0
			;fstp st(0)						;zdejmuje ze stosu deltę
			;fxch st(1)
			;fstp st(0)						;zdejmuje ze stosu zero
			fldz							;ładuje na stos zera jako wartość kąta
			fild   word ptr ds:[position_y]
			fild   word ptr ds:[position_x]
			
			;po inicjalizacji koprocesora na stosie mamy 0 -> pozycja x = 0;
			;1 - pozycja y = 0; 2 - pozycja kąta = 0; 3 - kąt o który przesuwamy = pi/3
			;na dalszych pozycjach koprocesora mamy 1/3 oraz 180, ale jest to niepotrzebne i przepada
			
			;mov dx,offset textcoprocessorstart
			;mov ah,9h 						;funkcja wypisywania
			;int 21h
			
			pop si
			pop di
			pop dx
			pop cx
			pop bx
			pop ax
			ret	
coprocessorstart endp

;------------------------PROCEDURA RYSUJĄCA BOK-----------------------------------

drawline proc
			push ax
			push bx
			push cx
			push dx
			push di
			push si
			
			xor ax,ax
			xor bx,bx
			xor cx,cx
			xor dx,dx
			
			;mov dx,offset textdrawline
			;mov ah,9h 						;funkcja wypisywania
			;int 21h
			
			mov cl,byte ptr ds:[len]	;pobranie długości boku do licznika

					;obliczenie sinusa i cosinusa
			
			fld st(2)					;powielenie wartości kąta na szczycie stosu koprocesora
			fsincos						;wyliczenie wartości sin i cos dla st0, wyniki cos w st0, sin w st1
			fxch st(2)					;wymiany pozycji by otrzymać kolejność -> x,y,cos,sin,kat,delta
			fxch st(1)
			fxch st(3)
			fxch st(1)		
					
stilldraw:	call drawpixel				;rysowanie pixela 
			loop stilldraw				;pętla odpowiadająca za narysowanie odpowiedniej liczby pixeli
					
					;usuwa ze stosu sinus i cosinus
			
			fxch st(2)				;wyciągnięcie cos na szczyt stosu
			fstp st(0)				;usunięcie wierzchołka stosu
			fxch st(2)				;wyciągnięcie sin na szczyt stosu
			fstp st(0)
			
			pop si
			pop di
			pop dx
			pop cx
			pop bx
			pop ax
			ret	
drawline endp

;------------------------PROCEDURA ODEJMUJĄCA KĄT-----------------------------------

minus proc
			push ax
			push bx
			push cx
			push dx
			push di
			push si
			
			;mov dx,offset textminus
			;mov ah,9h 						;funkcja wypisywania
			;int 21h
			
			fxch st(2)					;wymiana miejscami na stosie wartości kąta z pozycją x	
			fsub st(0),st(3)			;odjęcie kąta
			fxch st(2)					;powrót wartości kąta na swoje miejsce
			
			pop si
			pop di
			pop dx
			pop cx
			pop bx
			pop ax
			ret	
minus endp

;------------------------PROCEDURA DODAJĄCA KĄT-----------------------------------

plus proc
			push ax
			push bx
			push cx
			push dx
			push di
			push si
			
			;mov dx,offset textplus
			;mov ah,9h 						;funkcja wypisywania
			;int 21h
			
			fxch st(2)					;wymiana miejscami na stosie wartości kąta z pozycją x	
			fadd st(0),st(3)			;dodanie kąta
			fxch st(2)					;powrót wartości kąta na swoje miejsce
			
			pop si
			pop di
			pop dx
			pop cx
			pop bx
			pop ax
			ret	
plus endp

;------------------------PROCEDURA OBLICZAJĄCA POZYCJĘ PIXELA I RYSUJĄCA GO-----------------------------------

drawpixel proc
			push ax
			push bx
			push cx
			push dx
			push di
			push si
			
			;mov dx,offset textdrawpixel
			;mov ah,9h 						;funkcja wypisywania
			;int 21h
			
			fadd st(0),st(2)
			fist word ptr ds:[position_x]	;obcina ewentualną część całkowitą i zapisuje do pamięci
			fxch st(1)						
			fadd st(0),st(3)
			fist word ptr ds:[position_y]	
			;fld st(0)						;ładuje liczbę całkowitą z pamięci // do st(0) wczytaj st(0), powielenie szczytu stosu
			;fabs							;zastępuje st(0) wartością bezwzględną
			;fistp word ptr ds:[position_y]		;tak samo jak fist tylko ściąga również ze stosu
			fxch st(1)
			
			xor ax,ax
			xor bx,bx
			xor cx,cx
			xor dx,dx
			
			mov cx, word ptr ds:[position_x]
			mov ax, word ptr ds:[position_y]
			;sub ax, word ptr position_y
			mov dx,ax
		
			cmp cx,00h
			jb outofscreen
			cmp cx, maxposition_x
			ja outofscreen
			cmp dx,00h
			jb outofscreen
			cmp dx, maxposition_y
			ja outofscreen				
		
			mov ah,0ch						;rysuje pixel, w al kolor, w bh numer strony, w cx x, w dx y
			mov al, byte ptr ds:[color]
			int 10h
			
outofscreen:pop si
			pop di
			pop dx
			pop cx
			pop bx
			pop ax
			ret	
drawpixel endp


code1 ends


stack1 segment stack

			dw 200 dup (0)
top1		dw ?

stack1 ends


end start    