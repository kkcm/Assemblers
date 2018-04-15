;Jakub Kołoczek
;nieudaczna próba napisania parsera wczytującego dane,znajającego 
;białe znaki i wypisującego argumenty w kolejnych wersach

.286

data1 segment

noargs		db "Brak argumentow. Powinienes podac dwa argumenty.$"
toosmall	db "Za malo argumnetow. Powinienes podac dwa argumenty.$"
toomuch		db "Za duzo argumentow. Powinienes podac dwa argumenty.$"
bad0		db "Niepoprawny pierwszy argument. Pierwszy argument powinien byc jednocyfrowy.$"
bad1		db "Niepoprawny pierwszy argument. Pierwszy argument powinien byc zerem lub jedynka.$"
bad2		db "Niepoprawny drugi argument. Drugi argument powinien skladac sie z 32 znakow.$"
bad3		db "Niepoprawny drugi argument. Drugi argument powinien skladac sie z cyfr lub liter od a do f.$"
;-----------TEKSTY POMOCNICZNE WYKORZYSTYWANE DO SPRAWDZENIA CZY KOD ODPOWIEDNIO DZIAŁA----------------------
great		db "Wprowadzone przez Ciebie dane sa poprawne.$"
;number1		db 10,13,"Wczytales cyfre.$"
;letter1		db 10,13,"Wczytales litere.$"
;------------------------------------------------------------------------------------------------------------
rsa			db 10,13,"+--[ RSA 1024 ]---+",13,10,"$"
signature	db "+-[Jakub Koloczek]+$"
sign		db ' ', '.','o','+','=','*','B','O','X','@','%','&','#','/','^'
moves		db 153 dup (0h)
args		db 128 dup (?) 					;tablica w której będą przechowywane dane wejściowe
binary		db 16 dup (?)					;tablica z przekonwertowanym drugim argumentem
numofargs	db 0h 							;liczba argumentow
counter		db 0h

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
			call convert
			call movemaker
			call toascii			
			call writeascii

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
			je exita 						;jeśli cx=0 to szukany był pierwszy argument więc agres mamy

				;itereuje po tablicy z zapisanymi znakami wejściowymi by znaleźć kolejne argumenty, które oddzielone są dolarami
			
ifnotdolar:	
			mov al, byte ptr ds:[si]		;wczytuje znak z tablicy z argumentami
			inc si							;zwiększenie przesunięcia o 1 w tablicy
			cmp al,24h 						;znak dolara
			jne ifnotdolar					;skok jeśli wczytany znak nie jest dolarem
			dec cx							;zmniejsza licznik argumentu
			cmp cx,0h						;sprawdzenie czy jest to interesujący nas argument
			jne ifnotdolar					;jeśli nierówne zero to wczytuje kolejny argumnet z tablica
			
exita:		mov ax,si						;przekazanie adresu w rejestrze ax
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
			jb error1						;skacze jeśli mniejsze od 30h // kod ascii 0
			cmp al,31h						
			ja error1						;skacze jesli wieksze od 31h // kod ascii 1
			inc di							;przesunięcie na znak dolara
			mov si,di						;skopiowanie przesunięcia (zatrzymanego na znaku dolara) w razie potrzeby

				;sprawdzanie długości drugiego argumentu
			
secondarg1:	inc di							;przesunięcie na kolejny znak
			inc cl							;zwiększenie licznika znaków
			mov al, byte ptr ds:[di]		;wczytanie kolejnego znaku
			cmp al,24h						;sprawdzenie czy jest dolarem		
			jne secondarg1
			dec cl                          ;zmniejszenie licznika, bo został wczytany znak dolara
			cmp cl,20h						;sprawdzenie czy dlugos poprawna
			jne	error2

				;sprawdzanie poprawnści znaków w drugim argumencie	
				
secondarg2:	dec di							;cofamy się o jeden znak w drugim argumencie
			mov al,byte ptr ds:[di]
			cmp al,30h					
			jb	error3						;skaczemy jeśli mniejsze od 0
			cmp al,66h
			ja	error3						;skaczemy jeśli większe  od f
			cmp al,61h
			jb iferror						;skaczemy jeśli mniejsze od a
ok:			dec cl							;zmniejszamy licznik znaków
			cmp cl,0h						
			je good							;skaczemy jeśli sprawdziliśmy wszystkie znaki
			jmp secondarg2

				;sprawdzenie czy znak nie jest w "dziurze" między '9', a 'a'
			
iferror:	cmp al,39h						
			ja error3						;skok jeśli większe od '9'
			jbe ok							;skok jeśli mniejsze lub równe	

				;ładowanie odpowiednich offsetów z tekstami o błędach do wypisania
			
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
			
error2:		mov dx,offset bad2
			jmp theend
			
error3:		mov dx,offset bad3
			jmp theend
			
				;pomocnicze wypisanie, że argumenty są poprawne
			
good:		mov dx,offset great
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

;------------------------PROCEDURA ZAMIENIAJĄCA ZNAKI ASCII NA WARTOŚĆ LOGICZNĄ-----------------------------------

convert proc
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

			lea si,args						;znajduje adres początku tablicy z argumentami
			lea di,binary					;znajduje adres początku tablicy z liczbami w formie binarnej
			inc si							;przesunięcie na dolar za pierwszym argumentem
			mov cx,10h						;licznik ustawiam na 16, bo tyle liczb chcę przekonwertować
	
				;pętla pobierająca kolejne bajty danych i zapisująca skonwertowane dane w nowej tablicy
	
getarg:		inc si							;zwiększanie przesunięcia by wczytać kolejną liczbę
			mov al, byte ptr ds:[si]		;wczytanie liczby z tablicy argumentów
			call numorlet					;wywołuje procedurę rozpoznawczą czy litera czy cyfra
			shl al,4						;przesunięcie o cztery bity w lewo czyli pomnożenie razy 16
			mov bl,al 						;kopia al do bl
			inc si							;analogicznie jak wyżej tylko dla drugiej cyfry z liczby heaksadecymalna
			mov al, byte ptr ds:[si]
			call numorlet
			add al,bl						;dodanie zapamiętanej liczby w bl do al
			mov byte ptr ds:[di],al			;zapisanie do tablicy z liczbami w postaci binarnej
			inc di							;przesunięcie pozycji w tablicy binarnej
			loop getarg						;pętla, która sama zmniejsza licznik cx

			pop si
			pop di
			pop dx
			pop cx
			pop bx
			pop ax
			ret
convert endp

;------------------------PROCEDURA ROZPOZNAJĄCA CZY ZNAK JEST CYFRĄ CZY LITERĄ I ZAMIENIAJĄCA GO WARTOŚĆ LOGICZNĄ-----------------------------------
			
numorlet proc
			push bx
			push cx
			push dx
			
			xor bx,bx
			xor cx,cx
			xor dx,dx

				;rozpoznanie czy znak czy litera
			
			cmp al,39h						;39h kod ascii znaku 9
			jbe number						;jeśli mniejsze lub równe to cyfra i skok
			cmp al,66h						;66h kod ascii znaku f
			jbe letter

				;zmiana znaku ascii na wartość logiczną + w komentarz pomocnicze wypisywanie co wczytaliśmy by sprawdzić poprawność działania
			
letter:		sub al,57h						;odjęcie 57h, żeby litera a miała wartość logiczną 10
;           push ax							;odłożenie na stos, bo przy przerwaniu int zmieniał się cały rejestr i tracił wartość
;			mov dx,offset letter1
			jmp exit4
			
number:		sub al,30h  					;odjęcie 30h żeby znak 0 miał wartość logiczną 0
;           push ax
;			mov dx,offset number1
			jmp exit4

exit4:	;	mov ah,9h 						;funkcja wypisywania
;			int 21h    
;			pop ax							;odzyskanie wartości logicznej liczby/litery
			pop dx
			pop cx
			pop bx
			ret
numorlet endp

;------------------------PROCEDURA ANALIZUJĄCA PARY BITÓW I DECYDUJĄCA JAKI RUCH WYKONAĆ ORAZ ZAPISUJĄCA LICZBĘ ODWIEDZIN TABLICY RUCHÓW-----------------------------------

movemaker proc
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
	
				;przygotowanie rejestru es w którym będzie numer modyfikacji oraz aktualna pozycja gońca
	
			lea si,args						;znajduje adres początku tablicy z wprowadzonymi argumentami
			mov ah, byte ptr ds:[si]		;wczytujemy wartość modyfikacji do starszej części rejestru
			sub ah,30h						;zamieniamy znak ascii na wartość logiczną
			inc	ah							;zwiększam ah żeby 0 stało się 1, a 1 zmieniła się na 2, bo potem będę ładował do licznika ruchów gońca
			mov al,4ch						;w młodszej części bitu umieszczamy pozycję początkową.
			mov es,ax						;pakujemy cały rejestr ax do es, który będziemy potem rozpakowawywać i korzystać z pozycji i modyfikacji
			
				;przygotowanie pętli analizującej wszystkie 16 liczb heksadecymalnych w postaci binarnej
			
			lea di,binary					;znajduje adres początku tablicy z liczbami w formie binarnej
			mov cx,10h						;licznik dużej pętli, ustawiony na 16, tyle ile liczb heksadecymalnych
				
;jeśli będzie trzeba indeksować obie pozycje w modyfikacji
;to w tym miejscu należy zrobić pętlę podwójnej analizy
;tej samej liczby heksadecymalnej
;a usunąć pętle ze wszystkich ruchów

				;pobieranie liczby + tworzenie jego kopii + ustawienie licznika
				
getbyte:	mov al, byte ptr ds:[di]		;pobranie pierwszego argumentu
			mov bl,al						;kopia wartości z pamięci
			push cx							;cx na stos przed kolejną pętlą
			mov cx,4h
			
				;analiza par bitów oraz decyzja o wykonywanym ruchu
			
bitanalize:	and al,00000001b				;cyfra na ostatnim miejscu decyduje o ruchu w prawo(1) lub lewo (0)
			cmp al,00000001b				
			je rightmove					;jeśli równe, idzie w prawo
			jne leftmove					;jeśli nie równe,idzie w lewo
back1:		mov al,bl
			and al,00000010b				;cyfra na przedostatnim miejscu decyduje o ruchu w górę(0) lub w dół (1)
			cmp al,00000010b
			je downmove						;jeśli równe, idzie w dół
			jne upmove						;jeśli nie, idzie w górę
back2:		mov al,bl						;przywracanie wartości al
			shr al,2						;przesunięcie bitowe o 2 w prawo, czyli dzielenie przez 4
			mov bl,al						;kopia al do późniejszego odtwarzania
			push bx
			push ax
		
				;WERSJA ZLICZANIA - PODBIJA LICZNIK W MIEJSCU GDZIE ZATRZYMAŁ SIĘ GONIEC
		;----------------------------------------------------------------------------------------
			mov bx,es						
			xor bh,bh           			
			mov dx,offset moves				;znalezienie offsetu tablicy ruchów
			add bx,dx						;zsumowanie offsetu z aktualnym miejscem w tablicy
			mov al,byte ptr ds:[bx]			;wczytanie liczby odwiedzin z tablicy
			inc al							;zwiększenie liczby odwiedzin
			mov byte ptr ds:[bx],al			;przeniesienie liczby odwiedzin do tablicy
		;----------------------------------------------------------------------------------------	
			pop ax
			pop bx
			loop bitanalize
			inc di							;zwiększenie przesunięcia
			pop cx							;zabranie ze stosu licznika dużej pętli
			loop getbyte
            
			jmp exit5
			
				;wywołania odpowiednich ruchów oraz powroty do właściwych miejsc
			
rightmove:  call moveright
			jmp back1

leftmove:   call moveleft
			jmp back1

downmove:   call movedown
			jmp back2

upmove:     call moveup
			jmp back2

	
exit5:		pop si
			pop di
			pop dx
			pop cx
			pop bx
			pop ax
			ret
movemaker endp

;------------------------PROCEDURA WYKONUJĄCA RUCH W PRAWO-----------------------------------

moveright proc
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
			
			mov bx,es						;rozpakowanie es do bx
			mov cl,bh						;numer modyfikacji+1 wrzucam do licznika pętli
			
mover:		mov al,bl						;ładuję aktualną pozycję do al
            mov dl,11h						;w dl umieszczam 17 przez, które będę dzielił
			div dl							;dzielę ax przez dl
			cmp ah,10h						;w ah reszta, jeśli równa 16 to znaczy, że jesteśmy przy prawej krawędzi
			je exitr						;jeśli jesteśmy przy prawej krawędzi to nie wykonujemy ruchu i skaczemy do wyjścia
			inc bl							;jeśli nie przy krawędzi to zwiększam pozycję o 1
			loop mover
		
exitr:		mov es,bx						;zapisanie aktualnej pozycji
			pop si
			pop di
			pop dx
			pop cx
			pop bx
			pop ax
			ret	
			
moveright endp

;------------------------PROCEDURA WYKONUJĄCA RUCH W LEWO-----------------------------------

moveleft proc
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
			
			mov bx,es						;rozpakowanie es do bx
			mov cl,bh						;numer modyfikacji+1 wrzucam do licznika pętli
			
movel:		mov al,bl						;ładuję aktualną pozycję do al
            mov dl,11h						;w dl umieszczam 17 przez, które będę dzielił
			div dl							;dzielę ax przez dl
			cmp ah,0h						;w ah reszta, jeśli równa 0 to znaczy, że jesteśmy przy lewej krawędzi
			je exitl						;jeśli jesteśmy przy lewej krawędzi to nie wykonujemy ruchu i skaczemy do wyjścia
			dec bl							;jeśli nie przy krawędzi to zwiększam pozycję o 1
			loop movel
		
exitl:		mov es,bx
			pop si
			pop di
			pop dx
			pop cx
			pop bx
			pop ax
			ret	
			
moveleft endp

;------------------------PROCEDURA WYKONUJĄCA RUCH W GÓRĘ-----------------------------------

moveup proc
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
			
			mov bx,es						;rozpakowanie es do bx
			mov cl,bh						;numer modyfikacji+1 wrzucam do licznika pętli
			
moveu:		mov al,bl						;ładuję aktualną pozycję do al
            mov dl,11h						;w dl umieszczam 17 przez, które będę dzielił
			div dl							;dzielę ax przez dl
			cmp al,0h						;w al całości, jeśli równe 0 to znaczy, że jesteśmy przy górnej krawędzi
			je exitu						;jeśli jesteśmy przy górnej krawędzi to nie wykonujemy ruchu i skaczemy do wyjścia
			sub bl,11h						;jeśli nie przy krawędzi idziemy o jeden w górę czyli cofamy się o 17 pozycji
			loop moveu
		
exitu:		mov es,bx
			pop si
			pop di
			pop dx
			pop cx
			pop bx
			pop ax
			ret	
			
moveup endp				

;------------------------PROCEDURA WYKONUJĄCA RUCH W DÓŁ-----------------------------------

movedown proc
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
			
			mov bx,es						;rozpakowanie es do bx
			mov cl,bh						;numer modyfikacji+1 wrzucam do licznika pętli
			
moved:		mov al,bl						;ładuję aktualną pozycję do al
            mov dl,11h						;w dl umieszczam 17 przez, które będę dzieliłmov al,bl
			div dl							;dzielę ax przez dl
			cmp al,8h						;w al całości, jeśli równe 8 to znaczy, że jesteśmy przy dolnej krawędzi
			je exitd						;jeśli jesteśmy przy dolnej krawędzi to nie wykonujemy ruchu i skaczemy do wyjścia
			add bl,11h						;jeśli nie przy krawędzi idziemy o jeden w dół czyli do przesuwamy się do przodu o 17 pozycji
			loop moved
		
exitd:		mov es,bx
			pop si
			pop di
			pop dx
			pop cx
			pop bx
			pop ax
			ret	
			
movedown endp				

;------------------------PROCEDURA ZAMIENIAJĄCA LICZBĘ RUCHÓW NA ODPOWIEDNI ZNAK-----------------------------------

toascii proc                            
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

			lea si,moves
			mov dx,offset sign			
			
			mov cx,99h						;ustawienie licznika na 153, tyle ile pozycji w tablicy
makeart:	mov	al,byte ptr ds:[si]			;pobranie liczby z tablicy ruchów
			cmp al,0eh						;porównanie z 14
			jae over13						;skok jeśli większe lub równe 14
			mov bl,al						
			add bx,dx						;dodanie do liczby z tablicy ruchów offsetu tablicy znaków
			mov al,byte ptr ds:[bx]			;pobranie odpowiedniego znaku
			mov byte ptr ds:[si],al			;wstawienie znaku do tablicy ruchów
back13:		inc si							;przesunięcie w tablicy ruchów
			xor bx,bx						;zerowanie, bo przy dodawaniu dx wskakują wartości do bh
			loop makeart
			
			mov dx,offset moves
			mov ax,es						;rozpakowanie es w którym zapisana jest modyfikacja oraz ostatnia pozycja
			xor ah,ah						;zerowanie modyfikacji
			add ax,dx						;zsumowanie offsetu tablicy ruchów oraz ostatniej pozycji
			mov si,ax
			mov byte ptr ds:[si],45h		;wrzucenie znaku 'E' na pozycję końcową
			
			mov ax,4ch						;pozycja początkowa
			add ax,dx
			mov si,ax
			mov byte ptr ds:[si],53h		;wrucenie znaku 'S' w pozycję początkową
			
			pop si
			pop di
			pop dx
			pop cx
			pop bx
			pop ax
			ret

over13:		mov byte ptr ds:[si],5eh			;wpisanie do tablicy ruchów znaku '^'
			jmp back13
			
toascii endp

;------------------------PROCEDURA WYPISUJĄCA TABLICĘ RUCHÓW RAZEM Z OBRAMOWANIEM-----------------------------------

writeascii proc
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
			
			mov ax,seg noargs
			mov ds,ax
			mov dx,offset rsa			;wypisywanie napisu
			mov ah,9h
			int 21h
			
			lea si,moves 
			mov cx,9h					;ustawienie licznika na 9
			
write2:		mov al,7ch					;kod znaku "|"
			mov dl,al
			mov ah,2h					;wypisuje znak z dl
			int 21h
			
			push cx 					;odłożenie zewnętrznego licznika na stos
            mov cx,11h					;ustawienie licznika wewnętrznej pętli na 17
writemoves:	
			mov al, byte ptr ds:[si]	;wczytanie znaku z tablicy ruchów
			mov dl,al
			mov ah,2h					;wypisanie znaku z dl
			int 21h
			inc si						;zwiększenie pozycji w tablicy
			loop writemoves
			
			pop cx						;ściągnięcie licznika zewnętrznej pętli
			mov al,7ch					;kod znaku "|"
			mov dl,al
			mov ah,2h					;wypisuje znak z dl
			int 21h
			
			mov al,0ah					;kod nowej linii
			mov dl,al
			mov ah,2h					;wypisuje znak z dl
			int 21h
			
			mov al,0dh					;kod powrotu kursora na początek
			mov dl,al
			mov ah,2h					;wypisuje znak z dl
			int 21h
			
			loop write2
			
			mov dx,offset signature		;wypisanie podpisu
			mov ah,9h
			int 21h

			pop si
			pop di
			pop dx
			pop cx
			pop bx
			pop ax
			ret	
			
writeascii endp	

code1 ends


stack1 segment stack

			dw 200 dup (0)
top1		dw ?

stack1 ends


end start                              
