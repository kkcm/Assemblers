;Jakub Kołoczek
;kolejne podejście do projektu z assemblerów
;mam nadzieję, że tym razem będzie okupione mniejszym bólem
;program szyfrujący - Vigenere'a

;.286

data1 segment

noargs		db "Brak argumentow. Powinienes podac trzy argumenty oraz ewentualna opcje programu.",10,13,"$"
toosmall	db "Za malo argumnetow. Powinienes podac trzy argumenty oraz ewentualna opcje programu.",10,13,"$"
toomuch		db "Za duzo argumentow. Powinienes podac trzy argumenty oraz ewentualna opcje programu.",10,13,"$"
bad0		db "Wybrales wersje rozszyfrowujaca tekst, ale nie podales klucza.",10,13,"$"
bad1		db "Nazwa jednego z plikow jest za dluga. W systemie DOS nazwa powinna skladac sie z 8 znakow + kropki + 3 znakowego rozszerzenia.",10,13,"$"
bad2		db "Niepoprawne znaki znajduja sie w nazwie jednego z plikow.",10,13,"$"
doserror	db "Wykryto blad o numerze: ",10,13,"$"
dos01h		db "01  Invalid function number",10,13,"$"
dos02h		db "02  File not found",10,13,"$"
dos03h		db "03  Path not found",10,13,"$"
dos04h		db "04  Too many open files (no handles left)",10,13,"$"
dos05h		db "05  Access denied",10,13,"$"
dos06h		db "06  Invalid handle",10,13,"$"
dos0ch		db "0C  Invalid access mode (open mode is invalid)",10,13,"$"
dos56h		db "56  Invalid password",10,13,"$"
;-----------TEKSTY POMOCNICZNE WYKORZYSTYWANE DO SPRAWDZENIA CZY KOD ODPOWIEDNIO DZIAŁA----------------------
great		db "Wprowadzone przez Ciebie nazwy plikow nie zawieraja bledow.",10,13,"$"
info		db "Pobieram pakiet tekstu.",10,13,"$"
bufor2 		db "Wykryto koniec pliku.",10,13,"$"
;------------------------------------------------------------------------------------------------------------
args		db 128 dup (?) 					;tablica w której będą przechowywane dane wejściowe
readfile	db 16 dup (0h),'$'
writefile	db 16 dup (0h),10,13,"$"
numofargs	db 0h 							;liczba argumentow
counter		db 0h
version		dw 0h
bufor 		db 1000h dup (0),10,13,"$"
buforsize 	dw 1000h
inhand		dw	?							;wskaźnik do pliku wejściowego
outhand		dw	?							;wskaźnik do pliku wyjściowego

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
			call makeaccesspath
			call modify
			
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
			
			mov di,offset args				;ustawienie przesunięcie początku tablicy z danymi
			mov al,[numofargs]				;wczytanie liczby argumenów
		
				;sprawdzanie liczby argumentów

			cmp al, 0h						
			je lackargs						;skacze jeśli równe 0
			cmp al, 3h
			jb givemore						;skacze jeśli mniejsze od 3
			je threeargs					;skacze jeśli równe
			cmp al, 4h
			ja giveless						;skacze jeśli większe od 4
			je fourargs						;skacze jeśli równe
			
threeargs:	mov al,byte ptr ds:[di]			;wczytuje pierwszy znak argumentu
			cmp al,2dh
			jne check1						;jeśli nie '-' to skacze
			mov al,byte ptr ds:[di+1]
			cmp al,64h
			jne check1						;jeślnie nie 'd' to skacze
			mov al,byte ptr ds:[di+2]
			cmp al,24h						
			je error0						;jeśli '$' to wyrzuca błąd o braku kodu do rozszyfrowania
		
			
check1:		mov ax,0h
			mov es,ax 						;ustawienie flagi, szyfrowanie - '0'
			mov ax,di						;przekazujemy wartość di w ax
			call checkfilename				;wywołanie procedury sprawdzającej poprawność nazwy pliku wejściowego
			mov di,ax						
			inc di							;przesunięcie na początek następnego argumentu, pomiędzy znajduje się dolar
			mov ax,di
			call checkfilename				;sprawdzenie nazwy pliku wyjsciowego
			jmp good						;jeśli tu wróciliśmy to nazwy są poprawne i skaczemy do good					

fourargs:	mov al,byte ptr ds:[di]			;wczytuje pierwszy znak argumentu
			cmp al,2dh
			jne giveless					;jeśli nie '-' to skacze
			mov al,byte ptr ds:[di+1]
			cmp al,64h
			jne giveless					;jeśli nie 'd' to skacze
			mov al,byte ptr ds:[di+2]
			cmp al,24h						
			jne giveless					;jeśli nie '-' to skacze
			mov ax,1h
			mov [version],ax				;ustawienie flagi, rozszyfrowanie - '1'
			
			mov ax,di
			add ax,3h						;przesunięcie o trzy pozycje do przodu (by pominąć '-d' i znak dolara) oraz przekazanie od razu w ax parametru
			call checkfilename
			mov di,ax
			inc di							;przesunięcie na początek następnego argumentu, pomiędzy znajduje się dolar
			mov ax,di
			call checkfilename
			jmp good

lackargs:	mov dx,offset noargs			;wczytuje przesunięcie odpowiedniego komentarza // tak samo poniżej
			jmp theend						;skok bezwarunkowy
			
givemore:	mov dx,offset toosmall
			jmp theend

giveless:	mov dx,offset toomuch
			jmp theend

error0:		mov dx,offset bad0
			jmp theend
			
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
		
;------------------------PROCEDURA SPRAWDZAJĄCA POPRAWNOŚĆ NAZW WPLIKÓW-----------------------------------

checkfilename proc
			push bx
			push cx
			push dx
			push di
			push si
			
			xor bx,bx
			xor cx,cx
			xor dx,dx 			

			mov di,ax						;przekazany parament zapisujemy w di
			
partarg1:	mov al, byte ptr ds:[di]
			inc di							;przesunięcie po tablicy argumentów
			inc cl							;zwiększenie licznika
			cmp al,24h						;sprawdzenie czy dolar
			jne partarg1
			dec cl							;znaleźliśmy dolara więc zmniejszamy i licznik i przesunięcie
			dec di
			cmp cl,0ch						;był dolar, więc sprawdzamy czy licznik jest większy od 12, 8 znaków na nazwę + . + rozszerzenie trzy literowe
			ja error1						;jeśli większy to wyrzucamy błąd
			
			mov si,di						;kopiujemy di do si, bo si będziemy się wracać po tablicy
			mov ax,si
			sub ax,04h
			mov si,ax
			
			sub cx,04h						;zmniejszenie cx o 4 znaki na rozszerzenie, głównie po to by nie wywalił na kropce błędu
			

			
partarg2:	dec si							;cofnięcie się o jeden znak w tablicy argumentów
			mov al, byte ptr ds:[si]
			cmp al,22h						;porównanie ze znakiem '"'
			je error2						;skok jeśli równe
			cmp al,2bh						; '+'
			je error2
			cmp al,2ch						; ','
			je error2
			cmp al,2eh						; '.'
			je error2
			cmp al,2fh						; '/'
			je error2
			cmp al,3ah						; ':'
			je error2
			cmp al,2bh						; ';'
			je error2
			cmp al,3ch						; '<'
			je error2
			cmp al,3eh						; '>'
			je error2
			cmp al,3dh						; '='
			je error2
			cmp al,5bh						; '['
			je error2
			cmp al,5ch						; '\'
			je error2
			cmp al,5dh						; ']'
			je error2
			cmp al,7ch						; '|'
			je error2
			cmp al,2ah						; '*'
			je error2
			cmp al,3fh						; '?'
			je error2
			loop partarg2					;pętla w której licznikiem jest wcześniej ustawiony cl
			
			mov ax,di						;zwrócenie argumentu w ax
			jmp exit4
			
error1:		mov dx,offset bad1				;wczytanie ofsetów napisów z informacją o błędzie
			jmp theend2
			
error2:		mov dx,offset bad2
			jmp theend2
				
exit4:		pop si
			pop di
			pop dx
			pop cx
			pop bx
			ret	

				;jeśli był błąd to kończymy całkowicie program
			
theend2:	mov ah,9h 						;funkcja wypisywania
			int 21h
			jmp endofprog					;skok do etykiety kończącej cały program, a nie tą procedurę // wcześniej wystąpił błąd
		
checkfilename endp

;------------------------PROCEDURA TWORZĄCA ŚCIEŻKĘ DOSTĘPU DO PLIKU------------------------------------------
	
makeaccesspath proc
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
			
			mov di,offset args				;offset tablicy argumentów
			mov si,offset readfile			;offset tablicy w której zapiszemy plik wejściowy
			mov ax,[version]				;sprawdzenie która wersja programu została odpalona
			cmp ax,0h
			je create
			
			mov ax,di						;jeśli rozszyfrowanie to przesuwamy do przodu by pominąć '-d$'
			add ax,3h
			mov di,ax
			
create:		mov byte ptr ds:[si],'C'		;wpisanie początkowego 'C:\'
			inc si
			mov byte ptr ds:[si],':'
			inc si
			mov byte ptr ds:[si],'\'
			inc si
			
rewrite1:	mov al,byte ptr ds:[di]			;przepisanie pierwszego argumentu
			cmp al,24h
			je next1
			mov byte ptr ds:[si],al
			inc di
			inc si
			jmp rewrite1
			
next1:		mov si,offset writefile			;offset tablicy drugiej ścieżki dostępu
			inc di							;przesunięcie za znak dolara
			
			mov byte ptr ds:[si],'C'		;analogicznie jak poprzednio
			inc si
			mov byte ptr ds:[si],':'
			inc si
			mov byte ptr ds:[si],'\'
			inc si
			
rewrite2:	mov al,byte ptr ds:[di]
			cmp al,24h
			je next2
			mov byte ptr ds:[si],al
			inc di
			inc si
			jmp rewrite2
			
next2:		mov dx,offset readfile			;pomocnicze wypisania // DO WYŁĄCZENIA PODCZAS SPRAWDZANIA ZADANIA
			mov ah,9h
			int 21h
			mov dx,offset writefile
			mov ah,9h
			int 21h
			
			
			
exit5:		pop si
			pop di
			pop dx
			pop cx
			pop bx
			pop ax
			ret	
			
makeaccesspath endp		

;------------------------PROCEDURA POBIERAJĄCA FRAGMENT TEKSTU Z PLIKU------------------------------------------

getchar proc
			push bx
			push cx
			push dx
			push di
			push si
			
			xor bx,bx
			xor cx,cx
			xor dx,dx           
						
			mov bx,[inhand]					;wczytanie uchwytu do pliku							
			;mov cx,ax						;POMYŚLEĆ NAD LEPSZYM WYKORZYSTANIEM CX:DX, tutaj przechowywane jest przesunięcie, adres fizyczny = 10h * segment + offset
			;mov dx,[buforsize]
			;xor cx,cx
			
			;mov ah,42h						;ustawia pozycję w pliku
											;możliwe błędy (01h,06h)
			;mov al,01h						;początek ruchu, 00 od początku pliku, 01 od pozycji obecnej, 02 od końca pliku
			;int 21h							;nowa pozycja w DX:AX
			
			;jnc jumpok
			;call fileerror
			
jumpok:		mov ah,3fh						;funkcja czytania, w bx dalej jest uchwyt
											;możliwe błędy (05h,06h)
			mov dx,offset bufor				;tutaj wczytujemy znaki
			mov cx,bp						;ilość danych do odczytu
											;uchwyt dalej w bx, bo nie był zmieniany
			int 21h							;liczba wczytanych znaków w ax
			
			jnc getok
			call fileerror
			
getok:		cmp ax,cx						;sprawdzamy czy przeczytało tyle ile chcieliśmy czy mniej, jeśli mnie to koniec pliku
    		je closefile

		
endoffile: 	mov bp,ax						;końcówka pliku, zapisujemy liczbę wczytanych znaków
											;jeśli będzie trzeba zmieniać flagę to tutaj, ale wątpię
			;mov dx, offset bufor2			;pomocnicze wypisanie, że znaleźliśmy koniec pliku // DO WYŁĄCZENIA PODCZAS ODDAWANIA ZADANIA 
			;mov ah,9h
			;int 21h

closefile:	;mov dx, offset bufor			;pomocniczne wypisywanie bufora // COŚ TUTAJ SIĘ PIEPRZY Z WPISYWANIEM, ale całość działa poprawnie
			;mov ah,9h
			;int 21h

            pop si
			pop di
			pop dx
			pop cx
			pop bx
			ret	
			
getchar endp

;------------------------PROCEDURA ZAPISUJĄCA FRAGMENT TEKSTU W PLIKU------------------------------------------

putchar proc
			push bx
			push cx
			push dx
			push di
			push si
			
			xor bx,bx
			xor cx,cx
			xor dx,dx           				

			mov bx,[outhand]				;wczytanie uchwytu pliku do zapisu							
			;mov cx,ax						;POMYŚLEĆ NAD LEPSZYM WYKORZYSTANIEM CX:DX, tutaj przechowywane jest przesunięcie, adres fizyczny = 10h * segment + offset
			;mov dx,cx
			;xor cx,cx
			 ;          mov dx,[buforsize]
			;mov ah,42h						;ustawia pozycję w pliku
											;możliwe błędy (01h,06h)
			;mov al,01h						;początek ruchu, 00 od początku pliku, 01 od pozycji obecnej, 02 od końca pliku
			;int 21h							;nowa pozycja w DX:AX
			;
			;jnc jumpok2
			;call fileerror
			
jumpok2:	mov ah,40h						;funkcja zapisująca
											;możliwe błędy (05h,06h)
			mov dx,offset bufor				;tutaj wczytujemy znaki
			mov cx,bp						;ilość danych do odczytu
											;uchwy dalej w bx, bo nie był zmieniany
			int 21h
			
			jnc putok2
			call fileerror
			
putok2:		pop si
			pop di
			pop dx
			pop cx
			pop bx
			ret	
			
putchar endp	

;------------------------PROCEDURA ODPOWIADAJĄCA ZA ZARZĄDZANIE POBIERANIEM DANYCH, ICH MODYFIKACJĄ ORAZ ZAPISEM -----------------------------------

modify proc
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
			
			mov ah,3ch						;tworzy nowy plik bez sprawdzenia czy taki już istnieje, czyli nadpisuje
											;możliwe błędy (03h,04h,05h) 
											;5bh sprawdza i zwraca błąd 80 // SPRAWDZIĆ I PRZEPISAĆ POTEM -> nie trzeba, ma być wersja nadpisująca
											;możliwe błędy (03h,04h,05h,50h)
											;w obu w CX atrybuty, cokolwiek to jest....???
			mov dx, offset writefile
			int 21h
			
			jnc createok					;skok jeśli nie ma błędu
			call fileerror	
			
createok:	mov ah,3dh 						;funkcja otwarcia pliku
											;możliwe błędy (01h,02h,03h,04h,05h,0Ch,56h)
			xor al,al 						;w al ustawia się tryby, 00 odczytu, 01 zapisu, 02 oba
			mov dx, offset readfile			;adres nazwy pliku do otwarcia
			int 21h
			
			jnc openok						;cf=0, brak błędu
			call fileerror

openok:		mov bx,ax
			mov [inhand],bx					;zapisanie uchwytu do pliku wejściowego
			
			mov ah,3dh 						;funkcja otwarcia pliku
											;możliwe błędy (01h,02h,03h,04h,05h,0Ch,56h)
			mov al,01h 						;w al ustawia się tryby, 00 odczytu, 01 zapisu, 02 oba
			mov dx, offset writefile		;adres nazwy pliku do otwarcia
			int 21h
			
			jnc openok2						;cf=0, brak błędu
			call fileerror
						
openok2:	mov bx,ax
			mov [outhand],bx				;zapisanie uchwytu do pliku wyjściowego
			
			;wtedy wystarczyłoby tylko dodawać bufor do aktualnej pozycji
			
			mov ax,[buforsize]						
			mov bp,ax						;przeniesienie rozmiaru bufora do bp, przekazywany między funkcjami getchar i putchar
			xor cx,cx	

			mov ax,[version]				;w es jest przechowywana wersja programu
			cmp ax,0h
			je three						;skok jeśli zero
			jmp four						;skok w przeciwnym wypadku
			
							;wersja szyfrująca programu
			
three:		mov ax,2h						;załadowanie liczby dolarów po których będzie pierwszy znak naszego kodu szyfrującego
			call adress						;wywołanie procedury odnajdującej adres argumentu
			mov es,ax						;schowanie adresu w es
			push es							;odłożenie na stos, żeby potem pętla dobrze działała
			jmp do
			
							;wersja rozszyfrująca programu
			
four:		mov ax,3h
			call adress						;wywołanie procedury odnajdującej adres argumentu
			mov es,ax						;schowanie adresu w es
			push es							;odłożenie na stos, żeby potem pętla dobrze działała
			
do:			mov ax,cx						;przekazanie licznika przesunięcia
			call getchar
			
			pop ax							;pobiera adres aktualnego znaku kodu szyfrującego
			call vigenere
			push ax							;odkłada adres aktualnego znaku kodu szyfrującego
		
			mov ax,cx						;przekazanie licznika przesunięcia
			call putchar
			add cx,[buforsize]				;zwiększenie licznika przesunięcia
			cmp bp,[buforsize]				
			je do							;pętla dopóki bufor ciągle pełny
			pop es							;"wyrównujące" ściągnięcie ze stosu
			jmp close
			
close:		mov bx,[inhand]
			mov ah,3eh						;funkcja zamykająca plik
											;możliwy błąd (06h)
											;uchwyt dalej w bx
			int 21h
			
			jnc closeok
			call fileerror		
			
closeok:	mov bx,[outhand]
			mov ah,3eh						;funkcja zamykająca plik
											;możliwy błąd (06h)
											;uchwyt dalej w bx
			int 21h
			
			jnc closeok2
			call fileerror

closeok2:
			
exit8:		pop si
			pop di
			pop dx
			pop cx
			pop bx
			pop ax
			ret	

modify endp	

;------------------------PROCEDURA SZYFRUJĄCA--------------------------------------------

vigenere proc
			push bx
			push cx
			push dx
			push di
			push si
			
			xor bx,bx
			xor cx,cx
			xor dx,dx
					
			mov di,ax						;przeniesienie przekazanego argumentu czyli adresu aktualnego znaku kodu szyfrującego do di 
			mov si, offset bufor			;załadowanie ofsetu bufora
			mov cx,[buforsize]				;załadowanie do licznika rozmiaru bufora
			
			mov ax,[version]
			cmp ax,00h
			jne unvige
			
vige:       xor ax,ax
			xor dx,dx						;zeruję dx, bo jest potrzebny przy dzieleniu
            mov al, byte ptr ds:[si]		;pobranie znaku z tekstu do zaszyfrowania
			mov bl, byte ptr ds:[di]		;pobranie znaku kodu szyfrującego
			cmp bl,24h						;sprawdzenie czy nie jest dolarem
			je dolar1
back1:										;powrót jeśli był dolar
			add al,bl						;rejestry są 8 bitowe więc modulo wykona się same, utnie przepełnienie
			mov byte ptr ds:[si],al			;zapisanie zmienionego znaku z powrotem do bufora
			inc di							;przesunięcie po tablicy znaków kodujących
			inc si							;przesunięcie w buforze //jeśli był znak biały to tylko tutaj zwiększamy i nic więcej nie robimy
			loop vige						;pętla wykonująca się tyle razy jaki rozmiar ma bufor
			jmp done
			
unvige:     xor ax,ax
			xor dx,dx						;zeruję dx, bo jest potrzebny przy dzieleniu
            mov al, byte ptr ds:[si]		;pobranie znaku z tekstu do zaszyfrowania
			mov bl, byte ptr ds:[di]		;pobranie znaku kodu szyfrującego
			cmp bl,24h						;sprawdzenie czy nie jest dolarem
			je dolar2

back2:										;powrót jeśli był dolar
			sub al,bl						;rejestry są 8 bitowe więc modulo wykona się same, utnie przepełnienie
			mov byte ptr ds:[si],al			;zapisanie zmienionego znaku z powrotem do bufora
			inc di							;przesunięcie po tablicy znaków kodujących
			inc si							;przesunięcie w buforze //jeśli był znak biały to tylko tutaj zwiększamy i nic więcej nie robimy
			loop unvige						;pętla wykonująca się tyle razy jaki rozmiar ma bufor
			
done:		mov ax,di						;przekazanie aktualnej pozycji znaku szyfrującego w ax

exit9:		pop si
			pop di
			pop dx
			pop cx
			pop bx
			ret

dolar1:		mov di,es						;załadowanie do di adresu początkowego klucza szyfrującego
			mov bl, byte ptr ds:[di]		;pobranie znaku szyfrującego
			jmp back1		
			
dolar2:		mov di,es						;załadowanie do di adresu początkowego klucza szyfrującego
			mov bl, byte ptr ds:[di]		;pobranie znaku szyfrującego
			jmp back2				

vigenere endp	

;------------------------PROCEDURA SPRAWDZAJĄCA I WYŚWIETLAJĄCA INFORMACJE O BŁĘDACH------------------------------------------------------------------------------------
	
fileerror proc						
			push bx
			push cx
			push dx
			push di
			push si
			
			xor bx,bx
			xor cx,cx
			xor dx,dx
			            
			push ax     					;odłożenie przekazanego numeru błędu na stos       
			            
			mov dx,offset doserror		
			mov ah,09h 						;funkcja wypisująca ds:dx do dolara
			int 21h
			
			pop ax							;ściągnięcie numeru błędu ze stosu
			
			cmp ax,01h						;sprawdzenie numeru błędu
			je fail01h
			cmp ax,02h
			je fail02h
			cmp ax,03h
			je fail03h
			cmp ax,04h
			je fail04h
			cmp ax,05h
			je fail05h
			cmp ax,06h
			je fail06h
			cmp ax,0ch
			je fail0ch
			cmp ax,56h
			je fail56h
			
fail01h:	mov dx,offset dos01h			;wczytanie offsetu z tekstem błędu		
			jmp theend3
			
fail02h:	mov dx,offset dos02h		
			jmp theend3
			
fail03h:	mov dx,offset dos03h		
			jmp theend3
			
fail04h:	mov dx,offset dos04h		
			jmp theend3
			
fail05h:	mov dx,offset dos05h		
			jmp theend3
			
fail06h:	mov dx,offset dos06h		
			jmp theend3
			
fail0ch:	mov dx,offset dos0ch		
			jmp theend3
			
fail56h:	mov dx,offset dos56h		
			jmp theend3
			
			
exit12:		pop si
			pop di
			pop dx
			pop cx
			pop bx
			ret
			
theend3: 	mov ah,09h 						;funkcja wypisująca ds:dx do dolara
			int 21h			
			jmp endofprog
			
fileerror endp

			
code1 ends


stack1 segment stack

			dw 200 dup (0)
top1		dw ?

stack1 ends


end start 