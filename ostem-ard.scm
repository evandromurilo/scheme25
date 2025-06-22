;; esse programa transforma uma palavra em uma frase com nomes de personagens ou locais dos livros de ostem ard
;; inspirado num exercício do simply scheme


(define (letter-to-magic l)
  (cond ((equal? l `a) `a-sua) ;; antigo nome do castelo de hayholt
	((equal? l `b) `binabik) ;; troll das cancoes
	((equal? l `c) `camaris) ;; grande cavaleiro
	((equal? l `d) `dragonbone-chair) ;; trono de osso de dragao
	((equal? l `e) `elvritshalla) ;; reino ao norte de ostem ard
	;; f
	((equal? l `g) `green-angel-tower) ;; remaining tower from a`sua
	((equal? l `h) `hayholt) ;; castelo do reino de ostem ard
	((equal? l `i) `isgrimnur) ;; duque de elvritshalla
	((equal? l `j) `josua) ;; principe herdeiro filho de camaris
	;; k
	;; l
	((equal? l `m) `miriamele) ;; rainha de ostem ard
	((equal? l `n) `nabban) ;; regiao ao sul de ostem ard
	((equal? l `o) `ostem-ard) ;; nome do continente
	((equal? l `p) `porto) ;; cavaleiro que lutou na guerra contra os norns
	((equal? l `q) `qantaca) ;; um lobo muito honrado
	;; r
	((equal? l `s) `seoman) ;; rei simon
	((equal? l `t) `tiamak) ;; estudioso do wran
	((equal? l `u) `uttuku) ;; rainha dos norns
	((equal? l `v) (`viyeki)) ;; alto mestre dos construtores denakkiga
	((equal? l `w) `wran) ;; regi'ao pantanosa ao sul de ostem ard
	;; x
	((equal? l `y) `yicanuq)) ;; lingua dos trolls
	;; z
	(else "")))

(define (magic wr)
  (every letter-to-magic wr))

	
