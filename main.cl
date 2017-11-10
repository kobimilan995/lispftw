;;;;;; unos dimenzija

(defun enter-dimension()
  (format t "Unesite zeljenu dimenziju mape  ")
  (setq dimenzija (read))
  (setq current-state (draw-map dimenzija dimenzija))
  (format t "Unesite prvog igraca! (x ili o)  ")
  (setq current-player (read))
  (stampaj current-state dimenzija)
)


;;;;;;;;; crtanje oks igraca
(defun draw-ox(dimenzija)
                 (if(zerop dimenzija) '()
                   (append '(O) (draw-ox(- dimenzija 1)))
                   ) 
  )
;;;;;;;;; crtanje iks igraca
(defun draw-x(dimenzija)
                 (if(zerop dimenzija) '()
                   (append '(X) (draw-x(- dimenzija 1)))
                   ) 
  )

;;;;;;;;; crtanje praznog polja
(defun draw-empty(dimenzija)
                 (if(zerop dimenzija) '()
                   (append '(_) (draw-empty(- dimenzija 1)))
                   ) 
  )

;stampaj-brojke
(defun stampaj-brojke(n first)
  (cond
   ((equalp (+ n 1) first) (format t " "))
   ((zerop first) (prog1(format t "- ")(stampaj-brojke n (+ 1 first))))
   (t (prog2(format t "~a " first)(stampaj-brojke n (+ 1 first))) )
   )  
  )

;;;;;;; stampanje jedne liste matrice
(defun stampaj-listu-matrice(first lista)
  (format t "~a~%" (car lista) (format t "~a" (code-char(+ first 65))) )
  (if (< first dimenzija) (stampaj-listu-matrice (+ 1 first) (cdr lista)))
  )



;;;;;;;;; generisanje matrice
(defun draw-map(n dimenzija)
  (cond
   ((zerop n) '() )
   ((> n (- dimenzija 2)) (cons(draw-ox dimenzija)(draw-map(- n 1) dimenzija)))
   ((< n 3) (cons(draw-x dimenzija)(draw-map(- n 1) dimenzija)))
   (t (cons(draw-empty dimenzija)(draw-map(- n 1) dimenzija)))
   )
  )


;;;;;;;;;;;;;;;;STAMPAJ
(defun stampaj(state n)
  (if nil state '())
  (stampaj-brojke dimenzija 0) (format t "~%")
  (stampaj-listu-matrice 0 state)

  )


;;;;;;;;;;;;;;;;POTEZ
(defun vrati_element(x y pocetna1 pocetna2 char)
  ()
)

;;;;;;;;;;POTEZ ZA IGRACA OKS
(defun potez-oks(x1 y1 x2 y2 map)
  (setf (nth y1 (nth x1 map)) '_ )
  (setf (nth y2 (nth x2 map)) 'o )
  (setf current-state map)
  (stampaj current-state dimenzija)
  )

;;;;;;;;;;POTEZ ZA IGRACA IKS
(defun potez-iks(x1 y1 x2 y2 map)
  (setf (nth y1 (nth x1 map)) '_ )
  (setf (nth y2 (nth x2 map)) 'X )
  (setf current-state map)
  (stampaj current-state dimenzija)
  )

;;;;;;;;;;POTEZ u zavisnosti od trenutnog igraca

(defun potez(x1 y1 x2 y2 map)
  (cond
   ((equalp current-player 'X) (potez-iks x1 y1 x2 y2 map))
   (t (potez-oks x1 y1 x2 y2 map))
   )
  (cond
   ((equalp current-player 'X) (setf current-player 'o))
   (t (setf current-player 'x))
   )
  )

;;;;;;;;;;;validiraj potez

(defun validiraj-potez (x1 y1 x2 y2 map)
 (cond
  ((not(equalp (nth y1 (nth x1 map)) current-player)) (format t "nije tvoja figura"))
  ((not(or (equalp x1 x2) (equalp y1 y2))) (format t "ne moze da ides dijagonalno"))
  ((not(equalp (nth y2 (nth x2 map)) '_)) (format t "zauzeto polje"))
  (t (potez x1 y1 x2 y2 map))
 ) 
  )

(defun prebroji-na-dole(x2 y2 map)
  (cond
   ((> x2 (- dimenzija 3)) '0)
   ((< x2 3) '0)
   (t (cond
       ((equalp (nth y2(nth x2 map)) 'x) (+ 1 (prebroji-na-dole(+ 1 x2) y2 map)))
       (t '0)
       ))
   )
  )

(defun prebroji-na-gore(x2 y2 map)
  (cond
   ((> x2 (- dimenzija 3)) '0)
   ((< x2 3) '0)
   (t (cond
       ((equalp (nth y2(nth x2 map)) 'x) (+ 1 (prebroji-na-gore(- x2 1) y2 map)))
       (t '0)
       ))
   )
  )

(defun prebroji-za-pobedu-vertikalno (x2 y2 map)
  (cond
   ((> x2 (- dimenzija 3)) '0)
   ((< x2 3) '0)
   (t (+ (prebroji-na-dole x2 y2 map) (prebroji-na-gore (- x2 1) y2 map)))
 )
)
