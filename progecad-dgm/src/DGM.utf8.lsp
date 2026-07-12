;;; ======================================================================
;;; DGM.lsp - Εργαλεία σύνταξης Διαγραμμάτων Γεωμετρικών Μεταβολών (ΔΓΜ)
;;; για ProgeCAD / IntelliCAD (AutoLISP)
;;;
;;; Εντολές:
;;;   DGML     - Δημιουργία όλων των τυποποιημένων layers ΔΓΜ
;;;   DGMK     - Αρίθμηση κορυφών polyline (σημάδια σε layer "σημείο_...")
;;;   DGMP     - Πίνακας συντεταγμένων κορυφών (Σημείο | Χ | Y | Αποστάσεις)
;;;   DGME     - Πίνακας αρχικών και τελικών εμβαδών επηρεαζόμενων γεωτεμαχίων
;;;   DGMT     - Πίνακας για τη διόρθωση των γεωτεμαχίων (ΤΜΗΜΑ|ΕΜΒΑΔΟΝ|ΑΠΟ|ΣΕ)
;;;   DGMA     - Χαρακτηρισμός τμήματος (ΤΜΗΜΑ Α1/Δ1) και μεταφορά σε AREA_A/AREA_D
;;;   DGMKAEK  - Τοποθέτηση κειμένου ΚΑΕΚ με έλεγχο μορφής
;;;   DGMC     - Έλεγχοι ορθότητας πριν την υποβολή (layers, κλειστότητα κ.λπ.)
;;;   DGMHELP  - Λίστα εντολών
;;;
;;; Σημείωση: Το αρχείο διανομής DGM.lsp είναι κωδικοποιημένο σε Windows-1253
;;; (ελληνικά Windows) ώστε τα ελληνικά να εμφανίζονται σωστά στο ProgeCAD.
;;; ======================================================================

;;; ------------------- Καθολικές ρυθμίσεις / layers ---------------------

(setq dgm:*layers*
 '(("PST_KAEK"              16)
   ("PST"                    7)
   ("TOPO_PROP"             30)
   ("TOPO_PROP_NEW"        224)
   ("DGM_PROP_FINAL"         4)
   ("BOUND_IMPL"            42)
   ("BOUND_UNIMPL"          61)
   ("DBOUND_RYM"            80)
   ("DBOUND_AIG"             1)
   ("DBOUND_PRL"             2)
   ("DBOUND_PAIG"          152)
   ("DBOUND_REM"           141)
   ("DBOUND_APAL"           84)
   ("DBOUND_PROP"            7)
   ("ROAD"                   2)
   ("OT"                    80)
   ("BLD"                  163)
   ("VST"                  233)
   ("EAS"                  233)
   ("MINE"                 157)
   ("LINE_XM"                1)
   ("LINE_XM_VST"            1)
   ("VST_FINAL"              4)
   ("EAS_FINAL"              4)
   ("MINE_FINAL"             4)
   ("AREA_A"                 6)
   ("AREA_D"                 4)
   ("OBJ"                    3)
   ("AREA_A-labels"          6)
   ("AREA_D-labels"          4)
   ("AREA_A-hatch"           3)
   ("AREA_D-hatch"          10)
   ("σημείο_TOPO_PROP"       5)
   ("σημείο_PST_KAEK"       16)
   ("σημείο_DGM_PROP_FINAL"  4)
   ("σημείο_AREAS"           3)
   ("pinakas_sintetagmenon"  7)
  ))

(if (null dgm:*h*)   (setq dgm:*h* 0.5))    ; προεπιλεγμένο ύψος κειμένου
(if (null dgm:*num*) (setq dgm:*num* 0))    ; τρέχων μετρητής αρίθμησης κορυφών

;;; ------------------------- Βοηθητικές: σχεδίαση -----------------------

(defun dgm:layer (name color / )
  (if (not (tblsearch "LAYER" name))
    (entmake (list '(0 . "LAYER")
                   '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord")
                   (cons 2 name)
                   '(70 . 0)
                   (cons 62 color)
                   '(6 . "Continuous"))))
  name)

(defun dgm:layer-std (name / a)
  (setq a (assoc name dgm:*layers*))
  (dgm:layer name (if a (cadr a) 7)))

(defun dgm:text (pt h str lay)
  (entmake (list '(0 . "TEXT") (cons 8 lay)
                 (cons 10 (list (car pt) (cadr pt) 0.0))
                 (cons 40 h) (cons 1 str) '(50 . 0.0))))

(defun dgm:textc (pt h str lay)
  (entmake (list '(0 . "TEXT") (cons 8 lay)
                 (cons 10 (list (car pt) (cadr pt) 0.0))
                 (cons 11 (list (car pt) (cadr pt) 0.0))
                 (cons 40 h) (cons 1 str) '(50 . 0.0)
                 '(72 . 1) '(73 . 2))))

(defun dgm:line (p1 p2 lay)
  (entmake (list '(0 . "LINE") (cons 8 lay)
                 (cons 10 (list (car p1) (cadr p1) 0.0))
                 (cons 11 (list (car p2) (cadr p2) 0.0)))))

(defun dgm:rect (x1 y1 x2 y2 lay)
  (dgm:line (list x1 y1) (list x2 y1) lay)
  (dgm:line (list x2 y1) (list x2 y2) lay)
  (dgm:line (list x2 y2) (list x1 y2) lay)
  (dgm:line (list x1 y2) (list x1 y1) lay))

(defun dgm:point (pt lay)
  (entmake (list '(0 . "POINT") (cons 8 lay)
                 (cons 10 (list (car pt) (cadr pt) 0.0)))))

(defun dgm:circle (pt r lay)
  (entmake (list '(0 . "CIRCLE") (cons 8 lay)
                 (cons 10 (list (car pt) (cadr pt) 0.0))
                 (cons 40 r))))

;;; ------------------------ Βοηθητικές: γεωμετρία -----------------------

;; Κορυφές LWPOLYLINE ως λίστα (x y). Αφαιρεί διπλή τελευταία κορυφή.
(defun dgm:lwpts (e / d r g)
  (setq d (entget e) r nil)
  (foreach g d
    (if (= 10 (car g))
      (setq r (cons (list (cadr g) (caddr g)) r))))
  (setq r (reverse r))
  (if (and (> (length r) 1)
           (< (distance (car r) (last r)) 1e-8))
    (setq r (reverse (cdr (reverse r)))))
  r)

(defun dgm:closedp (e)
  (= 1 (logand 1 (cdr (assoc 70 (entget e))))))

;; Έχει τόξα (bulges);
(defun dgm:hasbulge (e / d g r)
  (setq d (entget e) r nil)
  (foreach g d
    (if (and (= 42 (car g)) (/= 0.0 (cdr g)))
      (setq r T)))
  r)

;; Εμβαδόν πολυγώνου (τύπος Gauss)
(defun dgm:area (pts / s n i p1 p2)
  (setq s 0.0 n (length pts) i 0)
  (while (< i n)
    (setq p1 (nth i pts)
          p2 (nth (rem (1+ i) n) pts))
    (setq s (+ s (- (* (car p1) (cadr p2)) (* (car p2) (cadr p1)))))
    (setq i (1+ i)))
  (abs (/ s 2.0)))

;; Απόσταση σημείου από ευθύγραμμο τμήμα a-b
(defun dgm:pseg (p a b / dx dy l2 tp q)
  (setq dx (- (car b) (car a))
        dy (- (cadr b) (cadr a)))
  (setq l2 (+ (* dx dx) (* dy dy)))
  (if (< l2 1e-12)
    (distance p a)
    (progn
      (setq tp (/ (+ (* (- (car p) (car a)) dx)
                     (* (- (cadr p) (cadr a)) dy))
                  l2))
      (if (< tp 0.0) (setq tp 0.0))
      (if (> tp 1.0) (setq tp 1.0))
      (setq q (list (+ (car a) (* tp dx)) (+ (cadr a) (* tp dy))))
      (distance p q))))

;; Σημείο μέσα σε πολύγωνο (ray casting)
(defun dgm:inpoly (p pts / n i j inside xi yi xj yj)
  (setq n (length pts) inside nil j (1- n) i 0)
  (while (< i n)
    (setq xi (car (nth i pts)) yi (cadr (nth i pts))
          xj (car (nth j pts)) yj (cadr (nth j pts)))
    (if (and (/= (> yi (cadr p)) (> yj (cadr p)))
             (< (car p)
                (+ xi (/ (* (- xj xi) (- (cadr p) yi)) (- yj yi)))))
      (setq inside (not inside)))
    (setq j i i (1+ i)))
  inside)

;;; ------------------------- Βοηθητικές: είσοδος ------------------------

(defun dgm:getreal (msg def / v)
  (setq v (getreal (strcat msg " <" (rtos def 2 2) ">: ")))
  (if v v def))

(defun dgm:getint (msg def / v)
  (setq v (getint (strcat msg " <" (itoa def) ">: ")))
  (if v v def))

(defun dgm:getstr (msg def / v)
  (setq v (getstring T
            (strcat msg
                    (if (and def (/= def "")) (strcat " <" def ">") "")
                    ": ")))
  (if (= v "") def v))

(defun dgm:sel-poly (msg / en)
  (setq en (entsel msg))
  (cond
    ((null en) nil)
    ((/= "LWPOLYLINE" (cdr (assoc 0 (entget (car en)))))
     (princ "\n** Το αντικείμενο δεν είναι LWPolyline. **")
     nil)
    (t (car en))))

;;; --------------------- Αναζήτηση αριθμών κορυφών ----------------------
;;; Διαβάζει τα κείμενα αρίθμησης από τα layers "σημείο_*" ώστε οι πίνακες
;;; να παίρνουν τους ίδιους αριθμούς με τα σημάδια του σχεδίου.

(defun dgm:marks-load ( / ss i d ip)
  (setq dgm:*marks* nil)
  (setq ss (ssget "_X" '((0 . "TEXT") (8 . "σημείο_*"))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq d (entget (ssname ss i))
              ip (cdr (assoc 10 d)))
        (setq dgm:*marks*
              (cons (list (car ip) (cadr ip) (cdr (assoc 1 d)))
                    dgm:*marks*))
        (setq i (1+ i)))))
  dgm:*marks*)

(defun dgm:findnum (pt tol / best bd dd m)
  (setq best nil bd tol)
  (foreach m dgm:*marks*
    (setq dd (distance (list (car m) (cadr m)) pt))
    (if (< dd bd) (setq bd dd best (caddr m))))
  best)

;;; --------------------------- Μηχανή πινάκων ---------------------------
;;; ins   : πάνω-αριστερό σημείο
;;; title : τίτλος (string ή nil)
;;; heads : λίστα στηλών, κάθε στήλη = λίστα γραμμών κειμένου
;;; wids  : λίστα πλατών στηλών (μονάδες σχεδίου)
;;; rows  : λίστα γραμμών, κάθε γραμμή = λίστα strings
;;; h     : βασικό ύψος κειμένου, lay: layer

(defun dgm:fit-h (str w h / n)
  (setq n (max 1 (strlen str)))
  (min h (/ (* 0.95 w) (* 0.8 n))))

(defun dgm:table (ins title heads wids rows h lay
                  / tot x0 y0 th hh nhl y x i k w col row s ht yy)
  (setq tot (apply '+ wids)
        x0  (car ins)
        y0  (cadr ins)
        th  (* 2.0 h))
  (setq y y0)
  ;; Τίτλος
  (if (and title (/= title ""))
    (progn
      (dgm:rect x0 (- y (* 2.4 h)) (+ x0 tot) y lay)
      (dgm:textc (list (+ x0 (/ tot 2.0)) (- y (* 1.2 h)))
                 (dgm:fit-h title tot h) title lay)
      (setq y (- y (* 2.4 h)))))
  ;; Επικεφαλίδες
  (setq nhl (apply 'max (mapcar 'length heads)))
  (setq hh (* h (+ 1.2 (* 1.5 nhl))))
  (setq x x0 i 0)
  (foreach col heads
    (setq w (nth i wids))
    (setq k 0)
    (foreach s col
      (setq yy (- y (* h (+ 1.35 (* 1.5 k)))
                  (* 0.75 h (- nhl (length col))))) ; κατακόρυφο κεντράρισμα
      (dgm:textc (list (+ x (/ w 2.0)) yy)
                 (dgm:fit-h s w (* 0.9 h)) s lay)
      (setq k (1+ k)))
    (setq x (+ x w) i (1+ i)))
  ;; Γραμμές δεδομένων
  (setq yy (- y hh))
  (foreach row rows
    (setq x x0 i 0)
    (foreach s row
      (setq w (nth i wids))
      (if (/= s "")
        (dgm:textc (list (+ x (/ w 2.0)) (- yy (/ th 2.0)))
                   (dgm:fit-h s w (* 0.9 h)) s lay))
      (setq x (+ x w) i (1+ i)))
    (setq yy (- yy th)))
  ;; Πλέγμα
  ;; εξωτερικό πλαίσιο επικεφαλίδων + δεδομένων
  (dgm:rect x0 yy (+ x0 tot) y lay)
  ;; κατακόρυφες
  (setq x x0 i 0)
  (while (< i (1- (length wids)))
    (setq x (+ x (nth i wids)))
    (dgm:line (list x y) (list x yy) lay)
    (setq i (1+ i)))
  ;; οριζόντιες
  (dgm:line (list x0 (- y hh)) (list (+ x0 tot) (- y hh)) lay)
  (setq k 1)
  (while (< k (length rows))
    (dgm:line (list x0 (- y hh (* k th)))
              (list (+ x0 tot) (- y hh (* k th))) lay)
    (setq k (1+ k)))
  (princ))

;;; ============================ ΕΝΤΟΛΕΣ =================================

;;; DGML - Δημιουργία τυποποιημένων layers
(defun c:DGML ( / n)
  (setq n 0)
  (foreach l dgm:*layers*
    (if (not (tblsearch "LAYER" (car l)))
      (progn (dgm:layer (car l) (cadr l)) (setq n (1+ n)))))
  (princ (strcat "\nΔημιουργήθηκαν " (itoa n)
                 " layers (σύνολο τυποποιημένων: "
                 (itoa (length dgm:*layers*)) ")."))
  (princ))

;;; DGMK - Αρίθμηση κορυφών polyline
(defun c:DGMK ( / en lay mlay a pts h n pt)
  (setq en (dgm:sel-poly "\nΕπιλέξτε polyline για αρίθμηση κορυφών: "))
  (if en
    (progn
      (setq lay  (cdr (assoc 8 (entget en)))
            mlay (strcat "σημείο_" lay)
            a    (assoc mlay dgm:*layers*))
      (dgm:layer mlay (if a (cadr a) 3))
      (setq h (dgm:getreal "\nΎψος κειμένου αρίθμησης" dgm:*h*))
      (setq dgm:*h* h)
      (setq n (dgm:getint "\nΑριθμός πρώτης κορυφής" (1+ dgm:*num*)))
      (setq pts (dgm:lwpts en))
      (foreach pt pts
        (dgm:point pt mlay)
        (dgm:circle pt (* 0.35 h) mlay)
        (dgm:text (list (+ (car pt) (* 0.6 h)) (+ (cadr pt) (* 0.6 h)))
                  h (itoa n) mlay)
        (setq n (1+ n)))
      (setq dgm:*num* (1- n))
      (princ (strcat "\nΑριθμήθηκαν " (itoa (length pts))
                     " κορυφές στο layer " mlay
                     ". Τελευταίος αριθμός: " (itoa dgm:*num*)))))
  (princ))

;;; DGMP - Πίνακας συντεταγμένων κορυφών
(defun c:DGMP ( / en pts h typ kaek tmima title nums ok n i p q rows row
                  maxr len ng rowsper mrows heads wids ins g idx d)
  (setq en (dgm:sel-poly "\nΕπιλέξτε polyline για πίνακα συντεταγμένων: "))
  (if en
    (progn
      (setq pts (dgm:lwpts en))
      (setq h (dgm:getreal "\nΎψος κειμένου πίνακα" dgm:*h*))
      (setq dgm:*h* h)
      (princ "\nΤύπος πίνακα:")
      (princ "\n  1 = Αρχικό γεωτεμάχιο με ΚΑΕΚ")
      (princ "\n  2 = Τελικό γεωτεμάχιο (ενημέρωση κτηματολογικής βάσης)")
      (princ "\n  3 = Αποκοπτόμενο τμήμα")
      (princ "\n  4 = Διεκδικούμενο τμήμα")
      (princ "\n  5 = Πίνακας κορυφών ιδιοκτησίας")
      (princ "\n  6 = Ελεύθερος τίτλος")
      (setq typ (dgm:getint "\nΕπιλογή" 1))
      (cond
        ((= typ 1)
         (setq kaek (dgm:getstr "\nΚΑΕΚ" ""))
         (setq title (strcat "ΠΙΝΑΚΑΣ ΣΥΝΤΕΤΑΓΜΕΝΩΝ ΚΟΡΥΦΩΝ ΑΡΧΙΚΟΥ ΓΕΩΤΕΜΑΧΙΟΥ ΜΕ ΚΑΕΚ " kaek)))
        ((= typ 2)
         (setq kaek (dgm:getstr "\nΚΑΕΚ" ""))
         (setq title (strcat "ΠΙΝΑΚΑΣ ΣΥΝΤΕΤΑΓΜΕΝΩΝ ΚΟΡΥΦΩΝ ΤΕΛΙΚΟΥ ΓΕΩΤΕΜΑΧΙΟΥ ΜΕ ΚΑΕΚ " kaek
                              " ΓΙΑ ΤΗΝ ΕΝΗΜΕΡΩΣΗ ΤΗΣ ΚΤΗΜΑΤΟΛΟΓΙΚΗΣ ΒΑΣΗΣ")))
        ((= typ 3)
         (setq tmima (dgm:getstr "\nΌνομα τμήματος (π.χ. Α1)" "Α1"))
         (setq kaek (dgm:getstr "\nΑπό ΚΑΕΚ" ""))
         (setq title (strcat "ΠΙΝΑΚΑΣ ΣΥΝΤΕΤΑΓΜΕΝΩΝ ΚΟΡΥΦΩΝ ΑΠΟΚΟΠΤΟΜΕΝΟΥ ΤΜΗΜΑΤΟΣ "
                             tmima " ΑΠΟ ΚΑΕΚ " kaek)))
        ((= typ 4)
         (setq tmima (dgm:getstr "\nΌνομα τμήματος (π.χ. Δ1)" "Δ1"))
         (setq kaek (dgm:getstr "\nΑπό ΚΑΕΚ" ""))
         (setq title (strcat "ΠΙΝΑΚΑΣ ΣΥΝΤΕΤΑΓΜΕΝΩΝ ΚΟΡΥΦΩΝ ΔΙΕΚΔΙΚΟΥΜΕΝΟΥ ΤΜΗΜΑΤΟΣ "
                             tmima " ΑΠΟ ΚΑΕΚ " kaek)))
        ((= typ 5)
         (setq title "ΠΙΝΑΚΑΣ ΚΟΡΥΦΩΝ ΙΔΙΟΚΤΗΣΙΑΣ"))
        (t
         (setq title (dgm:getstr "\nΤίτλος πίνακα" "ΠΙΝΑΚΑΣ ΣΥΝΤΕΤΑΓΜΕΝΩΝ ΚΟΡΥΦΩΝ"))))
      ;; Αριθμοί κορυφών: πρώτα από τα σημάδια "σημείο_*", αλλιώς διαδοχική αρίθμηση
      (dgm:marks-load)
      (setq nums nil ok T)
      (foreach p pts
        (setq n (dgm:findnum p (* 2.5 h)))
        (if (null n) (setq ok nil))
        (setq nums (cons n nums)))
      (setq nums (reverse nums))
      (if (not ok)
        (progn
          (princ "\nΔεν βρέθηκαν σημάδια αρίθμησης για όλες τις κορυφές.")
          (setq n (dgm:getint "\nΑριθμός πρώτης κορυφής" 1))
          (setq nums nil i 0)
          (while (< i (length pts))
            (setq nums (cons (itoa (+ n i)) nums))
            (setq i (1+ i)))
          (setq nums (reverse nums))))
      ;; Γραμμές πίνακα
      (setq rows nil i 0)
      (while (< i (length pts))
        (setq p (nth i pts))
        (setq d (if (> i 0)
                  (strcat (nth (1- i) nums) " - " (nth i nums) ": "
                          (rtos (distance (nth (1- i) pts) p) 2 2))
                  ""))
        (setq rows (cons (list (nth i nums)
                               (rtos (car p) 2 3)
                               (rtos (cadr p) 2 3)
                               d)
                         rows))
        (setq i (1+ i)))
      (setq rows (reverse rows))
      (if (dgm:closedp en)
        (setq rows
              (append rows
                      (list (list "" "" ""
                                  (strcat (last nums) " - " (car nums) ": "
                                          (rtos (distance (last pts) (car pts)) 2 2)))))))
      ;; Σπάσιμο σε πολλαπλές ομάδες στηλών αν οι γραμμές είναι πολλές
      (setq maxr (dgm:getint "\nΜέγιστες γραμμές ανά ομάδα στηλών" 30))
      (setq len (length rows))
      (setq ng (1+ (/ (1- len) maxr)))
      (setq rowsper (1+ (/ (1- len) ng)))
      (setq mrows nil i 0)
      (while (< i rowsper)
        (setq row nil g 0)
        (while (< g ng)
          (setq idx (+ i (* g rowsper)))
          (setq row (append row
                            (if (< idx len)
                              (nth idx rows)
                              (list "" "" "" ""))))
          (setq g (1+ g)))
        (setq mrows (cons row mrows))
        (setq i (1+ i)))
      (setq mrows (reverse mrows))
      (setq heads nil wids nil g 0)
      (while (< g ng)
        (setq heads (append heads
                            (list (list "Σημείο") (list "Χ") (list "Y")
                                  (list "Αποστάσεις"))))
        (setq wids (append wids
                           (list (* 8 h) (* 16 h) (* 16 h) (* 17 h))))
        (setq g (1+ g)))
      (setq ins (getpoint "\nΣημείο εισαγωγής πίνακα (πάνω αριστερή γωνία): "))
      (if ins
        (progn
          (dgm:layer-std "pinakas_sintetagmenon")
          (dgm:table ins title heads wids mrows h "pinakas_sintetagmenon")
          (princ (strcat "\nΟ πίνακας δημιουργήθηκε ("
                         (itoa (length pts)) " κορυφές)."))))))
  (princ))

;;; DGME - Πίνακας αρχικών και τελικών εμβαδών
(defun c:DGME ( / h rows kaek e1 e2 v1 v2 ins heads wids en)
  (setq h (dgm:getreal "\nΎψος κειμένου πίνακα" dgm:*h*))
  (setq dgm:*h* h)
  (setq rows nil)
  (setq kaek (getstring T "\nΚΑΕΚ γεωτεμαχίου (Enter για τέλος): "))
  (while (/= kaek "")
    ;; αρχικό εμβαδόν
    (setq en (entsel "\nΑρχική γεωμετρία - επιλέξτε polyline (Enter για πληκτρολόγηση): "))
    (if (and en (= "LWPOLYLINE" (cdr (assoc 0 (entget (car en))))))
      (setq v1 (rtos (dgm:area (dgm:lwpts (car en))) 2 2))
      (setq v1 (dgm:getstr "\nΑρχικό εμβαδόν (τ.μ.)" "")))
    ;; τελικό εμβαδόν
    (setq en (entsel "\nΤελική γεωμετρία - επιλέξτε polyline (Enter για πληκτρολόγηση): "))
    (if (and en (= "LWPOLYLINE" (cdr (assoc 0 (entget (car en))))))
      (setq v2 (rtos (dgm:area (dgm:lwpts (car en))) 2 2))
      (setq v2 (dgm:getstr "\nΤελικό εμβαδόν (τ.μ.)" "-")))
    (setq rows (cons (list kaek v1 v2) rows))
    (setq kaek (getstring T "\nΚΑΕΚ γεωτεμαχίου (Enter για τέλος): ")))
  (setq rows (reverse rows))
  (if rows
    (progn
      (setq heads (list (list "ΕΜΠΛΕΚΟΜΕΝΑ ΚΑΕΚ")
                        (list "ΑΡΧΙΚΟ ΕΜΒΑΔΟΝ" "(τ.μ.)")
                        (list "ΤΕΛΙΚΟ ΕΜΒΑΔΟΝ ΜΕΤΑ ΤΗ" "ΔΙΟΡΘΩΣΗ ΤΩΝ ΟΡΙΩΝ ΒΑΣΕΙ"
                              "ΑΙΤΗΣΗΣ / ΔΙΚΟΓΡΑΦΟΥ (τ.μ.)")))
      (setq wids (list (* 22 h) (* 16 h) (* 26 h)))
      (setq ins (getpoint "\nΣημείο εισαγωγής πίνακα (πάνω αριστερή γωνία): "))
      (if ins
        (progn
          (dgm:layer-std "pinakas_sintetagmenon")
          (dgm:table ins "ΠΙΝΑΚΑΣ ΑΡΧΙΚΩΝ ΚΑΙ ΤΕΛΙΚΩΝ ΕΜΒΑΔΩΝ ΕΠΗΡΕΑΖΟΜΕΝΩΝ ΓΕΩΤΕΜΑΧΙΩΝ"
                     heads wids rows h "pinakas_sintetagmenon")
          (princ (strcat "\nΟ πίνακας δημιουργήθηκε (" (itoa (length rows))
                         " γεωτεμάχια)."))))))
  (princ))

;;; DGMT - Πίνακας για τη διόρθωση των γεωτεμαχίων
(defun c:DGMT ( / h rows nm en v from to ins heads wids)
  (setq h (dgm:getreal "\nΎψος κειμένου πίνακα" dgm:*h*))
  (setq dgm:*h* h)
  (setq rows nil)
  (setq nm (getstring T "\nΌνομα τμήματος π.χ. Α1 (Enter για τέλος): "))
  (while (/= nm "")
    (setq en (entsel "\nΕπιλέξτε polyline τμήματος (Enter για πληκτρολόγηση εμβαδού): "))
    (if (and en (= "LWPOLYLINE" (cdr (assoc 0 (entget (car en))))))
      (setq v (rtos (dgm:area (dgm:lwpts (car en))) 2 2))
      (setq v (dgm:getstr "\nΕμβαδόν τμήματος (τ.μ.)" "")))
    (setq from (dgm:getstr "\nΑΠΟ ΚΑΕΚ" ""))
    (setq to   (dgm:getstr "\nΣΕ ΚΑΕΚ" ""))
    (setq rows (cons (list nm v from to) rows))
    (setq nm (getstring T "\nΌνομα τμήματος (Enter για τέλος): ")))
  (setq rows (reverse rows))
  (if rows
    (progn
      (setq heads (list (list "ΤΜΗΜΑ")
                        (list "ΕΜΒΑΔΟΝ" "(τ.μ.)")
                        (list "ΑΠΟ ΚΑΕΚ")
                        (list "ΣΕ ΚΑΕΚ")))
      (setq wids (list (* 10 h) (* 14 h) (* 18 h) (* 18 h)))
      (setq ins (getpoint "\nΣημείο εισαγωγής πίνακα (πάνω αριστερή γωνία): "))
      (if ins
        (progn
          (dgm:layer-std "pinakas_sintetagmenon")
          (dgm:table ins "ΠΙΝΑΚΑΣ ΓΙΑ ΤΗ ΔΙΟΡΘΩΣΗ ΤΩΝ ΓΕΩΤΕΜΑΧΙΩΝ"
                     heads wids rows h "pinakas_sintetagmenon")
          (princ "\nΟ πίνακας δημιουργήθηκε.")))))
  (princ))

;;; DGMA - Χαρακτηρισμός τμήματος (αποκοπτόμενο/διεκδικούμενο)
(defun c:DGMA ( / en typ lay lablay nm h pt d)
  (setq en (dgm:sel-poly "\nΕπιλέξτε polyline τμήματος: "))
  (if en
    (progn
      (princ "\nΤύπος τμήματος:  1 = Αποκοπτόμενο (Α)   2 = Διεκδικούμενο (Δ)")
      (setq typ (dgm:getint "\nΕπιλογή" 1))
      (if (= typ 2)
        (setq lay "AREA_D" lablay "AREA_D-labels")
        (setq lay "AREA_A" lablay "AREA_A-labels"))
      (dgm:layer-std lay)
      (dgm:layer-std lablay)
      (setq nm (dgm:getstr "\nΌνομα τμήματος" (if (= typ 2) "Δ1" "Α1")))
      ;; μεταφορά polyline στο σωστό layer
      (setq d (entget en))
      (entmod (subst (cons 8 lay) (assoc 8 d) d))
      ;; ετικέτα
      (setq h (dgm:getreal "\nΎψος κειμένου ετικέτας" dgm:*h*))
      (setq dgm:*h* h)
      (setq pt (getpoint "\nΘέση ετικέτας: "))
      (if pt (dgm:textc pt h (strcat "ΤΜΗΜΑ " nm) lablay))
      (princ (strcat "\nΤο τμήμα " nm " μεταφέρθηκε στο layer " lay
                     " (Εμβαδόν: "
                     (rtos (dgm:area (dgm:lwpts en)) 2 2) " τ.μ.)"))))
  (princ))

;;; DGMKAEK - Τοποθέτηση κειμένου ΚΑΕΚ
(defun c:DGMKAEK ( / s typ lay h pt)
  (setq s (getstring T "\nΚΑΕΚ: "))
  (if (/= 12 (strlen s))
    (princ (strcat "\n** Προσοχή: ο ΚΑΕΚ έχει " (itoa (strlen s))
                   " χαρακτήρες αντί για 12. **")))
  (princ "\nLayer:  1 = PST_KAEK (επηρεαζόμενο)   2 = PST (όμορο)")
  (setq typ (dgm:getint "\nΕπιλογή" 1))
  (setq lay (if (= typ 2) "PST" "PST_KAEK"))
  (dgm:layer-std lay)
  (setq h (dgm:getreal "\nΎψος κειμένου" dgm:*h*))
  (setq dgm:*h* h)
  (setq pt (getpoint "\nΘέση κειμένου ΚΑΕΚ: "))
  (if pt
    (progn
      (dgm:text pt h s lay)
      (princ (strcat "\nΤοποθετήθηκε ΚΑΕΚ " s " στο layer " lay))))
  (princ))

;;; DGMC - Έλεγχοι πριν την υποβολή
(defun c:DGMC ( / tol ss i e d et lay nerr nwarn polys texts pts p
                  pstpolys pstsegs finpolys segs a b j k dd mind found
                  sum1 sum2 cnt inpts)
  ;; καθάρισμα παλιών σημαδιών ελέγχου
  (setq ss (ssget "_X" '((8 . "DGM_CHECK"))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (entdel (ssname ss i))
        (setq i (1+ i)))))
  (dgm:layer "DGM_CHECK" 1)
  (setq tol (dgm:getreal "\nΑνοχή ταύτισης κορυφών (m)" 0.001))
  (setq nerr 0 nwarn 0)
  (princ "\n==================== ΕΛΕΓΧΟΣ ΔΓΜ ====================")
  ;; 1. Υποχρεωτικά layers
  (foreach lay '("PST_KAEK" "DGM_PROP_FINAL")
    (if (not (tblsearch "LAYER" lay))
      (progn
        (princ (strcat "\n[ΣΦΑΛΜΑ] Δεν υπάρχει το layer " lay))
        (setq nerr (1+ nerr)))))
  ;; 2. Έλεγχος οντοτήτων στα βασικά layers
  (foreach lay '("PST_KAEK" "DGM_PROP_FINAL" "TOPO_PROP" "TOPO_PROP_NEW"
                 "PST" "AREA_A" "AREA_D")
    (setq ss (ssget "_X" (list (cons 8 lay))))
    (if ss
      (progn
        (setq i 0 polys 0 texts 0)
        (while (< i (sslength ss))
          (setq e (ssname ss i)
                d (entget e)
                et (cdr (assoc 0 d)))
          (cond
            ((= et "LWPOLYLINE")
             (setq polys (1+ polys))
             (setq pts (dgm:lwpts e))
             (if (not (dgm:closedp e))
               (progn
                 (princ (strcat "\n[ΣΦΑΛΜΑ] Ανοιχτή polyline στο layer " lay
                                " κοντά στο σημείο ("
                                (rtos (caar pts) 2 2) ", "
                                (rtos (cadar pts) 2 2) ")"))
                 (dgm:circle (car pts) 2.0 "DGM_CHECK")
                 (setq nerr (1+ nerr))))
             (if (dgm:hasbulge e)
               (progn
                 (princ (strcat "\n[ΠΡΟΣΟΧΗ] Polyline με τόξα (bulges) στο layer " lay))
                 (setq nwarn (1+ nwarn))))
             ;; μηδενικά τμήματα / διπλές κορυφές
             (setq j 0)
             (while (< j (1- (length pts)))
               (if (< (distance (nth j pts) (nth (1+ j) pts)) 1e-6)
                 (progn
                   (princ (strcat "\n[ΠΡΟΣΟΧΗ] Διπλή κορυφή στο layer " lay
                                  " στο σημείο ("
                                  (rtos (car (nth j pts)) 2 2) ", "
                                  (rtos (cadr (nth j pts)) 2 2) ")"))
                   (dgm:circle (nth j pts) 2.0 "DGM_CHECK")
                   (setq nwarn (1+ nwarn))))
               (setq j (1+ j))))
            ((= et "TEXT") (setq texts (1+ texts)))
            ((or (= et "LINE") (= et "ARC") (= et "SPLINE")
                 (= et "POLYLINE") (= et "ELLIPSE"))
             (princ (strcat "\n[ΣΦΑΛΜΑ] Οντότητα " et " στο layer " lay
                            " - τα όρια πρέπει να είναι LWPolylines."))
             (setq nerr (1+ nerr))))
          (setq i (1+ i)))
        (princ (strcat "\n[INFO] Layer " lay ": " (itoa polys)
                       " polylines, " (itoa texts) " κείμενα.")))))
  ;; 3. Αντιστοιχία πολυγώνων/ΚΑΕΚ στο PST_KAEK
  (setq pstpolys nil texts nil)
  (setq ss (ssget "_X" '((8 . "PST_KAEK"))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq e (ssname ss i)
              d (entget e)
              et (cdr (assoc 0 d)))
        (cond
          ((= et "LWPOLYLINE")
           (if (dgm:closedp e)
             (setq pstpolys (cons (dgm:lwpts e) pstpolys))))
          ((= et "TEXT")
           (setq texts (cons (cdr (assoc 10 d)) texts))))
        (setq i (1+ i)))))
  (foreach pts pstpolys
    (setq cnt 0)
    (foreach p texts
      (if (dgm:inpoly p pts) (setq cnt (1+ cnt))))
    (if (= cnt 0)
      (progn
        (princ "\n[ΣΦΑΛΜΑ] Πολύγωνο PST_KAEK χωρίς κείμενο ΚΑΕΚ στο εσωτερικό του.")
        (dgm:circle (car pts) 2.0 "DGM_CHECK")
        (setq nerr (1+ nerr)))))
  ;; 4. Κορυφές DGM_PROP_FINAL πάνω στα όρια PST_KAEK
  (setq pstsegs nil)
  (foreach pts pstpolys
    (setq j 0 k (length pts))
    (while (< j k)
      (setq pstsegs (cons (list (nth j pts) (nth (rem (1+ j) k) pts)) pstsegs))
      (setq j (1+ j))))
  (setq finpolys nil)
  (setq ss (ssget "_X" '((0 . "LWPOLYLINE") (8 . "DGM_PROP_FINAL"))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq finpolys (cons (dgm:lwpts (ssname ss i)) finpolys))
        (setq i (1+ i)))))
  (if (and pstsegs finpolys)
    (progn
      (setq cnt 0 inpts 0)
      (foreach pts finpolys
        (foreach p pts
          (setq mind 1e99)
          (foreach segs pstsegs
            (setq dd (dgm:pseg p (car segs) (cadr segs)))
            (if (< dd mind) (setq mind dd)))
          (cond
            ((<= mind tol) (setq cnt (1+ cnt)))
            ((<= mind 0.10)
             (princ (strcat "\n[ΠΡΟΣΟΧΗ] Κορυφή DGM_PROP_FINAL σε απόσταση "
                            (rtos mind 2 3) " m από όριο PST_KAEK ("
                            (rtos (car p) 2 2) ", " (rtos (cadr p) 2 2)
                            ") - πιθανή αστοχία ταύτισης."))
             (dgm:circle p 2.0 "DGM_CHECK")
             (setq nwarn (1+ nwarn)))
            (t (setq inpts (1+ inpts))))))
      (princ (strcat "\n[INFO] DGM_PROP_FINAL: " (itoa cnt)
                     " κορυφές πάνω στα όρια PST_KAEK, " (itoa inpts)
                     " κορυφές εκτός ορίων (νέα/εσωτερικά όρια)."))))
  ;; 5. Σύγκριση εμβαδών
  (setq sum1 0.0)
  (foreach pts pstpolys (setq sum1 (+ sum1 (dgm:area pts))))
  (setq sum2 0.0)
  (foreach pts finpolys (setq sum2 (+ sum2 (dgm:area pts))))
  (if (and pstpolys finpolys)
    (progn
      (princ (strcat "\n[INFO] Άθροισμα εμβαδών PST_KAEK: "
                     (rtos sum1 2 2) " τ.μ."))
      (princ (strcat "\n[INFO] Άθροισμα εμβαδών DGM_PROP_FINAL: "
                     (rtos sum2 2 2) " τ.μ."))
      (princ (strcat "\n[INFO] Διαφορά: " (rtos (- sum2 sum1) 2 2) " τ.μ."))))
  ;; Σύνοψη
  (princ (strcat "\n======================================================"
                 "\nΣΦΑΛΜΑΤΑ: " (itoa nerr)
                 "   ΠΡΟΕΙΔΟΠΟΙΗΣΕΙΣ: " (itoa nwarn)))
  (if (or (> nerr 0) (> nwarn 0))
    (princ "\nΤα προβληματικά σημεία επισημάνθηκαν με κύκλους στο layer DGM_CHECK."))
  (textscr)
  (princ))

;;; DGMHELP - Βοήθεια
(defun c:DGMHELP ()
  (princ "\n--------------- Εντολές ΔΓΜ ---------------")
  (princ "\nDGML     Δημιουργία τυποποιημένων layers")
  (princ "\nDGMK     Αρίθμηση κορυφών polyline")
  (princ "\nDGMP     Πίνακας συντεταγμένων κορυφών")
  (princ "\nDGME     Πίνακας αρχικών/τελικών εμβαδών")
  (princ "\nDGMT     Πίνακας για τη διόρθωση των γεωτεμαχίων")
  (princ "\nDGMA     Χαρακτηρισμός τμήματος (Α/Δ) + ετικέτα")
  (princ "\nDGMKAEK  Τοποθέτηση κειμένου ΚΑΕΚ")
  (princ "\nDGMC     Έλεγχοι ορθότητας πριν την υποβολή")
  (princ "\n--------------------------------------------")
  (princ))

(princ "\nDGM.lsp: Φορτώθηκαν τα εργαλεία ΔΓΜ. Πληκτρολογήστε DGMHELP για λίστα εντολών.")
(princ)
