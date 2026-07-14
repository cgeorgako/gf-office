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

(defun dgm:table (ins title heads wids rows h lay footer
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
  ;; προαιρετική τελική γραμμή εμβαδού (ενιαίο κελί σε όλο το πλάτος)
  (if footer
    (progn
      (dgm:rect x0 (- yy th) (+ x0 tot) yy lay)
      (dgm:textc (list (+ x0 (/ tot 2.0)) (- yy (/ th 2.0)))
                 (dgm:fit-h footer tot (* 0.9 h)) footer lay)))
  (princ))

;;; ============================ ΕΝΤΟΛΕΣ =================================

;;; DGML - Δημιουργία τυποποιημένων layers ανά τύπο διαγράμματος
(setq dgm:*lay-topo*
 '("BOUND_IMPL" "BOUND_UNIMPL" "PST_KAEK" "TOPO_PROP"
   "ROAD" "OT" "BLD" "VST" "EAS" "MINE" "OBJ"
   "DBOUND_RYM" "DBOUND_AIG" "DBOUND_PRL" "DBOUND_PAIG"
   "DBOUND_REM" "DBOUND_APAL" "DBOUND_PROP"
   "pinakas_sintetagmenon"))

(setq dgm:*lay-dgm*
 (append dgm:*lay-topo*
  '("TOPO_PROP_NEW" "DGM_PROP_FINAL" "LINE_XM" "LINE_XM_VST"
    "AREA_A" "AREA_D" "AREA_A-labels" "AREA_D-labels"
    "AREA_A-hatch" "AREA_D-hatch"
    "VST_FINAL" "EAS_FINAL" "MINE_FINAL")))

(setq dgm:*lay-praksi*
 (append dgm:*lay-topo*
  '("TOPO_PROP_NEW" "DGM_PROP_FINAL" "LINE_XM" "LINE_XM_VST"
    "VST_FINAL" "EAS_FINAL" "MINE_FINAL")))

(defun dgm:asktype ( / typ)
  (princ "\nΤύπος διαγράμματος:")
  (princ "\n  1 = Τοπογραφικό Διάγραμμα")
  (princ "\n  2 = ΔΓΜ (Διόρθωσης)")
  (princ "\n  3 = Διάγραμμα Πράξης")
  (dgm:getint "\nΕπιλογή" (if dgm:*typ* dgm:*typ* 2)))

(defun c:DGML ( / typ lst n)
  (setq typ (dgm:asktype))
  (setq dgm:*typ* typ)
  (setq lst (cond ((= typ 1) dgm:*lay-topo*)
                  ((= typ 3) dgm:*lay-praksi*)
                  (t dgm:*lay-dgm*)))
  (setq n 0)
  (foreach l lst
    (if (not (tblsearch "LAYER" l))
      (progn (dgm:layer-std l) (setq n (1+ n)))))
  (princ (strcat "\nΔημιουργήθηκαν " (itoa n) " νέα layers ("
                 (itoa (length lst)) " τυποποιημένα για τον τύπο αυτόν)."))
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
          (dgm:table ins title heads wids mrows h "pinakas_sintetagmenon"
                     (if (dgm:closedp en)
                       (strcat "Ε (" (car nums) ",...," (last nums) ", "
                               (car nums) "): "
                               (rtos (dgm:area pts) 2 2) " τ.μ.")
                       nil))
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
                     heads wids rows h "pinakas_sintetagmenon" nil)
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
                     heads wids rows h "pinakas_sintetagmenon" nil)
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

;;; Κανόνες οντοτήτων ανά layer: (όνομα δέχεται-polyline μόνο-κλειστές δέχεται-κείμενο)
(setq dgm:*layrules*
 '(("PST_KAEK"       T   T   T)
   ("DGM_PROP_FINAL" T   T   nil)
   ("TOPO_PROP"      T   T   nil)
   ("TOPO_PROP_NEW"  T   T   nil)
   ("AREA_A"         T   T   nil)
   ("AREA_D"         T   T   nil)
   ("BLD"            T   T   T)
   ("VST"            T   T   T)
   ("EAS"            T   T   T)
   ("MINE"           T   T   nil)
   ("VST_FINAL"      T   T   nil)
   ("EAS_FINAL"      T   T   nil)
   ("MINE_FINAL"     T   T   nil)
   ("LINE_XM"        T   nil nil)
   ("LINE_XM_VST"    T   nil nil)
   ("BOUND_IMPL"     T   nil nil)
   ("BOUND_UNIMPL"   T   nil nil)
   ("DBOUND_PROP"    T   T   T)
   ("ROAD"           nil nil T)
   ("OT"             nil nil T)
   ("AREA_A-labels"  nil nil T)
   ("AREA_D-labels"  nil nil T)))

;; Είναι ο χαρακτήρας ψηφίο;
(defun dgm:digitp (c) (and (>= (ascii c) 48) (<= (ascii c) 57)))

(defun dgm:alldigits (s from to / i ok)
  (setq ok T i from)
  (while (<= i to)
    (if (not (dgm:digitp (substr s i 1))) (setq ok nil))
    (setq i (1+ i)))
  ok)

;; Μορφή κειμένου στο PST_KAEK: 'KAEK 'NEO 'TRYPA ή nil (λάθος μορφή)
(defun dgm:kaekfmt (s)
  (cond
    ((and (>= (strlen s) 3) (= (substr s 1 3) "ΝΕΟ")) 'NEO)
    ((and (>= (strlen s) 5) (= (substr s 1 5) "ΤΡΥΠΑ")) 'TRYPA)
    ((/= 12 (strlen s)) nil)
    ((dgm:alldigits s 1 12) 'KAEK)
    ((and (dgm:alldigits s 1 5)
          (= (substr s 6 2) "ΕΚ")
          (dgm:alldigits s 8 12))
     'KAEK)
    (t nil)))

;;; DGMC - Έλεγχος ορθότητας σχεδίου πριν την υποβολή
(defun c:DGMC ( / tol typ req rule ktexts tx kc nc tc island ip p2 pair labs
                  ss i e d et lay nerr nwarn polys texts pts p
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
  (setq typ (dgm:asktype))
  (setq dgm:*typ* typ)
  (setq tol (dgm:getreal "\nΑνοχή ταύτισης κορυφών (m)" 0.001))
  (setq nerr 0 nwarn 0)
  (princ "\n================= ΕΛΕΓΧΟΣ ΟΡΘΟΤΗΤΑΣ =================")
  ;; 1. Υποχρεωτικά layers με οντότητες, ανά τύπο διαγράμματος
  (setq req (cond
              ((= typ 1) '("BOUND_IMPL" "BOUND_UNIMPL" "PST_KAEK" "TOPO_PROP"))
              ((= typ 3) '("BOUND_IMPL" "BOUND_UNIMPL" "PST_KAEK"
                           "TOPO_PROP" "TOPO_PROP_NEW"))
              (t '("BOUND_IMPL" "BOUND_UNIMPL" "PST_KAEK"
                   "TOPO_PROP" "TOPO_PROP_NEW" "DGM_PROP_FINAL"))))
  (foreach lay req
    (if (null (ssget "_X" (list (cons 8 lay))))
      (progn
        (princ (strcat "\n[ΣΦΑΛΜΑ] Δεν υπάρχει οντότητα στο layer " lay))
        (setq nerr (1+ nerr)))))
  (if (= typ 2)
    (if (and (null (ssget "_X" '((8 . "AREA_A"))))
             (null (ssget "_X" '((8 . "AREA_D")))))
      (progn
        (princ "\n[ΣΦΑΛΜΑ] Απαιτείται τουλάχιστον ένα από τα layers AREA_A / AREA_D με οντότητες.")
        (setq nerr (1+ nerr)))))
  (if (= typ 3)
    (if (not (or (ssget "_X" '((8 . "LINE_XM")))
                 (ssget "_X" '((8 . "LINE_XM_VST")))
                 (ssget "_X" '((8 . "VST_FINAL")))
                 (ssget "_X" '((8 . "EAS_FINAL")))
                 (ssget "_X" '((8 . "MINE_FINAL")))
                 (ssget "_X" '((8 . "DGM_PROP_FINAL")))))
      (progn
        (princ "\n[ΣΦΑΛΜΑ] Απαιτείται τουλάχιστον ένα από τα layers LINE_XM / LINE_XM_VST / VST_FINAL / EAS_FINAL / MINE_FINAL / DGM_PROP_FINAL.")
        (setq nerr (1+ nerr)))))
  ;; 2. Έλεγχος οντοτήτων ανά layer βάσει επιτρεπόμενων τύπων
  (foreach rule dgm:*layrules*
    (setq lay (car rule))
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
             (if (not (cadr rule))
               (progn
                 (princ (strcat "\n[ΣΦΑΛΜΑ] Το layer " lay
                                " δέχεται μόνο κείμενα - βρέθηκε polyline."))
                 (dgm:mkpoly pts "DGM_CHECK" (dgm:closedp e))
                 (setq nerr (1+ nerr))))
             (if (and (caddr rule) (not (dgm:closedp e)))
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
            ((or (= et "TEXT") (= et "MTEXT"))
             (setq texts (1+ texts))
             (if (not (cadddr rule))
               (progn
                 (princ (strcat "\n[ΠΡΟΣΟΧΗ] Κείμενο στο layer " lay
                                " - το layer δεν προβλέπει κείμενα."))
                 (setq nwarn (1+ nwarn)))))
            ((or (= et "LINE") (= et "ARC") (= et "SPLINE")
                 (= et "POLYLINE") (= et "ELLIPSE") (= et "CIRCLE"))
             (princ (strcat "\n[ΣΦΑΛΜΑ] Οντότητα " et " στο layer " lay
                            " - τα όρια πρέπει να είναι LWPolylines."))
             (setq nerr (1+ nerr))))
          (setq i (1+ i)))
        (princ (strcat "\n[INFO] Layer " lay ": " (itoa polys)
                       " polylines, " (itoa texts) " κείμενα.")))))
  ;; 3. Κείμενα ΚΑΕΚ: μορφή και αντιστοίχιση με πολύγωνα
  (setq pstpolys nil ktexts nil)
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
          ((or (= et "TEXT") (= et "MTEXT"))
           (setq ktexts (cons (cons (list (cadr (assoc 10 d))
                                          (caddr (assoc 10 d)))
                                    (cdr (assoc 1 d)))
                              ktexts))))
        (setq i (1+ i)))))
  ;; 3α. Μορφή κειμένων ΚΑΕΚ
  (foreach tx ktexts
    (if (null (dgm:kaekfmt (cdr tx)))
      (progn
        (princ (strcat "\n[ΣΦΑΛΜΑ] Κείμενο με λάθος μορφή στο PST_KAEK: \""
                       (cdr tx)
                       "\" (12 ψηφία, 5ψηφ+ΕΚ+5ψηφ, ΝΕΟ... ή ΤΡΥΠΑ...)"))
        (dgm:circle (car tx) 2.0 "DGM_CHECK")
        (setq nerr (1+ nerr)))))
  ;; 3β. Κάθε υφιστάμενος ΚΑΕΚ πρέπει να βρίσκεται εντός πολυγώνου PST_KAEK
  (foreach tx ktexts
    (if (eq 'KAEK (dgm:kaekfmt (cdr tx)))
      (progn
        (setq found nil)
        (foreach pts pstpolys
          (if (dgm:inpoly (car tx) pts) (setq found T)))
        (if (not found)
          (progn
            (princ (strcat "\n[ΣΦΑΛΜΑ] Ο ΚΑΕΚ " (cdr tx)
                           " δεν βρίσκεται εντός πολυγώνου PST_KAEK."))
            (dgm:circle (car tx) 2.0 "DGM_CHECK")
            (setq nerr (1+ nerr)))))))
  ;; 3γ. Πολύγωνα PST_KAEK: ακριβώς ένας υφιστάμενος ΚΑΕΚ (νησίδες/ΤΡΥΠΑ εξαιρούνται)
  (foreach pts pstpolys
    (setq kc 0 tc 0 island nil)
    (foreach tx ktexts
      (if (dgm:inpoly (car tx) pts)
        (cond
          ((eq 'KAEK (dgm:kaekfmt (cdr tx))) (setq kc (1+ kc)))
          ((eq 'TRYPA (dgm:kaekfmt (cdr tx))) (setq tc (1+ tc))))))
    (if (= kc 0)
      (progn
        (setq ip (dgm:innerpt pts))
        (foreach p2 pstpolys
          (if (and ip (not (eq p2 pts)) (dgm:inpoly ip p2))
            (setq island T)))
        (cond
          ((> tc 0)
           (princ "\n[INFO] Πολύγωνο PST_KAEK χαρακτηρισμένο ως ΤΡΥΠΑ."))
          (island
           (princ "\n[ΠΡΟΣΟΧΗ] Νησίδα PST_KAEK χωρίς ΚΑΕΚ - θα εμφανιστεί ως ΑΓΝΩΣΤΟ ΚΑΕΚ.")
           (setq nwarn (1+ nwarn)))
          (t
           (princ "\n[ΣΦΑΛΜΑ] Πολύγωνο PST_KAEK χωρίς κείμενο ΚΑΕΚ στο εσωτερικό του.")
           (dgm:circle (car pts) 2.0 "DGM_CHECK")
           (setq nerr (1+ nerr)))))
      (if (> kc 1)
        (progn
          (princ "\n[ΣΦΑΛΜΑ] Πολύγωνο PST_KAEK με περισσότερα από ένα κείμενα ΚΑΕΚ.")
          (dgm:circle (car pts) 2.0 "DGM_CHECK")
          (setq nerr (1+ nerr))))))
  ;; 3δ. Πολύγωνα DGM_PROP_FINAL: ένας (ΝΕΟ ή υφιστάμενος) ΚΑΕΚ, όχι και τα δύο
  (setq ss (ssget "_X" '((0 . "LWPOLYLINE") (8 . "DGM_PROP_FINAL"))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq e (ssname ss i))
        (if (dgm:closedp e)
          (progn
            (setq pts (dgm:lwpts e) kc 0 nc 0)
            (foreach tx ktexts
              (if (dgm:inpoly (car tx) pts)
                (cond
                  ((eq 'KAEK (dgm:kaekfmt (cdr tx))) (setq kc (1+ kc)))
                  ((eq 'NEO (dgm:kaekfmt (cdr tx))) (setq nc (1+ nc))))))
            (cond
              ((and (= kc 0) (= nc 0))
               (princ "\n[ΣΦΑΛΜΑ] Πολύγωνο DGM_PROP_FINAL χωρίς ΚΑΕΚ (ΝΕΟ ή υφιστάμενο) στο εσωτερικό του.")
               (dgm:circle (car pts) 2.0 "DGM_CHECK")
               (setq nerr (1+ nerr)))
              ((> nc 1)
               (princ "\n[ΣΦΑΛΜΑ] Πολύγωνο DGM_PROP_FINAL με περισσότερα από ένα κείμενα ΝΕΟ.")
               (dgm:circle (car pts) 2.0 "DGM_CHECK")
               (setq nerr (1+ nerr)))
              ((and (>= kc 1) (>= nc 1))
               (princ "\n[ΣΦΑΛΜΑ] Πολύγωνο DGM_PROP_FINAL με ΝΕΟ και υφιστάμενο ΚΑΕΚ ταυτόχρονα.")
               (dgm:circle (car pts) 2.0 "DGM_CHECK")
               (setq nerr (1+ nerr))))))
        (setq i (1+ i)))))
  ;; 3ε. Πολύγωνα AREA_A/AREA_D πρέπει να περιέχουν ετικέτα στο αντίστοιχο layer labels
  (foreach pair '(("AREA_A" . "AREA_A-labels") ("AREA_D" . "AREA_D-labels"))
    (setq labs nil)
    (setq ss (ssget "_X" (list '(0 . "TEXT,MTEXT") (cons 8 (cdr pair)))))
    (if ss
      (progn
        (setq i 0)
        (while (< i (sslength ss))
          (setq d (entget (ssname ss i)))
          (setq labs (cons (list (cadr (assoc 10 d)) (caddr (assoc 10 d))) labs))
          (setq i (1+ i)))))
    (setq ss (ssget "_X" (list '(0 . "LWPOLYLINE") (cons 8 (car pair)))))
    (if ss
      (progn
        (setq i 0)
        (while (< i (sslength ss))
          (setq e (ssname ss i))
          (if (dgm:closedp e)
            (progn
              (setq pts (dgm:lwpts e) cnt 0)
              (foreach p labs
                (if (dgm:inpoly p pts) (setq cnt (1+ cnt))))
              (if (= cnt 0)
                (progn
                  (princ (strcat "\n[ΣΦΑΛΜΑ] Πολύγωνο " (car pair)
                                 " χωρίς ετικέτα στο layer " (cdr pair) "."))
                  (dgm:circle (car pts) 2.0 "DGM_CHECK")
                  (setq nerr (1+ nerr))))))
          (setq i (1+ i))))))
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

;;; ==================== ΧΩΡΙΚΕΣ ΠΡΑΞΕΙΣ ΠΟΛΥΓΩΝΩΝ ======================
;;; Οι πράξεις βασίζονται σε "ακύρωση" των κοινών πλευρών (XOR): οι πλευρές
;;; που εμφανίζονται και στα δύο πολύγωνα αφαιρούνται και οι υπόλοιπες
;;; αλυσιδώνονται σε νέο κλειστό περίγραμμα. Προϋπόθεση (όπως και στις
;;; προδιαγραφές): τα κοινά όρια να έχουν ταυτισμένες κορυφές εντός ανοχής.

;; Δημιουργία LWPOLYLINE
(defun dgm:mkpoly (pts lay closed)
  (entmake (append (list '(0 . "LWPOLYLINE")
                         '(100 . "AcDbEntity")
                         (cons 8 lay)
                         '(100 . "AcDbPolyline")
                         (cons 90 (length pts))
                         (cons 70 (if closed 1 0)))
                   (mapcar '(lambda (p) (cons 10 (list (car p) (cadr p))))
                           pts))))

;; Κλειδί σημείου (στρογγυλοποίηση 4 δεκαδικών = 0.1 mm)
(defun dgm:rkey (p)
  (strcat (rtos (car p) 2 4) "_" (rtos (cadr p) 2 4)))

;; Αφαίρεση συγκεκριμένου στοιχείου (κατά eq) από λίστα
(defun dgm:rm (item lst / out x)
  (setq out nil)
  (foreach x lst (if (not (eq x item)) (setq out (cons x out))))
  (reverse out))

;; Ταξινόμηση λίστας ζευγών (key . data) κατά αύξον αριθμητικό key
(defun dgm:sortpairs (lst / sorted item out x done)
  (setq sorted nil)
  (foreach item lst
    (setq out nil done nil)
    (foreach x sorted
      (if (and (not done) (< (car item) (car x)))
        (setq out (cons item out) done T))
      (setq out (cons x out)))
    (if (not done) (setq out (cons item out)))
    (setq sorted (reverse out)))
  sorted)

;; Απαλοιφή διαδοχικών διπλών κορυφών (και τελευταίας = πρώτης)
(defun dgm:dedup (pts / out p)
  (setq out nil)
  (foreach p pts
    (if (or (null out) (> (distance p (car out)) 1e-8))
      (setq out (cons p out))))
  (setq out (reverse out))
  (if (and (> (length out) 1)
           (< (distance (car out) (last out)) 1e-8))
    (setq out (reverse (cdr (reverse out)))))
  out)

;; "Κανονικοποίηση" σημείου: όλα τα σημεία εντός tol ταυτίζονται
(defun dgm:canon (p tol / found c)
  (setq found nil)
  (foreach c dgm:*canon*
    (if (and (not found) (< (distance c p) tol)) (setq found c)))
  (if found
    found
    (progn (setq dgm:*canon* (cons p dgm:*canon*)) p)))

;; Πύκνωση πολυγώνου: σπάει κάθε πλευρά στα σημεία της allpts που
;; βρίσκονται πάνω της (εντός tol)
(defun dgm:densify (pts allpts tol / n i a b mids p out)
  (setq n (length pts) i 0 out nil)
  (while (< i n)
    (setq a (nth i pts) b (nth (rem (1+ i) n) pts))
    (setq out (cons a out))
    (setq mids nil)
    (foreach p allpts
      (if (and (> (distance p a) tol)
               (> (distance p b) tol)
               (< (dgm:pseg p a b) tol))
        (setq mids (cons (cons (distance a p) p) mids))))
    (foreach p (dgm:sortpairs mids)
      (setq out (cons (cdr p) out)))
    (setq i (1+ i)))
  (reverse out))

;; Επόμενη κορυφή κατά τη δεξιόστροφη διάσχιση όψεων επίπεδου γραφήματος:
;; από την κατευθυνόμενη πλευρά kprev->kcur επιλέγεται η αμέσως επόμενη
;; δεξιόστροφα πλευρά γύρω από την kcur.
(defun dgm:nextcw (kprev kcur adj verts / pp pc thin best bestd th d c)
  (setq pp (cdr (assoc kprev verts))
        pc (cdr (assoc kcur verts)))
  (setq thin (atan (- (cadr pp) (cadr pc)) (- (car pp) (car pc))))
  (setq best nil bestd 1e99)
  (foreach c (cdr (assoc kcur adj))
    (setq th (car c) d (- thin th))
    (while (<= d 1e-9) (setq d (+ d 6.283185307179586)))
    (while (> d 6.283185307179586) (setq d (- d 6.283185307179586)))
    (if (< d bestd) (setq bestd d best (cdr c))))
  best)

;; XOR πλευρών πολυγώνων: επιστρέφει λίστα κλειστών βρόχων (όψεις CCW).
;; Οι κοινές πλευρές ακυρώνονται, οι πλευρές τέμνονται στα σημεία τομής τους
;; και οι εναπομείνασες αλυσιδώνονται με γωνιακή διάσχιση όψεων.
(defun dgm:xorloops (polys tol / cpolys pts p allpts dpolys edges n i a b k
                       rec loops verts adj ka kb cell th used dirs d0
                       face kprev kcur knext guard pts2
                       ia ib ptsa ptsb na nb j c d ip)
  (setq dgm:*canon* nil)
  ;; κανονικοποίηση κορυφών
  (setq cpolys nil)
  (foreach pts polys
    (setq cpolys (cons (mapcar '(lambda (p) (dgm:canon p tol)) pts) cpolys)))
  ;; τομές πλευρών μεταξύ διαφορετικών πολυγώνων -> νέα κοινά σημεία
  (setq ia 0)
  (foreach ptsa cpolys
    (setq ib 0)
    (foreach ptsb cpolys
      (if (> ib ia)
        (progn
          (setq na (length ptsa) nb (length ptsb) i 0)
          (while (< i na)
            (setq a (nth i ptsa) b (nth (rem (1+ i) na) ptsa))
            (setq j 0)
            (while (< j nb)
              (setq c (nth j ptsb) d (nth (rem (1+ j) nb) ptsb))
              (setq ip (inters a b c d T))
              (if ip (dgm:canon (list (car ip) (cadr ip)) tol))
              (setq j (1+ j)))
            (setq i (1+ i)))))
      (setq ib (1+ ib)))
    (setq ia (1+ ia)))
  (setq allpts dgm:*canon*)
  ;; πύκνωση με τις κορυφές όλων των πολυγώνων
  (setq dpolys nil)
  (foreach pts cpolys
    (setq dpolys (cons (dgm:densify pts allpts tol) dpolys)))
  ;; συλλογή πλευρών με XOR (η 2η εμφάνιση ακυρώνει την 1η)
  (setq edges nil)
  (foreach pts dpolys
    (setq n (length pts) i 0)
    (while (< i n)
      (setq a (nth i pts) b (nth (rem (1+ i) n) pts))
      (if (> (distance a b) (* 0.5 tol))
        (progn
          (setq k (if (< (dgm:rkey a) (dgm:rkey b))
                    (strcat (dgm:rkey a) "|" (dgm:rkey b))
                    (strcat (dgm:rkey b) "|" (dgm:rkey a))))
          (if (setq rec (assoc k edges))
            (setq edges (dgm:rm rec edges))
            (setq edges (cons (list k a b) edges)))))
      (setq i (1+ i))))
  ;; κατασκευή λιστών γειτνίασης (κορυφή -> γωνίες προς γείτονες)
  (setq verts nil adj nil)
  (foreach rec edges
    (setq a (cadr rec) b (caddr rec)
          ka (dgm:rkey a) kb (dgm:rkey b))
    (if (not (assoc ka verts)) (setq verts (cons (cons ka a) verts)))
    (if (not (assoc kb verts)) (setq verts (cons (cons kb b) verts)))
    (setq th (atan (- (cadr b) (cadr a)) (- (car b) (car a))))
    (setq cell (assoc ka adj))
    (if cell
      (setq adj (subst (cons ka (cons (cons th kb) (cdr cell))) cell adj))
      (setq adj (cons (list ka (cons th kb)) adj)))
    (setq th (atan (- (cadr a) (cadr b)) (- (car a) (car b))))
    (setq cell (assoc kb adj))
    (if cell
      (setq adj (subst (cons kb (cons (cons th ka) (cdr cell))) cell adj))
      (setq adj (cons (list kb (cons th ka)) adj))))
  ;; εξαγωγή όψεων με γωνιακή διάσχιση - κρατάμε τις CCW (εσωτερικές)
  (setq used nil loops nil dirs nil)
  (foreach rec edges
    (setq ka (dgm:rkey (cadr rec)) kb (dgm:rkey (caddr rec)))
    (setq dirs (cons (cons ka kb) (cons (cons kb ka) dirs))))
  (foreach d0 dirs
    (if (not (member (strcat (car d0) ">" (cdr d0)) used))
      (progn
        (setq face (list (car d0) (cdr d0)))
        (setq used (cons (strcat (car d0) ">" (cdr d0)) used))
        (setq kprev (car d0) kcur (cdr d0) guard 0)
        (while (and face (< guard 100000))
          (setq guard (1+ guard))
          (setq knext (dgm:nextcw kprev kcur adj verts))
          (cond
            ((null knext) (setq face nil))
            ((and (= kcur (car d0)) (= knext (cdr d0)))
             (setq guard 100000))
            (t
             (setq used (cons (strcat kcur ">" knext) used))
             (setq face (append face (list knext)))
             (setq kprev kcur kcur knext))))
        (if face
          (progn
            (setq pts2 (dgm:dedup
                         (mapcar '(lambda (k) (cdr (assoc k verts))) face)))
            (if (and (> (length pts2) 2) (> (dgm:sarea pts2) 1e-9))
              (setq loops (cons pts2 loops))))))))
  loops)

;; Επιλογή πολλών κλειστών polylines
(defun dgm:selpolys (msg / ss i out e)
  (princ msg)
  (setq ss (ssget '((0 . "LWPOLYLINE"))))
  (setq out nil)
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq e (ssname ss i))
        (if (dgm:closedp e)
          (setq out (cons (dgm:lwpts e) out))
          (princ "\n[ΠΡΟΣΟΧΗ] Παραλείφθηκε ανοιχτή polyline."))
        (setq i (1+ i)))))
  out)

;;; DGMUNION - Συνένωση όμορων πολυγώνων
(defun c:DGMUNION ( / polys tol loops lay pts)
  (setq polys (dgm:selpolys "\nΕπιλέξτε 2 ή περισσότερα όμορα κλειστά πολύγωνα: "))
  (if (< (length polys) 2)
    (princ "\n** Χρειάζονται τουλάχιστον 2 κλειστά πολύγωνα. **")
    (progn
      (setq tol (dgm:getreal "\nΑνοχή ταύτισης κορυφών (m)" 0.001))
      (setq loops (dgm:xorloops polys tol))
      (cond
        ((null loops)
         (princ "\n** Δεν προέκυψε αποτέλεσμα. **"))
        ((= (length loops) (length polys))
         (princ "\n** Τα πολύγωνα δεν έχουν κοινές πλευρές (με την ανοχή αυτή) - δεν έγινε συνένωση. **"))
        (t
         (setq lay (dgm:getstr "\nLayer αποτελέσματος" "DGM_PROP_FINAL"))
         (dgm:layer-std lay)
         (foreach pts loops
           (dgm:mkpoly pts lay T)
           (princ (strcat "\nΝέο πολύγωνο: " (itoa (length pts))
                          " κορυφές, εμβαδόν "
                          (rtos (dgm:area pts) 2 2) " τ.μ.")))
         (princ (strcat "\nΔημιουργήθηκαν " (itoa (length loops))
                        " πολύγωνα στο layer " lay "."))))))
  (princ))

;;; DGMCUT - Αποκοπή τμήματος από γεωτεμάχιο
(defun c:DGMCUT ( / e1 e2 p1 p2 tol loops lay pts expct)
  (setq e1 (dgm:sel-poly "\nΕπιλέξτε το γεωτεμάχιο: "))
  (if e1
    (progn
      (setq e2 (dgm:sel-poly "\nΕπιλέξτε το αποκοπτόμενο τμήμα: "))
      (if e2
        (if (or (not (dgm:closedp e1)) (not (dgm:closedp e2)))
          (princ "\n** Και τα δύο πολύγωνα πρέπει να είναι κλειστά. **")
          (progn
            (setq p1 (dgm:lwpts e1) p2 (dgm:lwpts e2))
            (setq tol (dgm:getreal "\nΑνοχή ταύτισης κορυφών (m)" 0.001))
            (setq expct (- (dgm:area p1) (dgm:area p2)))
            (setq loops (dgm:xorloops (list p1 p2) tol))
            (cond
              ((or (null loops) (/= 1 (length loops)))
               (princ "\n** Η αποκοπή δεν έδωσε ενιαίο πολύγωνο. **")
               (princ "\nΕλέγξτε ότι το τμήμα εφάπτεται στο όριο του γεωτεμαχίου")
               (princ "\nμε ταυτισμένες κορυφές (όχι εσωτερική νησίδα)."))
              (t
               (setq lay (dgm:getstr "\nLayer αποτελέσματος" "DGM_PROP_FINAL"))
               (dgm:layer-std lay)
               (setq pts (car loops))
               (dgm:mkpoly pts lay T)
               (princ (strcat "\nΥπόλοιπο γεωτεμάχιο στο layer " lay ": "
                              (rtos (dgm:area pts) 2 2)
                              " τ.μ. (αναμενόμενο: "
                              (rtos expct 2 2) " τ.μ.)")))))))))
  (princ))

;;; DGMSPLIT - Κατάτμηση πολυγώνου με γραμμή κοπής
(defun c:DGMSPLIT ( / en ec pts cpts n m ints i j k a b c d ip f rec two
                      sorted i1 i2 t1 t2 pt1 pt2 mid fwd bwd pa pb lay del)
  (setq en (dgm:sel-poly "\nΕπιλέξτε το κλειστό πολύγωνο προς κατάτμηση: "))
  (if (and en (not (dgm:closedp en)))
    (progn (princ "\n** Η polyline δεν είναι κλειστή. **") (setq en nil)))
  (setq ec (if en
             (dgm:sel-poly "\nΕπιλέξτε τη γραμμή κοπής (polyline που διαπερνά το πολύγωνο): ")))
  (if (and en ec)
    (progn
      (setq pts (dgm:lwpts en) cpts (dgm:lwpts ec))
      (setq n (length pts) m (length cpts))
      ;; εύρεση τομών γραμμής κοπής με το περίγραμμα
      (setq ints nil i 0)
      (while (< i n)
        (setq a (nth i pts) b (nth (rem (1+ i) n) pts))
        (setq j 0)
        (while (< j (1- m))
          (setq c (nth j cpts) d (nth (1+ j) cpts))
          (setq ip (inters a b c d T))
          (if ip
            (progn
              (setq ip (list (car ip) (cadr ip)))
              (setq f nil)
              (foreach rec ints
                (if (< (distance (nth 4 rec) ip) 1e-6) (setq f T)))
              (if (not f)
                (setq ints (cons (list i
                                       (distance a ip)
                                       j
                                       (+ (* 1000000.0 j) (distance c ip))
                                       ip)
                                 ints)))))
          (setq j (1+ j)))
        (setq i (1+ i)))
      (if (/= 2 (length ints))
        (princ (strcat "\n** Βρέθηκαν " (itoa (length ints))
                       " σημεία τομής - υποστηρίζεται κοπή με ακριβώς 2. "
                       "Η γραμμή πρέπει να διαπερνά πλήρως το πολύγωνο. **"))
        (progn
          ;; ταξινόμηση των 2 τομών κατά μήκος της γραμμής κοπής
          (setq sorted (dgm:sortpairs
                         (mapcar '(lambda (x) (cons (cadddr x) x)) ints)))
          (setq rec (cdr (car sorted)) two (cdr (cadr sorted)))
          (setq i1 (car rec) t1 (cadr rec) pt1 (nth 4 rec)
                i2 (car two) t2 (cadr two) pt2 (nth 4 two))
          ;; ενδιάμεσες κορυφές της γραμμής κοπής μεταξύ των 2 τομών
          (setq mid nil j (1+ (caddr rec)))
          (while (<= j (caddr two))
            (setq mid (append mid (list (nth j cpts))))
            (setq j (1+ j)))
          ;; εμπρόσθια διαδρομή περιγράμματος pt1 -> pt2
          (cond
            ((and (= i1 i2) (< t1 t2)) (setq fwd nil))
            ((= i1 i2)
             (setq fwd nil j (rem (1+ i1) n) k 0)
             (while (< k n)
               (setq fwd (append fwd (list (nth j pts)))
                     j (rem (1+ j) n) k (1+ k))))
            (t
             (setq fwd nil j (rem (1+ i1) n))
             (while (/= j (rem (1+ i2) n))
               (setq fwd (append fwd (list (nth j pts)))
                     j (rem (1+ j) n)))))
          ;; οπίσθια διαδρομή περιγράμματος pt2 -> pt1
          (cond
            ((and (= i1 i2) (> t1 t2)) (setq bwd nil))
            ((= i1 i2)
             (setq bwd nil j (rem (1+ i2) n) k 0)
             (while (< k n)
               (setq bwd (append bwd (list (nth j pts)))
                     j (rem (1+ j) n) k (1+ k))))
            (t
             (setq bwd nil j (rem (1+ i2) n))
             (while (/= j (rem (1+ i1) n))
               (setq bwd (append bwd (list (nth j pts)))
                     j (rem (1+ j) n)))))
          ;; σύνθεση των δύο νέων πολυγώνων
          (setq pa (dgm:dedup (append (list pt1) fwd (list pt2) (reverse mid))))
          (setq pb (dgm:dedup (append (list pt2) bwd (list pt1) mid)))
          (setq lay (dgm:getstr "\nLayer αποτελεσμάτων"
                                (cdr (assoc 8 (entget en)))))
          (dgm:layer-std lay)
          (dgm:mkpoly pa lay T)
          (dgm:mkpoly pb lay T)
          (princ (strcat "\nΤμήμα 1: " (rtos (dgm:area pa) 2 2)
                         " τ.μ.   Τμήμα 2: " (rtos (dgm:area pb) 2 2)
                         " τ.μ.   (Αρχικό: "
                         (rtos (dgm:area pts) 2 2) " τ.μ.)"))
          (princ "\nΔιαγραφή αρχικού πολυγώνου;  1 = Όχι   2 = Ναι")
          (setq del (dgm:getint "\nΕπιλογή" 1))
          (if (= del 2) (entdel en))))))
  (princ))

;;; ===================== HASH DXF / ΚΗΔ =================================

;; Αφαίρεση κενών από string
(defun dgm:nospace (s / out i c)
  (setq out "" i 1)
  (while (<= i (strlen s))
    (setq c (substr s i 1))
    (if (/= c " ") (setq out (strcat out c)))
    (setq i (1+ i)))
  out)

;;; DGMHASH - Υπολογισμός hash SHA512 αρχείου DXF μέσω certutil
(defun c:DGMHASH ( / dxf algname tmp cmd ok sh f line hash pt h)
  (setq dxf (getfiled "Επιλογή αρχείου DXF" "" "dxf" 0))
  (if dxf
    (progn
      (setq algname "SHA512")
      (setq tmp (strcat (getenv "TEMP") "\\dgm_hash.txt"))
      (setq cmd (strcat "cmd /c certutil -hashfile \"" dxf "\" " algname
                        " > \"" tmp "\""))
      (setq ok nil)
      (if vl-load-com (vl-load-com))
      (if vlax-create-object
        (progn
          (setq sh (vlax-create-object "WScript.Shell"))
          (if sh
            (progn
              (vlax-invoke-method sh 'Run cmd 0 :vlax-true)
              (vlax-release-object sh)
              (setq ok T)))))
      (if (not ok)
        (progn
          (startapp cmd)
          (getstring "\nΟ υπολογισμός τρέχει σε παράθυρο cmd. Πατήστε Enter όταν ολοκληρωθεί... ")))
      ;; ανάγνωση αποτελέσματος
      (setq hash nil)
      (setq f (open tmp "r"))
      (if f
        (progn
          (read-line f)                      ; γραμμή τίτλου certutil
          (setq line (read-line f))          ; γραμμή με το hash
          (close f)
          (if line (setq hash (dgm:nospace line)))))
      (if (and hash (= 128 (strlen hash)))
        (progn
          (princ (strcat "\n" algname ": " hash))
          (setq pt (getpoint "\nΘέση κειμένου hash στο σχέδιο (Enter για παράλειψη): "))
          (if pt
            (progn
              (setq h (dgm:getreal "\nΎψος κειμένου" dgm:*h*))
              (setq dgm:*h* h)
              (dgm:text pt h (strcat algname " DXF: " hash)
                        (getvar "CLAYER")))))
        (princ "\n** Δεν ήταν δυνατή η ανάγνωση του hash. Δοκιμάστε το tools/dxf-hash.cmd εκτός CAD. **"))))
  (princ))

;;; DGMKHD - Τοποθέτηση ΚΗΔ (Κωδικός Ηλεκτρονικού Διαγράμματος) στο σχέδιο
(defun c:DGMKHD ( / s pt h)
  (setq s (getstring T "\nΚΗΔ (Κωδικός Ηλεκτρονικού Διαγράμματος): "))
  (if (/= s "")
    (progn
      (setq h (dgm:getreal "\nΎψος κειμένου" dgm:*h*))
      (setq dgm:*h* h)
      (setq pt (getpoint "\nΘέση κειμένου: "))
      (if pt (dgm:text pt h (strcat "ΚΗΔ: " s) (getvar "CLAYER")))))
  (princ))

;;; ============== ΕΡΓΑΛΕΙΑ ΡΟΗΣ ΕΡΓΑΣΙΑΣ (τύπου KtimaDGM) ===============

;; Προσημασμένο εμβαδόν (θετικό = CCW)
(defun dgm:sarea (pts / s n i p1 p2)
  (setq s 0.0 n (length pts) i 0)
  (while (< i n)
    (setq p1 (nth i pts) p2 (nth (rem (1+ i) n) pts))
    (setq s (+ s (- (* (car p1) (cadr p2)) (* (car p2) (cadr p1)))))
    (setq i (1+ i)))
  (/ s 2.0))

;; Περίμετρος κλειστού πολυγώνου
(defun dgm:perim (pts / s n i)
  (setq s 0.0 n (length pts) i 0)
  (while (< i n)
    (setq s (+ s (distance (nth i pts) (nth (rem (1+ i) n) pts))))
    (setq i (1+ i)))
  s)

;; Αντιπροσωπευτικό εσωτερικό σημείο κλειστού βρόχου
(defun dgm:innerpt (pts / n i a b len mx my nx ny p)
  (setq n (length pts) i 0 p nil)
  (while (and (< i n) (null p))
    (setq a (nth i pts) b (nth (rem (1+ i) n) pts))
    (setq len (distance a b))
    (if (> len 1e-9)
      (progn
        (setq mx (/ (+ (car a) (car b)) 2.0)
              my (/ (+ (cadr a) (cadr b)) 2.0)
              nx (/ (- (cadr a) (cadr b)) len)
              ny (/ (- (car b) (car a)) len))
        (foreach ee (list (* 0.01 len) (* 0.001 len) 0.005)
          (if (null p)
            (cond
              ((dgm:inpoly (list (+ mx (* ee nx)) (+ my (* ee ny))) pts)
               (setq p (list (+ mx (* ee nx)) (+ my (* ee ny)))))
              ((dgm:inpoly (list (- mx (* ee nx)) (- my (* ee ny))) pts)
               (setq p (list (- mx (* ee nx)) (- my (* ee ny))))))))))
    (setq i (1+ i)))
  p)

;; Θέση σημείου πάνω στο περίγραμμα: (iedge . απόσταση από αρχή πλευράς) ή nil
(defun dgm:onboundary (p pts tol / n i a b res)
  (setq n (length pts) i 0 res nil)
  (while (and (< i n) (null res))
    (setq a (nth i pts) b (nth (rem (1+ i) n) pts))
    (if (< (dgm:pseg p a b) tol)
      (setq res (cons i (distance a p))))
    (setq i (1+ i)))
  res)

;; Ελάχιστη απόσταση σημείου από περίγραμμα πολυγώνου
(defun dgm:pmindist (p pts / n i a b d md)
  (setq n (length pts) i 0 md 1e99)
  (while (< i n)
    (setq a (nth i pts) b (nth (rem (1+ i) n) pts))
    (setq d (dgm:pseg p a b))
    (if (< d md) (setq md d))
    (setq i (1+ i)))
  md)

;; Μήκος κοινού ορίου βρόχου loop με πολύγωνο pts
(defun dgm:sharedlen (loop pts tol / n i a b m tot)
  (setq n (length loop) i 0 tot 0.0)
  (while (< i n)
    (setq a (nth i loop) b (nth (rem (1+ i) n) loop))
    (setq m (list (/ (+ (car a) (car b)) 2.0)
                  (/ (+ (cadr a) (cadr b)) 2.0)))
    (if (< (dgm:pmindist m pts) tol)
      (setq tot (+ tot (distance a b))))
    (setq i (1+ i)))
  tot)

;; Κοπή κλειστού πολυγώνου pts με χορδή cpts (άκρα πάνω στο περίγραμμα)
;; Επιστρέφει (pa pb) ή nil
(defun dgm:splitchord (pts cpts tol / n pt1 pt2 e1 e2 i1 t1 i2 t2 mid
                         fwd bwd j k pa pb)
  (setq n (length pts)
        pt1 (car cpts)
        pt2 (last cpts))
  (setq e1 (dgm:onboundary pt1 pts tol)
        e2 (dgm:onboundary pt2 pts tol))
  (if (and e1 e2 (> (length cpts) 1))
    (progn
      (setq i1 (car e1) t1 (cdr e1)
            i2 (car e2) t2 (cdr e2))
      (setq mid (reverse (cdr (reverse (cdr cpts)))))
      (cond
        ((and (= i1 i2) (< t1 t2)) (setq fwd nil))
        ((= i1 i2)
         (setq fwd nil j (rem (1+ i1) n) k 0)
         (while (< k n)
           (setq fwd (append fwd (list (nth j pts)))
                 j (rem (1+ j) n) k (1+ k))))
        (t
         (setq fwd nil j (rem (1+ i1) n))
         (while (/= j (rem (1+ i2) n))
           (setq fwd (append fwd (list (nth j pts)))
                 j (rem (1+ j) n)))))
      (cond
        ((and (= i1 i2) (> t1 t2)) (setq bwd nil))
        ((= i1 i2)
         (setq bwd nil j (rem (1+ i2) n) k 0)
         (while (< k n)
           (setq bwd (append bwd (list (nth j pts)))
                 j (rem (1+ j) n) k (1+ k))))
        (t
         (setq bwd nil j (rem (1+ i2) n))
         (while (/= j (rem (1+ i1) n))
           (setq bwd (append bwd (list (nth j pts)))
                 j (rem (1+ j) n)))))
      (setq pa (dgm:dedup (append (list pt1) fwd (list pt2) (reverse mid))))
      (setq pb (dgm:dedup (append (list pt2) bwd (list pt1) mid)))
      (if (and (> (length pa) 2) (> (length pb) 2))
        (list pa pb)))))

;; Συγχρονισμένη εκτέλεση εντολής Windows
(defun dgm:runsync (cmd / ok sh)
  (setq ok nil)
  (if vl-load-com (vl-load-com))
  (if vlax-create-object
    (progn
      (setq sh (vlax-create-object "WScript.Shell"))
      (if sh
        (progn
          (vlax-invoke-method sh 'Run cmd 0 :vlax-true)
          (vlax-release-object sh)
          (setq ok T)))))
  (if (not ok)
    (progn
      (startapp cmd)
      (getstring "\nΗ εντολή τρέχει σε παράθυρο cmd. Πατήστε Enter όταν ολοκληρωθεί... ")))
  T)

;; Επανακατασκευή polyline με νέες κορυφές
(defun dgm:rebuild (e pts closed / lay)
  (setq lay (cdr (assoc 8 (entget e))))
  (entdel e)
  (dgm:mkpoly pts lay closed))

;; Συλλογή LWPOLYLINES από layers: λίστα από (ename pts closed)
(defun dgm:collect (lays closedonly / ss i e out)
  (setq out nil)
  (foreach lay lays
    (setq ss (ssget "_X" (list '(0 . "LWPOLYLINE") (cons 8 lay))))
    (if ss
      (progn
        (setq i 0)
        (while (< i (sslength ss))
          (setq e (ssname ss i))
          (if (or (not closedonly) (dgm:closedp e))
            (setq out (cons (list e (dgm:lwpts e) (dgm:closedp e)) out)))
          (setq i (1+ i))))))
  out)

;; Off/επαναφορά layer
(defun dgm:layoff (nm / d)
  (setq d (entget (tblobjname "LAYER" nm)))
  (if d (entmod (subst (cons 62 (- (abs (cdr (assoc 62 d)))))
                       (assoc 62 d) d))))
(defun dgm:laycolor (nm col / d)
  (setq d (entget (tblobjname "LAYER" nm)))
  (if d (entmod (subst (cons 62 col) (assoc 62 d) d))))

;;; DGMCLEAN - Αυτόματος καθαρισμός τυπικών σφαλμάτων (τύπου KTM_CleanUp)
(defun dgm:cleangroup (items tol seed densifyp closep / allc pts new fix orig)
  (setq dgm:*canon* nil fix 0)
  (foreach p seed (dgm:canon p tol))
  (setq items
        (mapcar '(lambda (it)
                   (list (car it)
                         (mapcar '(lambda (p) (dgm:canon p tol)) (cadr it))
                         (caddr it)))
                items))
  (setq allc dgm:*canon*)
  (foreach it items
    (setq orig (dgm:lwpts (car it)))
    (setq pts (cadr it))
    (setq new (if densifyp (dgm:densify pts allc tol) pts))
    (setq new (dgm:dedup new))
    (if (or (not (equal new orig 1e-9))
            (and closep (not (caddr it))))
      (progn
        (dgm:rebuild (car it) new (if closep T (caddr it)))
        (setq fix (1+ fix)))))
  fix)

(defun c:DGMCLEAN ( / tol fix seedt seedn)
  (setq tol (dgm:getreal "\nΑνοχή καθαρισμού (m)" 0.001))
  (setq fix 0)
  ;; 1. PST_KAEK + DGM_PROP_FINAL: κλείσιμο, κούμπωμα κορυφών,
  ;;    προσθήκη κορυφών στα κοινά όρια, διαγραφή διπλών
  (setq fix (+ fix (dgm:cleangroup
                     (dgm:collect '("PST_KAEK" "DGM_PROP_FINAL") nil)
                     tol nil T T)))
  ;; κορυφές-στόχοι για τις επόμενες ομάδες
  (setq seedn nil)
  (foreach it (dgm:collect '("PST_KAEK" "DGM_PROP_FINAL") nil)
    (foreach p (cadr it) (setq seedn (cons p seedn))))
  ;; 2. AREA_A / AREA_D: κούμπωμα στα PST/DGM + διαγραφή διπλών
  (setq fix (+ fix (dgm:cleangroup (dgm:collect '("AREA_A" "AREA_D") nil)
                                   tol seedn nil nil)))
  ;; 3. BOUND_IMPL / BOUND_UNIMPL: κούμπωμα κορυφών
  (setq fix (+ fix (dgm:cleangroup (dgm:collect '("BOUND_IMPL" "BOUND_UNIMPL") nil)
                                   tol seedn nil nil)))
  ;; 4. VST / BLD / EAS: κούμπωμα στο TOPO_PROP
  (setq seedt nil)
  (foreach it (dgm:collect '("TOPO_PROP") nil)
    (foreach p (cadr it) (setq seedt (cons p seedt))))
  (setq fix (+ fix (dgm:cleangroup (dgm:collect '("VST" "BLD" "EAS") nil)
                                   tol seedt nil nil)))
  ;; 5. VST_FINAL / EAS_FINAL: κούμπωμα στο TOPO_PROP_NEW (αλλιώς TOPO_PROP)
  (setq seedn nil)
  (foreach it (dgm:collect '("TOPO_PROP_NEW") nil)
    (foreach p (cadr it) (setq seedn (cons p seedn))))
  (if (null seedn) (setq seedn seedt))
  (setq fix (+ fix (dgm:cleangroup (dgm:collect '("VST_FINAL" "EAS_FINAL") nil)
                                   tol seedn nil nil)))
  (princ (strcat "\nΚαθαρισμός ολοκληρώθηκε: διορθώθηκαν " (itoa fix)
                 " polylines."))
  (princ))

;;; DGMORIGIN - Αλλαγή αρχής και φοράς κλειστής polyline
(defun c:DGMORIGIN ( / en pts n p i bd best dir rot)
  (setq en (dgm:sel-poly "\nΕπιλέξτε κλειστή polyline: "))
  (cond
    ((null en))
    ((not (dgm:closedp en))
     (princ "\n** Η polyline δεν είναι κλειστή. **"))
    (t
     (setq pts (dgm:lwpts en) n (length pts))
     (setq p (getpoint "\nΕπιλέξτε τη νέα κορυφή αρχής: "))
     (if p
       (progn
         (setq p (list (car p) (cadr p)))
         (setq best 0 bd 1e99 i 0)
         (while (< i n)
           (if (< (distance p (nth i pts)) bd)
             (setq bd (distance p (nth i pts)) best i))
           (setq i (1+ i)))
         (princ "\nΦορά:  1 = CW (δεξιόστροφα)   2 = CCW (αριστερόστροφα)")
         (setq dir (dgm:getint "\nΕπιλογή" 1))
         (setq rot nil i 0)
         (while (< i n)
           (setq rot (append rot (list (nth (rem (+ best i) n) pts))))
           (setq i (1+ i)))
         (if (or (and (= dir 1) (> (dgm:sarea rot) 0))
                 (and (= dir 2) (< (dgm:sarea rot) 0)))
           (setq rot (cons (car rot) (reverse (cdr rot)))))
         (dgm:rebuild en rot T)
         (princ "\nΕνημερώθηκαν η αρχή και η φορά της polyline.")))))
  (princ))

;;; DGMBND / DGMBNDPD - Πολύγωνο από εσωτερικό σημείο (τύπου KTM_GetBoundary)
(defun dgm:boundary-core (lay / pt eb old cnt made)
  (dgm:layer-std lay)
  (setq old (getvar "CLAYER"))
  (setvar "CLAYER" lay)
  (setq cnt 0 made nil)
  (setq pt (getpoint "\nΕπιλέξτε εσωτερικό σημείο (Enter για τέλος): "))
  (while pt
    (setq eb (entlast))
    (command "_.-BOUNDARY" pt "")
    (if (not (eq eb (entlast)))
      (progn
        (setq cnt (1+ cnt))
        (setq made (cons (entlast) made)))
      (princ "\n** Δεν δημιουργήθηκε πολύγωνο στο σημείο αυτό. **"))
    (setq pt (getpoint "\nΕπόμενο εσωτερικό σημείο (Enter για τέλος): ")))
  (setvar "CLAYER" old)
  (princ (strcat "\nΔημιουργήθηκαν " (itoa cnt) " πολύγωνα στο layer " lay "."))
  made)

(defun c:DGMBND ( / lay made ans polys loops)
  (setq lay (dgm:getstr "\nLayer αποτελέσματος" "DGM_PROP_FINAL"))
  (setq made (dgm:boundary-core lay))
  (if (> (length made) 1)
    (progn
      (princ "\nΣυνένωση των πολυγώνων που δημιουργήθηκαν;  1 = Όχι   2 = Ναι")
      (setq ans (dgm:getint "\nΕπιλογή" 1))
      (if (= ans 2)
        (progn
          (setq polys (mapcar 'dgm:lwpts made))
          (setq loops (dgm:xorloops polys 0.001))
          (if (= 1 (length loops))
            (progn
              (foreach e made (entdel e))
              (dgm:mkpoly (car loops) lay T)
              (princ "\nΈγινε συνένωση σε ενιαίο πολύγωνο."))
            (princ "\n** Τα πολύγωνα δεν είναι όμορα - δεν έγινε συνένωση. **"))))))
  (princ))

(defun c:DGMBNDPD ( / states l nm)
  (dgm:layer-std "PST_KAEK")
  (dgm:layer-std "DGM_PROP_FINAL")
  (setq states nil)
  (setq l (tblnext "LAYER" T))
  (while l
    (setq nm (cdr (assoc 2 l)))
    (setq states (cons (cons nm (cdr (assoc 62 l))) states))
    (if (and (/= nm "PST_KAEK") (/= nm "DGM_PROP_FINAL"))
      (dgm:layoff nm))
    (setq l (tblnext "LAYER")))
  (dgm:boundary-core "DGM_PROP_FINAL")
  (foreach s states (dgm:laycolor (car s) (cdr s)))
  (princ))

;;; DGMKAT / DGMVST - Υποδιαίρεση πολυγώνων με γραμμές κοπής
(defun dgm:katcore (srclays cutlay outlay tol
                    / cuts work lpts item res newwork found cnt mp fin)
  (setq cuts (dgm:collect (list cutlay) nil))
  (setq work nil)
  (foreach item (dgm:collect srclays nil)
    (if (caddr item)
      (setq work (cons (list (cadr item) nil) work))))
  (cond
    ((null cuts)
     (princ (strcat "\n** Δεν βρέθηκαν γραμμές στο layer " cutlay ". **")))
    ((null work)
     (princ "\n** Δεν βρέθηκαν κλειστά πολύγωνα πηγής. **"))
    (t
     (setq cnt 0)
     (foreach ln cuts
       (setq lpts (cadr ln) found nil newwork nil)
       (setq mp (if (> (length lpts) 2)
                  (nth (/ (length lpts) 2) lpts)
                  (list (/ (+ (car (car lpts)) (car (last lpts))) 2.0)
                        (/ (+ (cadr (car lpts)) (cadr (last lpts))) 2.0))))
       (foreach item work
         (if (and (not found)
                  (dgm:onboundary (car lpts) (car item) tol)
                  (dgm:onboundary (last lpts) (car item) tol)
                  (dgm:inpoly mp (car item)))
           (progn
             (setq res (dgm:splitchord (car item) lpts tol))
             (if res
               (progn
                 (setq newwork (cons (list (car res) T)
                                     (cons (list (cadr res) T) newwork)))
                 (setq found T cnt (1+ cnt)))
               (setq newwork (cons item newwork))))
           (setq newwork (cons item newwork))))
       (setq work newwork)
       (if (not found)
         (progn
           (princ "\n[ΠΡΟΣΟΧΗ] Γραμμή κοπής χωρίς αντιστοίχιση: τα άκρα της πρέπει")
           (princ "\n          να ακουμπούν στο περίγραμμα πολυγώνου πηγής."))))
     (dgm:layer-std outlay)
     (setq fin 0)
     (foreach item work
       (if (cadr item)
         (progn
           (dgm:mkpoly (car item) outlay T)
           (setq fin (1+ fin)))))
     (princ (strcat "\nΕκτελέστηκαν " (itoa cnt) " κοπές - δημιουργήθηκαν "
                    (itoa fin) " πολύγωνα στο layer " outlay ".")))))

(defun c:DGMKAT ( / tol)
  (setq tol (dgm:getreal "\nΑνοχή (m)" 0.001))
  (dgm:katcore '("PST_KAEK") "LINE_XM" "DGM_PROP_FINAL" tol)
  (princ "\nΥπενθύμιση: τοποθετήστε κείμενα ΝΕΟ ... (layer PST_KAEK) στα νέα πολύγωνα.")
  (princ))

(defun c:DGMVST ( / tol)
  (setq tol (dgm:getreal "\nΑνοχή (m)" 0.001))
  (dgm:katcore '("PST_KAEK" "DGM_PROP_FINAL") "LINE_XM_VST" "VST_FINAL" tol)
  (princ))

;;; DGMGM - Αυτόματη συμπλήρωση DGM_PROP_FINAL από PST_KAEK (KTM_ComputeDgmFromPst)
(defun c:DGMGM ( / tol pst dgmp texts loops loop has best bl len d cnt mrg
                  ss i dd merged)
  (setq tol (dgm:getreal "\nΑνοχή (m)" 0.001))
  (setq pst nil)
  (foreach it (dgm:collect '("PST_KAEK") nil)
    (if (caddr it) (setq pst (cons (cadr it) pst))))
  (setq dgmp nil)
  (foreach it (dgm:collect '("DGM_PROP_FINAL") nil)
    (if (caddr it) (setq dgmp (cons (cons (car it) (cadr it)) dgmp))))
  (setq texts nil)
  (setq ss (ssget "_X" '((0 . "TEXT,MTEXT") (8 . "PST_KAEK"))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq dd (entget (ssname ss i)))
        (setq texts (cons (list (cadr (assoc 10 dd)) (caddr (assoc 10 dd)))
                          texts))
        (setq i (1+ i)))))
  (cond
    ((null pst) (princ "\n** Δεν υπάρχουν κλειστά πολύγωνα PST_KAEK. **"))
    ((null dgmp) (princ "\n** Δεν υπάρχουν κλειστά πολύγωνα DGM_PROP_FINAL. **"))
    (t
     (setq loops (dgm:xorloops (append pst (mapcar 'cdr dgmp)) tol))
     (dgm:layer-std "DGM_PROP_FINAL")
     (setq cnt 0 mrg 0)
     (foreach loop loops
       (setq has nil)
       (foreach tx texts
         (if (dgm:inpoly tx loop) (setq has T)))
       (if has
         (progn
           (dgm:mkpoly loop "DGM_PROP_FINAL" T)
           (setq cnt (1+ cnt)))
         (progn
           (setq best nil bl 0.0)
           (foreach d dgmp
             (setq len (dgm:sharedlen loop (cdr d) tol))
             (if (> len bl) (setq bl len best d)))
           (if best
             (progn
               (setq merged (dgm:xorloops (list loop (cdr best)) tol))
               (if (= 1 (length merged))
                 (progn
                   (entdel (car best))
                   (dgm:mkpoly (car merged) "DGM_PROP_FINAL" T)
                   (setq dgmp (subst (cons (entlast) (car merged)) best dgmp))
                   (setq mrg (1+ mrg)))
                 (princ "\n[ΠΡΟΣΟΧΗ] Αποτυχία αυτόματης συνένωσης υπολοίπου - ελέγξτε χειροκίνητα.")))
             (princ "\n[ΠΡΟΣΟΧΗ] Υπόλοιπο χωρίς ΚΑΕΚ και χωρίς όμορο DGM_PROP_FINAL.")))))
     (princ (strcat "\nΔημιουργήθηκαν " (itoa cnt)
                    " νέα πολύγωνα DGM_PROP_FINAL και " (itoa mrg)
                    " υπόλοιπα συνενώθηκαν με όμορα."))))
  (princ))

;;; DGMAREAS - Αυτόματη δημιουργία AREA_A/AREA_D (KTM_CreateAreas)
(defun dgm:countlabels (lay / ss)
  (setq ss (ssget "_X" (list '(0 . "TEXT,MTEXT") (cons 8 lay))))
  (if ss (sslength ss) 0))

(defun c:DGMAREAS ( / tol h minw e1 e2 p d loops loop ip inp ind na nd w nm ar)
  (setq e1 (dgm:sel-poly "\nΕπιλέξτε το πολύγωνο PST_KAEK της ιδιοκτησίας: "))
  (setq e2 (if e1 (dgm:sel-poly "\nΕπιλέξτε το πολύγωνο DGM_PROP_FINAL της ιδιοκτησίας: ")))
  (if (and e1 e2)
    (if (or (not (dgm:closedp e1)) (not (dgm:closedp e2)))
      (princ "\n** Και τα δύο πολύγωνα πρέπει να είναι κλειστά. **")
      (progn
        (setq tol (dgm:getreal "\nΑνοχή (m)" 0.001))
        (setq h (dgm:getreal "\nΎψος χαρακτήρων ετικετών" dgm:*h*))
        (setq dgm:*h* h)
        (setq minw (dgm:getreal "\nΔιαγραφή πολυγώνων με πλάτος <" 0.01))
        (setq p (dgm:lwpts e1)
              d (dgm:lwpts e2))
        (setq loops (dgm:xorloops (list p d) tol))
        (dgm:layer-std "AREA_A")
        (dgm:layer-std "AREA_D")
        (dgm:layer-std "AREA_A-labels")
        (dgm:layer-std "AREA_D-labels")
        (setq na (1+ (dgm:countlabels "AREA_A-labels"))
              nd (1+ (dgm:countlabels "AREA_D-labels")))
        (foreach loop loops
          (setq ar (dgm:area loop)
                w (if (> (dgm:perim loop) 1e-9)
                    (/ (* 2.0 ar) (dgm:perim loop))
                    0.0))
          (if (< w minw)
            (princ (strcat "\n[INFO] Παραλείφθηκε τμήμα με πλάτος "
                           (rtos w 2 3) " m (όριο " (rtos minw 2 3) " m)."))
            (progn
              (setq ip (dgm:innerpt loop))
              (setq inp (and ip (dgm:inpoly ip p))
                    ind (and ip (dgm:inpoly ip d)))
              (cond
                ((and inp (not ind))
                 (setq nm (strcat "ΤΜΗΜΑ Α" (itoa na)) na (1+ na))
                 (dgm:mkpoly loop "AREA_A" T)
                 (if ip (dgm:textc ip h nm "AREA_A-labels"))
                 (princ (strcat "\n" nm " (αποκοπτόμενο): "
                                (rtos ar 2 2) " τ.μ.")))
                ((and ind (not inp))
                 (setq nm (strcat "ΤΜΗΜΑ Δ" (itoa nd)) nd (1+ nd))
                 (dgm:mkpoly loop "AREA_D" T)
                 (if ip (dgm:textc ip h nm "AREA_D-labels"))
                 (princ (strcat "\n" nm " (διεκδικούμενο): "
                                (rtos ar 2 2) " τ.μ.")))
                ((and inp ind)
                 (princ (strcat "\n[INFO] Κοινό τμήμα (εντός και των δύο): "
                                (rtos ar 2 2) " τ.μ. - παραλείπεται.")))
                (t
                 (princ "\n[ΠΡΟΣΟΧΗ] Τμήμα με ασαφή χαρακτηρισμό - τοποθετήθηκε στο DGM_CHECK.")
                 (dgm:layer "DGM_CHECK" 1)
                 (dgm:mkpoly loop "DGM_CHECK" T))))))
        (princ "\nΟλοκληρώθηκε η δημιουργία AREA_A/AREA_D."))))
  (princ))

;;; DGMCOPY - Αντιγραφή γραμμών TOPO_PROP σε άλλα layers
(defun c:DGMCOPY ( / typ src dst ss i e cnt)
  (princ "\nΑντιγραφή όλων των γραμμών TOPO_PROP σε:")
  (princ "\n  1 = DGM_PROP_FINAL   2 = BOUND_IMPL   3 = BOUND_UNIMPL")
  (setq typ (dgm:getint "\nΕπιλογή" 1))
  (setq src "TOPO_PROP"
        dst (cond ((= typ 2) "BOUND_IMPL")
                  ((= typ 3) "BOUND_UNIMPL")
                  (t "DGM_PROP_FINAL")))
  (dgm:layer-std dst)
  (setq ss (ssget "_X" (list '(0 . "LWPOLYLINE") (cons 8 src)))
        cnt 0)
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq e (ssname ss i))
        (dgm:mkpoly (dgm:lwpts e) dst (dgm:closedp e))
        (setq cnt (1+ cnt))
        (setq i (1+ i)))))
  (princ (strcat "\nΑντιγράφηκαν " (itoa cnt) " polylines από " src
                 " στο " dst "."))
  (princ))

;;; DGMTXT - Εξαγωγή συντεταγμένων κορυφών σε αρχείο TXT
(defun c:DGMTXT ( / f fn ss i e pts h nn)
  (princ "\nΕξαγωγή συντεταγμένων κορυφών επιλεγμένων polylines σε αρχείο TXT.")
  (setq ss (ssget '((0 . "LWPOLYLINE"))))
  (if ss
    (progn
      (setq fn (getfiled "Αποθήκευση συντεταγμένων" "" "txt" 1))
      (if fn
        (progn
          (setq f (open fn "w"))
          (dgm:marks-load)
          (setq h (if dgm:*h* dgm:*h* 0.5))
          (setq i 0)
          (while (< i (sslength ss))
            (setq e (ssname ss i)
                  pts (dgm:lwpts e))
            (foreach p pts
              (setq nn (dgm:findnum p (* 2.5 h)))
              (write-line (strcat (if nn nn "-") "\t"
                                  (rtos (car p) 2 3) "\t"
                                  (rtos (cadr p) 2 3))
                          f))
            (setq i (1+ i)))
          (close f)
          (princ (strcat "\nΓράφτηκε το αρχείο: " fn))))))
  (princ))

;;; DGMPREP - Προετοιμασία σχεδίου για υποβολή (KTM_PrepareDwg)
(defun dgm:hasxref ( / b r)
  (setq b (tblnext "BLOCK" T) r nil)
  (while b
    (if (= 4 (logand 4 (cdr (assoc 70 b)))) (setq r T))
    (setq b (tblnext "BLOCK")))
  r)

(defun dgm:delall (etype / ss i cnt)
  (setq ss (ssget "_X" (list (cons 0 etype))) cnt 0)
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (entdel (ssname ss i))
        (setq cnt (1+ cnt) i (1+ i)))))
  cnt)

(defun c:DGMPREP ( / a1 a2 a3 a4 ax ss i n)
  (princ "\nΠροετοιμασία σχεδίου για υποβολή (1 = Ναι, 0 = Όχι):")
  (setq a1 (dgm:getint "\nΔιαγραφή εικόνων (Images)" 1))
  (setq a2 (dgm:getint "\nΔιάλυση blocks (Explode)" 1))
  (setq a3 (dgm:getint "\nΔιαγραφή αντικειμένων OLE" 1))
  (setq a4 (dgm:getint "\nΔιαγραφή Proxy entities" 1))
  (princ "\nXrefs:  0 = καμία ενέργεια   1 = Detach όλα   2 = Bind όλα")
  (setq ax (dgm:getint "\nΕπιλογή" 1))
  (if (= a1 1)
    (princ (strcat "\nΔιαγράφηκαν " (itoa (dgm:delall "IMAGE")) " εικόνες.")))
  (if (= a3 1)
    (princ (strcat "\nΔιαγράφηκαν " (itoa (dgm:delall "OLE2FRAME")) " OLE.")))
  (if (= a4 1)
    (princ (strcat "\nΔιαγράφηκαν "
                   (itoa (dgm:delall "ACAD_PROXY_ENTITY")) " proxies.")))
  (if (= a2 1)
    (progn
      (setq n 0)
      (setq ss (ssget "_X" '((0 . "INSERT"))))
      (while (and ss (< n 5))
        (setq i 0)
        (while (< i (sslength ss))
          (command "_.EXPLODE" (ssname ss i))
          (setq i (1+ i)))
        (setq n (1+ n))
        (setq ss (ssget "_X" '((0 . "INSERT")))))
      (princ "\nΈγινε διάλυση των blocks.")))
  (if (and (/= ax 0) (dgm:hasxref))
    (if (= ax 2)
      (command "_.-XREF" "_B" "*")
      (command "_.-XREF" "_D" "*")))
  (princ "\nΗ προετοιμασία ολοκληρώθηκε. Εκτελέστε DGMC για τελικό έλεγχο.")
  (princ))

;;; DGMZIP - Δημιουργία ZIP παραδοτέων (DXF + υπογεγραμμένο PDF)
(defun c:DGMZIP ( / dxf pdf zip cmd)
  (setq dxf (getfiled "Επιλογή αρχείου DXF" "" "dxf" 0))
  (setq pdf (if dxf (getfiled "Επιλογή υπογεγραμμένου PDF" "" "pdf" 0)))
  (if (and dxf pdf)
    (progn
      (setq zip (strcat (substr dxf 1 (- (strlen dxf) 4)) ".zip"))
      (setq cmd (strcat "powershell -NoProfile -Command \"Compress-Archive -Force -LiteralPath '"
                        dxf "','" pdf "' -DestinationPath '" zip "'\""))
      (dgm:runsync cmd)
      (if (findfile zip)
        (princ (strcat "\nΔημιουργήθηκε το αρχείο παράδοσης: " zip))
        (princ "\n** Το ZIP δεν δημιουργήθηκε - ελέγξτε τα ονόματα αρχείων. **"))))
  (princ))

;;; DGMSIGN - Άνοιγμα του PDF στο JSignPdf για ψηφιακή υπογραφή
(defun c:DGMSIGN ( / js pdf)
  (setq js (getenv "GFD_JSIGNPDF"))
  (if (or (null js) (null (findfile js)))
    (progn
      (princ "\nΕντοπίστε το εκτελέσιμο του JSignPdf (μία φορά - αποθηκεύεται).")
      (setq js (getfiled "Εντοπισμός JSignPdf.exe" "" "exe" 0))
      (if js (setenv "GFD_JSIGNPDF" js))))
  (if js
    (progn
      (setq pdf (getfiled "PDF προς υπογραφή" "" "pdf" 0))
      (if pdf
        (progn
          (startapp (strcat "\"" js "\"") (strcat "\"" pdf "\""))
          (princ "\nΆνοιξε το JSignPdf. Υπογράψτε το PDF και μετά εκτελέστε DGMZIP.")))))
  (princ))

;;; ================= ΚΛΙΜΑΚΑ - ΚΑΝΑΒΟΣ - ΒΟΡΡΑΣ =========================

;; Ερώτηση κλίμακας σχεδίασης - επιστρέφει και αποθηκεύει τον παρονομαστή
(defun dgm:askscale ( / k def)
  (setq def (cond ((null dgm:*scale*) 3)
                  ((= dgm:*scale* 100) 1)
                  ((= dgm:*scale* 200) 2)
                  ((= dgm:*scale* 1000) 4)
                  ((= dgm:*scale* 2000) 5)
                  (t 3)))
  (princ "\nΚλίμακα σχεδίασης:")
  (princ "\n  1 = 1:100   2 = 1:200   3 = 1:500   4 = 1:1000   5 = 1:2000")
  (setq k (dgm:getint "\nΕπιλογή" def))
  (setq dgm:*scale*
        (cond ((= k 1) 100) ((= k 2) 200) ((= k 4) 1000)
              ((= k 5) 2000) (t 500)))
  ;; προτεινόμενο ύψος κειμένων = 2 mm στην εκτύπωση
  (setq dgm:*h* (* 0.002 dgm:*scale*))
  dgm:*scale*)

;;; DGMSCALE - Ορισμός κλίμακας σχεδίασης
(defun c:DGMSCALE ( / m)
  (setq m (dgm:askscale))
  (setvar "LTSCALE" (/ m 100.0))
  (princ (strcat "\nΟρίστηκε κλίμακα σχεδίασης 1:" (itoa m)
                 ". Προτεινόμενο ύψος κειμένων: "
                 (rtos dgm:*h* 2 2) " m (2 mm εκτύπωσης), LTSCALE = "
                 (rtos (/ m 100.0) 2 2) "."))
  (princ))

;; Σύμβολο βορρά στο σημείο pt (κατακόρυφο, προς τα πάνω), μέγεθος ανά κλίμακα
(defun dgm:north (pt m / x y r)
  (setq x (car pt) y (cadr pt)
        r (* 0.01 m))                       ; ακτίνα κύκλου = 10 mm εκτύπωσης
  (dgm:layer "KANABOS" 8)
  ;; κύκλος
  (dgm:circle (list x y) r "KANABOS")
  ;; βελόνα (κλειστό περίγραμμα): κορυφή - δεξιά βάση - κέντρο - αριστερή βάση
  (dgm:mkpoly (list (list x (+ y (* 1.30 r)))
                    (list (+ x (* 0.30 r)) (- y (* 0.80 r)))
                    (list x (- y (* 0.40 r)))
                    (list (- x (* 0.30 r)) (- y (* 0.80 r))))
              "KANABOS" T)
  ;; γεμισμένο αριστερό μισό της βελόνας
  (entmake (list '(0 . "SOLID") '(8 . "KANABOS")
                 (cons 10 (list x (+ y (* 1.30 r)) 0.0))
                 (cons 11 (list (- x (* 0.30 r)) (- y (* 0.80 r)) 0.0))
                 (cons 12 (list x (- y (* 0.40 r)) 0.0))
                 (cons 13 (list x (- y (* 0.40 r)) 0.0))))
  ;; γράμμα Β πάνω από την κορυφή
  (dgm:textc (list x (+ y (* 1.75 r))) (* 0.65 r) "Β" "KANABOS")
  (princ))

;;; DGMGRID - Κάναβος σχεδίασης + σύμβολο βορρά
(defun c:DGMGRID ( / m step p1 p2 xmin ymin xmax ymax x0 y0 x y arm h lbl
                    dec pt)
  ;; α) κλίμακα, αν δεν έχει οριστεί
  (if (null dgm:*scale*)
    (progn
      (princ "\nΔεν έχει οριστεί κλίμακα σχεδίασης.")
      (dgm:askscale)))
  (setq m dgm:*scale*)
  ;; β) βήμα κανάβου
  (setq step (dgm:getreal "\nΒήμα κανάβου (m)" (/ m 10.0)))
  (if (<= step 0.0) (setq step (/ m 10.0)))
  (setq p1 (getpoint "\nΠρώτη γωνία περιοχής κανάβου: "))
  (setq p2 (if p1 (getcorner p1 "\nΑπέναντι γωνία: ")))
  (if (and p1 p2)
    (progn
      (setq xmin (min (car p1) (car p2))  xmax (max (car p1) (car p2))
            ymin (min (cadr p1) (cadr p2)) ymax (max (cadr p1) (cadr p2)))
      ;; ευθυγράμμιση σε ακέραια πολλαπλάσια του βήματος (ΕΓΣΑ '87)
      (setq x0 (* step (fix (/ xmin step))))
      (while (< x0 xmin) (setq x0 (+ x0 step)))
      (setq y0 (* step (fix (/ ymin step))))
      (while (< y0 ymin) (setq y0 (+ y0 step)))
      (dgm:layer "KANABOS" 8)
      (setq arm (* 0.0025 m)               ; μισό σκέλος σταυρού = 2.5 mm
            h   (* 0.002 m)                ; ύψος κειμένων = 2 mm
            dec (if (equal step (float (fix step)) 1e-9) 0 2))
      (setq lbl (dgm:getint "\nΑναγραφή συντεταγμένων περιμετρικά; 1 = Ναι, 0 = Όχι" 1))
      (setq x x0)
      (while (<= x (+ xmax 1e-9))
        (setq y y0)
        (while (<= y (+ ymax 1e-9))
          ;; σταυρός κανάβου
          (dgm:line (list (- x arm) y) (list (+ x arm) y) "KANABOS")
          (dgm:line (list x (- y arm)) (list x (+ y arm)) "KANABOS")
          (if (= lbl 1)
            (progn
              ;; τιμές Χ κάτω από την πρώτη σειρά, τιμές Υ αριστερά της πρώτης στήλης
              (if (equal y y0 1e-9)
                (dgm:textc (list x (- y0 (* 2.2 h))) h (rtos x 2 dec) "KANABOS"))
              (if (equal x x0 1e-9)
                (dgm:textc (list (- x0 (* 4.5 h)) y) h (rtos y 2 dec) "KANABOS"))))
          (setq y (+ y step)))
        (setq x (+ x step)))
      (princ (strcat "\nΣχεδιάστηκε κάναβος με βήμα " (rtos step 2 dec)
                     " m (κλίμακα 1:" (itoa m) ") στο layer KANABOS."))
      ;; σύμβολο βορρά
      (setq pt (getpoint "\nΘέση συμβόλου βορρά (Enter για παράλειψη): "))
      (if pt (dgm:north pt m))))
  (princ))

;;; DGMHELP - Βοήθεια
(defun c:DGMHELP ()
  (princ "\n----------------- Εντολές GF-DGM -----------------")
  (princ "\nΠροετοιμασία:")
  (princ "\n  DGML      Δημιουργία layers ανά τύπο διαγράμματος")
  (princ "\n  DGMSCALE  Κλίμακα σχεδίασης (1:100 ως 1:2000)")
  (princ "\n  DGMGRID   Κάναβος σχεδίασης + σύμβολο βορρά")
  (princ "\n  DGMCLEAN  Αυτόματος καθαρισμός τυπικών σφαλμάτων")
  (princ "\n  DGMC      Έλεγχος ορθότητας σχεδίου")
  (princ "\nΠολύγωνα:")
  (princ "\n  DGMBND    Πολύγωνο από εσωτερικό σημείο")
  (princ "\n  DGMBNDPD  Πολύγωνο από σημείο (μόνο PST_KAEK/DGM_PROP_FINAL)")
  (princ "\n  DGMKAT    DGM_PROP_FINAL: κατάτμηση PST_KAEK με LINE_XM")
  (princ "\n  DGMGM     DGM_PROP_FINAL: αυτόματα υπόλοιπα από PST_KAEK")
  (princ "\n  DGMVST    VST_FINAL από LINE_XM_VST")
  (princ "\n  DGMAREAS  Αυτόματη δημιουργία AREA_A / AREA_D")
  (princ "\n  DGMSPLIT  Κατάτμηση πολυγώνου με γραμμή κοπής")
  (princ "\n  DGMUNION  Συνένωση όμορων πολυγώνων")
  (princ "\n  DGMCUT    Αποκοπή τμήματος από γεωτεμάχιο")
  (princ "\n  DGMA      Χειροκίνητος χαρακτηρισμός τμήματος Α/Δ")
  (princ "\n  DGMCOPY   Αντιγραφή TOPO_PROP σε άλλα layers")
  (princ "\nΑρίθμηση - Πίνακες:")
  (princ "\n  DGMK      Αρίθμηση κορυφών polyline")
  (princ "\n  DGMORIGIN Αλλαγή αρχής/φοράς κλειστής polyline")
  (princ "\n  DGMP      Πίνακας συντεταγμένων κορυφών")
  (princ "\n  DGME      Πίνακας αρχικών/τελικών εμβαδών")
  (princ "\n  DGMT      Πίνακας για τη διόρθωση των γεωτεμαχίων")
  (princ "\n  DGMTXT    Εξαγωγή συντεταγμένων σε TXT")
  (princ "\nΠαραδοτέα:")
  (princ "\n  DGMPREP   Προετοιμασία σχεδίου (images/blocks/OLE/xrefs)")
  (princ "\n  DGMHASH   Hash SHA512 αρχείου DXF")
  (princ "\n  DGMKHD    Τοποθέτηση ΚΗΔ στο σχέδιο")
  (princ "\n  DGMSIGN   Υπογραφή PDF με JSignPdf")
  (princ "\n  DGMZIP    ZIP παραδοτέων (DXF + υπογεγραμμένο PDF)")
  (princ "\n---------------------------------------------------")
  (princ))

(princ "\nDGM.lsp: Φορτώθηκαν τα εργαλεία ΔΓΜ. Πληκτρολογήστε DGMHELP για λίστα εντολών.")
(princ)
