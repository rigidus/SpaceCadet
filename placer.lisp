(quicklisp:quickload "alexandria")
(quicklisp:quickload "uiop")
(quicklisp:quickload "split-sequence")
(quicklisp:quickload "parse-number")

(defparameter *input-file* "SpaceCadet.kicad_pcb")

(defparameter *raw* (read-from-string
                     (uiop:read-file-string *input-file*)))

(defparameter *output-file* "test4.kicad_pcb")

(defconstant +unit-size+ 19.05)

;; (print *raw*)

(defun replacer (tree fn)
  (cond
    ((null tree) nil)
    ((atom tree) tree)
    ((let ((it (funcall fn (car tree))))
       (unless (null it)
         (cons it
               (replacer (cdr tree) fn)))))
    (t
     (cons (car tree)
           (replacer (cdr tree) fn)))))

(defun find-sublist (subtree first &optional aux)
  (cond
    ((null subtree) nil)
    ((atom subtree) nil)
    ((and (listp subtree)
          (atom (car subtree))
          (symbolp (car subtree))
          (equal first (car subtree))
          (if aux
              (funcall aux subtree)
              t))
     subtree) ; Возвращаем subtree
    ((atom (car subtree))
     (find-sublist (cdr subtree) first aux))
    (t (or (find-sublist (car subtree) first aux)
           (find-sublist (cdr subtree) first aux)))))

(defun find-property (subtree property)
  (caddr
   (find-sublist subtree 'PROPERTY
                 #'(lambda (subtree)
                     (and (stringp (cadr subtree))
                          (string= property (cadr subtree)))))))

;; (find-property *raw* "dep")

(defun process-footprint-for-dag (node)
  "use special var *dep-dag*"
  (if (and (listp node)
           (equal (car node) 'FOOTPRINT))
      (let ((name (cadr node))
            (dep (find-property node "dep")))
        ;; (print (list :name name :dep dep))
        (when dep
          (push (list :name name
                      :foot (cadr node)
                      :dep (find-property node "dep")
                      :ref (find-property node "Reference"))
                *dep-dag*)))
      ;; else
      node))

(defun get-graph (dep-dag)
  "building dag on hash-table"
  (let ((graph (make-hash-table :test #'equal)))
    (loop for node in dep-dag do
      (let ((dep  (getf node :DEP))
            (ref  (getf node :REF)))
        (if (equal 'none (gethash ref graph 'none))
            (setf (gethash ref graph) (list dep))
            (setf (gethash ref graph) (pushnew dep (gethash ref graph))))))
    graph))

(defun get-top-vertexes (graph)
  "finding top-vertexes" ;; узлы без зависимостей
  (let ((top-vertexes))
    (loop for val being the hash-values of graph
            using (hash-key key) do
              (progn
                ;; (format t "~&~A -> ~{~A~}" key val)
                (loop for item in val do
                  (when (equal 'none (gethash item graph 'none))
                    (pushnew item top-vertexes)))))
    top-vertexes))

;; представим себе картинку клавиатуры: данная функция строит цепочки кнопок
;; порядово от левого края к правому. Где в начале ряда самая левая кнопка -
;; т.е. такой узел, у которого нет зависимостей от других узлов.
(defun get-chains (graph top-vertexes)
  "get chains from dag from top-vetrexes"
  (let ((chains))
    (labels ((find-next (vertex)
               ;; найти элемент, для которого vertex - одна из зависимостей
               (loop for deps-list being the hash-values of graph
                       using (hash-key node-name) do
                         (if (member vertex deps-list :test #'string=)
                             (return-from find-next node-name))))
             (chain (starter)
               ;; получить список узлов, где
               ;; top (вершины графа) - это зависимости
               (loop for next = (find-next starter)
                     until (null next)
                     collect next
                     do (setf starter next))))
      (loop for top in top-vertexes do
        (push (list* top (chain top)) chains)))
    chains))

;; (setf *raw* `(footprint "Button_Switch_Keyboard:SW_MX_2U"
;;                         (layer "F.Cu")
;;                         (uuid "02a262cb-470c-40ca-be1a-8e82f454b693")
;;                         (at 330.788 442.106)
;;                         (property "Reference" "S_N1"
;;                                   (at 4.5 5.75 0)
;;                                   (layer "F.SilkS")
;;                                   (uuid "438b4544-75f2-4bed-a678-b76fb2d2dbc5")
;;                                   (effects
;;                                    (font
;;                                     (size 1 1)
;;                                     (thickness 0.12)
;;                                     )
;;                                    (justify right top)
;;                                    )
;;                                   )
;;                         (property "Value" "N"
;;                                   (at -4 -8.5 0)
;;                                   (layer "F.Fab")
;;                                   (uuid "a6c609e8-09bf-4903-a062-ee7f1f362442")
;;                                   (effects
;;                                    (font
;;                                     (size 1 1)
;;                                     (thickness 0.12)
;;                                     )
;;                                    (justify left top)
;;                                    )
;;                                   )))

(defun tab (cnt)
  (make-string cnt :initial-element #\Tab))

;; (tab 12)

(defmacro bprint (var)
  `(subseq (with-output-to-string (*standard-output*)  (pprint ,var)) 1))

(defun fancy-print-rec (expr out level)
  (cond ((null expr) (format out ")"))
        ((listp (car expr))
         (progn
           (format out "~% ~a(" (tab (+ 1 level)))
           (fancy-print-rec (car expr) out (+ 1 level))
           (fancy-print-rec (cdr expr) out level)))
        (t (progn
             (if (symbolp (car expr))
                 (format out "~a " (string-downcase
                                    (symbol-name (car expr))))
                 (format out "~a " (bprint
                                    (car expr))))
             (fancy-print-rec (cdr expr) out level)))))

(defun fancy-print (expr &optional file)
  (if file
      (with-open-file (out file :direction :output :if-exists :supersede)
        (format out "(")
        (fancy-print-rec expr out 0))
      ;; else
      (progn
        (format t "(")
        (fancy-print-rec expr t 0))))

;; (fancy-print *raw*)

(defun read-button-size (string)
  (loop for word in (split-sequence:split-sequence #\_ string) do
    (when (string= "u" (string-downcase
                        (subseq word  (- (length word) 1))))
      (return-from read-button-size
        (parse-number:parse-real-number word :end (- (length word) 1)))))
  (error 'read-button-size-error))

;; (read-button-size "Button_Switch_Keyboard:SW_MX_1.25U")
;; (read-button-size "Button_Switch_Keyboard:SW_Cherry_MX_1.00u_PCB")

(defun caclulate-button-size (string)
  (* +unit-size+ (read-button-size string)))

(defun calculate-new-x (cur-x-coord prev-button-size cur-button-size)
  (+ cur-x-coord (/ prev-button-size 2) (/ cur-button-size 2)))

(let ((*dep-dag*))
  (declare (special *dep-dag*))
  ;; Получаем несортированный набор кнопок,
  ;; ссылающихся друг на друга
  (replacer *raw* #'process-footprint-for-dag)
  ;; (print *dep-dag*)
  (let* (;; Получаем хэш-таблицу, где ключи -
         ;; это кнопки, а значения - зависимости
         (graph (get-graph *dep-dag*))
         ;; Получаем список узлов без зависимостей
         (top-vertexes (get-top-vertexes graph))
         ;; Получаем цепочку, построенную слева
         ;; направо от узлов без зависимостей
         (chains (get-chains graph top-vertexes))
         ;; Переменная для изменений
         (replaced *raw*))

    (fancy-print *raw*)

    ;; (print *dep-dag*)

    ;; (maphash #'(lambda (k v)
    ;;              (print (list :k k :v v)))
    ;;          graph)

    ;; (print top-vertexes)

    ;; go through chains (ordered rows of buttons as they exist on keyboard)
    ;; (loop for chain in chains do
    ;;   (let ((top  (car chain))
    ;;         (rest (cdr chain))
    ;;         (curr-x)
    ;;         (curr-y)
    ;;         (prev-button-size))
    ;;     ;;(format t "current chain is ~a ~%" chain)
    ;;     ;; get coordinates of the most left button
    ;;     (replacer *raw*
    ;;               #'(lambda (node)
    ;;                   (if (and (listp node)
    ;;                            (equal (car node) 'FOOTPRINT))
    ;;                       (let ((name (cadr node))
    ;;                             (ref (find-property node "Reference"))
    ;;                             (at (find-sublist node 'AT)))
    ;;                         (when (string= top ref)
    ;;                           (setf curr-x (cadr at))
    ;;                           (setf curr-y (caddr at))
    ;;                           (setf prev-button-size (caclulate-button-size (cadr node)))
    ;;                           (print (list :name name :ref ref :x curr-x :y curr-y))
    ;;                           )))
    ;;                   node))
    ;;     ;; go through each chain (row) except the most left button and change coordinates of buttons
    ;;     ;; according their row beginning
    ;;     (loop for next = (prog1 (car rest) (setf rest (cdr rest))) until (null next) do
    ;;       ;;(format t "~% current button is ~a ~%" next)
    ;;       (setf replaced
    ;;             (replacer replaced
    ;;                       #'(lambda (node)
    ;;                           (if (and (listp node)
    ;;                                    (equal (car node) 'FOOTPRINT))
    ;;                               (let ((ref (find-property node "Reference")))
    ;;                                 (if (string= ref next) ;; if name of node is name of button
    ;;                                     (replacer node ;; replace coordinates
    ;;                                               #'(lambda (footnode)
    ;;                                                   (if (and (listp footnode)
    ;;                                                            (equal (car footnode) 'AT))
    ;;                                                       (let* ((button-size (caclulate-button-size (cadr node)))
    ;;                                                              (new-x (calculate-new-x curr-x
    ;;                                                                                      prev-button-size
    ;;                                                                                      button-size)))
    ;;                                                         ;; (print (list :ref ref :prev-x curr-x :new-x new-x :prev-button-size prev-button-size :cur-button-size button-size))
    ;;                                                         ;; (print "---------------")
    ;;                                                         (setf curr-x new-x)
    ;;                                                         (setf prev-button-size button-size)
    ;;                                                         (list 'AT
    ;;                                                               new-x
    ;;                                                               curr-y))
    ;;                                                       ;; else
    ;;                                                       footnode)))))

    ;;                               ;; else
    ;;                               nil))
    ;;                       )))
    ;;     (fancy-print replaced *output-file*)))
    ))
