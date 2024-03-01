(quicklisp:quickload "alexandria")
(quicklisp:quickload "uiop")

(defparameter
    *raw*
  (read-from-string
   (uiop:read-file-string "SpaceCadet.kicad_pcb")))

(print *raw*)

(defparameter *raw2*
  '(KICAD_PCB
    (FOOTPRINT "Button_Switch_Keyboard:SW_MX_2U" (AT 86.76 138.865)
     (PROPERTY "Reference" "S_MACRO1" (AT 4.5 5.75 0)))
    (FOOTPRINT "Another_Footprint"
     (PROPERTY "Reference" "S_TERMINAL2" (AT 4.5 5.75 0))
     (PROPERTY "dep" "S_MACRO2" (AT 0 0 0)))
    (FOOTPRINT "Diode_SMD:D_SOD-123" (AT 399.058 487.366)
     (PROPERTY "Reference" "D_RIGHT_SPACE1" (AT 0 -2 0)))))

(defun tree-cnt (tree)
  (cond
    ((null tree) 0)
    ((atom tree) 1)
    (t (+ 1 (count-elements (car tree))
          (count-elements (cdr tree))))))

;; (tree-cnt *raw2*)

(defun replacer (tree fn)
  (cond
    ((null tree) nil)
    ((atom tree) tree)
    ((let ((it (funcall fn (car tree))))
       (cons it
             (replacer (cdr tree) fn))))
    (t
     (cons (car tree)
           (replacer (cdr tree) fn)))))

(defun find-property (subtree property)
  (cond
    ((null subtree) nil)
    ((atom subtree) nil)
    ((and (listp subtree)
          (atom (car subtree))
          (symbolp (car subtree))
          (equal 'PROPERTY (car subtree))
          (stringp (cadr subtree))
          (string= property (cadr subtree)))
     (caddr subtree)) ; Возвращаем текущий узел
    ((atom (car subtree))
     (find-property (cdr subtree) property))
    (t (or (find-property (car subtree) property)
           (find-property (cdr subtree) property)))))

;; (find-property *raw* "dep")

(defun process-footprint-for-dag (node)
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

(let ((*dep-dag*))
  (declare (special *dep-dag*))
  (replacer *raw* #'process-footprint-for-dag)
  ;; (print *dep-dag*)
  (let ((graph (make-hash-table :test #'equal))
        (top-vertexes)) ;; узлы без зависимостей
    (loop for node in *dep-dag* do
      (let ((dep  (getf node :DEP))
            (ref  (getf node :REF)))
        (when (equal 'none (gethash ref graph 'none))
          (setf (gethash ref graph) (list dep))
          (setf (gethash ref graph) (pushnew dep (gethash ref graph))))
        ))
    (loop for val being the hash-values of graph
            using (hash-key key) do
              (progn
                (format t "~&~A -> ~{~A~}" key val)
                (loop for item in val :do
                  (when (equal 'none (gethash item graph 'none))
                    (pushnew (car val) top-vertexes)))))
    (labels ((find-next (vertex)
               (loop for val being the hash-values of graph
                       using (hash-key key) do
                         (if (member vertex val :test #'string=)
                             (return-from find-next key))))
             (chain (starter)
               (loop for next = (find-next starter)
                     until (null next)
                     collect next
                     do (setf starter next))))
      (loop for top in top-vertexes do
        (print (list :top top :chain (chain top)))))
    top-vertexes
    ))
