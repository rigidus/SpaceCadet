(quicklisp:quickload "alexandria")
(quicklisp:quickload "uiop")

(defparameter
    *raw*
  (read-from-string
   (concatenate
    'string
    "'"
    (uiop:read-file-string "SpaceCadet.kicad_pcb"))))

(print *raw*)

(setf *raw*
      '(KICAD_PCB (VERSION 20240108) (GENERATOR "pcbnew") (GENERATOR_VERSION "8.0")
        (GENERAL (THICKNESS 1.6) (LEGACY_TEARDROPS NO)) (PAPER "A2")
        (LAYERS (0 "F.Cu" SIGNAL) (31 "B.Cu" SIGNAL) (32 "B.Adhes" USER "B.Adhesive")
         (33 "F.Adhes" USER "F.Adhesive") (34 "B.Paste" USER) (35 "F.Paste" USER)
         (36 "B.SilkS" USER "B.Silkscreen") (37 "F.SilkS" USER "F.Silkscreen")
         (38 "B.Mask" USER) (39 "F.Mask" USER) (40 "Dwgs.User" USER "User.Drawings"))))

(defun find-in-tree (item tree &key (test #'eql))
  (labels ((find-in-tree-aux (tree)
             (cond ((funcall test item tree)
                    (return-from find-in-tree tree))
                   ((consp tree)
                    (find-in-tree-aux (car tree))
                    (find-in-tree-aux (cdr tree))))))
    (find-in-tree-aux tree)))

(find-in-tree
 "dummy" *raw*
 :test #'(lambda (a b)
           ;; (print b)
           (and
            (listp b)
            (> (length b) 2)
            (equal (car b) 'FOOTPRINT)
            )))


(find-in-tree
 "dummy" *raw*
 :test #'(lambda (a b)
           ;; (print b)
           (and
            (listp b)
            (> (length b) 2)
            (equal (car b) 'PROPERTY)
            (equal (cadr b) "dep")
            )))
