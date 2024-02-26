(quicklisp:quickload "alexandria")
(quicklisp:quickload "uiop")

(defparameter
    *raw*
  (read-from-string
   (concatenate
    'string
    "'"
    (uiop:read-file-string "SpaceCadet.kicad_pcb"))))

(defun find-nodes (tree)
  (cond
    ((null tree) nil)
    ((atom tree) nil)
    ((eq (car tree) 'FOOTPRINT)
     (let* ((footprint-name (cadr tree))
            (footprint-rest (cddr tree))
            (dep-property (find-dep-property footprint-rest)))
         (if dep-property
             (list (cons footprint-name dep-property))
             nil)))
    (t (mapcan #'find-nodes (cdr tree)))))

(defun find-dep-property (subtree)
  (cond
    ((null subtree) nil)
    ((atom subtree) nil)
    ((and (listp (car subtree))
          (eq (caar subtree) 'PROPERTY)
          (stringp (cadr (car subtree)))
          (string= (cadr (car subtree)) "dep"))
     (print (caddar subtree))
     ;; (print (car subtree))
     )
    (t (find-dep-property (cdr subtree)))))

;; Пример использования
(defparameter *raw2*
  '(KICAD_PCB (VERSION 20240108) (GENERATOR "pcbnew") (GENERATOR_VERSION "8.0")
    (GENERAL (THICKNESS 1.6) (LEGACY_TEARDROPS NO)) (PAPER "A2")
    (LAYERS
     (0 "F.Cu" SIGNAL)
     (31 "B.Cu" SIGNAL))
    (SETUP
     (STACKUP (LAYER "F.SilkS" (TYPE "Top Silk Screen"))
      (LAYER "F.Mask" (TYPE "Top Solder Mask") (THICKNESS 0.01))
      (LAYER "F.Cu" (TYPE "copper") (THICKNESS 0.035))
      (LAYER "dielectric 1" (TYPE "core") (THICKNESS 1.51) (MATERIAL "FR4")
       (EPSILON_R 4.5) (LOSS_TANGENT 0.02))
      (LAYER "B.Cu" (TYPE "copper") (THICKNESS 0.035))
      (LAYER "B.Mask" (TYPE "Bottom Solder Mask") (THICKNESS 0.01))
      (COPPER_FINISH "None")
      (DIELECTRIC_CONSTRAINTS NO))
     (PAD_TO_MASK_CLEARANCE 0) (ALLOW_SOLDERMASK_BRIDGES_IN_FOOTPRINTS NO)
     (GRID_ORIGIN 320.35 96.35))
    (NET 0 "") (NET 2 "GND") (NET 3 "COL4") (NET 4 "ROW0")
    (FOOTPRINT "Button_Switch_Keyboard:SW_MX_2U"
     (LAYER "F.Cu")
     (UUID "7aa37245-bf7d-4e47-ada1-02a427f4a700")
     (PROPERTY "Reference" "S_TERMINAL1" (AT 4.5 5.75 0) (LAYER "F.SilkS") (UUID "3878a3ad-87a6-40fb-a546-9e15cbd3129d"))
     (PROPERTY "dep" "S_MACRO1"
      (AT 0 0 0) (UNLOCKED YES) (LAYER "F.Fab") (HIDE YES) (UUID "56d09925-b698-45b4-90a8-64fd14cb5201")
      (EFFECTS (FONT (SIZE 1 1) (THICKNESS 0.15)))))
    (FOOTPRINT "Another_Footprint"
     (LAYER "F.Cu")
     (PROPERTY "Reference" "S_TERMINAL2" (AT 4.5 5.75 0) (LAYER "F.SilkS")
      (UUID "3878a3ad-87a6-40fb-a546-9e15cbd3129d"))
     (PROPERTY "dep" "S_MACRO2"
      (AT 0 0 0) (UNLOCKED YES) (LAYER "F.Fab") (HIDE YES)
      (UUID "56d09925-b698-45b4-90a8-64fd14cb5201")
      (EFFECTS (FONT (SIZE 1 1) (THICKNESS 0.15)))))
    (FOOTPRINT "Diode_SMD:D_SOD-123" (LAYER "F.Cu")
     (UUID "79f526b6-fe1d-4c59-add9-ef4f80c2ee06") (AT 399.058 487.366)
     (DESCR "SOD-123") (TAGS "SOD-123")
     (PROPERTY "Reference" "D_RIGHT_SPACE1" (AT 0 -2 0) (LAYER "F.SilkS")
      (UUID "14ac8a9c-a948-4992-b4e2-cca7a399b33e")
      (EFFECTS (FONT (SIZE 1 1) (THICKNESS 0.15))))
     (PROPERTY "Value" "D" (AT 0 2.1 0) (LAYER "F.Fab")
      (UUID "1543bc95-9299-4666-8de4-6d2498fd5fd0")
      (EFFECTS (FONT (SIZE 1 1) (THICKNESS 0.15))))
     (PROPERTY "Footprint" "Diode_SMD:D_SOD-123" (AT 0 0 0) (UNLOCKED YES)
      (LAYER "F.Fab") (HIDE YES) (UUID "20d1b7f7-85ff-4234-9c81-00e17a126080")
      (EFFECTS (FONT (SIZE 1.27 1.27))))
     (PROPERTY "Datasheet" "" (AT 0 0 0) (UNLOCKED YES) (LAYER "F.Fab")
      (HIDE YES) (UUID "d2f0920e-38af-4616-b9cd-a4c76bf466d5")
      (EFFECTS (FONT (SIZE 1.27 1.27))))
     (PROPERTY "Description" "Diode, small symbol" (AT 0 0 0) (UNLOCKED YES)
      (LAYER "F.Fab") (HIDE YES) (UUID "734ac16b-073b-4d9e-857b-9baef79c42f7")
      (EFFECTS (FONT (SIZE 1.27 1.27))))
     (MODEL "${KICAD8_3DMODEL_DIR}/Diode_SMD.3dshapes/D_SOD-123.wrl"
      (OFFSET (XYZ 0 0 0)) (SCALE (XYZ 1 1 1)) (ROTATE (XYZ 0 0 0))))))

(print
 (find-nodes *raw2*))
