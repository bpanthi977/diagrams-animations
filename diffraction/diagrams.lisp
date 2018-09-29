(ql:quickload :vecto)
(defpackage :diag
  (:use :cl :vecto))
(in-package :diag)


(defun draw-line (ix iy r θ)
  "Draws a line from ix,iy in the direction θ and length r"
  (move-to ix iy)
  (line-to (+ ix (* r (cos θ))) (+ iy (* r (sin θ)))))

(defun draw-arrow (x y dir)
  "Draw an arrow at x,y pointing in dir direction"
  (draw-line x y 10 (+ dir pi (/ pi -6)))
  (draw-line x y 10 (- dir pi (/ pi -6)))
  (move-to x y))

(defun phasor-diagram (n δβ a file &key resultant arrows textn textβ (ix 100) (iy 10) (width 700) (height 700))
  "Creates the phasor diagram and saves it to file"
  (let ((angle 0) x y)
    (with-canvas (:width width :height height)
      (setf x ix y iy)
      ;; set the pen and start drawing
      (move-to x y)
      (loop for i from 1 to n do 
	   (setf x (+ x (* a (cos angle)))
		 y (+ y (* a (sin angle))))
	   (line-to x y)
	   (when arrows
	     (draw-arrow x y angle))
	   (incf angle δβ))
      (stroke)
      ;; show n = sth text ?
      (when textn
	(set-font (get-font (merge-pathnames "Roboto-Light.ttf" *load-pathname*)) 40)
	(draw-string 10 (- height 50) (format nil "n= ~a" n))
	(stroke))
      ;; show β = sth text ?
      (when textβ
	(set-font (get-font (merge-pathnames "Roboto-Light.ttf" *load-pathname*)) 40)
	(draw-string 10 (- height 100) (format nil "β= ~3f" (* angle 180 (/ pi))))
	(stroke)
	;; also draw graph of amplitude vs β
	(centered-circle-path (print (+ 100 (* angle 180 (/ pi) (/ 2))))  ;; β in degree
			      (print (+ (- height 100) (/ (sqrt (+ (expt (- y iy) 2)
								   (expt (- x ix) 2)))
							  7))) ;; amplitude
			      1)
	(fill-and-stroke))
      ;; show resultant line?
      (when resultant 
	(set-dash-pattern #(3 3) 0)
	(move-to ix iy)
	(line-to x y)
	(stroke)
	(when arrows
	  (set-dash-pattern #() 0)
	  (draw-arrow x y (atan (- y iy) (- x ix)))
	  (stroke)))
      (save-png file))))

(defun phasor-test (n)
  (phasor-diagram n (* 140  pi (/ 180) (/ (1- n))) (/ 900 n) #p"./phasor.png"
  		  :resultant t :arrows t :textn t :textβ t))

(defun animation-approx (n1 n2 β)
  "Animation showing the improvement in approximation as number of sections improves"
  (let ((dir #p"/tmp/animation/"))
    (ensure-directories-exist dir)
    (loop for n from n1 to n2 do
       (phasor-diagram n
		       (/ β (1- n))
		       (/ 900 n)
		       (merge-pathnames (format nil "~a.png" n) dir)
		       :resultant t
		       :arrows t
		       :textn t))
    (uiop:run-program  (format nil "convert -dispose background -delay 100 $(ls -v ~a*.png) ~aanimation.gif" dir dir)
		      :force-shell t)))


(defun animation-β (β1 β2)
  "Animation showing change in resultant due to change in β"
  (let ((dir #p"/tmp/animation2/"))
    (ensure-directories-exist dir)
    (loop for β from β1 to β2 by 0.01
       for count from 1 do
	 (phasor-diagram 30
			 (/ β 29)
			 (/ 450 29)
			 (merge-pathnames (format nil "~a.png" count) dir)
			 :resultant t
			 :arrows nil
			 :textn nil
			 :textβ t
			 :ix 245
			 :iy 30
			 :height 500))
    (uiop:run-program  (format nil "convert -dispose background -delay 20 $(ls -v ~a*.png) ~aanimation.gif" dir dir)
		       :force-shell t)))
