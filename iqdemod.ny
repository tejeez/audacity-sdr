;nyquist plug-in
;version 3
;type process
;name "Demodulate IQ signal"

;control center-freq "Center frequency" float "Hz" 5000 -1000000 1000000
;control bandwidth "Bandwidth" float "Hz" 5000 0 500000
;control demodulation "Demodulation" choice "None (only shift to 0 Hz and filter),AM,FM,AM left FM right,USB,LSB,Differential phase,BPSK PLL carrier recovery" 3
;control inv-delay "1/delay (diff.phase only)" float "Hz" 5000 0 500000
;control loop-speed "Loop speed (BPSK PLL only)" float "something" 0.01 0 0.1

(defun I (p) (aref p 0))
(defun Q (p) (aref p 1))
(defmacro get-srate () `(snd-srate (aref p 0)))

(defun complex-sine (hz)
  (vector (hzosc (abs hz) *table* 90)
          (hzosc (abs hz) *table* (if (plusp hz) 0 180))))

(defun complex-multiply (s1 s2)
  (vector
    (diff (mult (I s1) (I s2)) (mult (Q s1) (Q s2)))
    (sum  (mult (Q s1) (I s2)) (mult (I s1) (Q s2)))))

(defun complex-multiply-conjugate (s1 s2)
  (vector
    (sum  (mult (I s1) (I s2)) (mult (Q s1) (Q s2)))
    (diff (mult (Q s1) (I s2)) (mult (I s1) (Q s2)))))

(defun complex-multiply-to-real (s1 s2)
    (diff (mult (I s1) (I s2)) (mult (Q s1) (Q s2))))

(defun ddc (p)
  (lowpass8
    (complex-multiply-conjugate p (complex-sine center-freq))
    (* 0.5 bandwidth)))

(defun ^2 (p) (mult p p))
(defun am-demod (p)
  (s-sqrt (sum (^2 (I p)) (^2 (Q p)))))

(defmacro for-each-sample (operation)
 `(let* ((nsamples (snd-length (I p) 99999999))
         (out-array (make-array nsamples)))
     (dotimes (ii nsamples)
       (let ((si (snd-fetch (aref p 0))) (sq (snd-fetch (aref p 1))))
         (setf (aref out-array ii) ,operation)
       )
     )
     (snd-from-array 0 (get-srate) out-array)
  )
)

;; stereo output (maybe there's a more elegant way to do this)
(defmacro set-result-i (v) `(setf (aref out-a1 ii) ,v))
(defmacro set-result-q (v) `(setf (aref out-a2 ii) ,v))
(defmacro for-each-sample2 (operation)
 `(let* ((nsamples (snd-length (I p) 99999999))
         (out-a1 (make-array nsamples)) (out-a2 (make-array nsamples)))
     (dotimes (ii nsamples)
       (let ((si (snd-fetch (aref p 0))) (sq (snd-fetch (aref p 1))))
         ,operation
       )
     )
     (vector (snd-from-array 0 (get-srate) out-a1)
             (snd-from-array 0 (get-srate) out-a2))
  )
)

(defun snd-atan2 (p) (for-each-sample (atan sq si)))

;; xlisp doesn't have complex numbers so this is a bit more complex
(defun bpsk-pll (p)
 (let
  ((loop-speed2 (* loop-speed loop-speed)) ;; could have a better way to set loop filter coefficients
   (vco-p 0.0) (vco-f 0.0))
  (for-each-sample2 (progn (let*
   ((vco-i (cos vco-p)) (vco-q (sin vco-p))
    (out-i (- (* si vco-i) (* sq vco-q)))
    (out-q (+ (* sq vco-i) (* si vco-q)))
    (loop-error (if (zerop out-i) 0 (atan (/ out-q out-i))))
   )
   (setq vco-f (+ vco-f (* loop-speed2 loop-error)))
   (setq vco-p (- vco-p vco-f (* loop-speed loop-error)))
   (set-result-i out-i) (set-result-q out-q)
   ;; (set-result-q vco-f) ;; could be a third channel if it was possible
)))))

(defun 1-sample-delay (p) (feedback-delay p (/ 1 (get-srate)) 0 ))
(defun fm-demod (p)
  (mult (snd-atan2 (complex-multiply-conjugate p (1-sample-delay p)))
        (/ (get-srate) bandwidth 2 pi) ))

(defun usb-demod (p)
  (complex-multiply-to-real p (complex-sine (* 0.5 bandwidth))))

(defun lsb-demod (p)
  (complex-multiply-to-real p (complex-sine (* -0.5 bandwidth))))

(defun diff-phase-demod (p)
  (complex-multiply-conjugate p (feedback-delay p (/ 1 inv-delay) 0)))

(let ((ddcsig (ddc s)))
  (case demodulation
    (0 ddcsig)
    (1 (am-demod ddcsig))
    (2 (fm-demod ddcsig))
    (3 (vector (am-demod ddcsig) (fm-demod ddcsig)))
    (4 (usb-demod ddcsig))
    (5 (lsb-demod ddcsig))
    (6 (diff-phase-demod ddcsig))
    (7 (bpsk-pll ddcsig))
  )
)
