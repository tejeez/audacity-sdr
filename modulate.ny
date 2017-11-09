;nyquist plug-in
;version 3
;type process
;name "Modulate IQ signal"

;control center-freq "Center frequency" float "Hz" 5000 -1000000 1000000
;control bandwidth "Audio bandwidth" float "Hz" 3000 0 500000
;control deviation "FM deviation" float "Hz" 5000 0 500000
;control modulation "Modulation" choice "AM,FM,USB,LSB" 0

(defun I (p) (aref p 0))
(defun Q (p) (aref p 1))
(defmacro get-srate () `(snd-srate (aref p 0)))

(defun complex-sine (hz)
  (vector (hzosc (abs hz) *table* 90)
          (hzosc (abs hz) *table* (if (plusp hz) 0 180))))

(defun fm-mod (f d p)
  (vector (hzosc (sum f (mult d p)) *table* 90)
          (hzosc (sum f (mult d p)) *table* 0)))

(defun am-mod (f p)
  (complex-multiply-by-real (complex-sine f) (sum 0.5 (mult 0.5 p))))

(defun complex-multiply (s1 s2)
  (vector
    (diff (mult (I s1) (I s2)) (mult (Q s1) (Q s2)))
    (sum  (mult (Q s1) (I s2)) (mult (I s1) (Q s2)))))

(defun complex-multiply-by-real (s1 s2)
  (vector
    (mult (I s1) s2)
    (mult (Q s1) s2)))

(defun duc (p)
  (complex-multiply (lowpass8 p (* 0.5 bandwidth))
    (complex-sine center-freq)))

(defun usb-mod (p)
  (complex-multiply-by-real (complex-sine (* -0.5 bandwidth)) p))

(defun lsb-mod (p)
  (complex-multiply-by-real (complex-sine (* 0.5 bandwidth)) p))

(let ((sl (lowpass8 (aref s 0) bandwidth)))
  (case modulation
    (0 (am-mod center-freq sl))
    (1 (fm-mod center-freq deviation sl))
    (2 (duc (usb-mod sl)))
    (3 (duc (lsb-mod sl)))))
