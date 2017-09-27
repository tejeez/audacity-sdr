;nyquist plug-in
;version 3
;type process
;name "Demodulate IQ signal"

;control center-freq "Center frequency" float "Hz" 5000 -1000000 1000000
;control bandwidth "Bandwidth" float "Hz" 5000 0 500000
;control demodulation "Demodulation" choice "None (only shift to 0 Hz and filter),AM,FM,AM left FM right,USB,LSB" 3

(defun I (p) (aref p 0))
(defun Q (p) (aref p 1))
(defmacro get-srate () `(snd-srate (aref p 0)))

(defun complex-sine (hz)
  (vector (hzosc (abs hz) *table* 90)
          (hzosc (abs hz) *table* (if (> hz 0) 0 180))))

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

(defun snd-atan2 (p)
  (let* ((nsamples (snd-length (I p) 99999999))
         (out (make-array nsamples)))
    (dotimes (i nsamples)
      (setf (aref out i) (atan (snd-fetch (Q p)) (snd-fetch (I p))))
    )
    (snd-from-array 0 (get-srate) out)
  )
)

(defun 1-sample-delay (p) (feedback-delay p (/ 1 (get-srate)) 0 ))
(defun fm-demod (p)
  (mult (snd-atan2 (complex-multiply-conjugate p (1-sample-delay p)))
        (/ (get-srate) bandwidth 6.28) ))

(defun usb-demod (p)
  (complex-multiply-to-real p (complex-sine (* 0.5 bandwidth))))

(defun lsb-demod (p)
  (complex-multiply-to-real p (complex-sine (* -0.5 bandwidth))))

(let ((ddcsig (ddc s)))
  (case demodulation
    (0 ddcsig)
    (1 (am-demod ddcsig))
    (2 (fm-demod ddcsig))
    (3 (vector (am-demod ddcsig) (fm-demod ddcsig)))
    (4 (usb-demod ddcsig))
    (5 (lsb-demod ddcsig))
  )
)
