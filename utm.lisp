;; utm.lisp
;; Copyright (c) 2024 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package #:utm)

(declaim (inline deg2rad rad2deg))
(declaim (ftype (function (number) number)
                deg2rad rad2deg))
(defun deg2rad (d)
  (declare (type number d))
  (* d (/ PI 180.0d0)))

(defun rad2deg (r)
  (declare (type number r))
  (* r (/ 180.0d0 pi)))

;; This hash tables stores parameters about the ellipsoid each model uses to model the earth
(defparameter *ellipsoids* (make-hash-table :test 'equal))

;; Fills the ellipsoids hash table
(defun fill-ellipsoids ()
  "Fill the hash table of supported ellipsoids."
  (setf (gethash "NAD83" *ellipsoids*) (cons 6378137.0d0 6356752.3142d0))
  (setf (gethash "WGS84" *ellipsoids*) (cons 6378137.0d0 6356752.3142d0))
  (setf (gethash "GRS80" *ellipsoids*) (cons 6378137.0d0 6356752.3141d0))
  (setf (gethash "WGS72" *ellipsoids*) (cons 6378135.0d0 6356750.5d0))
  (setf (gethash "Australian1965" *ellipsoids*) (cons 6378160.0d0 6356774.7d0))
  (setf (gethash "Krasovsky1940" *ellipsoids*) (cons 6378245.0d0 6356863.0d0))
  (setf (gethash "International1924" *ellipsoids*) (cons 6378388.0d0 6356911.9d0))
  (setf (gethash "Hayford1909" *ellipsoids*) (cons 6378388.0d0 6356911.9d0))
  (setf (gethash "Clake1880" *ellipsoids*) (cons 6378249.1d0 6356514.9d0))
  (setf (gethash "Clarke1866" *ellipsoids*) (cons 6378206.4d0 6356583.8d0))
  (setf (gethash "Airy1830" *ellipsoids*) (cons 6377563.4d0 6356256.9d0))
  (setf (gethash "Bessel1841" *ellipsoids*) (cons 6377397.2d0 6356079.9d0))
  (setf (gethash "Everest1830" *ellipsoids*) (cons 6377276.3d0 6356075.4d0)))

;; Populate the hash table
(fill-ellipsoids)

(defun ellipsoid-names ()
  "Return the names of all supported ellipsoids."
  (loop :for key :being
          :the :hash-keys
            :in *ellipsoids*
        :collect key))

;; Constants used in the equations
(defconstant E0 500000.0d0)
(defconstant k0 0.9996d0)
(defconstant N0 10000000.0d0)

;; This math is hideous in Lisp, but also pretty ugly in other languages.
;; I'm not going to pretend to understand exactly what's going on here.
;; If curious, look at:
;; * http://www.uwgb.edu/dutchs/UsefulData/UTMFormulas.htm
;; * http://www.uwgb.edu/dutchs/FieldMethods/UTMSystem.htm
(declaim (ftype (function (number number
                                        &key
                                        (:ellipsoid string)
                                        (:zone (or null fixnum)))
                          (values number number fixnum))
                lat-lon-to-utm))
(defun lat-lon-to-utm (lat lon &key (ellipsoid "WGS84") (zone nil))
  "Convert a point given as latitude and longitude into UTM using the specified ellipsoid.  The default ellipsoid is WGS84."
  (declare (type number lat lon)
           (type (or null fixnum) zone))
  (let* ((lat-rad (deg2rad lat))
         (lon-rad (deg2rad lon))

         (a (car (gethash ellipsoid *ellipsoids*)))
         (b (cdr (gethash ellipsoid *ellipsoids*)))

         (f (/ (- a b)
               a))
         (e-squared (* f
                       (- 2.0d0 f)))
         (e-prime-squared (/ e-squared
                             (- 1.0d0 e-squared)))
         (nzone (if zone
                    zone
                    (ceiling (/ (+ lon 180.0d0)
                                6.0d0))))

         (long0 (deg2rad (- (* 6.0d0
                               (1- nzone))
                            177)))

         ;;(rho (/ (* a (- 1 e-squared)) (expt (- 1.0 (* e-squared (expt (sin lat-rad) 2.0))) (/ 3.0 2.0))))
         (nu (realpart (/ a
                          (expt (- 1.0d0
                                   (* e-squared
                                      (expt (sin lat-rad)
                                            2.0d0)))
                                0.5d0))))
         (p (- lon-rad long0))

         (M (realpart (* a (+ (* (- 1.0d0
                                    (/ e-squared
                                       4.0d0)
                                    (* (/ 3.0d0
                                          64.0d0)
                                       (expt e-squared
                                             2.0d0))
                                    (* (/ 5.0d0
                                          256.0d0)
                                       (expt e-squared
                                             3.0d0)))
                                 lat-rad)
                              (- (* (sin (* 2.0d0 lat-rad))
                                    (+ (* (/ 3.0d0
                                             8.0d0)
                                          e-squared)
                                       (* (/ 3.0d0
                                             32.0d0)
                                          (expt e-squared
                                                2.0d0))
                                       (* (/ 45.0d0
                                             1024.0d0)
                                          (expt e-squared
                                                3.0d0)))))
                              (* (sin (* 4.0d0 lat-rad))
                                 (+ (* (/ 15.0d0
                                          256.0d0)
                                       (expt e-squared
                                             2.0d0))
                                    (* (/ 45.0d0
                                          1024.0d0)
                                       (expt e-squared
                                             3.0d0))))
                              (- (* (sin (* 6.0d0 lat-rad))
                                    (* (/ 35.0d0
                                          3072.0d0)
                                       (expt e-squared
                                             3.0d0))))))))

         (K1 (* M k0))
         (k2 (* k0
                nu
                (sin (* 2.0d0 lat-rad))
                0.250d0))
         (k3 (realpart (*
                        (* k0
                           nu
                           (sin lat-rad)
                           (expt (cos lat-rad)
                                 3.0d0)
                           (/ 1.0d0
                              24.0d0))
                        (+ 5.0d0
                           (- (expt (tan lat-rad)
                                    2.0d0))
                           (* 9.0d0
                              e-prime-squared
                              (expt (cos lat-rad)
                                    2.0d0))
                           (* 4.0d0
                              (expt e-squared 2.0d0)
                              (expt (cos lat-rad)
                                    4.0d0))))))
         (k4 (* k0
                nu
                (cos lat-rad)))
         (k5 (realpart (* (* k0
                             nu
                             (expt (cos lat-rad) 3.0d0)
                             (/ 1.0d0
                                6.0d0))
                          (+ 1.0d0
                             (- (expt (tan lat-rad)
                                      2.0d0))
                             (* e-prime-squared
                                (expt (cos lat-rad)
                                      2.0d0))))))
         (easting (realpart (+ E0
                               (* k4 p)
                               (* k5
                                  (expt p
                                        3.0d0)))))
         (northing (realpart (+ k1
                                (* k2
                                   (expt p 2.0d0))
                                (* k3
                                   (expt p 4.0d0))))))
    (declare (type number
                   lat-rad lon-rad
                   a b
                   f e-squared e-prime-squared
                   long0 nu p M
                   K1 K2 K3 k4 k5
                   easting northing)
             (type fixnum nzone))
    (values easting
            (if (< lat 0)
                (+ N0 northing)
                northing)
            nzone)))

;; Again, see the references for an explanation of what's going on
;; * http://www.uwgb.edu/dutchs/UsefulData/UTMFormulas.htm
;; * http://www.uwgb.edu/dutchs/FieldMethods/UTMSystem.htm
(defun utm-to-lat-lon (easting northing zone &key (ellipsoid "WGS84"))
  "Convert a point given as UTM into latitude and longitude using the specified ellipsoid.  The default ellipsoid is WGS84."
  (declare
           (type number easting northing)
           (type fixnum zone)
           )
  (let*
      ((reasting (- easting 500000.0d0))
       (a (car (gethash ellipsoid *ellipsoids*)))
       (b (cdr (gethash ellipsoid *ellipsoids*)))
       (f (/ (- a b)
             a))
       (e-squared (* f
                     (- 2.0d0 f)))
       (e-prime-squared (/ e-squared
                           (- 1.0d0 e-squared)))
       (long0 (deg2rad (- (* 6.0d0
                             (1- zone))
                          177)))
       (M (/ northing k0))
       (mu (realpart (/ M
                        (* a
                           (- 1.0d0
                              (/ e-squared
                                 4.0d0)
                              (* (/ 3.0d0
                                    64.0d0)
                                 (expt e-squared
                                       2.0d0))
                              (* (/ 5.0d0
                                    256.0d0)
                                 (expt e-squared
                                       3.0d0)))))))
       (e1 (/ (- 1.0d0
                 (sqrt (- 1.0d0 e-squared)))
              (+ 1.0d0
                 (sqrt (- 1.0d0 e-squared)))))
       (j1 (realpart (- (*
                         (/ 3.0d0
                            2.0d0)
                         e1)
                        (* (/ 27.0d0
                              32.0d0)
                           (expt e1
                                 3.0d0)))))
       (j2 (realpart (- (*
                         (/ 21.0d0
                            16.0d0)
                         (expt e1
                               2.0d0))
                        (* (/ 55.0d0
                              32.0d0)
                           (expt e1
                                 4.0d0)))))
       (j3 (realpart (* (/ 151
                           96)
                        (expt e1
                              3.0d0))))
       (j4 (realpart (* (/ 1097.0d0
                           512.0d0)
                        (expt e1
                              4.0d0))))
       (fp (+ mu
              (* j1 (sin (* 2.0d0 mu )))
              (* j2 (sin (* 4.0d0 mu )))
              (* j3 (sin (* 6.0d0 mu )))
              (* j4 (sin (* 8.0d0 mu )))))
       (c1 (realpart (* e-prime-squared
                        (expt (cos fp)
                              2.0d0))))
       (t1 (realpart (expt (tan fp)
                           2.0d0)))
       (r1 (realpart (* a
                        (/ (- 1.0d0 e-squared) (expt (- 1.0d0 (* e-squared (expt (sin fp) 2.0d0))) (/ 3.0d0 2.0d0))))))
       (n1 (realpart (/ a
                        (expt (- 1.0d0
                                 (* e-squared
                                    (expt (sin fp)
                                          2.0d0)))
                              0.5))))
       (D (/ reasting
             (* n1
                k0)))
       (q1 (* n1
              (/ (tan fp)
                 r1)))
       (q2 (realpart (/ (expt d 2.0d0)
                        2.0d0)))
       (q3 (realpart (/ (* (+ 5
                              (* 3.0d0 t1)
                              (* 10.0d0 c1)
                              (* -4 (expt c1 2.0d0))
                              (* -9.0d0 e-prime-squared))
                           (expt D 4.0d0))
                        24.0d0)))
       (q4 (realpart (/ (* (+ 61
                              (* 90.0d0 t1)
                              (* 298.0d0 c1)
                              (* 45.0d0 (expt t1 2.0d0))
                              (* -3.0d0 (expt c1 2.0d0))
                              (* -252 e-prime-squared))
                           (expt D 6.0d0))
                        720.0d0)))
       (q5 d)
       (q6 (realpart (/ (* (+ 1.0d0 (* 2.0d0 t1) c1)
                           (expt d 3.0d0))
                        6.0d0)))
       (q7 (realpart (/ (* (+ 5.0d0
                              (* -2.0d0 c1)
                              (* 28.0d0 t1)
                              (* -3 (expt c1 2.0d0))
                              (* 8.0d0 e-prime-squared)
                              (* 24.0d0 (expt t1 2.0d0)))
                           (expt d 5.0d0))
                        120.0d0)))
       (lat (- fp
               (* q1
                  (+ q2
                     (- q3)
                     q4))))
       (lon (+ long0
               (/ (+ q5
                     (- q6)
                     q7)
                  (cos fp)))))
    (declare (type number reasting a b f e-squared e-prime-squared long0 M mu
                   e1 j1 j2 j3 j4 fp c1 t1 r1 n1 D q1 q2 q3 q4 q5 q6 q7 lat lon))
    (values (rad2deg (realpart lat))
            (rad2deg (realpart lon)))))

(defun deg-min-sec-to-decimal (degree minute second)
  "Convert degree, minute, second format to decimal.nn"
  (declare 
           (type number degree minute second))
  (+ degree (/ minute 60.0d0) (/ second (* 60.0d0 60.0d0))))

(defun decimal-to-deg-min-sec (decimal)
  "Convert degree, minute, second format to decimal."
  (declare 
           (type number decimal))
  (multiple-value-bind (degrees min-secs) (truncate decimal)
    (declare (type fixnum degrees)
             (type number min-secs))
    (when (< degrees 0)
      (setf min-secs (- min-secs))
      (decf degrees))
    (multiple-value-bind (minutes secs) (truncate (* 60 min-secs))
      (declare (type fixnum minutes)
               (type number secs))
      (values degrees minutes (* secs 60.0d0)))))
