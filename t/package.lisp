;;;; package.lisp
;;
;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

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

(in-package :cl-user)
(defpackage :utm.test
  (:use :cl
        :fiveam
        :utm))

(in-package :utm.test)

(def-suite :utm)
(in-suite :utm)

(defclass utm-test-case ()
  ((latitude :initarg :latitude)
   (longitude :initarg :longitude)
   (ellipsoid :initarg :ellipsoid)
   (zone :initarg :zone)
   (easting :initarg :easting)
   (northing :initarg :northing)))

(defmethod print-object ((object utm-test-case) stream)
  (with-slots (latitude longitude ellipsoid zone easting northing) object
    (format stream "{ latitude: ~a, longitude: ~a, ellipsoid: ~a, zone: ~a easting: ~a, northing: ~a }"
            latitude longitude ellipsoid zone easting northing)))

(defparameter *utm-tolerance* 0.002)

(defparameter *test-coords*
  (list (make-instance 'utm-test-case
                       :latitude 39.9835310406d0
                       :longitude -105.2536113560d0
                       :ellipsoid "NAD83"
                       :zone 13
                       :easting 478346.59856d0
                       :northing 4425960.12177d0)
        (make-instance 'utm-test-case
                       :latitude 46.8392935284d0
                       :longitude -115.3348159790d0
                       :ellipsoid "NAD83"
                       :zone 11
                       :easting 626974.542d0
                       :northing 5188651.724d0)
        (make-instance 'utm-test-case
                       :latitude 48.2338208531d0
                       :longitude 19.3716430664d0
                       :ellipsoid "GRS80"
                       :zone 34
                       :easting 379083.606d0
                       :northing 5343570.714d0)
        (make-instance 'utm-test-case
                       :latitude 59.8723979923d0
                       :longitude 105.1062011719d0
                       :ellipsoid "GRS80"
                       :zone 48
                       :easting 505946.450d0
                       :northing 6637205.350d0)
        (make-instance 'utm-test-case
                       :latitude -29.3438753994d0
                       :longitude 130.8032226563d0
                       :ellipsoid "GRS80"
                       :zone 52
                       :easting 675064.430d0
                       :northing 6752564.671d0)))

(defun utm-near (a b)
  (< (abs (- a b)) *utm-tolerance*))

(test lat-lon-to-utm
  (dolist (coord *test-coords*)
    (with-slots (latitude longitude ellipsoid zone easting northing) coord
      (let* ((translated (lat-lon-to-utm latitude longitude :ellipsoid ellipsoid))
             (translated-easting (car translated))
             (translated-northing (cadr translated))
             (translated-zone (caddr translated)))
        (is-true (utm-near translated-easting easting)
                 "Could not convert ~a to UTM got easting ~a" coord translated-easting)
        (is-true (utm-near translated-northing northing)
                 "Could not convert ~a to UTM got northing ~a" coord translated-northing)
        (is-true (= zone translated-zone)
                 "Could not convert ~a to UTM got zone ~a" coord translated-zone)))))


(test utm-to-lat-lon
  (dolist (coord *test-coords*)
    (with-slots (latitude longitude ellipsoid zone easting northing) coord
      (let* ((translated (utm-to-lat-lon easting northing zone :ellipsoid ellipsoid))
             (translated-latitude (car translated))
             (translated-longitude (cadr translated)))
        (is-true (utm-near translated-latitude latitude)
                 "Could not convert ~a to latitude/longitude... got latitude ~a"
                 coord translated-latitude)
        (is-true (utm-near translated-longitude longitude)
                 "Could not convert ~a to latitude/longitude... got longitude ~a"
                 coord translated-longitude)))))
