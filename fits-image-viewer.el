;;; fits-image-viewer.el --- View the contents of fits files from the comfort of emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  MORGAN Jay

;; Author: MORGAN Jay <jay.morgan@lis-lab.fr>
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Flexible Image Transport System (FITS) is a common file format for
;; the storage of astronomical data. fits-image-viewer allows you to
;; view the contents of a FITS file from the comfort of emacs. Though
;; fits files are often used to store more than astronimical imaging
;; that is meant for analysis rather than human consumption, this
;; image viewer enables the quick verification of contents to ensure
;; that the data is valid.
;;
;; Information on the standards of FITS files can be found at:
;; https://www.loc.gov/preservation/digital/formats/fdd/fdd000317.shtml
;; and:
;; https://fits.gsfc.nasa.gov/fits_primer.html

;;; Code:

;; A FITS file is comprised of a segments called Header Data Units
;; (HDU). The first segment is called the primary HDU. Every HDU
;; consists of a Header, containing some key = value data, and this
;; header is optionally followed by a data unit.

;; Each header, data unit is a multiple of 2880 bytes long

(setq fits-image-viewer--segment-length 2880)

;; If the data within the segment is less than this amount, it will be
;; padded with ASCII blanks or NULLs to fit to length.

;; Each header unit contains key = value pairs in the format:
;; KEYNAME = value / comment string

(setq fits-image-viewer---test-header-line
      "KEYNAME = value / comment string")

(setq fits-image-viewer---test-header-line-no-comment
      "KEYNAME = value")

(setq fits-image-viewer---test-header-lines
      "SIMPLE  =                    T / file conforms to FITS standard
BITPIX  =                   16 / number of bits per data pixel
NAXIS   =                    2 / number of data axes
NAXIS1  =                  440 / length of data axis 1
NAXIS2  =                  300 / length of data axis 2")

(defun fits-image-viewer--parsed-header-get-key (tokens)
  "Returns KEYNAME from a list of TOKENS of header line 'KEYNAME =
value / comment string'"
  (car tokens))

(defun fits-image-viewer--parsed-header-get-value (tokens)
  "Gets the value of a parsed header line as a list of TOKENS"
  (caddr tokens))

(defun fits-image-viewer--parsed-header-get-comment (tokens)
  "Gets the comment from a parsed header line as a list of TOKENS"
  (string-join (cddddr tokens) " "))

(defun fits-image-viewer--parse-header-line (line)
  "Given a single LINE (string) from a header unit of a HDU, convert
the string into a series of tokens upon which specific tokens can
be extracted for later use."
  (let ((tokens (string-split line)))
    (list
     (fits-image-viewer--parsed-header-get-key tokens)
     (fits-image-viewer--parsed-header-get-value tokens)
     (fits-image-viewer--parsed-header-get-comment tokens))))

(defun fits-image-viewer--parse-header-lines (lines)
  "Parse many header unit LINES"
  (cond ((stringp lines) (fits-image-viewer--parse-header-lines (string-split lines "\n")))
	((null lines) nil)
	(t (cons (fits-image-viewer--parse-header-line (car lines))
		 (fits-image-viewer--parse-header-lines (cdr lines))))))

(provide 'fits-image-viewer)
;;; fits-image-viewer.el ends here
