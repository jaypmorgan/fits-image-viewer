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

(defun fits-image-viewer--parsed-header-get-key (tokens)
  "Returns KEYNAME from a list of TOKENS of header line 'KEYNAME =
value / comment string'"
  (string-trim (car tokens)))

(defun fits-image-viewer--parsed-header-get-value (tokens)
  "Gets the value of a parsed header line as a list of TOKENS"
  (let ((v (string-trim (car (string-split (cadr tokens) "/")))))
    (cond ((string= "" v) "")
	  ((string= "'" (substring v 0 1)) (string-trim (string-trim v "'" "'")))
	  ((string= "T" v) "T")  ;; leave as string
	  (t (string-to-number v)))))

(defun fits-image-viewer--parsed-header-get-comment (tokens)
  "Gets the comment from a parsed header line as a list of TOKENS"
  (let ((comment (cadr (string-split (cadr tokens) "/"))))
    (unless (null comment)
      (string-trim comment))))

(defun fits-image-viewer--parse-header-split-line (line)
  "Split a single header unit LINE into multiple tokens"
  (list (substring line 0 8)
	(substring line 9)))

(defun fits-image-viewer--parse-header-line (line)
  "Given a single LINE (string) from a header unit of a HDU, convert
the string into a series of tokens upon which specific tokens can
be extracted for later use."
  (let ((tokens (fits-image-viewer--parse-header-split-line line)))
    (list
     (fits-image-viewer--parsed-header-get-key tokens)
     (fits-image-viewer--parsed-header-get-value tokens)
     (fits-image-viewer--parsed-header-get-comment tokens))))

(defun fits-image-viewer--split-header-lines (lines)
  (if (string= "" lines)
      nil
    (cons (substring lines 0 80)
	  (fits-image-viewer--split-header-lines (substring lines 80)))))


(defun fits-image-viewer--header-get-key (header-line)
  (car header-line))

(defun fits-image-viewer--header-get-value (header-line)
  (cadr header-line))

(defun fits-image-viewer--header-get-comment (header-line)
  (caddr header-line))

(defun fits-image-viewer--parse-header-reduce (lines)
  (cl-remove-if
   '(lambda (line)
      (let ((key (fits-image-viewer--header-get-key line)))
	(or (string= "END" key) (string= "" key))))
   lines))

(defun fits-image-viewer--parse-header-lines (lines)
  "Parse many header unit LINES"
  (cond ((stringp lines) (fits-image-viewer--parse-header-lines
			  (fits-image-viewer--split-header-lines lines)))
	((null lines) nil)
	(t (cons (fits-image-viewer--parse-header-line (car lines))
		 (fits-image-viewer--parse-header-lines (cdr lines))))))

(setq fits-image-viewer--test-file "./mh140831.075241.fits")

;;; Read HDU
;;;
;;; These functions pertain the reading of particular HDU segments of the
;;; FITS file format. In particular we have the reading of a header as formatted
;;; by a key = value / comment string. The next read function is for reading
;;; a stream of data (if there is one). We need the information in the header
;;; to know how to read this data.

(defun fits-image-viewer--read-header (filename)
  "Read the header HDU from the FILENAME"
  (cl-labels ((reader (iteration)
		(let ((coding-system-for-read 'binary))
		  (with-temp-buffer
		    (insert-file-contents
		     filename nil 0 (* iteration fits-image-viewer--segment-length))
		    (let ((contents (buffer-string)))
		      (if (string-match-p "\sEND\s" contents)
			  contents
			(reader (+ 1 iteration))))))))
    (fits-image-viewer--parse-header-reduce
     (fits-image-viewer--parse-header-lines
      (reader 1)))))

(defun fits-image-viewer--read-data (filename)
  (let ((coding-system-for-read 'binary))
    (with-temp-buffer
      (insert-file-contents
       filename nil (* 1 fits-image-viewer--segment-length)))))

;;; Define Major Mode
;;;
;;; This next section of code setups the major mode and the hook for
;;; its auto-activation upon reading files with a '.fits'
;;; extension. The major mode will read the filename of the
;;; buffer-file-name, grap the header content, and format it in the
;;; buffer instead of the actual content.

(define-derived-mode fits-image-viewer-mode
  org-mode "FITS"
  "Major mode for viewing FITS file contents"
  (erase-buffer)  ;; remove the contents of the original file from this buffer
  (setq-local org-hide-emphasis-markers t)  ;; hide */ in local buffer only
  (let ((header (fits-image-viewer--read-header (buffer-file-name))))
    ;; print out the header unit as a formatted table using bold font
    ;; for the key, normal for the value, and italic for the comment
    ;; if there is one.  We're using org-emphasis-markers to take
    ;; advantage of this being derived from an org-mode buffer.
    (dolist (hu header)  ;; iterate through the key=value pairs
      (insert (format
	       "%-12s%-30s %-10s\n"
	       (format "*%s*" (fits-image-viewer--header-get-key hu))
	       (fits-image-viewer--header-get-value hu)
	       (let ((comment (fits-image-viewer--header-get-comment hu)))
		 (or (and (string= "" comment) "")
		     (format "\/%s\/" comment)))))))
  (read-only-mode t)
  (beginning-of-buffer))

(add-to-list 'auto-mode-alist '("\\.fits\\'" . fits-image-viewer-mode))

(provide 'fits-image-viewer)
;;; fits-image-viewer.el ends here
