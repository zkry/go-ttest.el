;;; ttest.el --- Table Testing Utilities for Go -*- lexical-binding: t -*-

;; Author: Zachary Ryan Romero
;; Maintainer: Zachary Ryan Romero
;; Version: 0.1.0
;; Package-Requires: ()
;; Homepage: github.com/zkry/go-ttest
;; Keywords: go testing


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; go-ttest provides a number of utilities for working with table tests in Go.
;;
;; Table tests are a nice way to express variations of tests with
;; differing data but often times working with such tests can be
;; cumbersome.  You may have tens of tests but only want to run one of
;; them, or a subset of them.  To perform such actions, often time Go
;; programmers are left commenting out parts of the case structure.
;;
;; go-ttest seeks to solve this problem by providing a set of clean
;; commands for interacting with table tests.

;;; Code:

(defun go-ttest--block-beginning ()
  "Find the beginning point of the current block the point is in.
Return nil if no block found."
  (let ((ret-pt nil))
	(save-excursion
	  (beginning-of-line)
	  (while (and (not (looking-at "func Test"))
				  (not (equal (point-min) (point))))
		(forward-line -1)
		(setq ret-pt (point)))
	  (if (equal (point-min) ret-pt)
		  nil
		ret-pt))))

(defun go-ttest--block-end ()
  "Find the end point of the current block the point is in.
Return nil if no block found."
  (let ((ret-pt nil)
		(last-line-beginning-pos (save-excursion (goto-char (point-max)) (line-beginning-position))))
	(save-excursion
	  (beginning-of-line)
	  (while (and (not (looking-at "}"))
				  (not (equal (point) last-line-beginning-pos)))
		(forward-line 1)
		(setq ret-pt (point)))
	  (if (not (looking-at "}"))
		  nil
		ret-pt))))

(defun go-ttest--find-table ()
  "Find the point of the test table for the current test."
  (let ((start-point (point))
		(block-begin (go-ttest--block-beginning))
		(block-end (go-ttest--block-end))
		(table-begin nil))
	(unless (and block-begin block-end) (error "Unable to find current test"))
	;; Scan backwards for table test definition
	(save-excursion
	  (while (> (point) block-begin)
		(when (go-ttest--line-is-ttest-decl (thing-at-point 'line t))
		  (setq table-begin (point))
		  (goto-char (point-min)))
		(forward-line -1))
	  (if table-begin table-begin
		;; It didn't exist backwards, look forwards now
		(goto-char start-point)
		(while (< (point) block-end)
		  (when (go-ttest--line-is-ttest-decl (thing-at-point 'line t))
			(setq table-begin (point)))
		  (forward-line 1))
		table-begin))))

(defun go-ttest--find-parse ()
  "Return the parse of the table test thhe point is in."
  (save-excursion
	(let ((table-start (go-ttest--find-table)))
	  (unless table-start (error "Could not find a test table in current test"))
	  (save-excursion (goto-char table-start) (go-ttest--parse-ttest)))))

(defun go-ttest--find-case ()
  "Return (Pair location (or 'on 'off)) if the point is in a case definition, nil otherwise."
  (save-excursion
	(let ((at-point (point))
		  (table-parse (go-ttest--find-parse)))
	  (unless table-parse (error "Unable to extract the information in test table"))
	  (and (go-ttest--table-at-idx table-parse)
		   (nth (go-ttest--table-at-idx table-parse) (go-ttest--table-case-locs table-parse))))))

(defun go-ttest--find-case ()
  "Return (Pair location (or 'on 'off)) if the point is in a case definition, nil otherwise."
  (save-excursion
	(let ((at-point (point))
		  (table-parse (go-ttest--find-parse)))
	  (unless table-parse (error "Unable to extract the information in test table"))
	  (if (or (> at-point (go-ttest--table-end-pos table-parse))
			  (< at-point (go-ttest--table-start-pos table-parse))
			  (and (> (length (go-ttest--table-case-locs table-parse)) 0)
				   (< at-point (car (nth 0 (go-ttest--table-case-locs table-parse))))))
		  nil ; out of the table bounds or before first case, can't be in a test case.
		(let ((case-locs (cdr (go-ttest--table-case-locs table-parse)))
			  (i 0))
		  (forward-line 1)
		  (while (and (< i (length case-locs)) (< (car (nth i case-locs)) (point)))
			(setq i (1+ i)))
		  (nth i (go-ttest--table-case-locs table-parse)))))))

;; Description of algorithm:
;; - Look for a line of the pattern [Tt]est[[:alnum:]]* :?= []struct {
;;     in the current block.

;; * TODO Add more valid name cases to this.
(defun go-ttest--line-is-ttest-decl (line)
  "Return non-nil if LINE is the beginning of a table test declairation."
  (cond
   ((string-match "[Tt]est[[:alnum:]]* :?= \\[\\]struct {" line) 'slice)
   ((string-match "[Tt]est[[:alnum:]]* :?= map\\[\\(string\\|int\\)\\]struct {" line) 'map)
   (t nil)))

(defconst go-ttest--struct-type-regexp
  "^[^/[:alnum:]]*\\([[:alnum:]]+\\) +\\([[:graph:]].*\\)$")

(defun go-ttest--count-indentation ()
  "Return the number of tabs a line startss with."
  (save-excursion
	(beginning-of-line)
	(let ((ct 0))
	  (while (looking-at "\t")
		(setq ct (1+ ct))
		(forward-char 1))
	  ct)))

(cl-defstruct go-ttest--table
  ;; The type of the table test. Will either be 'slice or 'map
  type
  
  ;; A pair with car element name of name field and cdr order (stringp integerp).
  ;; nil if no name field exists.
  name-field

  ;; A list of pairs with car being the position of line and cdr 'on if not commented,
  ;; 'off if commented (integerp (or 'on off))
  case-locs
  at-idx
  ;; A list of (stringp:name string:type) pairs of the test cases.
  case-types

  start-pos
  end-pos
  
  indent-ct)

(defun go-ttest--parse-ttest ()
  "Return list of names of test with starting position if test are named, nil otherwise."
  (save-excursion
	(let ((start-pos (point))
		  (test-type (go-ttest--line-is-ttest-decl (thing-at-point 'line t)))
		  (name-name nil)
		  (case-types '())
		  (base-indent-ct 0)
		  (case-locs '())
		  (case-start-loc nil)
		  (at-idx nil) ; index of case-locs where point is at.
		  )
	  (unless test-type (error "Must be on definition of table test"))
	  (forward-line)
	  ;; We should be in the struct definition at this point
	  (while (not (string-match "}{" (thing-at-point 'line t)))
		(let ((line (thing-at-point 'line t))
			  (i 0))
		  (when (string-match go-ttest--struct-type-regexp line)
			(let ((n (match-string 1 line))
				  (tp (match-string 2 line)))
			  (setq case-types (append case-types (list (cons n tp))))
			  (when (and (string-match "[Nn]ame" n) (not name-name))
				(setq name-name (cons n i)))
			  (setq i (1+ i)))))
		(forward-line))
	  
	  (setq base-indent-ct (go-ttest--count-indentation))
	  (setq case-start-loc (point))
	  (forward-line)
	  (let ((ending-str (concat
						 (apply #'concat (make-list base-indent-ct "\t"))
						 "}"))
			;; Select the appropriate regexp to find the beginning of test case.
			(entry-begin-str (cond ((equal test-type 'slice)
									(concat (apply #'concat (make-list (1+ base-indent-ct) "\t"))
											"\\(//[ \t]*\\)?{"))
								   ((equal test-type 'map)
									(concat (apply #'concat (make-list (1+ base-indent-ct) "\t"))
											"\\(//[ \t]*\\)?\\(}, \\)?\".*\": {")))))
		;; We're at the beginning of all of the test cases; repeat until all cases have been iterated over.
		(while (not (looking-at ending-str))
		  (when (looking-at entry-begin-str)
			(let ((is-commented (string-match "^[ \t]*//[ \t]*.*{" (thing-at-point 'line t))))
			  ;; Check if we passed start-pos
			  (setq case-locs (append case-locs (list (cons (point) (if is-commented 'off 'on)))))
			  (when (and (> start-pos case-start-loc)
						 (< start-pos (point))
						 (not at-idx))
				(setq at-idx (length case-locs)))))
		  (forward-line))
		(when (and (> start-pos case-start-loc) (< start-pos (point)) (not at-idx))
		  (setq at-idx (1- (length case-locs)))))
	  (make-go-ttest--table :type test-type
							:name-field name-name
							:case-locs case-locs
							:at-idx at-idx
							:case-types case-types
							:start-pos start-pos
							:end-pos (point)
							:indent-ct base-indent-ct))))

(defun go-ttest--case-on-p (loc)
  "Return non-nil if test-case at position LOC is uncommented."
  (save-excursion
	(goto-char loc)
	(let ((case (go-ttest--find-case)))
	  (unless case (error "Test case at loc not found"))
	  (equal (cdr case) 'on))))

(defun go-ttest--case-on (loc)
  "Uncomment test-case at position LOC.  Return non-nil if changed."
  (if (go-ttest--case-on-p loc)
	  nil
	(save-excursion
	  (goto-char loc)
	  (let ((pos (car (go-ttest--find-case)))
			(parse (go-ttest--find-parse)))
		(goto-char pos)
		;; Now we should be at the base of the case
		(comment-line 1) (beginning-of-line)
		(while (not (looking-at (concat (apply #'concat (make-list (1+ (go-ttest--table-indent-ct parse)) "\t"))
										"//.*}")))
		  (comment-line 1) (beginning-of-line))
		(comment-line 1)))))

(defun go-ttest--case-off (loc)
  "Uncomment test-case at position LOC.  Return non-nil if changed."
  (if (not (go-ttest--case-on-p loc))
	  nil
	(save-excursion
	  (goto-char loc)
	  (let ((pos (car (go-ttest--find-case)))
			(parse (go-ttest--find-parse)))
		(goto-char pos)
		;; Now we should be at the base of the case
		(comment-line 1) (beginning-of-line)
		(while (not (looking-at (concat (apply #'concat (make-list (1+ (go-ttest--table-indent-ct parse)) "\t"))
										"}")))
		  (comment-line 1) (beginning-of-line))
		(comment-line 1)))))

(defun go-ttest-toggle ()
  "Toggle the comments of the test case that the pointer is on."
  (interactive))

(defun go-ttest-off ()
  "Toggle the comments of the test case that the pointer is on."
  (interactive))

(defun go-ttest-off-all ()
  "Toggle the comments of the test case that the pointer is on."
  (interactive))

(defun go-ttest-on ()
  "Toggle the comments of the test case that the pointer is on."
  (interactive))

(defun go-ttest-on-all ()
  "Toggle the comments of the test case that the pointer is on."
  (interactive))

(provide 'ttest)
;;; ttest.el ends here
