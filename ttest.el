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
  "Return the parse of the table test thhe point is in, nil if no parse found."
  (save-excursion
	(let ((table-start (go-ttest--find-table))
		  (start-pos (point)))
	  (if table-start
		  (save-excursion (goto-char table-start) (go-ttest--parse-ttest start-pos))
		nil))))

(defun go-ttest--find-case (parse)
  "Return (Pair location (or 'on 'off)) of where point was found in PARSE."
  (nth (go-ttest--table-at-idx parse) (go-ttest--table-case-locs parse)))

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

(defun go-ttest--parse-ttest (&optional start-pos)
  "Return a stucture containing pared test table.  Contain which case START-POS is at if given."
  (unless start-pos (setq start-pos -1)) ;; Default value of pos should be number to make at-idx nil.
  (save-excursion
	(let ((test-type (go-ttest--line-is-ttest-decl (thing-at-point 'line t)))
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
			  (when (and (string-match "[Nn]ame" 1) (not name-name))
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
				(setq at-idx (- (length case-locs) 2)))))
		  (forward-line))
		(when (and (> start-pos case-start-loc) (<= start-pos (point)) (not at-idx))
		  (setq at-idx (1- (length case-locs)))))
	  (make-go-ttest--table :type test-type
							:name-field name-name
							:case-locs case-locs
							:at-idx at-idx
							:case-types case-types
							:start-pos start-pos
							:end-pos (point)
							:indent-ct base-indent-ct))))

;;; YASNIPPET GENERATORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun go-ttest--make-case-snippet (parse)
  "Return a yasnippet string of last test case found in PARSE."
  ;; TODO we may want to have different kinds of snippets.
  (cond
   (t (go-ttest--make-blank-case-snippet parse))
		  ;((equal (go-ttest--table-type parse) 'map) (go-ttest--table-make-map-snippet parse))
		  ;((equal (go-ttest--table-type parse) 'slice) (go-ttest--table-make-slice-snippet parse))
		  ;(t (error "Unknown table parse type"))
   ))

(defun go-ttest--make-blank-case-snippet (parse)
  "Return a yasnippet string for the name-type pairs of the PARSE test table."
  (let* ((type (go-ttest--table-type parse))
		 (base-indent (apply #'concat (make-list (1+ (go-ttest--table-indent-ct parse)) "\t")))
		 (inner-indent (apply #'concat (make-list (+ 2 (go-ttest--table-indent-ct parse)) "\t")))
		 (fields (go-ttest--table-case-types parse))
		 (field-idx 2)
		 (ret base-indent))
	(when (equal type 'map) (setq ret (concat ret "\"${1:Name}\": ")))
	(setq ret (concat ret "{\n"))
	(dolist (nt fields)
	  (let ((name (car nt))
			(type (cdr nt)))
		(setq ret (concat ret (format "%s%s: ${%d:%s},\n" inner-indent name field-idx type)))
		(setq field-idx (1+ field-idx))))
	(setq ret (concat ret base-indent "},"))
	ret))

;;; ON/OFF FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun go-ttest--case-on-p (parse)
  "Return non-nil if test-case at position LOC is uncommented given PARSE."
  (let ((case (go-ttest--find-case parse)))
	(and case (equal (cdr case) 'on))))

(defun go-ttest--case-off-p (parse)
  "Return non-nil if test-case at position LOC is uncommented given PARSE."
  (let ((case (go-ttest--find-case parse)))
	(and case (equal (cdr case) 'off))))

(defun go-ttest--case-on (parse &optional loc)
  "Uncomment test-case at position LOC and parse PARSE.

If LOC is provided, turn on the case at position LOC.  Return
non-nil if changed."
  (go-ttest--apply-comment-func parse (if loc loc (car (go-ttest--find-case parse))) "//.*}"))

(defun go-ttest--case-off (parse &optional loc)
  "Comment test-case at position LOC and parse PARSE.

If LOC is provided, turn of the case at LOC.  Retrun non-nil if changed."
  (go-ttest--apply-comment-func parse (if loc loc (car (go-ttest--find-case parse))) "}"))

;; TODO `comment-line' doesn't seem to comment the line 100% to gofmt standards.
;;         It may be necessarry to write my own version of the function.
(defun go-ttest--apply-comment-func (parse pos stop-regexp)
  "Apply the `comment-line' func starting at POS.

Apply the `comment-line' func to PARSE struct, stopping applying
at STOP-REGEXP."
  (save-excursion
	(goto-char pos)
	;; Now we should be at the base of the case
	(comment-line 1) (beginning-of-line)
	(while (not (looking-at (concat (apply #'concat
										   (make-list (1+ (go-ttest--table-indent-ct parse)) "\t"))
									stop-regexp)))
	  (comment-line 1) (beginning-of-line))
	(comment-line 1)
	t))

(defun go-ttest--apply-comment-func-all (parse do-symbol do-func)
  "Apply a function to all cases of PARSE, matching DO-SYMBOL.

DO-FUNC is then ran on applicable lines."
  (let ((changes 0))
	(let ((change-ct (dolist (c (reverse (go-ttest--table-case-locs parse)) changes)
					   (let ((pos (car c))
							 (do (equal do-symbol (cdr c))))
						 (if do
							 (progn
							   (apply do-func (list parse pos))
							   (setq changes (1+ changes)))
						   nil)))))
	  (message (format "%d test cases turned %s." change-ct do-symbol)))))

;;; INTERACTIVE FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun go-ttest-toggle ()
  "Toggle the comments of the test case that the pointer is on."
  (interactive)
  (let ((parse (go-ttest--find-parse)))
	(unless parse (error "No test case found at point"))
	(if (go-ttest--case-on-p parse)
		(go-ttest--case-off parse)
	  (go-ttest--case-on parse))))

(defun go-ttest-off ()
  "Toggle the comments of the test case that the pointer is on."
  (interactive)
  (let* ((parse (go-ttest--find-parse))
		 (case (and parse (go-ttest--find-case parse))))
	(unless parse (error "No test case found at point"))
    (if (equal (cdr case) 'off)
		(message "Test case is already active.")
	  (let ((changed (go-ttest--case-off parse)))
		(when (not changed) (message "Test case is already active."))))))

(defun go-ttest-off-all ()
  "Toggle the comments of the test case that the pointer is on."
  (interactive)
  (let ((parse (go-ttest--find-parse)))
	(go-ttest--apply-comment-func-all parse 'on #'go-ttest--case-off)))

(defun go-ttest-on ()
  "Toggle the comments of the test case that the pointer is on."
  (interactive)
  (let* ((parse (go-ttest--find-parse))
		 (case (and parse (go-ttest--find-case parse))))
	(unless parse (error "No test case found at point"))
    (if (equal (cdr case) 'on)
		(message "Test case is already active.")
	  (let ((changed (go-ttest--case-on parse)))
		(when (not changed) (message "Test case is already active."))))))

(defun go-ttest-on-all ()
  "Toggle the comments of the test case that the pointer is on."
  (interactive)
  (let ((parse (go-ttest--find-parse)))
	(go-ttest--apply-comment-func-all parse 'off #'go-ttest--case-on)))

(defun go-ttest-add-test ()
  "Add a new test to the nearest test table."
  (interactive)
  ;; TODO Should optionally check for alternative test file.
  (let* ((parse (go-ttest--find-parse))
		 (template (go-ttest--make-case-snippet parse)))
	(goto-char (go-ttest--table-end-pos parse))
	(insert "\n")
	(forward-line -1)
	(yas-expand-snippet template)))

;; NOTES:
;; buffer-live-p: Test to see whether a buffer is live or not. The buffer is stored in a defvar.
;; with-current-buffer: Performs the functions in the body on the buffer.
;; (read-only-mode): Starter mode to view message.
;; (let ((inhibit-read-only t)) (erase-buffer) (local-set-key (kbd "q") 'kill-buffer-and-window) (insert ...))
;; [[file:~/dev/mu/mu4e/mu4e-headers.el::(if%20(eq%20mu4e-split-view%20'single-window)][Creating a new header]]
(defun go-ttest ()
  "Open a buffer to interactivley modify all of the table test cases."
  (interactive)
  (let* ((parse (go-ttest--find-parse))
		 (test-cases (go-ttest--table-case-locs parse))
		 (viewwin (split-window-vertically (- (1+ (length test-cases))))))
	(select-window viewwin)
	(switch-to-buffer (get-buffer-create "*go-table-test-cases*"))
	(read-only-mode)
	(let ((inhibit-read-only t))
	  (erase-buffer)
	  (local-set-key (kbd "q") 'kill-buffer-and-window)
	  (dolist (c test-cases)
		(insert ))
	  (insert "hello, this is a test."))))

(provide 'ttest)
;;; ttest.el ends here
