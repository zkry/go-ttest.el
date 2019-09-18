;;; go-ttest-test.el --- Unit tests for go-ttest -*- lexical-binding: t -*-

;; Author: Zachary Ryan Romero
;; Maintainer: Zachary Ryan Romero
;; Package-Requires: (dependencies)

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

(ert-deftest test-struct-type-regexp ()
  ;; Test positive cases
  (dolist (l '("		given    string"
			   "		expected string"
			   "		expected string"
			   "		rule             *entity.Rule"
			   "		requestContext   *bidderlogic.BidderRequestContext"
			   "		userDestinations []entity.UserDestinationSegmentation"
			   "		expectedResult   ruleCheckResults "
			   "		testName   string"
			   "		rule       *entity.Rule"
			   "		agent      string"
			   "		shouldPass bool"
			   "  		Responses        []*entity.TeaSelectionResult"
			   "		ExpectedResponse *entity.TeaSelectionResult"
			   "		ThemeBlacklist    []string"
			   "		DetectedVerticals []*rtb.BidRequest_Vertical"
			   "		ExpectedOK        bool"
			   "		UUIDTargeting []string"
			   "		UUID          string"
			   "		ExpectedOK    bool"
			   "		rule             *entity.Rule"
			   "		userSegments     []entity.UserDestinationSegmentation"
			   "		expectedSegments []entity.UserDestinationSegmentation"
			   "		expectedFilter   report.LocationFilter"))
	(should (string-match go-ttest--struct-type-regexp l)))
  
  ;; Test negative cases
  (dolist (l '("	//	userSegments     []entity.UserDestinationSegmentation"
			   "		//expectedSegments []entity.UserDestinationSegmentation"
			   "//		expectedFilter   report.LocationFilter"
			   "		}{  "))
	(should (not (string-match go-ttest--struct-type-regexp l))))
  
  ;; Test part extraction
  (let ((line "		expectedSegments []entity.UserDestinationSegmentation"))
	(string-match go-ttest--struct-type-regexp line)
	(should (equal (match-string 1 line) "expectedSegments"))
	(should (equal (match-string 2 line) "[]entity.UserDestinationSegmentation"))))


(ert-deftest test-parse-ttest-slice ()
  (with-temp-buffer
	(insert "	testData := []struct {
		Responses        []*entity.TeaSelectionResult
		ExpectedResponse *entity.TeaSelectionResult
	}{
		{
			[]*entity.TeaSelectionResult{
				{QZV: 1, Tea: entity.Tea{TeaData: entity.TeaData{TreeNo: 3}}},
				{QZV: 2, Tea: entity.Tea{TeaData: entity.TeaData{TreeNo: 3}}},
				{QZV: 3, Tea: entity.Tea{TeaData: entity.TeaData{TreeNo: 2}}},
				{QZV: 4, Tea: entity.Tea{TeaData: entity.TeaData{TreeNo: 2}}},
				{QZV: 5, Tea: entity.Tea{TeaData: entity.TeaData{TreeNo: 0}}},
				{QZV: 6, Tea: entity.Tea{TeaData: entity.TeaData{TreeNo: 0}}},
			},
			&entity.TeaSelectionResult{
				QZV:            2,
				Tea:          entity.Tea{TeaData: entity.TeaData{TreeNo: 3}},
				LocationReport: map[string]report.LocationFilter{\"abc\": report.LocationRespondWithTea},
				Rule:           &entity.Rule{CommonLocation: go_location_parse.CommonLocation{RuleID: \"abc\"}},
			},
		},
	}

")
	(goto-char (point-min))
	(let ((parse-results (go-ttest--parse-ttest)))
	  (should (equal (go-ttest--table-type parse-results) 'slice))
	  (should (equal (go-ttest--table-name-field parse-results)
					 nil))
	  (should (equal (length (go-ttest--table-case-locs parse-results))
					 1))
	  (should (equal (nth 0 (go-ttest--table-case-locs parse-results))
					 '(123 . on)))))
  (with-temp-buffer
	(insert "	testData := []struct {
		Name          string
		UUIDTargeting []string
		UUID          string
		ExpectedOK    bool
	}{
		{\"Test 1\", nil, \"asdf\", true},
		//		{\"Test 2\", []string{\"asdf\", \"qwer\"}, \"asdf\", true},
		{\"Test 3\", []string{\"asdf\", \"qwer\"}, \"qwer\", true},
		{\"Test 4\", []string{\"asdf\", \"qwer\"}, \"foof\", false},
		//{\"Test 5\", []string{}, \"foof\", false},
	}")
	(goto-char (point-min))
	(let ((parse-results (go-ttest--parse-ttest)))
	  (should (equal (go-ttest--table-type parse-results) 'slice))
	  (should (equal (go-ttest--table-name-field parse-results)
					 (cons "Name" 0)))
	  (should (equal (length (go-ttest--table-case-locs parse-results))
					 5))
	  (should (equal (nth 0 (go-ttest--table-case-locs parse-results))
					 '(121 . on)))
	  (should (equal (nth 1 (go-ttest--table-case-locs parse-results))
					 '(154 . off)))
	  (should (equal (nth 4 (go-ttest--table-case-locs parse-results))
					 '(321 . off)))))
  (with-temp-buffer
	(insert "	testData := []struct {
		cakeIngredientlisted map[string]struct{}
		userCake             []kitchen.UserDessertChoice
		expectedCake         []kitchen.UserDessertChoice
		expectedSifter       report.FlourSifter
	}{
		{nil, nil, nil, report.FlourSifterPassed},
		{
			map[string]struct{}{\"600,1\": {}},
			[]kitchen.UserDessertChoice{{}},
			nil,
			report.FlourNotInChoiceIngredientlist,
		},
		{
			map[string]struct{}{\"400,1\": {}},
			[]kitchen.UserDessertChoice{{Choice: \"400,1\"}, {Choice: \"500,1\"}},
			[]kitchen.UserDessertChoice{{Choice: \"400,1\"}},
			report.FlourSifterPassed,
		},
	}")
	(goto-char (point-min))
	(let ((parse-results (go-ttest--parse-ttest)))
	  (should (equal (go-ttest--table-type parse-results) 'slice))
	  (should (equal (go-ttest--table-name-field parse-results)
					 nil))
	  (should (equal (length (go-ttest--table-case-locs parse-results))
					 3))
	  (should (equal (nth 0 (go-ttest--table-case-locs parse-results))
					 '(216 . on)))
	  (should (equal (nth 1 (go-ttest--table-case-locs parse-results))
					 '(261 . on)))))
  (with-temp-buffer
	(insert "	testData := []struct {
		name         string
		givenName    string
		expectedName string
	}{
		{
			name:         \"Basic Name\",
			givenName:    \"mehmet\",
			expectedName: \"Mehmet\",
		},
		// {
		// 	name:         \"Another basic Name\",
		// 	givenName:    \"serkan\",
		// 	expectedName: \"Serkan\",
		// },
		{
			name:         \"Yet another basic name\",
			givenName:    \"kutay\",
			expectedName: \"Kutay\",
		},
	}")
	(goto-char (point-min))
	(let ((parse-results (go-ttest--parse-ttest)))
	  (should (equal (go-ttest--table-type parse-results) 'slice))
	  (should (equal (go-ttest--table-name-field parse-results)
					 '("name" . 0)))
	  (should (equal (length (go-ttest--table-case-locs parse-results))
					 3))
	  (should (equal (nth 0 (go-ttest--table-case-locs parse-results))
					 '(95 . on)))
	  (should (equal (nth 1 (go-ttest--table-case-locs parse-results))
					 '(189 . off))))))



(ert-deftest test-parse-ttest-map ()
  (with-temp-buffer
	(insert
"	testCases := map[string]struct {
		given    string
		expected int
	}{
		\"Identity\": {
			given:    \"((λ (x) x) 10)\",
			expected: 10,
		},
		\"Simple addition\": {
			given:    \"((λ (x y) (+ x y)) 1 2)\",
			expected: 3,
		},
		\"Curried application \": {
			given:    \"(((λ (x y) (λ (z) (+ x y z))) 1 2) 3)\",
			expected: 6,
		},
		// \"Bad syntax\": {
		// 	given:    \"((λ x λ (+ x y)) 1 2)\",
		// 	expected: -1,
		// },
		\"Unused variables\": {
			given:    \"(((λ (x y) (λ (z) 99)) 1 2) 3)\",
			expected: 99,
		},
	}
")
	(goto-char (point-min))
	(let ((parse-results (go-ttest--parse-ttest)))
	  (should (equal (go-ttest--table-type parse-results) 'map))
	  (should (equal (go-ttest--table-name-field parse-results)
					 nil))
	  (should (equal (length (go-ttest--table-case-locs parse-results))
					 5))
	  (should (equal (nth 0 (go-ttest--table-case-locs parse-results))
					 '(72 . on)))
	  (should (equal (nth 3 (go-ttest--table-case-locs parse-results))
					 '(328 . off))))))


(ert-deftest test-find-case ()
  (with-temp-buffer
	(insert "package main

func TestXxx(t *testing.T) {
	testCases := map[string]struct {
		given    string
		expected int
	}{
		\"Identity\": {
			given:    \"((λ (x) x) 10)\",
			expected: 10,
		},
		// \"Simple addition\": {
		// 	given:    \"((λ (x y) (+ x y)) 1 2)\",
		// 	expected: 3,
		// },
		\"Curried application \": {
			given:    \"(((λ (x y) (λ (z) (+ x y z))) 1 2) 3)\",
			expected: 6,
		},
	}
}")
	;; Case No. 0
	(dolist (cn '(115 132 141 165 166 182 183))
	  (goto-char cn)
	  (should (equal (go-ttest--find-case)
					 '(115 . on)))))
  
  
  ;; TODO support }, { 
  )
;; Bookmark: fix this test

;;; go-ttest-test.el ends here
