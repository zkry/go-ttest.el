# go-ttest: Table Tests in Go Made Easy

go-ttest is a package that includes a number of helper functions for working with table tests in Go code. Often times table tests in Go can become rather unwieldy, requiring a lot of movement, commenting, typing, etc. For example, adding a new field for a table test with 10 cases in it will require a lot of tiny code changes to the table. go-ttest provides a function that allows you to type the field name, type and default value one, and tab complete all of the entries to all of the cases.

Another common use case is that you have a large table test and one of the tests is failing. You add Println statements to debug the case but since all of the test cases are running you get a lot of unwantet input. go-ttest provieds a simple way to uncomment out all but the test case of interest.

## Installation

Currently to install go-ttest you must put the go-ttest.el file somewhere in your load path and call `(require 'go-ttest')`.  I intend to upload this to MELPA in the near future.

## Usage

The following is a list of the main commands that go-ttest provides:

- **go-ttest** : Opens up a buffer showing a list of all of the table tests.  go-ttest will try to infer the name of the test. When in this buffer there are a number of commands at your disposal.
  - **c*** : Comment out test at point
  - **C** : Comment out all tests
  - **u** : Uncomment the test at point
  - **U** : Uncomment all tests
  - **o** : Leave only the test at the current point uncommented

![Running the o command](/images/o-command.png)

- **go-ttest-add-field** : Propts for the name, type, and default value for a new field to be added. The field is then added to all test cases, using Yasnippet to fill in the value.

![Add Field Command](/images/add-field.gif)

- **go-ttest-add-test** : Adds a new test case at the bottom of the list of cases, allowing for tab-completion on the various fields of the test.

![Add Test Command](/images/add-case.gif)


