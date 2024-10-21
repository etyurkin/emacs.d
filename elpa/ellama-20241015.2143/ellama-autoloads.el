;;; ellama-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ellama" "ellama.el" (0 0 0 0))
;;; Generated autoloads from ellama.el

(autoload 'ellama-load-session "ellama" "\
Load ellama session from file." t nil)

(autoload 'ellama-session-remove "ellama" "\
Remove ellama session." t nil)

(autoload 'ellama-session-switch "ellama" "\
Change current active session." t nil)

(autoload 'ellama-session-rename "ellama" "\
Rename current ellama session." t nil)

(autoload 'ellama-context-add-file "ellama" "\
Add file to context." t nil)

(autoload 'ellama-context-add-file-quote "ellama" "\
Add file quote to context interactively." t nil)

(autoload 'ellama-context-add-buffer "ellama" "\
Add BUF to context.

\(fn BUF)" t nil)

(autoload 'ellama-context-add-selection "ellama" "\
Add file to context." t nil)

(autoload 'ellama-context-add-info-node "ellama" "\
Add info NODE to context.

\(fn NODE)" t nil)

(autoload 'ellama-context-add-info-node-quote "ellama" "\
Add info node quote to context interactively." t nil)

(autoload 'ellama-context-add-webpage-quote-eww "ellama" "\
Add webpage quote to context interactively from `eww'." t nil)

(autoload 'ellama-solve-reasoning-problem "ellama" "\
Solve reasoning PROBLEM with absctraction of thought.
Problem will be solved with the chain of questions to LLM.

\(fn PROBLEM)" t nil)

(autoload 'ellama-solve-domain-specific-problem "ellama" "\
Solve domain-specific PROBLEM with `ellama-chain'.

\(fn PROBLEM)" t nil)

(autoload 'ellama-chat "ellama" "\
Send PROMPT to ellama chat with conversation history.

If CREATE-SESSION set, creates new session even if there is an active session.
ARGS contains keys for fine control.

:provider PROVIDER -- PROVIDER is an llm provider for generation.

:session SESSION -- SESSION is a ellama conversation session.

:session-id ID -- ID is a ellama session unique identifier.

:on-done ON-DONE -- ON-DONE a function that's called with
the full response text when the request completes (with BUFFER current).

\(fn PROMPT &optional CREATE-SESSION &rest ARGS)" t nil)

(autoload 'ellama-ask-about "ellama" "\
Ask ellama about selected region or current buffer." t nil)

(autoload 'ellama-ask-selection "ellama" "\
Send selected region or current buffer to ellama chat." t nil)

(autoload 'ellama-complete "ellama" "\
Complete text in current buffer." t nil)

(autoload 'ellama-generate-commit-message "ellama" "\
Generate commit message based on diff." t nil)

(autoload 'ellama-ask-line "ellama" "\
Send current line to ellama chat." t nil)

(autoload 'ellama-translate "ellama" "\
Ask ellama to translate selected region or word at point." t nil)

(autoload 'ellama-translate-buffer "ellama" "\
Ask ellama to translate current buffer." t nil)

(autoload 'ellama-define-word "ellama" "\
Find definition of current word." t nil)

(autoload 'ellama-summarize "ellama" "\
Summarize selected region or current buffer." t nil)

(autoload 'ellama-summarize-killring "ellama" "\
Summarize text from the kill ring." t nil)

(autoload 'ellama-code-review "ellama" "\
Review code in selected region or current buffer." t nil)

(autoload 'ellama-change "ellama" "\
Change selected text or text in current buffer according to provided CHANGE.
When the value of EDIT-TEMPLATE is 4, or with one `universal-argument' as
prefix (\\[universal-argument]), prompt the user to amend the template.

\(fn CHANGE &optional EDIT-TEMPLATE)" t nil)

(autoload 'ellama-improve-grammar "ellama" "\
Enhance the grammar and spelling in the currently selected region or buffer.
When the value of EDIT-TEMPLATE is 4, or with one `universal-argument' as
prefix (\\[universal-argument]), prompt the user to amend the template.

\(fn &optional EDIT-TEMPLATE)" t nil)

(autoload 'ellama-improve-wording "ellama" "\
Enhance the wording in the currently selected region or buffer.
When the value of EDIT-TEMPLATE is 4, or with one `universal-argument' as
prefix (\\[universal-argument]), prompt the user to amend the template.

\(fn &optional EDIT-TEMPLATE)" t nil)

(autoload 'ellama-improve-conciseness "ellama" "\
Make the text of the currently selected region or buffer concise and simple.
When the value of EDIT-TEMPLATE is 4, or with one `universal-argument' as
prefix (\\[universal-argument]), prompt the user to amend the template.

\(fn &optional EDIT-TEMPLATE)" t nil)

(autoload 'ellama-code-edit "ellama" "\
Change selected code or code in current buffer according to provided CHANGE.

\(fn CHANGE)" t nil)

(autoload 'ellama-code-improve "ellama" "\
Change selected code or code in current buffer according to provided CHANGE." t nil)

(autoload 'ellama-code-complete "ellama" "\
Complete selected code or code in current buffer." t nil)

(autoload 'ellama-code-add "ellama" "\
Add new code according to DESCRIPTION.
Code will be generated with provided context from selected region or current
buffer.

\(fn DESCRIPTION)" t nil)

(autoload 'ellama-make-format "ellama" "\
Render selected text or text in current buffer as NEEDED-FORMAT.

\(fn NEEDED-FORMAT)" t nil)

(autoload 'ellama-make-list "ellama" "\
Create markdown list from active region or current buffer." t nil)

(autoload 'ellama-make-table "ellama" "\
Create markdown table from active region or current buffer." t nil)

(autoload 'ellama-provider-select "ellama" "\
Select ellama provider." t nil)

(autoload 'ellama-chat-translation-enable "ellama" "\
Enable chat translation." t nil)

(autoload 'ellama-chat-translation-disable "ellama" "\
Enable chat translation." t nil)

(register-definition-prefixes "ellama" '("ellama-"))

;;;***

;;;### (autoloads nil nil ("ellama-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ellama-autoloads.el ends here
