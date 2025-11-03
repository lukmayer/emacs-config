;;; better-quarto.el --- helpful functions for using quarto in emacs
;; Package-Requires: ((emacs "26.1") (general "0.5"))
;;; Commentary:
;;; Keyboard-shortcuts for navigating cells
;;; Keyboard-shortcuts for pasting code-chunks into a REPL via vterm
;;; Color changes to make YAML, code blocks, divs more appealing

;;; Code:

;; Require dependencies
(require 'general)

(defun find-yaml-block ()
  "Find the first thing that appears to be a YAML block."
  (save-excursion
    (goto-char (point-min))
    ;; skip white space / newline
    (skip-chars-forward "\n\t ")

    (when (looking-at "^---[ \t]*$")
      (let ((start-pos (point)))
        (forward-line 1)
	(if (re-search-forward "^---[ \t]*$" nil t)
            (progn
              (forward-line 1)  
              (list start-pos (point)))
          nil)))))

(defun highlight-yaml-block ()
  "Highlight the YAML frontmatter block with a background color."
  (interactive)
  (let ((block (find-yaml-block)))
    (when block
      (let* ((start (nth 0 block))
             (end (nth 1 block))
             (ov (make-overlay start end))
             (bg-color (face-attribute 'mode-line-inactive :background nil t)))
        (overlay-put ov 'face `(:background ,bg-color :extend t))
        (overlay-put ov 'yaml-highlight t)))))

(defun remove-yaml-highlight ()
  "Remove YAML block highlighting."
  (interactive)
  (remove-overlays (point-min) (point-max) 'yaml-highlight t))

;; Code block identification
(defun find-next-code-block ()
  "Find the next code block and return (start end language) or nil."
  (when (re-search-forward "^```{\\([A-Za-z]+\\).*}$" nil t)
    (let ((start-pos (match-beginning 0))
          (language (match-string 1)))
      (if (re-search-forward "^```$" nil t)
          (progn
            (forward-line 1)
            (list start-pos (point) language))
        (list start-pos (point-max) language)))))

(defun find-all-code-blocks ()
  "Find all code blocks in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((blocks '())
          (current-block nil))  
      (while (setq current-block (find-next-code-block))  
        (push current-block blocks))
      (reverse blocks))))

;; Code block navigation
(defun goto-next-code-block ()
  "Navigate to the next code block."
  (interactive)
  (if (re-search-forward "^```{\\([A-Za-z]+\\).*}$" nil t)
      (message "Found %s block" (match-string 1))
    (message "No more code blocks")))


(defun goto-prev-code-block ()
  "Navigate to the previous code block."
  (interactive)
  (if (re-search-backward "^```{\\([A-Za-z]+\\).*}$" nil t)
      (message "Found %s block" (match-string 1))
    (message "No more code blocks")))


;; Code block styling
(defun highlight-all-code-blocks ()
  "Highlight all code blocks with a background color."
  (interactive)
  (let ((blocks (find-all-code-blocks)))
    (dolist (block blocks)
      (let* ((start (nth 0 block))
             (end (nth 1 block))
             (language (nth 2 block))
             (ov (make-overlay start end))
             (bg-color (face-attribute 'mode-line-inactive :background nil t)))
        (overlay-put ov 'face `(:background ,bg-color :extend t))
        (overlay-put ov 'code-block-highlight t)))))

;; Styling non-interactive code blocks
;; Non-interactive code block identification
(defun find-next-passive-block ()
  "Find the next non-interactive code block and return (start end language) or nil."
  (when (re-search-forward "^```[A-Za-z]+$" nil t)
    (let ((start-pos (match-beginning 0))
          (language (match-string 1)))
      (if (re-search-forward "^```$" nil t)
          (progn
            (forward-line 1)
            (list start-pos (point) language))
        (list start-pos (point-max) language)))))

(defun find-all-passive-blocks ()
  "Find all non-interactive code blocks in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((blocks '())
          (current-block nil))  
      (while (setq current-block (find-next-passive-block))  
        (push current-block blocks))
      (reverse blocks))))


;; Non-interactive code block styling
(defun highlight-all-passive-blocks ()
  "Highlight all code blocks with a background color."
  (interactive)
  (let ((blocks (find-all-passive-blocks)))
    (dolist (block blocks)
      (let* ((start (nth 0 block))
             (end (nth 1 block))
             (language (nth 2 block))
             (ov (make-overlay start end))
             (bg-color (face-attribute 'mode-line-inactive :background nil t)))
        (overlay-put ov 'face `(:background ,bg-color :extend t))
        (overlay-put ov 'code-block-highlight t)))))

;; Colored divs
;; div highlighting colors
(defface quarto-div-depth-1-face
  '((t (:foreground "#e06c75")))  ; red
  "Face for depth 1 divs.")

(defface quarto-div-depth-2-face
  '((t (:foreground "#98c379")))  ; green
  "Face for depth 2 divs.")

(defface quarto-div-depth-3-face
  '((t (:foreground "#61afef")))  ; blue
  "Face for depth 3 divs.")

(defface quarto-div-depth-4-face
  '((t (:foreground "#c678dd")))  ; purple
  "Face for depth 4 divs.")

(defface quarto-div-depth-5-face
  '((t (:foreground "#e5c07b")))  ; yellow
  "Face for depth 5 divs.")

(defface quarto-div-unmatched-face
  '((t (:foreground "#ff0000" :weight bold)))  ; bright red, bold
  "Face for unmatched divs.")


(defun find-all-divs ()
  "Find all div markers and return list of (line-start line-end colon-count is-opening)."
  (save-excursion
    (goto-char (point-min))
    (let ((divs '()))
      (while (re-search-forward "^\\(:::+\\)\\(.*\\)$" nil t)
        (let* ((line-start (line-beginning-position))
               (line-end (line-end-position))
               (colons (match-string 1))
               (colon-count (length colons))
               (rest (string-trim (match-string 2)))
               ;; Opening if has non-comment content
               (is-opening (and (> (length rest) 0)
                               (not (string-match-p "^<!--.*-->$" rest)))))
          (push (list line-start line-end colon-count is-opening) divs)))
      (reverse divs))))


(defun pair-divs (divs)
  "Pair opening and closing DIVS, matching innermost first.
Returns (matched-pairs unmatched-divs)."
  (let ((stack '())
        (pairs '())
        (unmatched '())
        (depth 0))
    (dolist (div divs)
      (let ((line-start (nth 0 div))
            (line-end (nth 1 div))
            (colon-count (nth 2 div))
            (is-opening (nth 3 div)))
        (if is-opening
            ;; Push opener onto stack with current depth
            (progn
              (push (list line-start line-end colon-count depth) stack)
              (setq depth (1+ depth)))
          ;; Closing div - try to match with top of stack
          (if (and stack
                   (= colon-count (nth 2 (car stack))))
              ;; Match found!
              (let ((opener (pop stack)))
                (setq depth (1- depth))
                (push (list (nth 0 opener) (nth 1 opener)    ; opener line bounds
                           line-start line-end                ; closer line bounds
                           (nth 3 opener))                    ; depth
                      pairs))
            ;; No match - unmatched closer
            (push (list line-start line-end 'unmatched) unmatched)))))

    ;; Anything left on stack is unmatched opener
    (dolist (opener stack)
      (push (list (nth 0 opener) (nth 1 opener) 'unmatched) unmatched))

    (list (reverse pairs) (reverse unmatched))))



(defun highlight-all-divs ()
  "Highlight all div markers with rainbow colors based on nesting depth."
  (interactive)
  (let* ((divs (find-all-divs))
         (result (pair-divs divs))
         (pairs (nth 0 result))
         (unmatched (nth 1 result)))

    ;; highlight matched pairs
    (dolist (pair pairs)
      (let* ((open-start (nth 0 pair))
             (open-end (nth 1 pair))
             (close-start (nth 2 pair))
             (close-end (nth 3 pair))
             (depth (nth 4 pair))
             ;; cycle through 5 colors
             (face-num (1+ (mod depth 5)))
             (face (intern (format "quarto-div-depth-%d-face" face-num))))

        ;; highlight opening div
        (let ((ov (make-overlay open-start open-end)))
          (overlay-put ov 'face face)
          (overlay-put ov 'div-highlight t))

        ;; highlight closing div
        (let ((ov (make-overlay close-start close-end)))
          (overlay-put ov 'face face)
          (overlay-put ov 'div-highlight t))))

    ;; highlight unmatched divs in red
    (dolist (unmatched-div unmatched)
      (let* ((start (nth 0 unmatched-div))
             (end (nth 1 unmatched-div))
             (ov (make-overlay start end)))
        (overlay-put ov 'face 'quarto-div-unmatched-face)
        (overlay-put ov 'div-highlight t)))))

(defun remove-div-highlights ()
  "Remove all div highlighting."
  (interactive)
  (remove-overlays (point-min) (point-max) 'div-highlight t))



;; LaTeX 
(defun find-all-latex-expressions ()
  "Find all LaTeX expressions: inline ($...$), display ($$...$$), and multiline."
  (save-excursion
    (goto-char (point-min))
    (let ((expressions '()))
      ;; Find display math $$ $$ (multiline supported)
      (while (re-search-forward "\\$\\$" nil t)
        (let ((start (match-beginning 0)))
          (when (re-search-forward "\\$\\$" nil t)
            (let ((end (match-end 0)))
              (push (list start end 'display) expressions)))))
      ;; Find inline math $ $ (not preceded/followed by $)
      (goto-char (point-min))
      (while (re-search-forward "\\([^$]\\|^\\)\\$\\([^$]+\\)\\$" nil t)
        (let ((start (match-beginning 2))
              (end (match-end 2)))
          (push (list start end 'inline) expressions)))
      (reverse expressions))))

(defface quarto-latex-face
  '((t (:extend t)))
  "Face for LaTeX expressions.")

(defun highlight-all-latex ()
  "Highlight all LaTeX expressions with the same color as code blocks."
  (interactive)
  (let ((expressions (find-all-latex-expressions))
        (bg-color (face-attribute 'mode-line-inactive :background nil t)))
    (dolist (expr expressions)
      (let* ((start (nth 0 expr))
             (end (nth 1 expr))
             (ov (make-overlay start end)))
        (overlay-put ov 'face `(:background ,bg-color :extend t))
        (overlay-put ov 'latex-highlight t)))))

(defun remove-latex-highlights ()
  "Remove all LaTeX highlighting."
  (interactive)
  (remove-overlays (point-min) (point-max) 'latex-highlight t))

;; Headers

(defun highlight-all-headers ()
  "Highlight all markdown headers with colors from theme."
  (interactive)
  (let ((headers (find-all-headers))
        (theme-colors (list
          (face-attribute 'font-lock-keyword-face :foreground nil t)
          (face-attribute 'font-lock-string-face :foreground nil t)
          (face-attribute 'font-lock-function-name-face :foreground nil t)
          (face-attribute 'font-lock-builtin-face :foreground nil t)
          (face-attribute 'font-lock-constant-face :foreground nil t)
          (face-attribute 'font-lock-warning-face :foreground nil t))))
    (dolist (header headers)
      (let* ((start (nth 0 header))
             (end (nth 1 header))
             (level (nth 2 header))
             (color (nth (min (1- level) 5) theme-colors))
             (ov (make-overlay start end)))
        (overlay-put ov 'face `(:foreground ,color :weight bold))
        (overlay-put ov 'header-highlight t)))))

(defun remove-header-highlights ()
  "Remove all header highlighting."
  (interactive)
  (remove-overlays (point-min) (point-max) 'header-highlight t))

(defun find-all-headers ()
  "Find all markdown headers and return list of (start end level)."
  (save-excursion
    (goto-char (point-min))
    (let ((headers '()))
      (while (re-search-forward "^\\(#+\\) " nil t)
        (let* ((hashes (match-string 1))
               (level (length hashes))
               (line-start (line-beginning-position))
               (line-end (line-end-position)))
          (push (list line-start line-end level) headers)))
      (reverse headers))))



;; REFRESH
(defun refresh-code-block-highlights ()
  "Remove and re-apply code block highlighting."
  (interactive)
  ;; remove old highlights
  (remove-overlays (point-min) (point-max) 'code-block-highlight t)
  (remove-overlays (point-min) (point-max) 'latex-highlight t)
  (remove-yaml-highlight)
  (remove-div-highlights)
  (remove-header-highlights)
  ;; apply new highlights
  (highlight-all-code-blocks)
  (highlight-all-passive-blocks) 
  (highlight-yaml-block)
  (highlight-all-divs)
  (highlight-all-latex)
  (highlight-all-headers))

;; REPL functionality
(defun find-current-code-block ()
  "Find the code block containing point."
  (let ((blocks (find-all-code-blocks))
        (current-pos (point))
        (result nil))
    (dolist (block blocks result)
      (let ((start (nth 0 block))
            (end (nth 1 block)))
        (when (and (>= current-pos start)
                   (<= current-pos end))
          (setq result block))))))


(defun get-code-block-content (block)
  "Extract the content from a code block."
  (let ((start (nth 0 block))
        (end (nth 1 block)))
    (save-excursion
      (goto-char start)
      (forward-line 1)  ; ignore ```{lang}
      (let ((code-start (point)))
        (goto-char end)
        (forward-line -2)  ; avoid ```
        (end-of-line)      ; end of last line
        (let ((code-end (point)))
          (buffer-substring-no-properties code-start code-end))))))

(defvar vterm-repl-commands ;; SPECIFY YOUR REPL COMMANDS HERE
  '(("python" . "ipython")
    ("r" . "radian")
    ("julia" . "julia"))
  "Mapping of code block language names to REPL commands.")

(defvar vterm-repl-language-aliases
  '(("py" . "python")
    ("jl" . "julia"))
  "Mapping of short language names to full names.")

(defun normalize-language-name (language)
  "Convert short language names to canonical names."
  (let ((alias (assoc language vterm-repl-language-aliases)))
    (if alias
        (cdr alias)      
      language)))       

(defun send-code-to-vterm (code language)
  "Send CODE to the appropriate vterm buffer based on LANGUAGE."
(let* ((language (normalize-language-name language))  
         (buffer-name (format "*vterm-%s*" language))
         (repl-command (cdr (assoc language vterm-repl-commands))))

    (unless repl-command
      (error "No REPL command defined for language: %s" language))

    ;; Create and display buffer if it doesn't exist
    (unless (get-buffer buffer-name)
      (split-window-right)
      (other-window 1)
      (vterm)
      (rename-buffer buffer-name)
      (vterm-send-string repl-command)
      (vterm-send-return)
      (other-window 1))

    ;; Send code using bracketed paste
    (with-current-buffer buffer-name
      (vterm-send-string "\e[200~")  ; Start bracketed paste
      (vterm-send-string code)
      (vterm-send-string "\e[201~")  ; End bracketed paste
      (vterm-send-return))))


(defun run-code-block ()
  "Send the current code block or selected region to its language-specific vterm."
  (interactive)
  (let ((block (find-current-code-block)))
    (if block
        (let* ((language (nth 2 block))
               (code (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (get-code-block-content block))))
          (send-code-to-vterm code language)
          (message "Sent %s code to vterm" language))
      (message "Not in a code block"))))

(defun run-all-code-blocks-above ()
  "Run all code blocks above current position."
  (interactive)
  (let* ((all-blocks (find-all-code-blocks))
         (current-pos (point))
         (blocks-above (seq-filter 
                        (lambda (block) 
                          (< (nth 1 block) current-pos))
                        all-blocks))
         (code-by-language '()))

    ;; Group code by language
    (dolist (block blocks-above)
      (let* ((language (nth 2 block))
             (code (get-code-block-content block))
             (entry (assoc language code-by-language)))
        (if entry
            (push code (cdr entry))
          (push (list language code) code-by-language))))

    ;; Send each language's code to vterm
    (dolist (entry code-by-language)
      (let* ((language (car entry))
             (code-list (reverse (cdr entry)))  ; reverse because push adds to front
             (combined-code (string-join code-list "\n")))
        (send-code-to-vterm combined-code language)
        (message "Sent %d %s blocks" (length code-list) language)))))

(defun run-all-code-blocks-below ()
  "Run all code blocks below current position."
  (interactive)
  (let* ((all-blocks (find-all-code-blocks))
         (current-pos (point))
         (blocks-below (seq-filter 
                        (lambda (block) 
                          (> (nth 0 block) current-pos))
                        all-blocks))
         (code-by-language '()))

    (dolist (block blocks-below)
      (let* ((language (nth 2 block))
             (code (get-code-block-content block))
             (entry (assoc language code-by-language)))
        (if entry
            (push code (cdr entry))
          (push (list language code) code-by-language))))

    (dolist (entry code-by-language)
      (let* ((language (car entry))
             (code-list (reverse (cdr entry)))
             (combined-code (string-join code-list "\n")))
        (send-code-to-vterm combined-code language)
        (message "Sent %d %s blocks" (length code-list) language)))))

;; Inserting code chunks
(defun insert-quarto-code-block ()
  "Insert a Quarto code block template with point positioned for language input."
  (interactive)
  (let ((start-pos (point)))
    ;; Insert the template
    (insert "```{}\n\n```")
    ;; Move point to inside the curly braces
    (goto-char (+ start-pos 4))
    ;; Optionally refresh highlights
    (refresh-code-block-highlights)))

;; Style blocks
(defun insert-style-block ()
  "Insert an HTML style block with point positioned between tags."
  (interactive)
  (insert "<style>\n\n</style>")
  (forward-line -1)  ; Move up one line to the empty line
  (refresh-code-block-highlights))

;; Style blocks
(defun insert-script-block ()
  "Insert an HTML script block with point positioned between tags."
  (interactive)
  (insert "<script>\n\n</script>")
  (forward-line -1)  ; Move up one line to the empty line
  (refresh-code-block-highlights))

;; Style blocks
(defun insert-div-block ()
  "Insert an ::: {.style} block with point positioned between tags."
  (interactive)
  (insert "::: {}\n\n:::")
  (forward-line -1)  ; Move up one line to the empty line
  (refresh-code-block-highlights))

;; keybindings
(with-eval-after-load 'markdown-mode
  (general-create-definer my-leader-def
    :states '(normal visual)
    :keymaps 'markdown-mode-map
    :prefix "SPC")

  (my-leader-def
    "n" 'goto-next-code-block
    "m" 'goto-prev-code-block
    "u" 'refresh-code-block-highlights
    "r" 'run-code-block
    "a" 'run-all-code-blocks-above
    "b" 'run-all-code-blocks-below
    "i" 'insert-quarto-code-block
    "s" 'insert-style-block
    "j" 'insert-script-block
    "q" 'insert-div-block
    "k" 'quarto-preview))

;; automatic application on file open
(defun better-quarto-setup ()
  "Setup highlighting for Quarto files."
  (when (and (buffer-file-name)
             (string-match-p "\\.qmd\\'" (buffer-file-name)))
    (refresh-code-block-highlights)))

(add-hook 'markdown-mode-hook 'better-quarto-setup)

(provide 'better-quarto)

;;; better-quarto.el ends here
