;;; Major mode for unix "tree" command
;;; TODO: check https://www.emacswiki.org/emacs/hide-lines.el

(require 'json)

(defvar tree-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "v" 'kwarks/tree-view-file)
    (define-key map "o" 'kwarks/tree-find-file)
    (define-key map (kbd "RET") 'kwarks/tree-find-file)
    (define-key map "c" 'kwarks/tree-copy-node-path)
    (define-key map "s" 'kwarks/tree-subtree)
    (define-key map "r" 'kwarks/tree-reload)
    (define-key map (kbd "TAB") 'kwarks/tree-toggle-node)
    (define-key map (kbd "<S-tab>") 'kwarks/tree-toggle-all)
    (define-key map "q" 'kwarks/quit-window)
    map)
  "Keymap for `tree-mode'.")

(defface kwarks/tree-directory-face
  `((t :inherit org-level-1 :height ,(face-attribute 'default :height)))
  "Face for the directory node."
  :group 'tree-mode)

(defface kwarks/tree-file-face
  `((t :inherit org-level-2 :height ,(face-attribute 'default :height)))
  "Face for the file node."
  :group 'tree-mode)

(defface kwarks/tree-link-face
  `((t :inherit org-level-3 :slant italic :height ,(face-attribute 'default :height)))
  "Face for the link node."
  :group 'tree-mode)

(defvar kwarks/tree-ignore-list '("Icon?" ".git" ".idea")
  "Do not list files that match the given pattern.")

(defvar-local tree-path ()
  "Initial tree path")

(defvar-local kwarks--tree-hidden-nodes ()
  "hash-table of invisible nodes.")

(defcustom kwarks/tree-inhibit-help-message nil
  "Non-nil inhibits the help message shown upon entering Tree mode."
  :type 'boolean
  :group 'tree-mode)

(defmacro kwarks--tree-node-action (action)
  `(let ((file-name (get-text-property (point) 'kbd-help)))
     (if file-name (,action file-name))))

(defun kwarks--tree-view-file (file-name)
  "Open FILE-NAME in other window but keep tree window active."
  (let ((tree-window (get-buffer-window (current-buffer))))
    (view-file-other-window file-name)
    (select-window tree-window)))

(defun kwarks/tree-view-file ()
  "View file."
  (interactive)
  (kwarks--tree-node-action kwarks--tree-view-file))

(defun kwarks/tree-find-file ()
  "Edit file."
  (interactive)
  (kwarks--tree-node-action find-file-other-window))

(defun kwarks/tree-subtree ()
  "Open subtree."
  (interactive)
  (kwarks--tree-node-action kwarks/tree))

(defun kwarks/tree-copy-node-path ()
  "Copy node path to clipboard."
  (interactive)
  (kwarks--tree-node-action kill-new))

(defun kwarks/tree-reload ()
  "Reload tree."
  (interactive)
  (kwarks--reload-tree tree-path))

(defun kwarks/quit-window ()
  "Close tree buffer."
  (interactive)
  (clrhash kwarks--tree-hidden-nodes)
  (setq-local kwarks--tree-hidden-nodes nil)
  (quit-window t))

(defun kwarks/tree-toggle-node ()
  "Collapse/expand node."
  (interactive)
  (kwarks--tree-node-action kwarks--tree-toggle-node))

(defun kwarks/tree-toggle-all ()
  "Collapse/expand node including child nodes."
  (interactive)
  (kwarks--tree-node-action kwarks--tree-toggle-all))

(defun kwarks--tree-toggle-all (node)
  (if (gethash node kwarks--tree-hidden-nodes)
      (kwarks--tree-expand-all node)
    (kwarks--tree-collapse-all node)))

(defun kwarks--tree-collapse-all (node)
  (kwarks--tree-collapse-node node t))

(defun kwarks--tree-expand-all (node)
  (save-excursion
    (mapc (lambda (parent)
            (if (string-prefix-p node parent)
                (kwarks--tree-expand-node parent)))
          (hash-table-keys kwarks--tree-hidden-nodes))))

(defun kwarks--tree-hide-node (node start end)
  "Add an overlay from `start' to `end' in the current buffer. Push the
overlay onto the kwarks--tree-hidden-nodes hash-table"
  (let ((overlay (make-overlay start end))
        (items (gethash node kwarks--tree-hidden-nodes)))
    (puthash node (cons overlay items) kwarks--tree-hidden-nodes)
    (overlay-put overlay 'invisible 'hl)))

(defun kwarks--tree-expand-node (node)
  (mapc (lambda (overlay) (delete-overlay overlay)) 
        (gethash node kwarks--tree-hidden-nodes))
  (remhash node kwarks--tree-hidden-nodes))

(defun kwarks--tree-collapse-node (node collapse-children-p)
  (save-excursion
    (forward-line 1)
    
    (while (string-prefix-p node (get-text-property (point) 'parent))
      (let* ((line-start (point))
             (parent (get-text-property (point) 'parent)))
        (kwarks--tree-hide-node
         (if collapse-children-p parent node) line-start (progn (forward-line 1) (point)))))))

(defun kwarks--tree-toggle-node (node)
  (if (gethash node kwarks--tree-hidden-nodes)
      (kwarks--tree-expand-node node)
    (kwarks--tree-collapse-node node nil)))

(defun kwarks--make-tree (root)
  (let ((cmd (format "tree \"%s\" --dirsfirst --noreport -J -a -q -I '%s'"
                     root
                     (string-join kwarks/tree-ignore-list "|")))
        (json-object-type 'plist)
        (json-array-type 'list)
        (json-key-type 'symbol))
    (json-read-from-string (shell-command-to-string cmd))))

(defun kwarks--tree-node-set-face (node is-directory)
  (cond (is-directory (propertize node 'font-lock-face 'kwarks/tree-directory-face))
        (t (propertize node 'font-lock-face 'kwarks/tree-file-face))))

(defun kwarks--tree-link-set-face (name target)
  (let ((target-face (if (file-directory-p target)
                         'kwarks/tree-directory-face
                       'kwarks/tree-file-face)))
    (format "%s -> %s" (propertize name 'font-lock-face 'kwarks/tree-link-face)
            (propertize target 'font-lock-face target-face))))

(defun kwarks--normalize-path (path record)
  "Add forward slash to the end of the PATH if it's a directory
   or a link to a directory."
  (let ((type (plist-get record 'type))
        (target (plist-get record 'target)))
    (cond ((string-match "\/$" path) path)
          ((or (and (string-equal type "link")
                    (file-directory-p target))
               (string-equal type "directory")) (format "%s/" path))
          (t path))))

(defun kwarks--make-full-name (parent name)
  (cond ((= 0 (length parent)) name)
        ((string-match "\/$" parent) (format "%s%s" parent name))
        (t (format "%s/%s" parent name))))

(defun kwarks--print-tree (tree parent spacing)
  (while tree
    (let* ((record (car tree))
           (type (plist-get record 'type))
           (is-link (string-equal type "link"))
           (name (plist-get record 'name))
           (full-name (kwarks--normalize-path
                       (kwarks--make-full-name parent name) record))
           (children (plist-get record 'contents))
           (is-root (= (length spacing) 0))
           (is-directory (string-equal type "directory"))
           (has-more (consp (cdr tree)))
           (prefix (if has-more "├──" "└──"))
           (pad (if is-root 0 3))
           (child-spacing (if has-more
                              (format "%s│%s" spacing (make-string pad ?\s))
                            (format "%s %s" spacing (make-string pad ?\s)))))
      (if is-root
          (insert name)
        (insert
         (propertize (format "%s%s %s" spacing prefix
                             (if is-link
                                 (kwarks--tree-link-set-face name (plist-get record 'target))
                               (kwarks--tree-node-set-face name is-directory)))
                     'kbd-help full-name
                     'parent parent)))
      
      (newline)
      (if (and (consp children) (null (plist-get (car children) 'error)))
          (kwarks--print-tree children full-name child-spacing))
      (setq tree (cdr tree)))))

(defun kwarks--get-buffer-directory ()
  "Return active buffer directory or '~'"
  (let ((name (buffer-file-name)))
    (cond ((null name) "~")
          (t (file-name-directory name)))))

(defun kwarks--reload-tree (path)
  (clrhash kwarks--tree-hidden-nodes)
  (let ((pos (point)))
    (read-only-mode -1)
    (erase-buffer)
    (kwarks--print-tree (kwarks--make-tree path) "" "")
    (goto-char pos)))

(define-derived-mode tree-mode special-mode "Tree"
  "Major mode for viewing directory tree."
  (setq-local help-at-pt-display-when-idle t)
  (setq-local kwarks--tree-hidden-nodes (make-hash-table :test 'equal))
  (help-at-pt-set-timer))

(defun kwarks--tree-show-help ()
  (unless kwarks/tree-inhibit-help-message
    (message "Tree mode: type h for commands, q to quit.")))

(defun kwarks/tree (path)
  "Display interactive directory tree."
  (interactive (list (read-directory-name "Directory: " 
                                          (kwarks--get-buffer-directory))))
  (let* ((actual-path (file-name-directory path))
         (buffer-name (format "*tree %s*" actual-path))
         (inhibit-message t))
    (get-buffer-create buffer-name)
    (set-buffer buffer-name)
    (switch-to-buffer buffer-name)
    (tree-mode)
    (setq-local tree-path actual-path)
    (kwarks--reload-tree actual-path)
    (toggle-truncate-lines 1))
  (kwarks--tree-show-help))
