;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

;; Configure Modus Vivendi with Helix-like colors
(setq modus-vivendi-palette-overrides
      '(;; Basic backgrounds and foregrounds closer to Helix's values
        (bg-main "#000000")          ; Pure black background
        (bg-dim "#1e1e1e")           ; Slightly lighter for contrast
        (fg-main "#ffffff")          ; Pure white foreground
        (fg-dim "#989898")           ; Dimmed text
        (fg-alt "#c6daff")           ; Alternative foreground

        ;; Prominent syntactic elements
        (builtin "#b6a0ff")          ; magenta-cooler from Helix
        (keyword "#b6a0ff")          ; magenta-cooler for keywords
        (string "#79a8ff")           ; blue-warmer for strings
        (constant "#00bcff")         ; blue-cooler for constants
        (fnname "#feacd0")           ; magenta for function names
        (variable "#6ae4b9")         ; cyan-cooler for variables
        (type "#6ae4b9")             ; cyan-cooler for types

        ;; Comments and documentation
        (comment "#989898")          ; Using fg-dim for comments
        (docstring "#c6daff")        ; Using fg-alt for docstrings

        ;; UI elements
        (border "#535353")           ; From Helix's bg-active
        (cursor "#ffffff")           ; Pure white cursor

        ;; Search and selection
        (bg-region "#535353")        ; Active selection background
        (fg-region "#ffffff")        ; Selection text color

        ;; Diffs and version control
        (bg-removed "#4f1119")       ; Red background for removed
        (bg-added "#00381f")         ; Green background for added
        (bg-changed "#363300")))       ; Yellow background for changed

;; Optional: Configure other Modus themes settings
(setq modus-themes-bold-constructs t)         ; Make syntactic elements bold
(setq modus-themes-italic-constructs t)       ; Use italics for docstrings etc.
(setq modus-themes-variable-pitch-ui nil)     ; Use fixed-pitch for UI elements
(setq doom-theme 'modus-vivendi-tritanopia)
(setq global-hl-line-mode nil)
(setq confirm-kill-emacs nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)))
(setq org-hide-emphasis-markers t)
(setq plantuml-jar-path
      "~/bin/plantuml-1.2024.7.jar")
(setq org-plantuml-jar-path "~/bin/plantuml-1.2024.7.jar")

;; Load ECLiPSe mode
(setq eclipse-program-call "~/eclipse/bin/x86_64_macosx/eclipse")
(autoload 'eclipse-mode "~/.config/emacs/lisp/eclipse.el" "ECLiPSe editing mode" t)
(autoload 'eclipse-esp-mode "~/.config/emacs/lisp/eclipse.el" "ECLiPSe-ESP editing mode" t)
(setq auto-mode-alist (cons '("\\.pl" . eclipse-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.esp" . eclipse-esp-mode) auto-mode-alist))
;; Set the ECLiPSe executable path

(add-to-list 'auto-mode-alist '("\\.pl\\'" . eclipse-mode))





(add-to-list 'default-frame-alist '(undecorated-round . t))
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(setq
 doom-font (font-spec :family "JetbrainsMono Nerd Font" :size 12.0)
 doom-variable-pitch-font (font-spec :family "Iosevka Aile")
 doom-serif-font (font-spec :family "Iosevka Aile"))

(after! doom-dashboard
  (setq fancy-splash-image "~/dotfiles/doom/xemacs_color.png"))

(setq projectile-project-search-path '("~/Projects/"))
(setq projectile-require-project-root t)
;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(defun org-hugo-new-subtree-post-capture-template ()
  "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
  (let* ((title (read-from-minibuffer "Thought Title: ")) ;Prompt to enter the post title
         (fname (org-hugo-slug title)))
    (mapconcat #'identity
               `(
                 ,(concat "* TODO " title)
                 ":PROPERTIES:"
                 ,(concat ":EXPORT_FILE_NAME: " fname)
                 ":END:"
                 "%?\n")          ;Place the cursor here finally
               "\n")))

(after! lsp-ui
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'at-point))

(after! org
  (setq org-log-done 'time)
  (add-to-list 'org-agenda-files "~/org/tasks.org")
  (add-hook 'org-mode-hook #'org-fragtog-mode))


(add-to-list 'org-capture-templates
             '("h"                ;`org-capture' binding + h
               "Blog Thought"
               entry
               (file+olp "blog-content/thoughts.org" "Thoughts")
               (function org-hugo-new-subtree-post-capture-template)))



(after! org-download
  (setq org-download-method 'directory)
  (setq org-download-image-dir (concat (file-name-sans-extension (buffer-file-name)) "-img"))
  (setq org-download-image-org-width 600)
  (setq org-download-link-format "[[file:%s]]\n"
        org-download-abbreviate-filename-function #'file-relative-name)
  (setq org-download-link-format-function #'org-download-link-format-function-default))
