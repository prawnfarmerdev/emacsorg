;;; menudo-theme.el --- Menudo theme (black backgrounds, gray text, blue highlights) -*- lexical-binding: t -*-

;; Author: PrawnFarmerDev
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces themes
;; URL: https://github.com/prawnfarmerdev/menudo-theme

;;; Commentary:
;; Menudo is a dark theme featuring:
;; - Pure black backgrounds for code and line numbers
;; - Bright gray text for readability
;; - Gold cursor
;; - Gold mode-line with bright text
;; - Blue and gold highlights for completion
;; - Red parenthesis matching
;; - Comprehensive org-mode support
;; - Rainbow delimiters for code
;;
;; This file also provides `weyland-yutani', an amber-on-near-black theme
;; inspired by the Weyland-Yutani Corporation from the Alien films.

;;; Code:

(deftheme menudo "Menudo theme with black backgrounds, gray text, and blue highlights.")

(defvar menudo-cursor-color         "#b58900")
(defvar menudo-replace-cursor-color "#dc322f")

(let* (;; Neutrals
       (menudo-fg              "#cccccc")
       (menudo-bg              "#111111")
       (menudo-bg-alt          "#1e1e1e")
       (menudo-hl-line         "#2a2a2a")
       (menudo-gray-dark       "#404040")
       (menudo-gray            "#666666")
       (menudo-gray-light      "#9ba290")

       ;; Yellows / Golds
       (menudo-gold            "#fcaa05")
       (menudo-gold-dark       "#9f5600")
       (menudo-gold-dim        "#b58900")
       (menudo-yellow          "#f0c674")
       (menudo-yellow-bright   "#edb211")
       (menudo-yellow-vivid    "#f0bb0c")

       ;; Oranges
       (menudo-orange          "#ffaa00")
       (menudo-orange-burnt    "#de451f")
       (menudo-orange-vivid    "#f0500c")

       ;; Reds
       (menudo-red             "#b3493f")
       (menudo-red-dusty       "#dc7575")

       ;; Blues
       (menudo-blue            "#268bd2")

       ;; Greens
       (menudo-green           "#8ea032")
       (menudo-green-dark      "#003939")
       (menudo-green-pepper    "#355e3b")
       (menudo-green-lime      "#003939")

       ;; Purples / Cyans
       (menudo-cyan            "#5fafaf")
       (menudo-lavender        "#af87d7")
       (menudo-teal            "#008080")

       ;; Browns
       (menudo-brown           "#bf9948")


       ;; Mode line
       (menudo-modeline-fg     "#cccccc")
       (menudo-modeline-bg     "#9f5600")
       (menudo-modeline-border "#161616"))

  (custom-theme-set-faces
   'menudo

   ;;=========================================================================
   ;; UI
   ;;=========================================================================
   `(default           ((t (:background ,menudo-bg              :foreground ,menudo-fg))))
   `(cursor            ((t (:background ,menudo-gold-dim))))
   `(region            ((t (:background ,menudo-green-dark))))
   `(highlight         ((t (:background ,menudo-green-dark))))
   `(fringe            ((t (:background ,menudo-bg))))
   `(vertical-border   ((t (:foreground ,menudo-bg))))
   `(shadow            ((t (:foreground ,menudo-gray-dark       :background ,menudo-bg))))
   `(minibuffer-prompt ((t (:foreground ,menudo-gold            :weight bold))))
   `(hl-line           ((t (:background ,menudo-hl-line))))

   ;;=========================================================================
   ;; LINE NUMBERS
   ;;=========================================================================
   `(line-number              ((t (:foreground ,menudo-gray-dark :background ,menudo-bg))))
   `(line-number-current-line ((t (:foreground ,menudo-gold-dark :background ,menudo-bg))))

   ;;=========================================================================
   ;; FONT LOCK
   ;;=========================================================================
   `(font-lock-comment-face       ((t (:foreground ,menudo-gray))))
   `(font-lock-keyword-face       ((t (:foreground ,menudo-yellow))))
   `(font-lock-string-face        ((t (:foreground ,menudo-orange))))
   `(font-lock-constant-face      ((t (:foreground ,menudo-orange))))
   `(font-lock-builtin-face       ((t (:foreground ,menudo-red-dusty))))
   `(font-lock-preprocessor-face  ((t (:foreground ,menudo-red-dusty))))
   `(font-lock-type-face          ((t (:foreground ,menudo-yellow-bright))))
   `(font-lock-function-name-face ((t (:foreground ,menudo-orange-burnt))))
   `(font-lock-variable-name-face ((t (:foreground ,menudo-fg))))
   `(font-lock-variable-use-face  ((t (:foreground ,menudo-lavender))))
   `(font-lock-warning-face       ((t (:foreground ,menudo-red      :weight bold))))
   `(font-lock-doc-face           ((t (:foreground ,menudo-green))))

   ;;=========================================================================
   ;; MODE LINE
   ;;=========================================================================
   `(mode-line
     ((t (:background ,menudo-modeline-bg
          :foreground ,menudo-modeline-fg
          :box (:line-width 1 :color ,menudo-modeline-border :style nil)))))
   `(mode-line-inactive
     ((t (:background ,menudo-gray
          :foreground ,menudo-fg
          :box (:line-width 1 :color ,menudo-modeline-border :style nil)))))
   `(mode-line-buffer-id ((t (:foreground ,menudo-orange :weight bold))))

   ;;=========================================================================
   ;; GIT / VC
   ;;=========================================================================
   `(magit-branch-local   ((t (:foreground ,menudo-green))))
   `(magit-branch-remote  ((t (:foreground ,menudo-blue))))
   `(magit-branch-current ((t (:foreground ,menudo-gold   :weight bold))))
   `(vc-mode              ((t (:foreground ,menudo-gold))))
   `(diff-hl-change       ((t (:background ,menudo-blue   :foreground ,menudo-blue))))
   `(diff-hl-insert       ((t (:background ,menudo-green  :foreground ,menudo-green))))
   `(diff-hl-delete       ((t (:background ,menudo-red    :foreground ,menudo-red))))

   ;;=========================================================================
   ;; SEARCH & MATCHING
   ;;=========================================================================
   `(match          ((t (:background ,menudo-yellow-vivid  :foreground ,menudo-bg))))
   `(isearch        ((t (:background ,menudo-orange-vivid  :foreground ,menudo-bg))))
   `(lazy-highlight ((t (:background ,menudo-yellow-vivid  :foreground ,menudo-bg))))
   `(ido-first-match ((t (:foreground ,menudo-yellow-vivid))))
   `(ido-only-match  ((t (:foreground ,menudo-orange-vivid))))

   ;;=========================================================================
   ;; COMPLETION (VERTICO / CORFU / ORDERLESS)
   ;;=========================================================================
   `(vertico-current              ((t (:background ,menudo-green-pepper :foreground ,menudo-fg))))
   `(orderless-match-face-0       ((t (:foreground ,menudo-yellow-vivid :weight bold))))
   `(orderless-match-face-1       ((t (:foreground ,menudo-blue         :weight bold))))
   `(orderless-match-face-2       ((t (:foreground ,menudo-green        :weight bold))))
   `(orderless-match-face-3       ((t (:foreground ,menudo-red-dusty    :weight bold))))
   `(completions-first-difference ((t (:foreground ,menudo-gold         :weight bold))))
   `(completions-common-part      ((t (:foreground ,menudo-fg))))

   `(corfu-current ((t (:background ,menudo-green-lime :foreground ,menudo-bg))))
   `(corfu-default ((t (:background ,menudo-bg-alt))))
   `(corfu-bar     ((t (:background ,menudo-blue))))
   `(corfu-border  ((t (:background ,menudo-gray-dark))))

   ;;=========================================================================
   ;; PARENTHESES
   ;;=========================================================================
   `(show-paren-match    ((t (:background ,menudo-gray-dark))))
   `(show-paren-mismatch ((t (:background ,menudo-lavender))))

   ;;=========================================================================
   ;; RAINBOW DELIMITERS
   ;;=========================================================================
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,menudo-yellow))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,menudo-green))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,menudo-red))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,menudo-red-dusty))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,menudo-gold))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,menudo-yellow-bright))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,menudo-orange))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,menudo-orange-vivid))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,menudo-cyan))))

   ;;=========================================================================
   ;; LSP & DIAGNOSTICS
   ;;=========================================================================
   `(eglot-highlight-symbol-face   ((t (:background ,menudo-bg-alt))))
   `(eglot-diagnostic-error-face   ((t (:underline (:color ,menudo-red    :style wave)))))
   `(eglot-diagnostic-warning-face ((t (:underline (:color ,menudo-gold   :style wave)))))
   `(eglot-diagnostic-note-face    ((t (:underline (:color ,menudo-blue   :style wave)))))
   `(eglot-diagnostic-hint-face    ((t (:underline (:color ,menudo-green  :style wave)))))

   `(flycheck-error   ((t (:underline (:color ,menudo-red  :style wave)))))
   `(flycheck-warning ((t (:underline (:color ,menudo-gold :style wave)))))
   `(flycheck-info    ((t (:underline (:color ,menudo-blue :style wave)))))

   ;;=========================================================================
   ;; COMPILATION
   ;;=========================================================================
   `(compilation-error          ((t (:foreground ,menudo-red))))
   `(compilation-info           ((t (:foreground ,menudo-green))))
   `(compilation-warning        ((t (:foreground ,menudo-brown  :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,menudo-red    :weight bold))))
   `(compilation-mode-line-exit ((t (:foreground ,menudo-green  :weight bold))))

   ;;=========================================================================
   ;; ORG MODE
   ;;=========================================================================
   `(org-level-1 ((t (:foreground ,menudo-gold         :weight bold :height 1.3))))
   `(org-level-2 ((t (:foreground ,menudo-brown        :weight bold :height 1.2))))
   `(org-level-3 ((t (:foreground ,menudo-red          :weight bold :height 1.1))))
   `(org-level-4 ((t (:foreground ,menudo-blue         :weight bold))))
   `(org-level-5 ((t (:foreground ,menudo-red-dusty    :weight bold))))
   `(org-level-6 ((t (:foreground ,menudo-lavender     :weight bold))))
   `(org-level-7 ((t (:foreground ,menudo-cyan         :weight bold))))
   `(org-level-8 ((t (:foreground ,menudo-orange       :weight bold))))

   `(org-document-title        ((t (:foreground ,menudo-red  :weight bold :height 1.5))))
   `(org-document-info         ((t (:foreground ,menudo-cyan))))
   `(org-document-info-keyword ((t (:foreground ,menudo-gray))))

   `(org-list-dt                  ((t (:foreground ,menudo-gold  :weight bold))))
   `(org-checkbox                 ((t (:foreground ,menudo-gold  :weight bold))))
   `(org-checkbox-statistics-todo ((t (:foreground ,menudo-red-dusty))))
   `(org-checkbox-statistics-done ((t (:foreground ,menudo-green))))

   `(org-todo          ((t (:foreground ,menudo-red       :weight bold))))
   `(org-done          ((t (:foreground ,menudo-green     :weight bold))))
   `(org-headline-todo ((t (:foreground ,menudo-red-dusty))))
   `(org-headline-done ((t (:foreground ,menudo-gray      :strike-through t))))

   `(org-special-keyword ((t (:foreground ,menudo-gray))))
   `(org-property-value  ((t (:foreground ,menudo-cyan))))
   `(org-drawer          ((t (:foreground ,menudo-gray))))
   `(org-meta-line       ((t (:foreground ,menudo-gray))))

   `(org-link ((t (:foreground ,menudo-blue :underline t))))
   `(org-tag  ((t (:foreground ,menudo-gold :weight bold))))

   `(org-block            ((t (:background ,menudo-bg-alt :foreground ,menudo-fg :extend t))))
   `(org-block-begin-line ((t (:foreground ,menudo-gray   :background ,menudo-bg :extend t))))
   `(org-block-end-line   ((t (:foreground ,menudo-gray   :background ,menudo-bg :extend t))))
   `(org-code             ((t (:foreground ,menudo-orange  :background ,menudo-bg-alt))))
   `(org-verbatim         ((t (:foreground ,menudo-green   :background ,menudo-bg-alt))))

   `(org-table ((t (:foreground ,menudo-cyan))))

   `(org-date                 ((t (:foreground ,menudo-lavender :underline t))))
   `(org-time-grid            ((t (:foreground ,menudo-yellow-vivid))))
   `(org-upcoming-deadline    ((t (:foreground ,menudo-red))))
   `(org-scheduled            ((t (:foreground ,menudo-green))))
   `(org-scheduled-today      ((t (:foreground ,menudo-gold    :weight bold))))
   `(org-scheduled-previously ((t (:foreground ,menudo-red-dusty))))

   `(org-priority ((t (:foreground ,menudo-orange-vivid :weight bold))))

   `(org-agenda-structure  ((t (:foreground ,menudo-blue         :weight bold))))
   `(org-agenda-date       ((t (:foreground ,menudo-cyan))))
   `(org-agenda-date-today ((t (:foreground ,menudo-gold         :weight bold :height 1.2))))
   `(org-agenda-done       ((t (:foreground ,menudo-gray))))

   `(org-footnote ((t (:foreground ,menudo-lavender :underline t))))

   `(org-bold   ((t (:foreground ,menudo-fg :weight bold))))
   `(org-italic ((t (:foreground ,menudo-fg :slant italic))))

   ;;=========================================================================
   ;; ORG-MODERN
   ;;=========================================================================
   `(org-modern-tag        ((t (:background ,menudo-green-pepper :foreground ,menudo-gold))))
   `(org-modern-priority   ((t (:background ,menudo-bg-alt       :foreground ,menudo-orange-vivid))))
   `(org-modern-todo       ((t (:background ,menudo-bg-alt       :foreground ,menudo-red    :weight bold))))
   `(org-modern-done       ((t (:background ,menudo-bg-alt       :foreground ,menudo-brown  :weight bold))))
   `(org-modern-date       ((t (:background ,menudo-bg-alt       :foreground ,menudo-lavender))))
   `(org-modern-time       ((t (:background ,menudo-bg-alt       :foreground ,menudo-yellow-vivid))))
   `(org-modern-statistics ((t (:background ,menudo-bg-alt       :foreground ,menudo-cyan))))

   ;;=========================================================================
   ;; TOOLTIP
   ;;=========================================================================
   `(tooltip ((t (:background ,menudo-brown :foreground ,menudo-gold))))))

;;=========================================================================
;; WEYLAND-YUTANI THEME
;;=========================================================================
(deftheme weyland-yutani
  "Weyland-Yutani theme - amber on near-black. Building better worlds.")

(let* (;; Neutrals
       (wy-fg              "#c9c4b4")
       (wy-bg              "#0b0b0d")
       (wy-bg-alt          "#161619")
       (wy-hl-line         "#1f1f26")
       (wy-gray-dark       "#3a3a40")
       (wy-gray            "#6b6b73")
       (wy-gray-light      "#9a958a")

       ;; Ambers / Golds (corporate amber)
       (wy-amber           "#ffb000")
       (wy-amber-bright    "#ffc850")
       (wy-amber-dim       "#b37700")
       (wy-gold            "#e6a817")
       (wy-gold-dark       "#9f7200")

       ;; Oranges
       (wy-orange          "#e08a2e")
       (wy-orange-burnt    "#d1601a")
       (wy-orange-vivid    "#ff7a1a")

       ;; Reds
       (wy-red             "#d64541")
       (wy-red-dusty       "#e07a7a")

       ;; Blues
       (wy-blue            "#5a8fbf")

       ;; Greens (Nostromo terminal)
       (wy-green           "#4ec07a")
       (wy-green-dark      "#12381f")
       (wy-green-pepper    "#1f4d33")

       ;; Cyans / Purples / Browns
       (wy-cyan            "#3fb6b6")
       (wy-lavender        "#a98bd6")
       (wy-teal            "#007a7a")
       (wy-brown           "#b08a52")

       ;; Mode line
       (wy-modeline-fg     "#0b0b0d")
       (wy-modeline-bg     "#ffb000")
       (wy-modeline-border "#161619"))

  (custom-theme-set-faces
   'weyland-yutani

   ;;=========================================================================
   ;; UI
   ;;=========================================================================
   `(default           ((t (:background ,wy-bg              :foreground ,wy-fg))))
   `(cursor            ((t (:background ,wy-amber))))
   `(region            ((t (:background ,wy-green-dark))))
   `(highlight         ((t (:background ,wy-green-dark))))
   `(fringe            ((t (:background ,wy-bg))))
   `(vertical-border   ((t (:foreground ,wy-bg))))
   `(shadow            ((t (:foreground ,wy-gray-dark       :background ,wy-bg))))
   `(minibuffer-prompt ((t (:foreground ,wy-amber            :weight bold))))
   `(hl-line           ((t (:background ,wy-hl-line))))

   ;;=========================================================================
   ;; LINE NUMBERS
   ;;=========================================================================
   `(line-number              ((t (:foreground ,wy-gray-dark :background ,wy-bg))))
   `(line-number-current-line ((t (:foreground ,wy-amber-dim :background ,wy-bg))))

   ;;=========================================================================
   ;; FONT LOCK
   ;;=========================================================================
   `(font-lock-comment-face       ((t (:foreground ,wy-gray))))
   `(font-lock-keyword-face       ((t (:foreground ,wy-amber))))
   `(font-lock-string-face        ((t (:foreground ,wy-orange))))
   `(font-lock-constant-face      ((t (:foreground ,wy-amber-bright))))
   `(font-lock-builtin-face       ((t (:foreground ,wy-red-dusty))))
   `(font-lock-preprocessor-face  ((t (:foreground ,wy-red-dusty))))
   `(font-lock-type-face          ((t (:foreground ,wy-gold))))
   `(font-lock-function-name-face ((t (:foreground ,wy-orange-burnt))))
   `(font-lock-variable-name-face ((t (:foreground ,wy-fg))))
   `(font-lock-variable-use-face  ((t (:foreground ,wy-lavender))))
   `(font-lock-warning-face       ((t (:foreground ,wy-red      :weight bold))))
   `(font-lock-doc-face           ((t (:foreground ,wy-green))))

   ;;=========================================================================
   ;; MODE LINE
   ;;=========================================================================
   `(mode-line
     ((t (:background ,wy-modeline-bg
          :foreground ,wy-modeline-fg
          :box (:line-width 1 :color ,wy-modeline-border :style nil)))))
   `(mode-line-inactive
     ((t (:background ,wy-gray
          :foreground ,wy-fg
          :box (:line-width 1 :color ,wy-modeline-border :style nil)))))
   `(mode-line-buffer-id ((t (:foreground ,wy-bg :weight bold))))

   ;;=========================================================================
   ;; GIT / VC
   ;;=========================================================================
   `(magit-branch-local   ((t (:foreground ,wy-green))))
   `(magit-branch-remote  ((t (:foreground ,wy-blue))))
   `(magit-branch-current ((t (:foreground ,wy-amber   :weight bold))))
   `(vc-mode              ((t (:foreground ,wy-amber))))
   `(diff-hl-change       ((t (:background ,wy-blue   :foreground ,wy-blue))))
   `(diff-hl-insert       ((t (:background ,wy-green  :foreground ,wy-green))))
   `(diff-hl-delete       ((t (:background ,wy-red    :foreground ,wy-red))))

   ;;=========================================================================
   ;; SEARCH & MATCHING
   ;;=========================================================================
   `(match          ((t (:background ,wy-amber-bright  :foreground ,wy-bg))))
   `(isearch        ((t (:background ,wy-orange-vivid  :foreground ,wy-bg))))
   `(lazy-highlight ((t (:background ,wy-amber         :foreground ,wy-bg))))
   `(ido-first-match ((t (:foreground ,wy-amber-bright))))
   `(ido-only-match  ((t (:foreground ,wy-orange-vivid))))

   ;;=========================================================================
   ;; COMPLETION (VERTICO / CORFU / ORDERLESS)
   ;;=========================================================================
   `(vertico-current              ((t (:background ,wy-green-pepper :foreground ,wy-fg))))
   `(orderless-match-face-0       ((t (:foreground ,wy-amber-bright :weight bold))))
   `(orderless-match-face-1       ((t (:foreground ,wy-blue         :weight bold))))
   `(orderless-match-face-2       ((t (:foreground ,wy-green        :weight bold))))
   `(orderless-match-face-3       ((t (:foreground ,wy-red-dusty    :weight bold))))
   `(completions-first-difference ((t (:foreground ,wy-amber         :weight bold))))
   `(completions-common-part      ((t (:foreground ,wy-fg))))

   `(corfu-current ((t (:background ,wy-green-dark :foreground ,wy-amber))))
   `(corfu-default ((t (:background ,wy-bg-alt))))
   `(corfu-bar     ((t (:background ,wy-blue))))
   `(corfu-border  ((t (:background ,wy-gray-dark))))

   ;;=========================================================================
   ;; PARENTHESES
   ;;=========================================================================
   `(show-paren-match    ((t (:background ,wy-gray-dark))))
   `(show-paren-mismatch ((t (:background ,wy-lavender))))

   ;;=========================================================================
   ;; RAINBOW DELIMITERS
   ;;=========================================================================
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,wy-amber))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,wy-green))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,wy-red))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,wy-red-dusty))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,wy-gold))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,wy-amber-bright))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,wy-orange))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,wy-orange-vivid))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,wy-cyan))))

   ;;=========================================================================
   ;; LSP & DIAGNOSTICS
   ;;=========================================================================
   `(eglot-highlight-symbol-face   ((t (:background ,wy-bg-alt))))
   `(eglot-diagnostic-error-face   ((t (:underline (:color ,wy-red    :style wave)))))
   `(eglot-diagnostic-warning-face ((t (:underline (:color ,wy-amber  :style wave)))))
   `(eglot-diagnostic-note-face    ((t (:underline (:color ,wy-blue   :style wave)))))
   `(eglot-diagnostic-hint-face    ((t (:underline (:color ,wy-green  :style wave)))))

   `(flycheck-error   ((t (:underline (:color ,wy-red   :style wave)))))
   `(flycheck-warning ((t (:underline (:color ,wy-amber :style wave)))))
   `(flycheck-info    ((t (:underline (:color ,wy-blue  :style wave)))))

   ;;=========================================================================
   ;; COMPILATION
   ;;=========================================================================
   `(compilation-error          ((t (:foreground ,wy-red))))
   `(compilation-info           ((t (:foreground ,wy-green))))
   `(compilation-warning        ((t (:foreground ,wy-brown  :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,wy-red    :weight bold))))
   `(compilation-mode-line-exit ((t (:foreground ,wy-green  :weight bold))))

   ;;=========================================================================
   ;; ORG MODE
   ;;=========================================================================
   `(org-level-1 ((t (:foreground ,wy-amber         :weight bold :height 1.3))))
   `(org-level-2 ((t (:foreground ,wy-brown         :weight bold :height 1.2))))
   `(org-level-3 ((t (:foreground ,wy-red           :weight bold :height 1.1))))
   `(org-level-4 ((t (:foreground ,wy-blue          :weight bold))))
   `(org-level-5 ((t (:foreground ,wy-red-dusty     :weight bold))))
   `(org-level-6 ((t (:foreground ,wy-lavender      :weight bold))))
   `(org-level-7 ((t (:foreground ,wy-cyan          :weight bold))))
   `(org-level-8 ((t (:foreground ,wy-orange        :weight bold))))

   `(org-document-title        ((t (:foreground ,wy-amber  :weight bold :height 1.5))))
   `(org-document-info         ((t (:foreground ,wy-cyan))))
   `(org-document-info-keyword ((t (:foreground ,wy-gray))))

   `(org-list-dt                  ((t (:foreground ,wy-amber  :weight bold))))
   `(org-checkbox                 ((t (:foreground ,wy-amber  :weight bold))))
   `(org-checkbox-statistics-todo ((t (:foreground ,wy-red-dusty))))
   `(org-checkbox-statistics-done ((t (:foreground ,wy-green))))

   `(org-todo          ((t (:foreground ,wy-red       :weight bold))))
   `(org-done          ((t (:foreground ,wy-green     :weight bold))))
   `(org-headline-todo ((t (:foreground ,wy-red-dusty))))
   `(org-headline-done ((t (:foreground ,wy-gray      :strike-through t))))

   `(org-special-keyword ((t (:foreground ,wy-gray))))
   `(org-property-value  ((t (:foreground ,wy-cyan))))
   `(org-drawer          ((t (:foreground ,wy-gray))))
   `(org-meta-line       ((t (:foreground ,wy-gray))))

   `(org-link ((t (:foreground ,wy-blue :underline t))))
   `(org-tag  ((t (:foreground ,wy-amber :weight bold))))

   `(org-block            ((t (:background ,wy-bg-alt :foreground ,wy-fg :extend t))))
   `(org-block-begin-line ((t (:foreground ,wy-gray   :background ,wy-bg :extend t))))
   `(org-block-end-line   ((t (:foreground ,wy-gray   :background ,wy-bg :extend t))))
   `(org-code             ((t (:foreground ,wy-orange  :background ,wy-bg-alt))))
   `(org-verbatim         ((t (:foreground ,wy-green   :background ,wy-bg-alt))))

   `(org-table ((t (:foreground ,wy-cyan))))

   `(org-date                 ((t (:foreground ,wy-lavender :underline t))))
   `(org-time-grid            ((t (:foreground ,wy-amber))))
   `(org-upcoming-deadline    ((t (:foreground ,wy-red))))
   `(org-scheduled            ((t (:foreground ,wy-green))))
   `(org-scheduled-today      ((t (:foreground ,wy-amber    :weight bold))))
   `(org-scheduled-previously ((t (:foreground ,wy-red-dusty))))

   `(org-priority ((t (:foreground ,wy-orange-vivid :weight bold))))

   `(org-agenda-structure  ((t (:foreground ,wy-blue   :weight bold))))
   `(org-agenda-date       ((t (:foreground ,wy-cyan))))
   `(org-agenda-date-today ((t (:foreground ,wy-amber  :weight bold :height 1.2))))
   `(org-agenda-done       ((t (:foreground ,wy-gray))))

   `(org-footnote ((t (:foreground ,wy-lavender :underline t))))

   `(org-bold   ((t (:foreground ,wy-fg :weight bold))))
   `(org-italic ((t (:foreground ,wy-fg :slant italic))))

   ;;=========================================================================
   ;; ORG-MODERN
   ;;=========================================================================
   `(org-modern-tag        ((t (:background ,wy-green-pepper :foreground ,wy-amber))))
   `(org-modern-priority   ((t (:background ,wy-bg-alt       :foreground ,wy-orange-vivid))))
   `(org-modern-todo       ((t (:background ,wy-bg-alt       :foreground ,wy-red    :weight bold))))
   `(org-modern-done       ((t (:background ,wy-bg-alt       :foreground ,wy-brown  :weight bold))))
   `(org-modern-date       ((t (:background ,wy-bg-alt       :foreground ,wy-lavender))))
   `(org-modern-time       ((t (:background ,wy-bg-alt       :foreground ,wy-amber))))
   `(org-modern-statistics ((t (:background ,wy-bg-alt       :foreground ,wy-cyan))))

   ;;=========================================================================
   ;; TOOLTIP
   ;;=========================================================================
   `(tooltip ((t (:background ,wy-brown :foreground ,wy-bg))))))

;;=========================================================================
;; THEME REGISTRATION
;;=========================================================================

(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'menudo)
(provide-theme 'weyland-yutani)

(when (or (display-graphic-p) (daemonp))
  (enable-theme 'menudo))

;;=========================================================================
;; THEME SWITCHER
;;=========================================================================

(defvar my/available-themes '(menudo weyland-yutani)
  "Custom themes provided by this file.")

(defun my/select-theme (theme)
  "Switch to THEME, disabling any currently enabled themes first."
  (interactive
   (list (intern (completing-read "Theme: " my/available-themes nil t))))
  (mapc #'disable-theme (copy-sequence custom-enabled-themes))
  (enable-theme theme)
  (message "Enabled theme: %s" theme))

(defun my/select-theme-weyland-yutani ()
  "Switch to the Weyland-Yutani theme."
  (interactive)
  (my/select-theme 'weyland-yutani))

(defun my/select-theme-menudo ()
  "Switch to the Menudo theme."
  (interactive)
  (my/select-theme 'menudo))

;;=========================================================================
;; HL-LINE & CURSOR SETUP
;;=========================================================================

(add-hook 'prog-mode-hook #'hl-line-mode)
(setq-default cursor-in-non-selected-windows nil
              cursor-type 'box)

;;=========================================================================
;; DAEMON MODE SUPPORT
;;=========================================================================

(defun menudo--enable-for-frame (&optional frame)
  "Re-apply the active custom theme(s) on FRAME if it is graphical."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      (dolist (theme (copy-sequence custom-enabled-themes))
        (enable-theme theme)))))

(add-hook 'after-make-frame-functions #'menudo--enable-for-frame)

(provide 'menudo-theme)
;;; menudo-theme.el ends here
