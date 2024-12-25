(require 'org-roam)
(require 'magit-section)
(require 'ts)

(defgroup org-roam-feed nil
  "Feed view for Org-roam nodes with spaced repetition."
  :group 'org-roam)

(defcustom org-roam-feed-default-interval 3
  "Default interval (in days) for reviewing nodes."
  :type 'integer
  :group 'org-roam-feed)

(defcustom org-roam-feed-damping-factor 0.85
  "Damping factor for PageRank-like algorithm (between 0 and 1)."
  :type 'float
  :group 'org-roam-feed)

(defvar org-roam-feed-review-history (make-hash-table :test 'equal)
  "Hash table storing review history for nodes.")

(defvar org-roam-feed-node-scores (make-hash-table :test 'equal)
  "Hash table storing computed scores for nodes.")

(defun org-roam-feed-get-backlinks-properties (node)
  "Get the alist of properties for each node that links to NODE."
  (mapcar #'car (org-roam-db-query
                 [:select [nodes:properties]
			  :from links
			  :join nodes
			  :on (= links:source nodes:id)
			  :where (= dest $s1)
			  :and (= type "id")
			  ]
                 (org-roam-node-id node))))

(defun org-roam-feed-get-outlinks-properties (node)
  "Get all nodes that NODE links to."
  (mapcar #'car (org-roam-db-query
                 [:select [nodes:properties]
			  :from links
			  :join nodes
			  :on (= links:dest nodes:id)
			  :where (= source $s1)
			  :and (= type "id")]
                 (org-roam-node-id node))))

(defun org-roam-feed-calculate-node-score (node)
  "Calculate a score for NODE-ID on its links and review history."
  (let* ((frecency (lambda (property-alist)
               (let* ((history (split-string-and-unquote (or (alist-get "MTIME" property-alist) "") ","))
		      (last-review (car history))
                      (review-count (length history)))
                 ;; Score based on recency and frequency of backlink reviews
                 (if last-review
                     (* (/ 1.0 (+ 1.0 (/ (float-time (time-since last-review)) 86400)))
                        (log (+ 1.0 review-count)))
                   0.0))))
	 (base-score (funcall frecency (org-roam-node-properties node)))
         (backlinks-properties (org-roam-feed-get-backlinks-properties node))
	 (backlink-scores
          (mapcar (lambda (n) (string-to-number (or (alist-get "FEED_SCORE" n nil nil 'string-equal) "0"))) backlinks-properties))
         (avg-backlink-score (if backlink-scores
				  (/ (apply #'+ backlink-scores) 
				     (float (length backlink-scores)))
				0.0)))
    ;; Apply damping factor and return value
    (+ (* (- 1 org-roam-feed-damping-factor) base-score)
       (* org-roam-feed-damping-factor avg-backlink-score))))


(defun org-roam-feed-set-property (node property value)
  "Visit an org-roam node and set a property to a value."
  (save-excursion
    (org-roam-node-visit node)
					;org-roam wants properties to be sets. We use org-mode's org-set-property to avoid this behaviour.
					; TODO attempt to restore honor to this floating point - see what it takes to make the conversion lossless.
    (org-set-property property (number-to-string value))
    (save-buffer)
    (org-mark-ring-goto)))

(defun org-roam-feed-insert-property (node property value)
  """Add a value to the set of values in an org-roam node's property.

visit an org-roam node and look for a property. Org-roam thinks of property values as sets by default. If the property exists, check for the argument value in the property's value set. If it is already present, do nothing. If it isn't, add it to the beginning. If the property iteslf deosn't exist, create it and set its value to the argument value.
"""
(save-excursion
  (org-roam-node-visit node)
  (let ((mtimes (split-string-and-unquote (or (alist-get "MTIME" (org-roam-node-properties node) nil nil 'string-equal) "") ",")))
    (org-roam-)
    (org-set-property property (combine-and-quote-strings (cons value mtimes) ",")))
  (org-mark-ring-goto)))

(defun org-roam-feed-update-scores ()
  "Update scores for all nodes in the network."
  (let ((nodes (org-roam-node-list)))
    (dolist (node nodes)
    (org-roam-feed-set-property node "FEED_SCORE" (org-roam-feed-calculate-node-score node)))))

(defun org-roam-feed-get-property-from-node (node property)
  (alist-get property (org-roam-node-properties node) nil nil 'string-equal))

(defun org-roam-feed-calculate-next-review (node)
  "Calculate when NODE should next be reviewed using PageRank and SRS."
  (let* ((history (split-string-and-unquote (or (org-roam-feed-get-property-from-node node "MTIME") "") ","))
         (review-count (length history))
         (base-interval (* org-roam-feed-default-interval
                          (if (= review-count 0) 1
                            (expt 2 (1- review-count)))))
         (score (string-to-number (or (org-roam-feed-get-property-from-node node "FEED_SCORE") "0.5")))
         ;; Adjust interval based on score - higher scores mean shorter intervals
         (adjusted-interval (round (* base-interval (/ 1.0 (+ 0.5 score))))))
    (ts-adjust 'day adjusted-interval (ts-now))))


(defun org-roam-feed-record-review (node)
  "Record a review for NODE and propagate effects through the network."
  ;; add the review time to the MTIME property in the node.
  (message "%s %s" node (type-of node))
  (org-roam-feed-insert-property node "MTIME" (format-time-string (org-time-stamp-format t t) (current-time)))
  ;; Update scores to reflect the new review and update the FEED_SCORE properties for all nodes.
  ;; TODO move this to feed refresh.
  (org-roam-feed-update-scores))

(require 'org-roam)
(require 'magit-section)
(require 'ts)

;; ... (previous definitions remain the same until org-roam-feed-get-nodes) ...

(defcustom org-roam-feed-page-size 10
  "Number of nodes to display per page."
  :type 'integer
  :group 'org-roam-feed)

(defvar-local org-roam-feed-current-page 0
  "Current page number (0-based) in the feed buffer.")

(defun org-roam-feed-get-nodes ()
  "Get all nodes sorted by review priority with PageRank influence."
  (org-roam-feed-update-scores)
  (let* ((nodes (org-roam-node-list))
         (nodes-with-schedule
          (mapcar (lambda (node)
                    (let* ((next-review (org-roam-feed-calculate-next-review node))
                           (score (or (org-roam-feed-get-property-from-node node "FEED_SCORE") 0.5)))
                      (cons node (cons next-review score))))
                  nodes)))
    ;; Sort by a combination of review time and score
    (mapcar #'car
            (sort nodes-with-schedule
                  (lambda (a b)
                    (let ((time-a (cadr a))
                          (score-a (cddr a))
                          (time-b (cadr b))
                          (score-b (cddr b)))
                      ;; Primary sort by time, but boost higher-scored nodes
                      (ts< (ts-adjust 'day (round (/ -7.0 score-a)) time-a)
                           (ts-adjust 'day (round (/ -7.0 score-b)) time-b))))))))


(defun org-roam-feed-get-page-nodes (page)
  "Get nodes for the specified PAGE number."
  ;; (org-roam-feed-update-scores)
  (let* ((nodes (org-roam-node-list))
         (nodes-with-schedule
          (mapcar (lambda (node)
                    (let* ((next-review (org-roam-feed-calculate-next-review node))
                           (score (string-to-number (or (org-roam-feed-get-property-from-node node "FEED_SCORE") "0.5"))))
                      (cons node (cons next-review score))))
                  nodes))
         (sorted-nodes
          (mapcar #'car
                  (-sort (lambda (a b) (> (cddr a) (cddr b))) nodes-with-schedule)))
         (start (* page org-roam-feed-page-size))
         (end (min (+ start org-roam-feed-page-size) (length sorted-nodes))))
    (seq-subseq sorted-nodes start end)))

(defun org-roam-feed-next-page ()
  "Move to the next page of nodes."
  (interactive)
  (let* ((total-nodes (length (org-roam-node-list)))
         (total-pages (ceiling (/ total-nodes (float org-roam-feed-page-size))))
         (next-page (1+ org-roam-feed-current-page)))
    (when (< next-page total-pages)
      (setq org-roam-feed-current-page next-page)
      (org-roam-feed-refresh))))

(defun org-roam-feed-prev-page ()
  "Move to the previous page of nodes."
  (interactive)
  (when (> org-roam-feed-current-page 0)
    (setq org-roam-feed-current-page (1- org-roam-feed-current-page))
    (org-roam-feed-refresh)))

(defun org-roam-feed-refresh ()
  "Refresh the feed buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (magit-insert-section (feed)
      (let* ((total-nodes (length (org-roam-node-list)))
             (total-pages (ceiling (/ total-nodes (float org-roam-feed-page-size)))))
        ;; Insert pagination info header
        (insert (format "Page %d/%d (Total nodes: %d)\n\n"
                       (1+ org-roam-feed-current-page)
                       total-pages
                       total-nodes))
        ;; Insert navigation help
        (insert "Navigation: n - next page, p - previous page, g - refresh\n\n"))
      (dolist (node (org-roam-feed-get-page-nodes org-roam-feed-current-page))
        (org-roam-feed-insert-node node)))))

(defun org-roam-feed-insert-node (node)
  "Insert NODE into the feed buffer with magit-section formatting."
  (let* ((score (or (org-roam-feed-get-property-from-node node "FEED_SCORE") 0.5))
         (history (org-roam-feed-get-property-from-node node "MTIME")))
    (magit-insert-section section (org-roam-node-section)
      (magit-insert-heading
        (format "%s (Score: %.2f, Reviews: %d)"
                (org-roam-node-title node)
                score
                (length history)))
      (oset section node node)
      (when history
        (insert (format "Last reviewed: %s\n"
                       (ts-format (car history)))))
      (let ((backlinks (org-roam-feed-get-backlinks-properties node))
            (outlinks (org-roam-feed-get-outlinks-properties node)))
        (insert (format "Backlinks: %d, Outlinks: %d\n"
			(length backlinks)
			(length outlinks))))
      (let ((point (org-roam-node-point node)))
	(magit-insert-section section (org-roam-preview-section)
	  (insert (org-roam-fontify-like-in-org-mode
		   (org-roam-preview-get-contents (org-roam-node-file node) point))
		  "\n")
	  (oset section file (org-roam-node-file node))
	  (oset section point point)
	  (insert ?\n)))
      (insert "\n"))
    ))

(defun org-roam-feed-record-review-interactive ()
  (interactive)
  (when-let ((node (org-roam-node-at-point)))
    (org-roam-feed-record-review node)
    (org-roam-feed-refresh)))

(defvar org-roam-feed-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'org-roam-feed-refresh)
    (define-key map (kbd "RET") 'org-roam-node-visit)
    (define-key map (kbd "r") 'org-roam-feed-record-review-interactive)
    (define-key map (kbd "n") 'org-roam-feed-next-page)
    (define-key map (kbd "p") 'org-roam-feed-prev-page)
    map)
  "Keymap for `org-roam-feed-mode'.")

(define-derived-mode org-roam-feed-mode magit-section-mode "Org-Roam Feed"
  "Major mode for viewing Org-roam nodes in a feed."
  :group 'org-roam-feed)

(defun org-roam-feed ()
  "Display Org-roam nodes in a feed buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*Org-Roam Feed*")))
    (with-current-buffer buffer
      (org-roam-feed-mode)
      (org-roam-feed-refresh))
    (switch-to-buffer buffer)))

(provide 'org-roam-feed)
