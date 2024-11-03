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

(defun org-roam-feed-get-backlinks (node-id)
  "Get all nodes that link to NODE-ID."
  (mapcar #'car (org-roam-db-query
                 [:select [source]
                  :from links
                  :where (= dest $s1)
                  :and (= type "id")]
                 node-id)))

(defun org-roam-feed-get-outlinks (node-id)
  "Get all nodes that NODE-ID links to."
  (mapcar #'car (org-roam-db-query
                 [:select [dest]
                  :from links
                  :where (= source $s1)
                  :and (= type "id")]
                 node-id)))

(defun org-roam-feed-calculate-node-score (node-id)
  "Calculate a score for NODE-ID based on its links and review history."
  (let* ((backlinks (org-roam-feed-get-backlinks node-id))
         (backlink-scores 
          (mapcar (lambda (link-id)
                    (let* ((history (gethash link-id org-roam-feed-review-history))
                           (last-review (car history))
                           (review-count (length history)))
                      ;; Score based on recency and frequency of backlink reviews
                      (if last-review
                          (* (/ 1.0 (+ 1.0 (ts-difference (ts-now) last-review)))
                             (log (+ 1.0 review-count)))
                        0.0)))
                  backlinks))
         (base-score (if backlink-scores
                        (/ (apply #'+ backlink-scores) 
                           (float (length backlink-scores)))
                      0.0)))
    ;; Apply damping factor and store
    (puthash node-id
             (+ (* org-roam-feed-damping-factor base-score)
                (* (- 1 org-roam-feed-damping-factor)
                   (if (gethash node-id org-roam-feed-review-history) 1.0 0.5)))
             org-roam-feed-node-scores)))

(defun org-roam-feed-update-scores ()
  "Update scores for all nodes in the network."
  (let ((nodes (org-roam-node-list)))
    (dolist (node nodes)
      (org-roam-feed-calculate-node-score (org-roam-node-id node)))))

(defun org-roam-feed-calculate-next-review (node-id)
  "Calculate when NODE-ID should next be reviewed using PageRank and SRS."
  (let* ((history (gethash node-id org-roam-feed-review-history))
         (review-count (length history))
         (base-interval (* org-roam-feed-default-interval
                          (if (= review-count 0) 1
                            (expt 2 (1- review-count)))))
         (score (or (gethash node-id org-roam-feed-node-scores) 0.5))
         ;; Adjust interval based on score - higher scores mean shorter intervals
         (adjusted-interval (round (* base-interval (/ 1.0 (+ 0.5 score))))))
    (ts-adjust 'day adjusted-interval (ts-now))))

(defun org-roam-feed-record-review (node-id)
  "Record a review for NODE-ID and propagate effects through the network."
  (let ((history (or (gethash node-id org-roam-feed-review-history)
                     (list))))
    (puthash node-id
             (cons (ts-now) history)
             org-roam-feed-review-history)
    ;; Update scores to reflect the new review
    (org-roam-feed-update-scores)))

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
                    (let* ((node-id (org-roam-node-id node))
                           (next-review (org-roam-feed-calculate-next-review node-id))
                           (score (or (gethash node-id org-roam-feed-node-scores) 0.5)))
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
  (org-roam-feed-update-scores)
  (let* ((nodes (org-roam-node-list))
         (nodes-with-schedule
          (mapcar (lambda (node)
                    (let* ((node-id (org-roam-node-id node))
                           (next-review (org-roam-feed-calculate-next-review node-id))
                           (score (or (gethash node-id org-roam-feed-node-scores) 0.5)))
                      (cons node (cons next-review score))))
                  nodes))
         (sorted-nodes
          (mapcar #'car
                  (sort nodes-with-schedule
                        (lambda (a b)
                          (let ((time-a (cadr a))
                                (score-a (cddr a))
                                (time-b (cadr b))
                                (score-b (cddr b)))
                            (ts< (ts-adjust 'day (round (/ -7.0 score-a)) time-a)
                                 (ts-adjust 'day (round (/ -7.0 score-b)) time-b)))))))
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
  (let* ((node-id (org-roam-node-id node))
         (score (or (gethash node-id org-roam-feed-node-scores) 0.5))
         (history (gethash node-id org-roam-feed-review-history)))
    (magit-insert-section org-roam-node
      (magit-insert-heading
        (format "%s (Score: %.2f, Reviews: %d)"
                (org-roam-node-title node)
                score
                (length history)))
      (when history
        (insert (format "Last reviewed: %s\n"
                       (ts-format (car history)))))
      (let ((backlinks (org-roam-feed-get-backlinks node-id))
            (outlinks (org-roam-feed-get-outlinks node-id)))
        (insert (format "Backlinks: %d, Outlinks: %d\n"
                       (length backlinks)
                       (length outlinks))))
      (org-roam-node-insert-section :source-node node
				    :point (org-roam-node-point node)
				    :properties (org-roam-node-properties node))
      (insert "\n"))
    ))

(defvar org-roam-feed-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'org-roam-feed-refresh)
    (define-key map (kbd "RET") 'org-roam-node-visit)
    (define-key map (kbd "r")
                (lambda ()
                  (interactive)
                  (when-let ((node (org-roam-node-at-point)))
                    (org-roam-feed-record-review
                     (org-roam-node-id node))
                    (org-roam-feed-refresh))))
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
