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

(defcustom org-roam-feed-index nil
  "ID of the roam node within which to build an index."
  :type 'string
  :group 'org-roam-feed)

(defcustom org-roam-feed-index-filter (lambda (node) t)
  "Set to a function that when passed an `org-roam-node` object, returns true for nodes that you want to be included in the index."
  :type 'function
  :group 'org-roam-feed
  )

(defvar org-roam-feed-review-history (make-hash-table :test 'equal)
  "Hash table storing review history for nodes.")

(defvar org-roam-feed-node-scores (make-hash-table :test 'equal)
  "Hash table storing computed scores for nodes.")


(defun org-roam-feed-get-backlinks (node)
  "Get the alist of properties for each node that links to NODE."
  (mapcar #'car (org-roam-db-query
                 [:select [nodes:id nodes:properties]
			  :from links
			  :join nodes
			  :on (= links:source nodes:id)
			  :where (= dest $s1)
			  :and (= type "id")
			  ]
                 (org-roam-node-id node))))

(defun org-roam-feed-get-outlinks (node)
  "Get all nodes that NODE links to."
  (mapcar #'car (org-roam-db-query
                 [:select [nodes:id nodes:properties]
			  :from links
			  :join nodes
			  :on (= links:dest nodes:id)
			  :where (= source $s1)
			  :and (= type "id")]
                 (org-roam-node-id node))))


(defun org-roam-feed-standalone-node-score (node)
  "Calculate a score for NODE-ID based only on its own properties."
  (let* ((history (org-roam-feed-get-history node))
	 (last-review (car history))
	 ;; (next-review (org-roam-feed-next-review node))
         (review-count (length history))
	 (frecency 
          (if last-review
	      (* (/ 1.0 (+ 1.0 (/ (float-time (time-since last-review)) 86400)))
                 (log (+ 1.0 review-count)))
            0.0))
 	 ;; inaccurate, here so we can check the plumbing.
	 ;; higher factor means shorter interval.
					; so, more frequent/recent in order to decrease repetition.
	 ;; (But this needs to be responsive to wanting to review
	 ;; something again soon.
	 ;; So, decent default choice but not the best one.)
	 (srs-delay-factor (/ 1 frecency))
	 (srs-interval (* (* org-roam-feed-default-interval
			     (expt 2 (1- review-count)))
			  (/ 1.0 (+ 0.5 srs-delay-factor))))
	 ;; (review-proximity (/ 1.0 (+ 1.0 (/ (ts-difference next-review (ts-now)) 86400))))
	 (standalone-score (/ 1 srs-interval)))
    (cl-check-type frecency number)
    (cl-check-type srs-interval number)
    (cl-check-type standalone-score number)
    (when (-any #'isnan (list frecency srs-interval standalone-score))
      (error "One of these isnan: Node %s SRS interval: %f Frecency: %f Score: %f" (org-roam-node-id node) srs-interval frecency standalone-score))
    standalone-score))



(defun org-roam-feed-pagerank-step (node-id-score-alist)
  (let* ((get-avg-backlink-score (lambda (node-id)
				   (let*
				       ((backlink-scores
					 (mapcar (lambda (n)
						   (or  (alist-get n node-id-score-alist nil nil #'equal)
							(error "no standalone score? node %s has score %s" n
							       (alist-get n node-id-score-alist "fake" nil #'equal))))
						 (org-roam-feed-get-backlinks (org-roam-node-from-id node-id))))
					(avg-backlink-score (if (not backlink-scores) 0
							      (/ (rx--foldl '+ 0 backlink-scores)
								 (float (length backlink-scores))))))
				     (cl-check-type avg-backlink-score number)
				     avg-backlink-score)))
	 (get-pagerank-score (lambda (node-id-score)
			       (let ((pagerank-score (+ (* (- 1 org-roam-feed-damping-factor)
							   (cdr node-id-score))
								   (* org-roam-feed-damping-factor
								      (funcall get-avg-backlink-score (car node-id-score))))))
				 (cl-check-type pagerank-score number)
				 (cons (car node-id-score) pagerank-score)))))
    (mapcar get-pagerank-score node-id-score-alist)))

(defun org-roam-feed-update-scores ()
  "Update scores for all nodes in the network."
  (let* ((nodes (org-roam-node-list))
	(standalone-scores (mapcar (lambda (node) (cons (org-roam-node-id node) (org-roam-feed-standalone-node-score node))) nodes))
	(pagerank-scores (org-roam-feed-pagerank-step (org-roam-feed-pagerank-step standalone-scores)))
	)
    (message "%s" standalone-scores)
    (dolist (node nodes)
      (puthash node (alist-get (org-roam-node-id node) pagerank-scores nil nil #'equal) org-roam-feed-node-scores))
    ))


(defun org-roam-feed-set-property (node property value)
  "Visit an org-roam node and set a property to a value."
  (save-excursion
    (org-roam-node-visit node)
					;org-roam wants properties to be sets. We use org-mode's org-set-property to avoid this behaviour.
					; TODO attempt to restore honor to this floating point - see what it takes to make the conversion lossless.
    (org-set-property property (number-to-string value))
    (save-buffer)
    (org-mark-ring-goto)))


(defun org-roam-feed-get-property-from-node (node property)
  (alist-get property (org-roam-node-properties node) nil nil 'string-equal))

(defun org-roam-feed-get-history (node)
  (let ((val (gethash node org-roam-feed-review-history)))
  (if val val
    (let ((fileval (org-roam-feed-get-property-from-node node "MTIME")))
      (if fileval (progn (puthash node (split-string-and-unquote fileval ",") org-roam-feed-review-history))
	(org-roam-feed-record-review node (org-roam-node-file-mtime node)))
      (gethash node org-roam-feed-review-history)))))


(defun org-roam-feed-record-review (&optional node review-time)
  "Record a review for NODE and propagate effects through the network."
  ;; add the review time to the MTIME property in the node.
  (interactive)
  (let ((node (or node (org-roam-node-at-point)))
	(review-time (or review-time (current-time)))
	(timestamp (format-time-string (org-time-stamp-format t t) review-time)))
    (save-excursion
      (org-roam-node-visit node)
      (let ((mtimes (split-string-and-unquote (or (alist-get "MTIME" (org-roam-node-properties node) nil nil 'string-equal) "") ",")))
	(org-set-property "MTIME" (combine-and-quote-strings (cons timestamp mtimes) ","))
	(puthash node (cons timestamp mtimes) org-roam-feed-review-history)) ;; update the hash table
      (org-mark-ring-goto))))

;; (defun org-roam-feed-rank-nodes (nodes)
;;   (org-roam-feed-update-scores)
;;   (let* ((nodes-with-schedule
;; 	  (mapcar (lambda (node)
;; 		    (let* ((next-review (org-roam-feed-next-review node))
;; 			   (score (gethash node org-roam-feed-node-scores)))
;; 		      (list node next-review score)))
;; 		  nodes)))))

(require 'org-roam)
(require 'magit-section)
(require 'ts)

(defcustom org-roam-feed-page-size 10
  "Number of nodes to display per page."
  :type 'integer
  :group 'org-roam-feed)

(defvar-local org-roam-feed-current-page 0
  "Current page number (0-based) in the feed buffer.")


(defun org-roam-feed-get-nodes ()
  "Get all nodes sorted by review priority with PageRank influence."
  (let* ((nodes (org-roam-node-list))
	 (_   (org-roam-feed-update-scores nodes))
         (nodes-with-schedule
          (mapcar (lambda (node)
                    (let* ((next-review (org-roam-feed-next-review node))
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

(defvar org-roam-feed-sorted-node-list nil)
(defun org-roam-feed-update-sorted-node-list ()
  (setq org-roam-feed-sorted-node-list (org-roam-node-list))
  (sort org-roam-feed-sorted-node-list (lambda (x y) (> (gethash x org-roam-feed-node-scores) (gethash y org-roam-feed-node-scores)))))

(defun org-roam-feed-update ()
    (org-roam-feed-update-scores)
    (org-roam-feed-update-sorted-node-list))

(defun org-roam-feed-get-page-nodes (page)
  "Get nodes for the specified PAGE number."
  (let* ((start (* page org-roam-feed-page-size))
         (end (min (+ start org-roam-feed-page-size) (length org-roam-feed-sorted-node-list))))
    (seq-subseq org-roam-feed-sorted-node-list start end)))

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
  (org-roam-feed-update)
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

(defun org-roam-feed-update-index ()
  """Create a list of org-mode links corresponding to the first page of feed results, and insert it into a dedicated heading in a roam node. Links to nodes are not included if `org-roam-feed-index-filter returns false when given the node ID.`

TODO:
- replace org-map-entries with an org-element or org-ml based operation.
- construct the list of links with org-element instead of string formatting."""
(let* ((node-list (ntake 10 (-filter org-roam-feed-index-filter (org-roam-feed-get-page-nodes 0))))
       (link-block (mapconcat (lambda (node)
				 (let ((link (concat "id:" (org-roam-node-id node)))
				       (title (org-roam-node-title node)))
				   (format "- [[%s][%s]]\n" link title)))
			       node-list))
	(review-heading (concat "* To Review\n" link-block)))
    ;; This works by killing and replacing a heading in a roam node with links corresponding to the first page.
    (if org-roam-feed-index 
	(save-excursion
	  (org-roam-node-visit (org-roam-node-from-id org-roam-feed-index))
	  (org-map-entries (lambda ()
			     (progn
			       (if (string= (nth 4 (org-heading-components)) "To Review")
				   (save-restriction
				     (org-mark-subtree)
				     (setq org-map-continue-from (region-beginning))
				     (delete-region (region-beginning) (region-end))))
			       (point-max-marker)
			       (insert review-heading)
			       (save-buffer)))
			   t 'file))
      (message "No index node set, skipping the creation of an index."))))


(defun org-roam-feed-insert-node (node)
  "Insert NODE into the feed buffer with magit-section formatting."
  (let* ((score (gethash node org-roam-feed-node-scores))
         (history (org-roam-feed-get-history node)))
    (magit-insert-section section (org-roam-node-section)
      (magit-insert-heading
        (format "%s (Score: %s, Reviews: %d)"
                (org-roam-node-title node)
                score
                (length history)))
      (oset section node node)
      (when history
        (insert (format "Last reviewed: %s\n"
                       (ts-format (car history)))))
      (let ((backlinks (org-roam-feed-get-backlinks node))
            (outlinks (org-roam-feed-get-outlinks node)))
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

(defvar org-roam-feed-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'org-roam-feed-refresh)
    (define-key map (kbd "RET") 'org-roam-node-visit)
    (define-key map (kbd "r") 'org-roam-feed-record-review)
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
