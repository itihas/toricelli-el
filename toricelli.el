(require 'org-roam)
(require 'magit-section)
(require 'ts)

(defgroup toricelli nil
  "Feed view for Org-roam nodes with spaced repetition."
  :group 'org-roam)

(defcustom toricelli-default-interval 3
  "Default interval (in days) for reviewing nodes."
  :type 'integer
  :group 'toricelli)

(defcustom toricelli-damping-factor 0.85
  "Damping factor for PageRank-like algorithm (between 0 and 1)."
  :type 'float
  :group 'toricelli)

(defcustom toricelli-index nil
  "ID of the roam node within which to build an index."
  :type 'string
  :group 'toricelli)

(defcustom toricelli-index-filter (lambda (node) t)
  "Set to a function that when passed an `org-roam-node` object, returns true for nodes that you want to be included in the index."
  :type 'function
  :group 'toricelli
  )

(defvar toricelli-review-history (make-hash-table :test 'equal)
  "Hash table storing review history for nodes.")

(defvar toricelli-node-scores (make-hash-table :test 'equal)
  "Hash table storing computed scores for nodes.")


(defun toricelli-get-backlinks (node)
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

(defun toricelli-get-outlinks (node)
  "Get all nodes that NODE links to."
  (mapcar #'car (org-roam-db-query
                 [:select [nodes:id nodes:properties]
			  :from links
			  :join nodes
			  :on (= links:dest nodes:id)
			  :where (= source $s1)
			  :and (= type "id")]
                 (org-roam-node-id node))))

(defun toricelli-standalone-node-score (node)
  "Calculate a score for NODE-ID based only on its own properties."
  (let* ((history (toricelli-get-history node))
	 (last-review (car history))
	 ;; (next-review (toricelli-next-review node))
         (review-count (length history))
	 (frecency 
          (if last-review
	      (* (/ 1.0 (+ 1.0 (/ (float-time (time-since last-review)) 86400)))
                 (log (+ 1.0 review-count)))
            0.0))
	 (feed-score (toricelli-get-property-from-node node "REVIEW_SCORE" #'string-to-number (number-to-string frecency)))
	 (srs-delay-factor (/ 1 (1+ feed-score)))
	 (srs-interval (* (* toricelli-default-interval
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

(defun toricelli-pagerank-step (node-id-score-alist)
  (let* ((get-avg-backlink-score (lambda (node-id)
				   (let*
				       ((backlink-scores
					 (mapcar (lambda (n)
						   (or  (alist-get n node-id-score-alist nil nil #'equal)
							(error "no standalone score? node %s has score %s" n
							       (alist-get n node-id-score-alist "fake" nil #'equal))))
						 (toricelli-get-backlinks (org-roam-node-from-id node-id))))
					(avg-backlink-score (if (not backlink-scores) 0
							      (/ (rx--foldl '+ 0 backlink-scores)
								 (float (length backlink-scores))))))
				     (cl-check-type avg-backlink-score number)
				     avg-backlink-score)))
	 (get-pagerank-score (lambda (node-id-score)
			       (let ((pagerank-score (+ (* (- 1 toricelli-damping-factor)
							   (cdr node-id-score))
								   (* toricelli-damping-factor
								      (funcall get-avg-backlink-score (car node-id-score))))))
				 (cl-check-type pagerank-score number)
				 (cons (car node-id-score) pagerank-score)))))
    (mapcar get-pagerank-score node-id-score-alist)))

(defun toricelli-update-scores ()
  "Update scores for all nodes in the network."
  (let* ((nodes (org-roam-node-list))
	(standalone-scores (mapcar (lambda (node) (cons (org-roam-node-id node) (toricelli-standalone-node-score node))) nodes))
	(pagerank-scores (toricelli-pagerank-step (toricelli-pagerank-step standalone-scores)))
	)
    (message "%s" standalone-scores)
    (dolist (node nodes)
      (puthash node (alist-get (org-roam-node-id node) pagerank-scores nil nil #'equal) toricelli-node-scores))
    ))

(defun toricelli-set-property (node property value)
  "Visit an org-roam node and set a property to a value."
  (save-excursion
    (org-roam-node-visit node)
					; org-roam wants properties to be sets. We use org-mode's org-set-property to avoid this behaviour.
					; TODO attempt to restore honor to this floating point - see what it takes to make the conversion lossless.
    (org-set-property property (number-to-string value))
    (save-buffer)
    (org-mark-ring-goto)))

;; TODO change this to take a target type
;; TODO change the default value to be in the target type, not the source type.
(defun toricelli-get-property-from-node (node property &optional transform default)
  (let ((default (or default ""))
	(value (or (alist-get property (org-roam-node-properties node) nil nil 'string-equal) default))
	(transform (or transform #'identity)))
    (funcall transform value)))

(defun toricelli-get-history (node)
  (let ((val (gethash node toricelli-review-history)))
    (if val val
      (let ((val (toricelli-get-property-from-node node "MTIME"
						   (lambda (n) (split-string-and-unquote n ","))
						   (toricelli-get-property-from-node node "CREATED" nil
										     (format-time-string (org-time-stamp-format t t)
													 (org-roam-node-file-mtime node))))))
	(puthash node val toricelli-review-history)
	val))))

(defun toricelli-record-review (&optional grade node review-time)
  "Record a review for NODE and propagate effects through the network."
  ;; add the review time to the MTIME property in the node.
  (interactive "NGrade: ")
  (let ((grade (float (cond ((not grade) 3)
		      ((> 0 grade) 0)
		      ((< 5 grade) 5)
		      (t grade))))
	(node (or node (org-roam-node-at-point)))
	(review-time (or review-time (current-time)))
	(timestamp (format-time-string (org-time-stamp-format t t) review-time)))
    (save-excursion
      (org-roam-node-visit node)
      (let ((feed-score (toricelli-get-property-from-node node "REVIEW_SCORE" #'string-to-number "0"))
	    (mtimes (toricelli-get-history node)))
	(org-set-property "REVIEW_SCORE" (number-to-string (* (1+ feed-score) grade))) ;; arbitrary choice of score update
	(org-set-property "MTIME" (combine-and-quote-strings (cons timestamp mtimes) ","))
	(puthash node (cons timestamp mtimes) toricelli-review-history)) ;; update the hash table
      (org-mark-ring-goto))))

;; (defun toricelli-rank-nodes (nodes)
;;   (toricelli-update-scores)
;;   (let* ((nodes-with-schedule
;; 	  (mapcar (lambda (node)
;; 		    (let* ((next-review (toricelli-next-review node))
;; 			   (score (gethash node toricelli-node-scores)))
;; 		      (list node next-review score)))
;; 		  nodes)))))

(require 'org-roam)
(require 'magit-section)
(require 'ts)

(defcustom toricelli-page-size 10
  "Number of nodes to display per page."
  :type 'integer
  :group 'toricelli)

(defvar-local toricelli-current-page 0
  "Current page number (0-based) in the feed buffer.")

(defvar toricelli-sorted-node-list nil)
(defun toricelli-update-sorted-node-list ()
  (setq toricelli-sorted-node-list (org-roam-node-list))
  (sort toricelli-sorted-node-list (lambda (x y) (> (gethash x toricelli-node-scores) (gethash y toricelli-node-scores)))))

(defvar toricelli-recent-node-list nil)
(defun toricelli-update-recent-node-list ()
  (setq toricelli-recent-node-list
	(-sort (lambda (x y) (let* ((hx (safe-date-to-time (car (toricelli-get-history x))))
				       (hy (safe-date-to-time (car (toricelli-get-history y))))
				       (g (time-less-p hy hx)))
				  ;; (message "%s > %s: %s" hx hy g)
				  g))
		  (org-roam-node-list))))

(defun toricelli-update ()
  (interactive)
    (toricelli-update-scores)
    (toricelli-update-sorted-node-list)
    (toricelli-update-recent-node-list))

(defun toricelli-update-index (&optional n)
  """Create a list of org-mode links corresponding to the first page of feed results, and insert it into a dedicated heading in a roam node. Links to nodes are not included if `toricelli-index-filter returns false when given the node ID.`

TODO:
- replace org-map-entries with an org-element or org-ml based operation.
- construct the list of links with org-element instead of string formatting."""
(interactive "N")
(let* ((node-list (ntake (or n 10) (-filter toricelli-index-filter toricelli-sorted-node-list)))
       (link-block (mapconcat (lambda (node)
				 (let ((link (concat "id:" (org-roam-node-id node)))
				       (title (org-roam-node-title node)))
				   (format "- [[%s][%s]]\n" link title)))
			       node-list))
	(review-heading (concat "* To Review\n" link-block)))
    ;; This works by killing and replacing a heading in a roam node with links corresponding to the first page.
    (if toricelli-index 
	(save-excursion
	  (org-roam-node-visit (org-roam-node-from-id toricelli-index))
	  (org-map-entries (lambda ()
			     (progn
			       (if (string= (nth 4 (org-heading-components)) "To Review")
				   (save-restriction
				     (org-mark-subtree)
				     (setq org-map-continue-from (region-beginning))
				     (delete-region (region-beginning) (region-end))))))
			   t 'file)
	  (point-max-marker)
	  (insert review-heading)
	  (save-buffer))
      (message "No index node set, skipping the creation of an index."))))

(defun toricelli-update-recently-reviewed (&optional n)
  """Create a list of org-mode links corresponding to the first page of feed results, and insert it into a dedicated heading in a roam node. Links to nodes are not included if `toricelli-index-filter returns false when given the node ID.`

TODO:
- replace org-map-entries with an org-element or org-ml based operation.
- construct the list of links with org-element instead of string formatting."""
(interactive "N")
(let* ((node-list (ntake (or n 10) (-filter toricelli-index-filter toricelli-recent-node-list)))
       (link-block (mapconcat (lambda (node)
				 (let ((link (concat "id:" (org-roam-node-id node)))
				       (title (org-roam-node-title node)))
				   (format "- [[%s][%s]]\n" link title)))
			       node-list))
	(review-heading (concat "* Recently Reviewed\n" link-block)))
    ;; This works by killing and replacing a heading in a roam node with links corresponding to the first page.
    (if toricelli-index
	(save-excursion
	  (org-roam-node-visit (org-roam-node-from-id toricelli-index))
	  (org-map-entries (lambda ()
			     (progn
			       (if (string= (nth 4 (org-heading-components)) "Recently Reviewed")
				   (save-restriction
				     (org-mark-subtree)
				     (setq org-map-continue-from (region-beginning))
				     (delete-region (region-beginning) (region-end))))))
			   t 'file)
	  (point-max-marker)
	  (insert review-heading)
	  (save-buffer))
      (message "No index node set, skipping the creation of an index."))))

(defun toricelli-get-page-nodes (page node-list)
  "Get nodes for the specified PAGE number."
  (let* ((start (* page toricelli-page-size))
         (end (min (+ start toricelli-page-size) (length node-list))))
    (seq-subseq node-list start end)))

(defun toricelli-next-page ()
  "Move to the next page of nodes."
  (interactive)
  (let* ((total-nodes (length toricelli-sorted-node-list))
         (total-pages (ceiling (/ total-nodes (float toricelli-page-size))))
         (next-page (1+ toricelli-current-page)))
    (when (< next-page total-pages)
      (setq toricelli-current-page next-page)
      (toricelli-refresh))))

(defun toricelli-prev-page ()
  "Move to the previous page of nodes."
  (interactive)
  (when (> toricelli-current-page 0)
    (setq toricelli-current-page (1- toricelli-current-page))
    (toricelli-refresh)))

(defun toricelli-refresh ()
  "Refresh the feed buffer."
  (interactive nil toricelli-mode)
  (when (= major-mode))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (magit-insert-section (feed)
      (let* ((total-nodes (length toricelli-sorted-node-list))
             (total-pages (ceiling (/ total-nodes (float toricelli-page-size)))))
        ;; Insert pagination info header
        (insert (format "To Review\nPage %d/%d (Total nodes: %d)\n\n"
                       (1+ toricelli-current-page)
                       total-pages
                       total-nodes))
        ;; Insert navigation help
        (insert "Navigation: n - next page, p - previous page, g - refresh u - update\n\n"))
      (dolist (node (toricelli-get-page-nodes toricelli-current-page toricelli-sorted-node-list))
        (toricelli-insert-node node)))
    (insert "\n\n\n")
    (magit-insert-section (feed)
      (let* ((total-nodes (length toricelli-recent-node-list))
             (total-pages (ceiling (/ total-nodes (float toricelli-page-size)))))
        ;; Insert pagination info header
        (insert (format "Recently Reviewed\nPage %d/%d (Total nodes: %d)\n\n"
                       (1+ toricelli-current-page)
                       total-pages
                       total-nodes))
        ;; Insert navigation help
        (insert "Navigation: n - next page, p - previous page, g - refresh u - update\n\n"))
      (dolist (node (toricelli-get-page-nodes toricelli-current-page toricelli-recent-node-list))
        (toricelli-insert-node node)))))

(defun toricelli-insert-node (node)
  "Insert NODE into the feed buffer with magit-section formatting."
  (let* ((score (gethash node toricelli-node-scores))
         (history (toricelli-get-history node)))
    (magit-insert-section section (org-roam-node-section)
      (magit-insert-heading
        (format "%s (Score: %s, Reviews: %d)"
                (org-roam-node-title node)
                score
                (length history)))
      t
      (oset section node node)
      (when history
        (insert (format "Last reviewed: %s\n"
                       (ts-format (car history)))))
      (let ((backlinks (toricelli-get-backlinks node))
            (outlinks (toricelli-get-outlinks node)))
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

(defvar toricelli-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'toricelli-refresh)
    (define-key map (kbd "u") 'toricelli-update)
    (define-key map (kbd "RET") 'org-roam-node-visit)
    (define-key map (kbd "r") 'toricelli-record-review)
    (define-key map (kbd "n") 'toricelli-next-page)
    (define-key map (kbd "p") 'toricelli-prev-page)
    map)
  "Keymap for `toricelli-mode'.")

(define-derived-mode toricelli-mode magit-section-mode "Toricelli"
  "Major mode for viewing Org-roam nodes in a feed."
  :group 'toricelli)

(defun toricelli ()
  "Display Org-roam nodes in a feed buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*Toricelli*")))
    (with-current-buffer buffer
      (toricelli-mode)
      (toricelli-refresh))
    (switch-to-buffer buffer)))

(provide 'toricelli)
