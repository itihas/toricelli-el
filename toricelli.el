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

(defcustom toricelli-reviewed-feed nil
  "ID of the roam node within which to build an index."
  :type 'string
  :group 'toricelli)

(defcustom toricelli-featured-feed nil
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
      (let* ((current-score (toricelli-get-property-from-node node "REVIEW_SCORE" #'string-to-number "0"))
	    (new-score (number-to-string (* (1+ current-score) grade)))(mtimes (toricelli-get-history node)))
	(org-set-property "REVIEW_SCORE" new-score) ;; arbitrary choice of score update
	(if (>= grade 4)
	    (org-set-property "FEATURED_REVIEW" timestamp))
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

(defun toricelli-get-featured-date (node)
  (toricelli-get-property-from-node
   node "FEATURED_REVIEW"
   #'identity
   (toricelli-get-property-from-node node "CREATED")))

(defvar toricelli-featured-node-list nil)
(defun toricelli-update-featured-node-list ()
  (setq toricelli-featured-node-list
	(-sort (lambda (x y) (let* ((hx (safe-date-to-time (toricelli-get-featured-date x)))
				    (hy (safe-date-to-time (toricelli-get-featured-date y)))
				    (g (time-less-p hy hx)))
			       ;; (message "%s > %s: %s" hx hy g)
			       g))
	       (org-roam-node-list))))

(defun toricelli-update ()
  (interactive)
    (toricelli-update-scores)
    (toricelli-update-sorted-node-list)
    (toricelli-update-recent-node-list)
    (toricelli-update-featured-node-list))

(defun toricelli--link-list (node-list)
  "Format NODE-LIST as an org plain list of id: links."
  (mapconcat (lambda (node)
	       (format "- [[id:%s][%s]]\n" (org-roam-node-id node) (org-roam-node-title node)))
	     node-list))

(defun toricelli--replace-index-section (name heading body)
  "Replace section NAME of the `toricelli-index' node with HEADING and BODY.
The existing section is found by its title starting with NAME (so
\"Featured\" matches \"* Featured ([[file:featured.xml][rss]])\"),
and is replaced where it stands, keeping the order of sections in
the file.  If there is none, the section goes just before the
first top-level heading, i.e. after the file's property drawer and
preamble, or at the end of the file if it has no headings."
  (if (not toricelli-index)
      (message "No index node set, skipping the creation of an index.")
    (let ((node (or (org-roam-node-from-id toricelli-index)
		    (error "toricelli-index %s is not an org-roam node; is its :ID: drawer still at the top of the file?"
			   toricelli-index)))
	  (section (concat "* " heading "\n" body "\n")))
      (with-current-buffer (find-file-noselect (org-roam-node-file node))
	(org-with-wide-buffer
	 (goto-char (point-min))
	 (let ((case-fold-search nil))
	   (if (re-search-forward (format "^\\* %s\\_>" (regexp-quote name)) nil t)
	       (let ((beg (line-beginning-position)))
		 (org-end-of-subtree t t)
		 (delete-region beg (point))
		 (goto-char beg))
	     (if (re-search-forward "^\\* " nil t)
		 (goto-char (line-beginning-position))
	       (goto-char (point-max))
	       (unless (bolp) (insert "\n")))))
	 (insert section))
	(when (buffer-modified-p) (save-buffer))))))

(defun toricelli-update-index (&optional n)
  "Replace the index's To Review section with the top N nodes by score.
Nodes for which `toricelli-index-filter' returns nil are skipped."
  (interactive "N")
  (toricelli--replace-index-section
   "To Review" "To Review"
   (toricelli--link-list (ntake (or n 10) (-filter toricelli-index-filter toricelli-sorted-node-list)))))

(defun toricelli-update-reviewed-feed ()
  (interactive)
  (let* ((node-list (ntake 100 (-filter toricelli-index-filter toricelli-recent-node-list)))
	 (link-heading (mapconcat (lambda (node)
				    (let ((link (concat (file-name-base (org-roam-node-file node)) ".html"))
					  (pubdate (car (toricelli-get-history  node)))
					  (title (org-roam-node-title node)))
				      (format "* %s\n:PROPERTIES:\n:PUBDATE: %s\n:RSS_PERMALINK: %s\n:END:\n\n" title pubdate link)))
				  node-list))
	 (feed-page (concat "#+title: itihas.recently.reviewed\n#+author:itihas \n" link-heading)))
    (message feed-page)
    (if toricelli-reviewed-feed
	(with-temp-file toricelli-reviewed-feed
	  (insert feed-page))
      (message "No feed page set, skipping the creation of one."))))

(defun toricelli-update-featured-feed ()
  (interactive)
  (let* ((node-list (ntake 100 (-filter toricelli-index-filter toricelli-featured-node-list)))
	 (link-heading (mapconcat (lambda (node)
				    (let ((link (concat (file-name-base (org-roam-node-file node)) ".html"))
					  (pubdate (toricelli-get-featured-date node))
					  (title (org-roam-node-title node)))
				      (format "* %s\n:PROPERTIES:\n:PUBDATE: %s\n:RSS_PERMALINK: %s\n:END:\n\n" title pubdate link)))
				  node-list))
	 (feed-page (concat "#+title: itihas.featured\n#+author:itihas \n" link-heading)))
    (message feed-page)
    (if toricelli-featured-feed
	(with-temp-file toricelli-featured-feed
	  (insert feed-page))
      (message "No feed page set, skipping the creation of one."))))

(defun toricelli-update-recently-reviewed (&optional n)
  "Replace the index's Recently Reviewed section with the N latest reviews.
Nodes for which `toricelli-index-filter' returns nil are skipped."
  (interactive "N")
  (toricelli--replace-index-section
   "Recently Reviewed" "Recently Reviewed ([[file:reviewed-feed.xml][rss]])"
   (toricelli--link-list (ntake (or n 10) (-filter toricelli-index-filter toricelli-recent-node-list)))))


(defun toricelli-publish-updates ()
  (interactive)
  (toricelli-update-featured-feed)
  (toricelli-update-reviewed-feed)
  (toricelli-update-index)
  (toricelli-update-featured)
  (toricelli-update-recently-reviewed))

(defun toricelli-update-featured (&optional n)
  "Replace the index's Featured section with the N latest featured nodes.
Nodes for which `toricelli-index-filter' returns nil are skipped."
  (interactive "N")
  (toricelli--replace-index-section
   "Featured" "Featured ([[file:featured.xml][rss]])"
   (toricelli--link-list (ntake (or n 10) (-filter toricelli-index-filter toricelli-featured-node-list)))))

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
