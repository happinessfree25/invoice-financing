;; invoice-attachments.clar
;; Lightweight invoice document attachment registry via content hashes

(define-constant ERR-NOT-FOUND (err u100))
(define-constant ERR-INVALID (err u101))

;; Per-invoice attachment counter
(define-map attachment-count
  { invoice-id: uint }
  { count: uint }
)

;; Attachment records keyed by invoice-id and doc-id
(define-map attachments
  { invoice-id: uint, doc-id: uint }
  {
    hash: (buff 32),          ;; content-addressed identifier (e.g., SHA-256)
    kind: (string-ascii 32),  ;; short type hint: 'pdf', 'po', 'receipt', etc.
    uploaded-by: principal,
    uploaded-at: uint
  }
)

(define-read-only (get-attachment-count (invoice-id uint))
  (default-to { count: u0 } (map-get? attachment-count { invoice-id: invoice-id }))
)

(define-read-only (get-attachment (invoice-id uint) (doc-id uint))
  (map-get? attachments { invoice-id: invoice-id, doc-id: doc-id })
)

(define-read-only (get-latest-attachment (invoice-id uint))
  (let ((c (get count (get-attachment-count invoice-id))))
    (if (> c u0)
      (get-attachment invoice-id c)
      none))
)

(define-public (add-attachment (invoice-id uint)
                               (hash (buff 32))
                               (kind (string-ascii 32)))
  (let ((curr (get count (get-attachment-count invoice-id)))
        (next (+ (get count (get-attachment-count invoice-id)) u1))
        (now stacks-block-height))
    (asserts! (> (len kind) u0) ERR-INVALID)
    (asserts! (> (len hash) u0) ERR-INVALID)

    (map-set attachments
      { invoice-id: invoice-id, doc-id: next }
      { hash: hash, kind: kind, uploaded-by: tx-sender, uploaded-at: now })

    (map-set attachment-count
      { invoice-id: invoice-id }
      { count: next })

    (ok next))
)
