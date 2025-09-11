;; title: invoice-watchlist
;; version: 1.0
;; summary: Personal watchlist system for tracking invoice IDs
;; description: Allows users to maintain curated lists of invoice IDs for monitoring and tracking

;; Error constants
(define-constant ERR-DUPLICATE (err u100))
(define-constant ERR-MAX-REACHED (err u101))
(define-constant ERR-NOT-FOUND (err u102))
(define-constant ERR-INVALID-INPUT (err u103))

;; Configuration constants
(define-constant MAX-WATCHLIST-SIZE u100)

;; Data maps
;; Tracks a user's personal watchlist of invoice IDs
(define-map watchlists
  { user: principal }
  { 
    invoices: (list 100 uint),
    created-at: uint,
    last-updated: uint
  }
)

;; Watchlist statistics for analytics
(define-map watchlist-stats
  { user: principal }
  {
    total-added: uint,
    total-removed: uint,
    current-count: uint
  }
)

;; Read-only functions

(define-read-only (get-watchlist (user principal))
  (let (
    (watchlist-data (default-to 
      { 
        invoices: (list), 
        created-at: u0, 
        last-updated: u0 
      }
      (map-get? watchlists { user: user })
    ))
  )
    (ok (get invoices watchlist-data))
  )
)

(define-read-only (get-watchlist-info (user principal))
  (let (
    (watchlist-data (default-to 
      { 
        invoices: (list), 
        created-at: u0, 
        last-updated: u0 
      }
      (map-get? watchlists { user: user })
    ))
    (stats-data (default-to
      {
        total-added: u0,
        total-removed: u0,
        current-count: u0
      }
      (map-get? watchlist-stats { user: user })
    ))
  )
    (ok {
      invoices: (get invoices watchlist-data),
      created-at: (get created-at watchlist-data),
      last-updated: (get last-updated watchlist-data),
      current-count: (len (get invoices watchlist-data)),
      total-added: (get total-added stats-data),
      total-removed: (get total-removed stats-data)
    })
  )
)

(define-read-only (is-watching (user principal) (invoice-id uint))
  (let (
    (watchlist (unwrap! (get-watchlist user) ERR-NOT-FOUND))
    (position (index-of watchlist invoice-id))
  )
    (ok (is-some position))
  )
)

(define-read-only (get-watchlist-count (user principal))
  (let (
    (watchlist (unwrap! (get-watchlist user) ERR-NOT-FOUND))
  )
    (ok (len watchlist))
  )
)

(define-read-only (get-watchlist-stats (user principal))
  (default-to
    {
      total-added: u0,
      total-removed: u0,
      current-count: u0
    }
    (map-get? watchlist-stats { user: user })
  )
)

;; Private helper functions

;; Data variable to pass target item to fold function
(define-data-var target-item-to-remove uint u0)

(define-private (remove-from-list (target-list (list 100 uint)) (target-item uint))
  (begin
    (var-set target-item-to-remove target-item)
    (fold remove-item-fold target-list (list))
  )
)

(define-private (remove-item-fold (item uint) (acc (list 100 uint)))
  (if (is-eq item (var-get target-item-to-remove))
    acc
    (unwrap! (as-max-len? (append acc item) u100) acc)
  )
)

(define-private (update-watchlist-stats (user principal) (added-count uint) (removed-count uint) (current-count uint))
  (let (
    (current-stats (get-watchlist-stats user))
  )
    (map-set watchlist-stats
      { user: user }
      {
        total-added: (+ (get total-added current-stats) added-count),
        total-removed: (+ (get total-removed current-stats) removed-count),
        current-count: current-count
      }
    )
  )
)

;; Public functions

(define-public (add-to-watchlist (invoice-id uint))
  (let (
    (current-watchlist-data (default-to 
      { 
        invoices: (list), 
        created-at: u0, 
        last-updated: u0 
      }
      (map-get? watchlists { user: tx-sender })
    ))
    (current-invoices (get invoices current-watchlist-data))
    (current-time stacks-block-height)
    (is-new-watchlist (is-eq (len current-invoices) u0))
  )
    ;; Input validation
    (asserts! (> invoice-id u0) ERR-INVALID-INPUT)
    
    ;; Check for duplicates
    (asserts! (is-none (index-of current-invoices invoice-id)) ERR-DUPLICATE)
    
    ;; Check maximum size
    (asserts! (< (len current-invoices) MAX-WATCHLIST-SIZE) ERR-MAX-REACHED)
    
    ;; Add invoice to watchlist
    (let (
      (new-invoices (unwrap! (as-max-len? (append current-invoices invoice-id) u100) ERR-MAX-REACHED))
    )
      ;; Update watchlist
      (map-set watchlists
        { user: tx-sender }
        {
          invoices: new-invoices,
          created-at: (if is-new-watchlist current-time (get created-at current-watchlist-data)),
          last-updated: current-time
        }
      )
      
      ;; Update statistics
      (update-watchlist-stats tx-sender u1 u0 (len new-invoices))
      
      (ok true)
    )
  )
)

(define-public (remove-from-watchlist (invoice-id uint))
  (let (
    (current-watchlist-data (default-to 
      { 
        invoices: (list), 
        created-at: u0, 
        last-updated: u0 
      }
      (map-get? watchlists { user: tx-sender })
    ))
    (current-invoices (get invoices current-watchlist-data))
    (current-time stacks-block-height)
  )
    ;; Input validation
    (asserts! (> invoice-id u0) ERR-INVALID-INPUT)
    
    ;; Check if invoice exists in watchlist
    (asserts! (is-some (index-of current-invoices invoice-id)) ERR-NOT-FOUND)
    
    ;; Remove invoice from watchlist
    (let (
      (new-invoices (remove-from-list current-invoices invoice-id))
    )
      ;; Update watchlist
      (map-set watchlists
        { user: tx-sender }
        {
          invoices: new-invoices,
          created-at: (get created-at current-watchlist-data),
          last-updated: current-time
        }
      )
      
      ;; Update statistics
      (update-watchlist-stats tx-sender u0 u1 (len new-invoices))
      
      (ok true)
    )
  )
)

(define-public (clear-watchlist)
  (let (
    (current-watchlist-data (default-to 
      { 
        invoices: (list), 
        created-at: u0, 
        last-updated: u0 
      }
      (map-get? watchlists { user: tx-sender })
    ))
    (current-invoices (get invoices current-watchlist-data))
    (current-time stacks-block-height)
    (removed-count (len current-invoices))
  )
    ;; Only proceed if watchlist has items
    (asserts! (> removed-count u0) ERR-NOT-FOUND)
    
    ;; Clear watchlist
    (map-set watchlists
      { user: tx-sender }
      {
        invoices: (list),
        created-at: (get created-at current-watchlist-data),
        last-updated: current-time
      }
    )
    
    ;; Update statistics
    (update-watchlist-stats tx-sender u0 removed-count u0)
    
    (ok true)
  )
)

(define-public (bulk-add-to-watchlist (invoice-ids (list 20 uint)))
  (let (
    (add-results (map add-to-watchlist invoice-ids))
  )
    (ok add-results)
  )
)

(define-public (bulk-remove-from-watchlist (invoice-ids (list 20 uint)))
  (let (
    (remove-results (map remove-from-watchlist invoice-ids))
  )
    (ok remove-results)
  )
)
