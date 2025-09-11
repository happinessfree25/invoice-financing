;; title: invoice-financing
;; version: 1.0
;; summary: Blockchain-based invoice financing for SMEs
;; description: Allows SMEs to tokenize invoices as NFTs and sell them at a discount to investors

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-invalid-amount (err u104))
(define-constant err-invoice-not-for-sale (err u105))
(define-constant err-invoice-already-funded (err u106))
(define-constant err-invoice-not-funded (err u107))
(define-constant err-invoice-not-due (err u108))
(define-constant err-invoice-expired (err u109))
(define-constant err-insufficient-funds (err u110))

;; Data variables
(define-data-var next-invoice-id uint u1)

;; Invoice status enum: 1=Created, 2=ForSale, 3=Funded, 4=Paid, 5=Defaulted
(define-map invoices
  { invoice-id: uint }
  {
    issuer: principal,
    debtor: principal,
    amount: uint,
    discount-rate: uint, ;; in basis points (1/100 of a percent)
    issue-date: uint,
    due-date: uint,
    status: uint,
    investor: (optional principal),
    funded-amount: uint,
    description: (string-utf8 256)
  }
)

;; Track invoice ownership
(define-map invoice-owner
  { invoice-id: uint }
  { owner: principal }
)

;; Track user balances
(define-map user-balance
  { user: principal }
  { balance: uint }
)

;; Read-only functions

(define-read-only (get-invoice (invoice-id uint))
  (match (map-get? invoices { invoice-id: invoice-id })
    invoice (ok invoice)
    err-not-found
  )
)

(define-read-only (get-invoice-owner (invoice-id uint))
  (match (map-get? invoice-owner { invoice-id: invoice-id })
    owner (ok owner)
    err-not-found
  )
)

(define-read-only (get-user-balance (user principal))
  (default-to
    { balance: u0 }
    (map-get? user-balance { user: user })
  )
)

(define-read-only (get-next-invoice-id)
  (var-get next-invoice-id)
)

(define-read-only (calculate-funding-amount (invoice-id uint))
  (match (map-get? invoices { invoice-id: invoice-id })
    invoice 
    (let (
      (full-amount (get amount invoice))
      (discount-rate (get discount-rate invoice))
      (discount-amount (/ (* full-amount discount-rate) u10000))
    )
      (ok (- full-amount discount-amount)))
    err-not-found
  )
)

;; Public functions

;; Create a new invoice
(define-public (create-invoice 
    (debtor principal) 
    (amount uint) 
    (discount-rate uint) 
    (due-date uint) 
    (description (string-utf8 256)))
  (let (
    (invoice-id (var-get next-invoice-id))
    (current-time stacks-block-height)
  )
    ;; Validate inputs
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (< discount-rate u10000) err-invalid-amount) ;; Max 100%
    (asserts! (> due-date current-time) err-invalid-amount)
    
    ;; Create the invoice
    (map-set invoices
      { invoice-id: invoice-id }
      {
        issuer: tx-sender,
        debtor: debtor,
        amount: amount,
        discount-rate: discount-rate,
        issue-date: current-time,
        due-date: due-date,
        status: u1, ;; Created
        investor: none,
        funded-amount: u0,
        description: description
      }
    )
    
    ;; Set ownership
    (map-set invoice-owner
      { invoice-id: invoice-id }
      { owner: tx-sender }
    )
    
    ;; Increment invoice ID
    (var-set next-invoice-id (+ invoice-id u1))
    
    (ok invoice-id)
  )
)

;; List invoice for sale
(define-public (list-invoice-for-sale (invoice-id uint))
  (let (
    (invoice-data (unwrap! (get-invoice invoice-id) err-not-found))
    (owner-data (unwrap! (get-invoice-owner invoice-id) err-not-found))
  )
    ;; Check ownership
    (asserts! (is-eq tx-sender (get owner (unwrap! (get-invoice-owner invoice-id) err-not-found))) err-unauthorized)
    ;; Check status
    (asserts! (is-eq (get status invoice-data) u1) err-invoice-not-for-sale)
    
    ;; Update status to ForSale
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice-data { status: u2 })
    )
    
    (ok true)
  )
)

;; Fund an invoice
(define-public (fund-invoice (invoice-id uint))
  (let (
    (invoice-data (unwrap! (get-invoice invoice-id) err-not-found))
    (funding-amount (unwrap! (calculate-funding-amount invoice-id) err-not-found))
    (user-data (get-user-balance tx-sender))
  )
    ;; Check invoice is for sale
    (asserts! (is-eq (get status invoice-data) u2) err-invoice-not-for-sale)
    ;; Check user has enough balance
    (asserts! (>= (get balance user-data) funding-amount) err-insufficient-funds)
    
    ;; Update invoice status
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice-data 
        { 
          status: u3, 
          investor: (some tx-sender),
          funded-amount: funding-amount
        }
      )
    )
    
    ;; Transfer funds to issuer
    (map-set user-balance
      { user: tx-sender }
      { balance: (- (get balance user-data) funding-amount) }
    )
    
    (map-set user-balance
      { user: (get issuer invoice-data) }
      { balance: (+ (get balance (get-user-balance (get issuer invoice-data))) funding-amount) }
    )
    
    (ok true)
  )
)

;; Deposit funds to your account
(define-public (deposit (amount uint))
  (let (
    (user-data (get-user-balance tx-sender))
  )
    (asserts! (> amount u0) err-invalid-amount)
    
    ;; Update user balance
    (map-set user-balance
      { user: tx-sender }
      { balance: (+ (get balance user-data) amount) }
    )
    
    (ok true)
  )
)

;; Withdraw funds from your account
(define-public (withdraw (amount uint))
  (let (
    (user-data (get-user-balance tx-sender))
  )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= (get balance user-data) amount) err-insufficient-funds)
    
    ;; Update user balance
    (map-set user-balance
      { user: tx-sender }
      { balance: (- (get balance user-data) amount) }
    )
    
    (ok true)
  )
)

;; Pay an invoice (by debtor)
(define-public (pay-invoice (invoice-id uint))
  (let (
    (invoice-data (unwrap! (get-invoice invoice-id) err-not-found))
    (user-data (get-user-balance tx-sender))
    (full-amount (get amount invoice-data))
  )
    ;; Check caller is debtor
    (asserts! (is-eq tx-sender (get debtor invoice-data)) err-unauthorized)
    ;; Check invoice is funded
    (asserts! (is-eq (get status invoice-data) u3) err-invoice-not-funded)
    ;; Check user has enough balance
    (asserts! (>= (get balance user-data) full-amount) err-insufficient-funds)
    
    ;; Update invoice status
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice-data { status: u4 })
    )
    
    ;; Transfer funds to investor
    (match (get investor invoice-data)
      investor
      (begin
        (map-set user-balance
          { user: tx-sender }
          { balance: (- (get balance user-data) full-amount) }
        )
        
        (map-set user-balance
          { user: investor }
          { balance: (+ (get balance (get-user-balance investor)) full-amount) }
        )
        (ok true)
      )
      err-invoice-not-funded
    )
  )
)

;; Mark invoice as defaulted (can only be done by contract owner)
(define-public (mark-invoice-defaulted (invoice-id uint))
  (let (
    (invoice-data (unwrap! (get-invoice invoice-id) err-not-found))
  )
    ;; Check caller is contract owner
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    ;; Check invoice is funded and past due date
    (asserts! (is-eq (get status invoice-data) u3) err-invoice-not-funded)
    (asserts! (> stacks-block-height (get due-date invoice-data)) err-invoice-not-due)
    
    ;; Update invoice status
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice-data { status: u5 })
    )
    
    (ok true)
  )
)

;; Cancel invoice listing (only if not yet funded)
(define-public (cancel-invoice-listing (invoice-id uint))
  (let (
    (invoice-data (unwrap! (get-invoice invoice-id) err-not-found))
  )
    ;; Check ownership
    (asserts! (is-eq tx-sender (get owner (unwrap! (get-invoice-owner invoice-id) err-not-found))) err-unauthorized)
    ;; Check status is ForSale
    (asserts! (is-eq (get status invoice-data) u2) err-invoice-not-for-sale)
    
    ;; Update status back to Created
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice-data { status: u1 })
    )
    
    (ok true)
  )
)

(define-constant err-invalid-trade (err u111))

(define-public (list-invoice-for-trade 
    (invoice-id uint)
    (new-discount-rate uint))
  (let (
    (invoice-data (unwrap! (get-invoice invoice-id) err-not-found))
  )
    (asserts! (is-eq (some tx-sender) (get investor invoice-data)) err-unauthorized)
    (asserts! (is-eq (get status invoice-data) u3) err-invoice-not-funded)
    (asserts! (< new-discount-rate u10000) err-invalid-amount)
    
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice-data 
        { 
          discount-rate: new-discount-rate,
          status: u6
        }
      )
    )
    (ok true)
  )
)

(define-public (buy-traded-invoice (invoice-id uint))
  (let (
    (invoice-data (unwrap! (get-invoice invoice-id) err-not-found))
    (trade-amount (unwrap! (calculate-funding-amount invoice-id) err-not-found))
    (buyer-balance (get balance (get-user-balance tx-sender)))
    (seller (unwrap! (get investor invoice-data) err-not-found))
  )
    (asserts! (is-eq (get status invoice-data) u6) err-invalid-trade)
    (asserts! (>= buyer-balance trade-amount) err-insufficient-funds)
    
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice-data 
        {
          investor: (some tx-sender),
          status: u3
        }
      )
    )
    
    (map-set user-balance 
      { user: tx-sender }
      { balance: (- buyer-balance trade-amount) }
    )
    
    (map-set user-balance
      { user: seller }
      { balance: (+ (get balance (get-user-balance seller)) trade-amount) }
    )
    
    (ok true)
  )
)


(define-constant err-invalid-batch (err u112))
(define-constant max-batch-size u10)

(define-public (create-invoice-batch
    (debtors (list 10 principal))
    (amounts (list 10 uint))
    (discount-rates (list 10 uint))
    (due-dates (list 10 uint))
    (descriptions (list 10 (string-utf8 256))))
  (let (
    (batch-size (len debtors))
  )
    (asserts! (> batch-size u0) err-invalid-batch)
    (asserts! (<= batch-size max-batch-size) err-invalid-batch)
    (asserts! (is-eq batch-size (len amounts)) err-invalid-batch)
    (asserts! (is-eq batch-size (len discount-rates)) err-invalid-batch)
    (asserts! (is-eq batch-size (len due-dates)) err-invalid-batch)
    (asserts! (is-eq batch-size (len descriptions)) err-invalid-batch)
    
    (ok (map create-invoice-internal 
      debtors 
      amounts 
      discount-rates 
      due-dates 
      descriptions))
  )
)

(define-private (create-invoice-internal
    (debtor principal)
    (amount uint)
    (discount-rate uint)
    (due-date uint)
    (description (string-utf8 256)))
  (create-invoice debtor amount discount-rate due-date description)
)