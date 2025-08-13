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

(define-constant err-invalid-rating (err u113))
(define-constant min-credit-score u300)
(define-constant max-credit-score u850)
(define-constant default-credit-score u600)

(define-map user-credit-profile
  { user: principal }
  {
    total-invoices: uint,
    paid-on-time: uint,
    total-defaults: uint,
    total-volume: uint,
    credit-score: uint,
    last-updated: uint
  }
)

(define-map invoice-ratings
  { invoice-id: uint }
  {
    risk-rating: uint,
    investor-rating: uint,
    payment-rating: uint,
    rated-by: (list 10 principal)
  }
)

(define-read-only (get-user-credit-profile (user principal))
  (default-to
    {
      total-invoices: u0,
      paid-on-time: u0,
      total-defaults: u0,
      total-volume: u0,
      credit-score: default-credit-score,
      last-updated: u0
    }
    (map-get? user-credit-profile { user: user })
  )
)

(define-read-only (get-invoice-rating (invoice-id uint))
  (default-to
    {
      risk-rating: u0,
      investor-rating: u0,
      payment-rating: u0,
      rated-by: (list)
    }
    (map-get? invoice-ratings { invoice-id: invoice-id })
  )
)

(define-constant err-escrow-insufficient (err u114))
(define-constant err-escrow-already-funded (err u115))
(define-constant err-escrow-not-ready (err u116))

(define-map user-escrow-balance
  { user: principal }
  { balance: uint }
)

(define-map invoice-escrow-funding
  { invoice-id: uint }
  { 
    funded: bool,
    amount: uint,
    funded-date: uint
  }
)

(define-read-only (get-user-escrow-balance (user principal))
  (default-to
    { balance: u0 }
    (map-get? user-escrow-balance { user: user })
  )
)

(define-read-only (get-invoice-escrow-status (invoice-id uint))
  (default-to
    { 
      funded: false,
      amount: u0,
      funded-date: u0
    }
    (map-get? invoice-escrow-funding { invoice-id: invoice-id })
  )
)

(define-public (deposit-to-escrow (amount uint))
  (let (
    (user-escrow-data (get-user-escrow-balance tx-sender))
    (user-balance-data (get-user-balance tx-sender))
  )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= (get balance user-balance-data) amount) err-insufficient-funds)
    
    (map-set user-balance
      { user: tx-sender }
      { balance: (- (get balance user-balance-data) amount) }
    )
    
    (map-set user-escrow-balance
      { user: tx-sender }
      { balance: (+ (get balance user-escrow-data) amount) }
    )
    
    (ok true)
  )
)

(define-public (withdraw-from-escrow (amount uint))
  (let (
    (user-escrow-data (get-user-escrow-balance tx-sender))
    (user-balance-data (get-user-balance tx-sender))
  )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= (get balance user-escrow-data) amount) err-escrow-insufficient)
    
    (map-set user-escrow-balance
      { user: tx-sender }
      { balance: (- (get balance user-escrow-data) amount) }
    )
    
    (map-set user-balance
      { user: tx-sender }
      { balance: (+ (get balance user-balance-data) amount) }
    )
    
    (ok true)
  )
)

(define-public (fund-invoice-escrow (invoice-id uint))
  (let (
    (invoice-data (unwrap! (get-invoice invoice-id) err-not-found))
    (escrow-status (get-invoice-escrow-status invoice-id))
    (debtor (get debtor invoice-data))
    (invoice-amount (get amount invoice-data))
    (debtor-escrow-data (get-user-escrow-balance debtor))
  )
    (asserts! (is-eq tx-sender debtor) err-unauthorized)
    (asserts! (is-eq (get status invoice-data) u3) err-invoice-not-funded)
    (asserts! (not (get funded escrow-status)) err-escrow-already-funded)
    (asserts! (>= (get balance debtor-escrow-data) invoice-amount) err-escrow-insufficient)
    
    (map-set user-escrow-balance
      { user: debtor }
      { balance: (- (get balance debtor-escrow-data) invoice-amount) }
    )
    
    (map-set invoice-escrow-funding
      { invoice-id: invoice-id }
      {
        funded: true,
        amount: invoice-amount,
        funded-date: stacks-block-height
      }
    )
    
    (ok true)
  )
)

(define-public (execute-escrow-payment (invoice-id uint))
  (let (
    (invoice-data (unwrap! (get-invoice invoice-id) err-not-found))
    (escrow-status (get-invoice-escrow-status invoice-id))
    (due-date (get due-date invoice-data))
    (current-time stacks-block-height)
    (investor-principal (unwrap! (get investor invoice-data) err-not-found))
    (invoice-amount (get amount invoice-data))
    (investor-balance-data (get-user-balance investor-principal))
  )
    (asserts! (is-eq (get status invoice-data) u3) err-invoice-not-funded)
    (asserts! (get funded escrow-status) err-escrow-not-ready)
    (asserts! (>= current-time due-date) err-invoice-not-due)
    
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice-data { status: u4 })
    )
    
    (map-set user-balance
      { user: investor-principal }
      { balance: (+ (get balance investor-balance-data) invoice-amount) }
    )
    
    (update-credit-profile-on-payment invoice-id)
  )
)

(define-read-only (get-escrow-ready-invoices (debtor principal))
  (let (
    (debtor-escrow-balance (get balance (get-user-escrow-balance debtor)))
  )
    (ok debtor-escrow-balance)
  )
)

(define-public (bulk-fund-escrow-invoices (invoice-ids (list 10 uint)))
  (let (
    (results (map fund-invoice-escrow invoice-ids))
  )
    (ok results)
  )
)

(define-read-only (calculate-credit-score (user principal))
  (let (
    (profile (get-user-credit-profile user))
    (total-invoices (get total-invoices profile))
    (paid-on-time (get paid-on-time profile))
    (total-defaults (get total-defaults profile))
  )
    (if (is-eq total-invoices u0)
      (ok default-credit-score)
      (let (
        (payment-ratio (/ (* paid-on-time u100) total-invoices))
        (default-ratio (/ (* total-defaults u100) total-invoices))
        (base-score (+ u300 (* payment-ratio u4)))
        (penalty (* default-ratio u10))
        (final-score (if (> base-score penalty) (- base-score penalty) min-credit-score))
      )
        (ok (if (> final-score max-credit-score) max-credit-score final-score))
      )
    )
  )
)

(define-public (rate-invoice 
    (invoice-id uint)
    (risk-rating uint)
    (investor-rating uint)
    (payment-rating uint))
  (let (
    (invoice-data (unwrap! (get-invoice invoice-id) err-not-found))
    (current-rating (get-invoice-rating invoice-id))
    (current-raters (get rated-by current-rating))
  )
    (asserts! (<= risk-rating u10) err-invalid-rating)
    (asserts! (<= investor-rating u10) err-invalid-rating)
    (asserts! (<= payment-rating u10) err-invalid-rating)
    (asserts! (> risk-rating u0) err-invalid-rating)
    (asserts! (> investor-rating u0) err-invalid-rating)
    (asserts! (> payment-rating u0) err-invalid-rating)
    (asserts! (not (is-some (index-of current-raters tx-sender))) err-already-exists)
    
    (map-set invoice-ratings
      { invoice-id: invoice-id }
      {
        risk-rating: (+ (get risk-rating current-rating) risk-rating),
        investor-rating: (+ (get investor-rating current-rating) investor-rating),
        payment-rating: (+ (get payment-rating current-rating) payment-rating),
        rated-by: (unwrap! (as-max-len? (append current-raters tx-sender) u10) err-invalid-rating)
      }
    )
    
    (ok true)
  )
)

(define-public (update-credit-profile-on-payment (invoice-id uint))
  (let (
    (invoice-data (unwrap! (get-invoice invoice-id) err-not-found))
    (debtor (get debtor invoice-data))
    (issuer (get issuer invoice-data))
    (amount (get amount invoice-data))
    (due-date (get due-date invoice-data))
    (current-time stacks-block-height)
    (debtor-profile (get-user-credit-profile debtor))
    (issuer-profile (get-user-credit-profile issuer))
  )
    (asserts! (is-eq (get status invoice-data) u4) err-invoice-not-funded)
    
    (map-set user-credit-profile
      { user: debtor }
      {
        total-invoices: (+ (get total-invoices debtor-profile) u1),
        paid-on-time: (+ (get paid-on-time debtor-profile) (if (<= current-time due-date) u1 u0)),
        total-defaults: (get total-defaults debtor-profile),
        total-volume: (+ (get total-volume debtor-profile) amount),
        credit-score: (unwrap! (calculate-credit-score debtor) err-invalid-rating),
        last-updated: current-time
      }
    )
    
    (map-set user-credit-profile
      { user: issuer }
      {
        total-invoices: (+ (get total-invoices issuer-profile) u1),
        paid-on-time: (+ (get paid-on-time issuer-profile) u1),
        total-defaults: (get total-defaults issuer-profile),
        total-volume: (+ (get total-volume issuer-profile) amount),
        credit-score: (unwrap! (calculate-credit-score issuer) err-invalid-rating),
        last-updated: current-time
      }
    )
    
    (ok true)
  )
)

(define-public (update-credit-profile-on-default (invoice-id uint))
  (let (
    (invoice-data (unwrap! (get-invoice invoice-id) err-not-found))
    (debtor (get debtor invoice-data))
    (amount (get amount invoice-data))
    (current-time stacks-block-height)
    (debtor-profile (get-user-credit-profile debtor))
  )
    (asserts! (is-eq (get status invoice-data) u5) err-invoice-not-funded)
    
    (map-set user-credit-profile
      { user: debtor }
      {
        total-invoices: (+ (get total-invoices debtor-profile) u1),
        paid-on-time: (get paid-on-time debtor-profile),
        total-defaults: (+ (get total-defaults debtor-profile) u1),
        total-volume: (+ (get total-volume debtor-profile) amount),
        credit-score: (unwrap! (calculate-credit-score debtor) err-invalid-rating),
        last-updated: current-time
      }
    )
    
    (ok true)
  )
)

(define-read-only (get-recommended-discount-rate (debtor principal))
  (let (
    (credit-score (get credit-score (get-user-credit-profile debtor)))
  )
    (if (>= credit-score u750)
      (ok u200)
      (if (>= credit-score u650)
        (ok u400)
        (if (>= credit-score u550)
          (ok u600)
          (ok u800)
        )
      )
    )
  )
)

(define-read-only (get-average-invoice-rating (invoice-id uint))
  (let (
    (rating-data (get-invoice-rating invoice-id))
    (num-raters (len (get rated-by rating-data)))
  )
    (if (is-eq num-raters u0)
      (ok { risk-avg: u0, investor-avg: u0, payment-avg: u0 })
      (ok {
        risk-avg: (/ (get risk-rating rating-data) num-raters),
        investor-avg: (/ (get investor-rating rating-data) num-raters),
        payment-avg: (/ (get payment-rating rating-data) num-raters)
      })
    )
  )
)

(define-public (enhanced-pay-invoice (invoice-id uint))
  (match (pay-invoice invoice-id)
    success (update-credit-profile-on-payment invoice-id)
    error (err error)
  )
)

(define-public (enhanced-mark-invoice-defaulted (invoice-id uint))
  (match (mark-invoice-defaulted invoice-id)
    success (update-credit-profile-on-default invoice-id)
    error (err error)
  )
)

;; Fractional Investment System
;; Allows multiple investors to own shares of a single invoice

(define-constant err-invalid-shares (err u117))
(define-constant err-shares-not-available (err u118))
(define-constant err-share-transfer-failed (err u119))
(define-constant err-insufficient-shares (err u120))
(define-constant err-not-shareholder (err u121))
(define-constant max-shareholders u50)
(define-constant min-share-price u100)

;; Track fractional invoice configuration
(define-map fractional-invoices
  { invoice-id: uint }
  {
    total-shares: uint,
    available-shares: uint,
    share-price: uint,
    min-investment: uint,
    is-fractional: bool,
    shareholders-count: uint
  }
)

;; Track individual share ownership
(define-map invoice-shares
  { invoice-id: uint, investor: principal }
  {
    shares-owned: uint,
    investment-amount: uint,
    purchase-date: uint
  }
)

;; Track all shareholders for an invoice
(define-map invoice-shareholders
  { invoice-id: uint }
  { shareholders: (list 50 principal) }
)

;; Share transfer offers
(define-map share-transfer-offers
  { invoice-id: uint, seller: principal, buyer: principal }
  {
    shares-amount: uint,
    price-per-share: uint,
    expiry-date: uint,
    active: bool
  }
)

;; Read-only functions for fractional system

(define-read-only (get-fractional-invoice-info (invoice-id uint))
  (default-to
    {
      total-shares: u0,
      available-shares: u0,
      share-price: u0,
      min-investment: u0,
      is-fractional: false,
      shareholders-count: u0
    }
    (map-get? fractional-invoices { invoice-id: invoice-id })
  )
)

(define-read-only (get-investor-shares (invoice-id uint) (investor principal))
  (default-to
    {
      shares-owned: u0,
      investment-amount: u0,
      purchase-date: u0
    }
    (map-get? invoice-shares { invoice-id: invoice-id, investor: investor })
  )
)

(define-read-only (get-invoice-shareholders (invoice-id uint))
  (default-to
    { shareholders: (list) }
    (map-get? invoice-shareholders { invoice-id: invoice-id })
  )
)

(define-read-only (get-share-transfer-offer 
    (invoice-id uint) 
    (seller principal) 
    (buyer principal))
  (default-to
    {
      shares-amount: u0,
      price-per-share: u0,
      expiry-date: u0,
      active: false
    }
    (map-get? share-transfer-offers 
      { invoice-id: invoice-id, seller: seller, buyer: buyer })
  )
)

(define-read-only (calculate-share-payout 
    (invoice-id uint) 
    (investor principal))
  (let (
    (invoice-data (unwrap! (get-invoice invoice-id) err-not-found))
    (investor-shares (get-investor-shares invoice-id investor))
    (fractional-info (get-fractional-invoice-info invoice-id))
    (total-amount (get amount invoice-data))
    (investor-share-count (get shares-owned investor-shares))
    (total-shares (get total-shares fractional-info))
  )
    (if (and (> investor-share-count u0) (> total-shares u0))
      (ok (/ (* total-amount investor-share-count) total-shares))
      (ok u0)
    )
  )
)

;; Public functions for fractional investment

(define-public (enable-fractional-funding 
    (invoice-id uint)
    (total-shares uint)
    (min-investment uint))
  (let (
    (invoice-data (unwrap! (get-invoice invoice-id) err-not-found))
    (funding-amount (unwrap! (calculate-funding-amount invoice-id) err-not-found))
    (share-price (/ funding-amount total-shares))
  )
    ;; Verify ownership and status
    (asserts! (is-eq tx-sender (get owner (unwrap! (get-invoice-owner invoice-id) err-not-found))) err-unauthorized)
    (asserts! (is-eq (get status invoice-data) u2) err-invoice-not-for-sale)
    (asserts! (> total-shares u1) err-invalid-shares)
    (asserts! (>= share-price min-share-price) err-invalid-amount)
    (asserts! (>= min-investment share-price) err-invalid-amount)
    
    ;; Enable fractional funding
    (map-set fractional-invoices
      { invoice-id: invoice-id }
      {
        total-shares: total-shares,
        available-shares: total-shares,
        share-price: share-price,
        min-investment: min-investment,
        is-fractional: true,
        shareholders-count: u0
      }
    )
    
    ;; Initialize empty shareholders list
    (map-set invoice-shareholders
      { invoice-id: invoice-id }
      { shareholders: (list) }
    )
    
    (ok true)
  )
)

(define-public (purchase-invoice-shares 
    (invoice-id uint)
    (shares-to-buy uint))
  (let (
    (invoice-data (unwrap! (get-invoice invoice-id) err-not-found))
    (fractional-info (get-fractional-invoice-info invoice-id))
    (user-balance-data (get-user-balance tx-sender))
    (current-shares (get-investor-shares invoice-id tx-sender))
    (purchase-amount (* shares-to-buy (get share-price fractional-info)))
    (current-shareholders (get shareholders (get-invoice-shareholders invoice-id)))
  )
    ;; Verify fractional funding is enabled and shares available
    (asserts! (get is-fractional fractional-info) err-shares-not-available)
    (asserts! (is-eq (get status invoice-data) u2) err-invoice-not-for-sale)
    (asserts! (>= (get available-shares fractional-info) shares-to-buy) err-shares-not-available)
    (asserts! (>= purchase-amount (get min-investment fractional-info)) err-invalid-amount)
    (asserts! (>= (get balance user-balance-data) purchase-amount) err-insufficient-funds)
    
    ;; Update user balance
    (map-set user-balance
      { user: tx-sender }
      { balance: (- (get balance user-balance-data) purchase-amount) }
    )
    
    ;; Update invoice issuer balance
    (map-set user-balance
      { user: (get issuer invoice-data) }
      { balance: (+ (get balance (get-user-balance (get issuer invoice-data))) purchase-amount) }
    )
    
    ;; Update investor shares
    (map-set invoice-shares
      { invoice-id: invoice-id, investor: tx-sender }
      {
        shares-owned: (+ (get shares-owned current-shares) shares-to-buy),
        investment-amount: (+ (get investment-amount current-shares) purchase-amount),
        purchase-date: stacks-block-height
      }
    )
    
    ;; Update fractional invoice info
    (map-set fractional-invoices
      { invoice-id: invoice-id }
      (merge fractional-info 
        {
          available-shares: (- (get available-shares fractional-info) shares-to-buy),
          shareholders-count: (if (is-eq (get shares-owned current-shares) u0)
                                (+ (get shareholders-count fractional-info) u1)
                                (get shareholders-count fractional-info))
        }
      )
    )
    
    ;; Add to shareholders list if new investor
    (if (is-eq (get shares-owned current-shares) u0)
      (map-set invoice-shareholders
        { invoice-id: invoice-id }
        { shareholders: (unwrap! (as-max-len? (append current-shareholders tx-sender) u50) err-invalid-shares) }
      )
      true
    )
    
    ;; Check if invoice is fully funded
    (if (is-eq (get available-shares (get-fractional-invoice-info invoice-id)) u0)
      (map-set invoices
        { invoice-id: invoice-id }
        (merge invoice-data { status: u3 })
      )
      true
    )
    
    (ok true)
  )
)

(define-public (create-share-transfer-offer
    (invoice-id uint)
    (buyer principal)
    (shares-amount uint)
    (price-per-share uint)
    (expiry-blocks uint))
  (let (
    (investor-shares (get-investor-shares invoice-id tx-sender))
    (expiry-date (+ stacks-block-height expiry-blocks))
  )
    ;; Verify share ownership and valid offer
    (asserts! (>= (get shares-owned investor-shares) shares-amount) err-insufficient-shares)
    (asserts! (> shares-amount u0) err-invalid-shares)
    (asserts! (> price-per-share u0) err-invalid-amount)
    (asserts! (> expiry-blocks u0) err-invalid-amount)
    
    ;; Create transfer offer
    (map-set share-transfer-offers
      { invoice-id: invoice-id, seller: tx-sender, buyer: buyer }
      {
        shares-amount: shares-amount,
        price-per-share: price-per-share,
        expiry-date: expiry-date,
        active: true
      }
    )
    
    (ok true)
  )
)

(define-public (accept-share-transfer-offer
    (invoice-id uint)
    (seller principal))
  (let (
    (transfer-offer (get-share-transfer-offer invoice-id seller tx-sender))
    (seller-shares (get-investor-shares invoice-id seller))
    (buyer-shares (get-investor-shares invoice-id tx-sender))
    (buyer-balance-data (get-user-balance tx-sender))
    (seller-balance-data (get-user-balance seller))
    (total-cost (* (get shares-amount transfer-offer) (get price-per-share transfer-offer)))
  )
    ;; Verify offer validity and buyer balance
    (asserts! (get active transfer-offer) err-share-transfer-failed)
    (asserts! (< stacks-block-height (get expiry-date transfer-offer)) err-share-transfer-failed)
    (asserts! (>= (get shares-owned seller-shares) (get shares-amount transfer-offer)) err-insufficient-shares)
    (asserts! (>= (get balance buyer-balance-data) total-cost) err-insufficient-funds)
    
    ;; Transfer payment
    (map-set user-balance
      { user: tx-sender }
      { balance: (- (get balance buyer-balance-data) total-cost) }
    )
    
    (map-set user-balance
      { user: seller }
      { balance: (+ (get balance seller-balance-data) total-cost) }
    )
    
    ;; Transfer shares
    (map-set invoice-shares
      { invoice-id: invoice-id, investor: seller }
      (merge seller-shares 
        { shares-owned: (- (get shares-owned seller-shares) (get shares-amount transfer-offer)) }
      )
    )
    
    (map-set invoice-shares
      { invoice-id: invoice-id, investor: tx-sender }
      {
        shares-owned: (+ (get shares-owned buyer-shares) (get shares-amount transfer-offer)),
        investment-amount: (+ (get investment-amount buyer-shares) total-cost),
        purchase-date: stacks-block-height
      }
    )
    
    ;; Deactivate transfer offer
    (map-set share-transfer-offers
      { invoice-id: invoice-id, seller: seller, buyer: tx-sender }
      (merge transfer-offer { active: false })
    )
    
    (ok true)
  )
)

;; Variable to hold current invoice id for distribution
(define-data-var current-distribution-invoice-id uint u0)

(define-public (distribute-fractional-payment (invoice-id uint))
  (let (
    (invoice-data (unwrap! (get-invoice invoice-id) err-not-found))
    (fractional-info (get-fractional-invoice-info invoice-id))
    (shareholders-data (get-invoice-shareholders invoice-id))
  )
    ;; Verify invoice is paid and fractional
    (asserts! (is-eq (get status invoice-data) u4) err-invoice-not-funded)
    (asserts! (get is-fractional fractional-info) err-shares-not-available)
    
    ;; Set current invoice id for distribution
    (var-set current-distribution-invoice-id invoice-id)
    
    ;; Distribute payments to all shareholders
    (ok (map distribute-to-shareholder-wrapper (get shareholders shareholders-data)))
  )
)

(define-private (distribute-to-shareholder-wrapper (shareholder principal))
  (distribute-to-shareholder shareholder (var-get current-distribution-invoice-id))
)

(define-private (distribute-to-shareholder (shareholder principal) (invoice-id uint))
  (match (calculate-share-payout invoice-id shareholder)
    ok-value 
    (let (
      (shareholder-balance-data (get-user-balance shareholder))
    )
      (if (> ok-value u0)
        (begin
          (map-set user-balance
            { user: shareholder }
            { balance: (+ (get balance shareholder-balance-data) ok-value) }
          )
          true
        )
        true
      )
    )
    err-value false
  )
)

