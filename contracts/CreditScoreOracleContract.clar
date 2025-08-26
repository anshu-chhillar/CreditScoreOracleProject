;; CreditScore Oracle Contract
;; On-chain credit scoring system using DeFi transaction history
;; to determine lending eligibility and rates

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-unauthorized (err u101))
(define-constant err-invalid-score (err u102))
(define-constant err-user-not-found (err u103))

;; Credit score data structure
(define-map user-credit-data 
  principal 
  {
    credit-score: uint,
    total-transactions: uint,
    total-volume: uint,
    last-updated: uint,
    risk-level: (string-ascii 10)
  })

;; Approved data providers (oracles that can update credit scores)
(define-map approved-providers principal bool)

;; Initialize contract owner as approved provider
(map-set approved-providers contract-owner true)

;; Function 1: Update Credit Score
;; Only approved providers can update user credit scores based on DeFi history
(define-public (update-credit-score 
  (user principal) 
  (transaction-count uint) 
  (total-volume uint) 
  (repayment-history uint))
  (let 
    (
      (calculated-score (calculate-score transaction-count total-volume repayment-history))
      (risk-category (get-risk-category calculated-score))
    )
    (begin
      ;; Only approved providers can update scores
      (asserts! (default-to false (map-get? approved-providers tx-sender)) err-unauthorized)
      
      ;; Validate score range (0-850, similar to traditional credit scores)
      (asserts! (<= calculated-score u850) err-invalid-score)
      
      ;; Update user credit data
      (map-set user-credit-data user {
        credit-score: calculated-score,
        total-transactions: transaction-count,
        total-volume: total-volume,
        last-updated: stacks-block-height,
        risk-level: risk-category
      })
      
      (ok calculated-score)
    )
  )
)

;; Function 2: Get Credit Assessment
;; Returns comprehensive credit assessment for lending decisions
(define-read-only (get-credit-assessment (user principal))
  (let 
    (
      (user-data (map-get? user-credit-data user))
    )
    (match user-data
      data 
      (let 
        (
          (score (get credit-score data))
          (suggested-rate (get-lending-rate score))
          (max-loan-amount (get-max-loan-amount score (get total-volume data)))
        )
        (ok {
          user: user,
          credit-score: score,
          risk-level: (get risk-level data),
          lending-eligible: (>= score u300),
          suggested-interest-rate: suggested-rate,
          max-loan-amount: max-loan-amount,
          total-transactions: (get total-transactions data),
          total-volume: (get total-volume data),
          last-updated: (get last-updated data)
        })
      )
      err-user-not-found
    )
  )
)

;; Helper function: Calculate credit score based on DeFi metrics
(define-private (calculate-score (tx-count uint) (volume uint) (repayment uint))
  (let 
    (
      ;; Base score calculation (simplified algorithm)
      (tx-score (* tx-count u2))           ;; 2 points per transaction
      (volume-score (/ volume u1000000))   ;; Volume in microSTX converted to score
      (repayment-score (* repayment u5))   ;; 5 points per successful repayment
      (base-score (+ (+ tx-score volume-score) repayment-score))
    )
    ;; Cap at maximum score of 850
    (if (> base-score u850) u850 base-score)
  )
)

;; Helper function: Determine risk category
(define-private (get-risk-category (score uint))
  (if (>= score u700)
    "LOW"
    (if (>= score u500)
      "MEDIUM" 
      "HIGH")
  )
)

;; Helper function: Calculate suggested lending rate based on credit score
(define-private (get-lending-rate (score uint))
  (if (>= score u700)
    u500    ;; 5.00% APR for high credit scores
    (if (>= score u500)
      u1200   ;; 12.00% APR for medium credit scores
      u2500)  ;; 25.00% APR for low credit scores (high risk)
  )
)

;; Helper function: Calculate maximum loan amount
(define-private (get-max-loan-amount (score uint) (volume uint))
  (let 
    (
      ;; Base loan amount is 10% of historical volume
      (base-amount (/ volume u10))
      ;; Multiply by score factor
      (score-multiplier (if (>= score u700) u3 
                       (if (>= score u500) u2 u1)))
    )
    (* base-amount score-multiplier)
  )
)

;; Admin function: Add approved provider
(define-public (add-approved-provider (provider principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set approved-providers provider true)
    (ok true)
  )
)

;; Read-only function: Check if provider is approved
(define-read-only (is-approved-provider (provider principal))
  (ok (default-to false (map-get? approved-providers provider)))
)