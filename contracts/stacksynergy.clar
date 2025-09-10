;; ================================================================
;;  stack-synergy
;;  A mega decentralized cooperative economy contract
;;  - Freelance job board with milestone payments
;;  - Milestone-based crowdfunding
;;  - Collective insurance pools
;;  - DAO governance + treasury
;;  - Staking + rewards
;;  - AI scoring (mocked)
;;  - Reputation + NFT badges
;;  - Prediction markets
;; ================================================================

(define-data-var next-job-id uint u1)
(define-data-var next-campaign-id uint u1)
(define-data-var next-pool-id uint u1)
(define-data-var next-claim-id uint u1)
(define-data-var next-proposal-id uint u1)
(define-data-var next-market-id uint u1)
(define-data-var next-badge-id uint u1)

;; ---------------------------------------------
;; FREELANCE JOB BOARD
;; ---------------------------------------------

(define-map jobs
  { id: uint }
  { client: principal, budget: uint, milestones: uint, assigned: (optional principal) })

(define-map job-milestones
  { job-id: uint, milestone-id: uint }
  { submitted: bool, approved: bool })

(define-public (create-job (budget uint) (milestones uint))
  (begin
    (map-set jobs { id: (var-get next-job-id) }
                 { client: tx-sender, budget: budget, milestones: milestones, assigned: none })
    (var-set next-job-id (+ (var-get next-job-id) u1))
    (ok "job-created")))

(define-public (assign-freelancer (job-id uint) (freelancer principal))
  (begin
    (map-set jobs { id: job-id }
      (merge (unwrap-panic (map-get? jobs { id: job-id }))
             { assigned: (some freelancer) }))
    (ok "freelancer-assigned")))

(define-public (submit-milestone (job-id uint) (milestone-id uint) (proof (string-utf8 200)))
  (begin
    (map-set job-milestones { job-id: job-id, milestone-id: milestone-id }
             { submitted: true, approved: false })
    (ok "milestone-submitted")))

(define-public (approve-milestone (job-id uint) (milestone-id uint))
  (begin
    (map-set job-milestones { job-id: job-id, milestone-id: milestone-id }
             { submitted: true, approved: true })
    (ok "milestone-approved")))

;; ---------------------------------------------
;; AI SCORING + REPUTATION
;; ---------------------------------------------

(define-map reputation
  { user: principal }
  { score: int })

(define-public (update-reputation (user principal) (delta int))
  (begin
    (map-set reputation { user: user }
             { score: (+ delta (default-to 0 (get score (map-get? reputation { user: user })))) })
    (ok "reputation-updated")))

(define-public (ai-grade-milestone (job-id uint) (milestone-id uint))
  (ok u80)) ;; mocked AI score out of 100

;; ---------------------------------------------
;; CROWDFUNDING
;; ---------------------------------------------

(define-map campaigns
  { id: uint }
  { owner: principal, goal: uint, raised: uint })

(define-public (create-campaign (goal uint))
  (begin
    (map-set campaigns { id: (var-get next-campaign-id) }
             { owner: tx-sender, goal: goal, raised: u0 })
    (var-set next-campaign-id (+ (var-get next-campaign-id) u1))
    (ok "campaign-created")))

(define-public (fund-campaign (campaign-id uint) (amount uint))
  (begin
    (map-set campaigns { id: campaign-id }
      (merge (unwrap-panic (map-get? campaigns { id: campaign-id }))
             { raised: (+ amount (get raised (unwrap-panic (map-get? campaigns { id: campaign-id })))) }))
    (ok "campaign-funded")))

;; ---------------------------------------------
;; INSURANCE POOLS
;; ---------------------------------------------

(define-map insurance-pools
  { id: uint }
  { creator: principal, premium: uint })

(define-map claims
  { id: uint }
  { pool-id: uint, claimant: principal, amount: uint, approved: bool })

(define-public (create-pool (premium uint))
  (begin
    (map-set insurance-pools { id: (var-get next-pool-id) }
             { creator: tx-sender, premium: premium })
    (var-set next-pool-id (+ (var-get next-pool-id) u1))
    (ok "pool-created")))

(define-public (submit-claim (pool-id uint) (amount uint))
  (begin
    (map-set claims { id: (var-get next-claim-id) }
             { pool-id: pool-id, claimant: tx-sender, amount: amount, approved: false })
    (var-set next-claim-id (+ (var-get next-claim-id) u1))
    (ok "claim-submitted")))

;; ---------------------------------------------
;; DAO GOVERNANCE + TREASURY
;; ---------------------------------------------

(define-map proposals
  { id: uint }
  { creator: principal, description: (string-utf8 200), votes-for: uint, votes-against: uint, executed: bool })

(define-data-var treasury uint u0)

(define-public (create-proposal (desc (string-utf8 200)))
  (begin
    (map-set proposals { id: (var-get next-proposal-id) }
             { creator: tx-sender, description: desc, votes-for: u0, votes-against: u0, executed: false })
    (var-set next-proposal-id (+ (var-get next-proposal-id) u1))
    (ok "proposal-created")))

(define-map votes 
  { proposal-id: uint, voter: principal }
  { voted: bool })

(define-map voter-registry 
  { proposal-id: uint, voter: principal }
  { has-voted: bool })

(define-read-only (can-vote (proposal-id uint) (voter principal))
  (let ((voter-data (map-get? voter-registry { proposal-id: proposal-id, voter: voter })))
    (and 
      (is-some (map-get? proposals { id: proposal-id }))
      (is-none voter-data))))

(define-public (vote-proposal (proposal-id uint) (support bool))
  (let 
    ((proposal-data (unwrap! (map-get? proposals { id: proposal-id }) (err u404))))
    (asserts! (< proposal-id (var-get next-proposal-id)) (err u1))
    (asserts! (not (get executed proposal-data)) (err u403))
    (asserts! (can-vote proposal-id tx-sender) (err u401))
    (begin
      (map-set voter-registry 
        { proposal-id: proposal-id, voter: tx-sender } 
        { has-voted: true })
      (map-set proposals { id: proposal-id }
        (if support
          (merge proposal-data { votes-for: (+ (get votes-for proposal-data) u1) })
          (merge proposal-data { votes-against: (+ (get votes-against proposal-data) u1) })))
      (ok true))))

(define-public (fund-treasury (amount uint))
  (begin
    (var-set treasury (+ (var-get treasury) amount))
    (ok "treasury-funded")))

(define-public (spend-treasury (amount uint) (recipient principal))
  (begin
    (var-set treasury (- (var-get treasury) amount))
    (ok "treasury-spent")))

;; ---------------------------------------------
;; STAKING
;; ---------------------------------------------

(define-map stakes
  { staker: principal }
  { amount: uint })

(define-public (stake (amount uint))
  (begin
    (map-set stakes { staker: tx-sender }
             { amount: (+ amount (default-to u0 (get amount (map-get? stakes { staker: tx-sender })))) })
    (ok "staked")))

(define-public (unstake (amount uint))
  (begin
    (map-set stakes { staker: tx-sender }
             { amount: (- (default-to u0 (get amount (map-get? stakes { staker: tx-sender }))) amount) })
    (ok "unstaked")))

;; ---------------------------------------------
;; PREDICTION MARKETS
;; ---------------------------------------------

(define-map markets
  { id: uint }
  { description: (string-utf8 200), resolved: bool, winning-option: (optional uint) })

(define-public (create-market (desc (string-utf8 200)))
  (begin
    (map-set markets { id: (var-get next-market-id) }
             { description: desc, resolved: false, winning-option: none })
    (var-set next-market-id (+ (var-get next-market-id) u1))
    (ok "market-created")))

(define-public (resolve-market (market-id uint) (winning uint))
  (begin
    (map-set markets { id: market-id }
             { description: (get description (unwrap-panic (map-get? markets { id: market-id }))),
               resolved: true,
               winning-option: (some winning) })
    (ok "market-resolved")))

;; ---------------------------------------------
;; NFT BADGES
;; ---------------------------------------------

(define-map badges
  { id: uint }
  { owner: principal, badge-type: (string-utf8 50) })

(define-public (mint-badge (user principal) (badge-type (string-utf8 50)))
  (begin
    (map-set badges { id: (var-get next-badge-id) }
             { owner: user, badge-type: badge-type })
    (var-set next-badge-id (+ (var-get next-badge-id) u1))
    (ok "badge-minted")))

(define-public (revoke-badge (badge-id uint))
  (begin
    (map-delete badges { id: badge-id })
    (ok "badge-revoked")))
