(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-guardians (err u103))
(define-constant err-invalid-threshold (err u104))
(define-constant err-recovery-in-progress (err u105))
(define-constant err-no-recovery-request (err u106))
(define-constant err-already-approved (err u107))
(define-constant err-not-guardian (err u108))
(define-constant err-recovery-expired (err u109))
(define-constant err-insufficient-approvals (err u110))

(define-data-var recovery-delay uint u144)

(define-map wallets 
    principal 
    {
        guardians: (list 10 principal),
        threshold: uint,
        owner: principal
    }
)

(define-map recovery-requests
    principal
    {
        new-owner: principal,
        approvals: (list 10 principal),
        created-at: uint,
        expires-at: uint
    }
)

(define-map guardian-approvals
    {wallet: principal, guardian: principal}
    bool
)

(define-public (setup-recovery (guardians (list 10 principal)) (threshold uint))
    (let 
        (
            (wallet-principal tx-sender)
            (guardian-count (len guardians))
        )
        (asserts! (>= guardian-count threshold) err-invalid-threshold)
        (asserts! (> threshold u0) err-invalid-threshold)
        (asserts! (<= threshold u10) err-invalid-threshold)
        (asserts! (is-none (map-get? wallets wallet-principal)) err-already-exists)
        
        (map-set wallets wallet-principal
            {
                guardians: guardians,
                threshold: threshold,
                owner: wallet-principal
            }
        )
        (ok true)
    )
)

(define-public (add-guardian (wallet principal) (new-guardian principal))
    (let 
        (
            (wallet-data (unwrap! (map-get? wallets wallet) err-not-found))
            (current-guardians (get guardians wallet-data))
        )
        (asserts! (is-eq (get owner wallet-data) tx-sender) err-owner-only)
        (asserts! (< (len current-guardians) u10) err-already-exists)
        (asserts! (is-none (index-of current-guardians new-guardian)) err-already-exists)
        
        (map-set wallets wallet
            (merge wallet-data 
                {guardians: (unwrap! (as-max-len? (append current-guardians new-guardian) u10) err-already-exists)}
            )
        )
        (ok true)
    )
)

(define-public (remove-guardian (wallet principal) (guardian-to-remove principal))
    (let 
        (
            (wallet-data (unwrap! (map-get? wallets wallet) err-not-found))
            (current-guardians (get guardians wallet-data))
            (new-guardians (filter is-not-target current-guardians))
        )
        (asserts! (is-eq (get owner wallet-data) tx-sender) err-owner-only)
        (asserts! (is-some (index-of current-guardians guardian-to-remove)) err-not-found)
        
        (var-set target-guardian guardian-to-remove)
        (map-set wallets wallet
            (merge wallet-data {guardians: new-guardians})
        )
        (ok true)
    )
)

(define-data-var target-guardian principal 'SP000000000000000000002Q6VF78)

(define-private (is-not-target (guardian principal))
    (not (is-eq guardian (var-get target-guardian)))
)

(define-public (request-recovery (wallet principal) (new-owner principal))
    (let 
        (
            (wallet-data (unwrap! (map-get? wallets wallet) err-not-found))
            (current-block stacks-block-height)
            (expires-at (+ current-block (var-get recovery-delay)))
        )
        (asserts! (is-none (map-get? recovery-requests wallet)) err-recovery-in-progress)
        
        (map-set recovery-requests wallet
            {
                new-owner: new-owner,
                approvals: (list),
                created-at: current-block,
                expires-at: expires-at
            }
        )
        (ok true)
    )
)

(define-public (approve-recovery (wallet principal))
    (let 
        (
            (wallet-data (unwrap! (map-get? wallets wallet) err-not-found))
            (recovery-data (unwrap! (map-get? recovery-requests wallet) err-no-recovery-request))
            (guardian tx-sender)
            (current-block stacks-block-height)
        )
        (asserts! (< current-block (get expires-at recovery-data)) err-recovery-expired)
        (asserts! (is-some (index-of (get guardians wallet-data) guardian)) err-not-guardian)
        (asserts! (is-none (map-get? guardian-approvals {wallet: wallet, guardian: guardian})) err-already-approved)
        
        (map-set guardian-approvals {wallet: wallet, guardian: guardian} true)
        (map-set recovery-requests wallet
            (merge recovery-data 
                {approvals: (unwrap! (as-max-len? (append (get approvals recovery-data) guardian) u10) err-already-approved)}
            )
        )
        (ok true)
    )
)

(define-public (execute-recovery (wallet principal))
    (let 
        (
            (wallet-data (unwrap! (map-get? wallets wallet) err-not-found))
            (recovery-data (unwrap! (map-get? recovery-requests wallet) err-no-recovery-request))
            (current-block stacks-block-height)
            (approval-count (len (get approvals recovery-data)))
        )
        (asserts! (< current-block (get expires-at recovery-data)) err-recovery-expired)
        (asserts! (>= approval-count (get threshold wallet-data)) err-insufficient-approvals)
        
        (map-set wallets wallet
            (merge wallet-data {owner: (get new-owner recovery-data)})
        )
        (map-delete recovery-requests wallet)
        (ok true)
    )
)

(define-public (cancel-recovery (wallet principal))
    (let 
        (
            (wallet-data (unwrap! (map-get? wallets wallet) err-not-found))
            (recovery-data (unwrap! (map-get? recovery-requests wallet) err-no-recovery-request))
        )
        (asserts! (is-eq (get owner wallet-data) tx-sender) err-owner-only)
        
        (map-delete recovery-requests wallet)
        (ok true)
    )
)

(define-read-only (get-wallet-info (wallet principal))
    (map-get? wallets wallet)
)

(define-read-only (get-recovery-info (wallet principal))
    (map-get? recovery-requests wallet)
)

(define-read-only (get-recovery-status (wallet principal))
    (match (map-get? recovery-requests wallet)
        recovery-data 
        (let 
            (
                (wallet-data (unwrap! (map-get? wallets wallet) err-not-found))
                (approval-count (len (get approvals recovery-data)))
                (required-approvals (get threshold wallet-data))
                (current-block stacks-block-height)
                (is-expired (>= current-block (get expires-at recovery-data)))
            )
            (ok {
                new-owner: (get new-owner recovery-data),
                approvals: approval-count,
                required: required-approvals,
                can-execute: (and (>= approval-count required-approvals) (not is-expired)),
                expired: is-expired
            })
        )
        err-no-recovery-request
    )
)

(define-read-only (has-guardian-approved (wallet principal) (guardian principal))
    (default-to false (map-get? guardian-approvals {wallet: wallet, guardian: guardian}))
)

(define-public (set-recovery-delay (new-delay uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set recovery-delay new-delay)
        (ok true)
    )
)

(define-map guardian-stats
    {wallet: principal, guardian: principal}
    {
        total-requests: uint,
        total-approvals: uint,
        total-response-time: uint,
        last-activity: uint,
        reliability-score: uint
    }
)

(define-map guardian-activity-log
    {wallet: principal, guardian: principal, request-id: uint}
    {
        request-created: uint,
        approval-time: uint,
        response-time: uint
    }
)

(define-data-var request-counter uint u0)

(define-private (update-guardian-stats (wallet principal) (guardian principal) (response-time uint))
    (let 
        (
            (current-stats (default-to 
                {total-requests: u0, total-approvals: u0, total-response-time: u0, last-activity: u0, reliability-score: u100}
                (map-get? guardian-stats {wallet: wallet, guardian: guardian})
            ))
            (new-total-requests (+ (get total-requests current-stats) u1))
            (new-total-approvals (+ (get total-approvals current-stats) u1))
            (new-total-response-time (+ (get total-response-time current-stats) response-time))
            (new-reliability-score (/ (* (get total-approvals current-stats) u100) new-total-requests))
        )
        (map-set guardian-stats {wallet: wallet, guardian: guardian}
            {
                total-requests: new-total-requests,
                total-approvals: new-total-approvals,
                total-response-time: new-total-response-time,
                last-activity: stacks-block-height,
                reliability-score: new-reliability-score
            }
        )
    )
)

(define-private (record-activity-log (wallet principal) (guardian principal) (request-created uint))
    (let 
        (
            (request-id (var-get request-counter))
            (approval-time stacks-block-height)
            (response-time (- approval-time request-created))
        )
        (map-set guardian-activity-log 
            {wallet: wallet, guardian: guardian, request-id: request-id}
            {
                request-created: request-created,
                approval-time: approval-time,
                response-time: response-time
            }
        )
        (var-set request-counter (+ request-id u1))
        response-time
    )
)

(define-read-only (get-guardian-stats (wallet principal) (guardian principal))
    (map-get? guardian-stats {wallet: wallet, guardian: guardian})
)

(define-read-only (get-guardian-reliability-ranking (wallet principal))
    (let 
        (
            (wallet-data (unwrap! (map-get? wallets wallet) err-not-found))
            (guardians (get guardians wallet-data))
        )
        (ok (map get-reliability-for-guardian guardians))
    )
)

(define-private (get-reliability-for-guardian (guardian principal))
    (get reliability-score
        (default-to 
            {total-requests: u0, total-approvals: u0, total-response-time: u0, last-activity: u0, reliability-score: u100}
            (map-get? guardian-stats {wallet: tx-sender, guardian: guardian})
        )
    )
)