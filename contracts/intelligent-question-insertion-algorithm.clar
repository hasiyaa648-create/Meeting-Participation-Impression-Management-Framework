;; Intelligent Question Insertion Algorithm
;; A sophisticated contract that analyzes meeting context and generates contextually relevant questions
;; at precisely the moment when attention starts to wane.

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_INVALID_MEETING (err u101))
(define-constant ERR_QUESTION_NOT_FOUND (err u102))
(define-constant ERR_ALREADY_USED (err u103))
(define-constant ERR_MEETING_NOT_ACTIVE (err u104))
(define-constant ERR_INSUFFICIENT_CONTEXT (err u105))
(define-constant ERR_INVALID_TIMING (err u106))

;; Maximum values for validation
(define-constant MAX_QUESTIONS_PER_MEETING u50)
(define-constant MIN_ATTENTION_THRESHOLD u20)
(define-constant MAX_ATTENTION_SCORE u100)
(define-constant OPTIMAL_QUESTION_INTERVAL u300) ;; 5 minutes in seconds
(define-constant CONTEXT_RELEVANCE_THRESHOLD u70)

;; Data Variables
(define-data-var meeting-counter uint u0)
(define-data-var global-question-counter uint u0)
(define-data-var algorithm-version uint u1)
(define-data-var maintenance-mode bool false)

;; Meeting Session Data Structure
(define-map meetings uint {
    host: principal,
    topic: (string-ascii 200),
    participants: uint,
    start-time: uint,
    end-time: (optional uint),
    status: (string-ascii 20),
    attention-baseline: uint,
    context-keywords: (list 10 (string-ascii 50))
})

;; Question Database with Context Matching
(define-map question-library uint {
    content: (string-ascii 500),
    category: (string-ascii 50),
    relevance-keywords: (list 15 (string-ascii 30)),
    effectiveness-score: uint,
    usage-count: uint,
    created-at: uint,
    created-by: principal
})

;; Attention Monitoring Data
(define-map attention-metrics uint {
    meeting-id: uint,
    timestamp: uint,
    attention-score: uint,
    participant-engagement: uint,
    context-drift: uint,
    optimal-intervention: bool
})

;; Question Usage Tracking
(define-map question-insertions uint {
    meeting-id: uint,
    question-id: uint,
    insertion-time: uint,
    attention-before: uint,
    attention-after: uint,
    effectiveness-rating: (optional uint),
    context-match: uint
})

;; Meeting Participation Scoring
(define-map participation-scores principal {
    total-meetings: uint,
    questions-asked: uint,
    average-effectiveness: uint,
    engagement-rating: uint,
    last-updated: uint
})

;; Context Analysis Cache
(define-map context-analysis uint {
    meeting-id: uint,
    analyzed-at: uint,
    topic-keywords: (list 20 (string-ascii 30)),
    sentiment-score: uint,
    complexity-level: uint,
    recommended-questions: (list 5 uint)
})

;; Public Functions

;; Initialize a new meeting session
(define-public (start-meeting-session (topic (string-ascii 200)) (participants uint) (keywords (list 10 (string-ascii 50))))
    (let (
        (meeting-id (+ (var-get meeting-counter) u1))
        (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
        (asserts! (not (var-get maintenance-mode)) ERR_NOT_AUTHORIZED)
        (asserts! (> participants u0) ERR_INVALID_MEETING)
        (asserts! (> (len topic) u0) ERR_INVALID_MEETING)
        
        (map-set meetings meeting-id {
            host: tx-sender,
            topic: topic,
            participants: participants,
            start-time: current-time,
            end-time: none,
            status: "active",
            attention-baseline: u75, ;; Default baseline attention
            context-keywords: keywords
        })
        
        (var-set meeting-counter meeting-id)
        (ok meeting-id)
    )
)

;; Add a new question to the library
(define-public (add-question (content (string-ascii 500)) (category (string-ascii 50)) (keywords (list 15 (string-ascii 30))))
    (let (
        (question-id (+ (var-get global-question-counter) u1))
        (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
        (asserts! (> (len content) u0) ERR_INSUFFICIENT_CONTEXT)
        (asserts! (> (len category) u0) ERR_INSUFFICIENT_CONTEXT)
        
        (map-set question-library question-id {
            content: content,
            category: category,
            relevance-keywords: keywords,
            effectiveness-score: u50, ;; Default starting score
            usage-count: u0,
            created-at: current-time,
            created-by: tx-sender
        })
        
        (var-set global-question-counter question-id)
        (ok question-id)
    )
)

;; Record attention metrics for analysis
(define-public (record-attention-metric (meeting-id uint) (attention-score uint) (engagement uint) (context-drift uint))
    (let (
        (meeting-info (unwrap! (map-get? meetings meeting-id) ERR_INVALID_MEETING))
        (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
        (metric-id (+ meeting-id (* current-time u1000))) ;; Unique metric ID
        (optimal-intervention (< attention-score MIN_ATTENTION_THRESHOLD))
    )
        (asserts! (is-eq (get status meeting-info) "active") ERR_MEETING_NOT_ACTIVE)
        (asserts! (<= attention-score MAX_ATTENTION_SCORE) ERR_INVALID_TIMING)
        
        (map-set attention-metrics metric-id {
            meeting-id: meeting-id,
            timestamp: current-time,
            attention-score: attention-score,
            participant-engagement: engagement,
            context-drift: context-drift,
            optimal-intervention: optimal-intervention
        })
        
        (if optimal-intervention
            (trigger-question-recommendation meeting-id attention-score)
            (ok true)
        )
    )
)

;; Insert a question during meeting
(define-public (insert-question (meeting-id uint) (question-id uint) (attention-before uint))
    (let (
        (meeting-info (unwrap! (map-get? meetings meeting-id) ERR_INVALID_MEETING))
        (question-info (unwrap! (map-get? question-library question-id) ERR_QUESTION_NOT_FOUND))
        (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
        (insertion-id (+ (* meeting-id u10000) question-id))
        (context-match (calculate-context-match meeting-id question-id))
    )
        (asserts! (is-eq (get status meeting-info) "active") ERR_MEETING_NOT_ACTIVE)
        (asserts! (>= context-match CONTEXT_RELEVANCE_THRESHOLD) ERR_INSUFFICIENT_CONTEXT)
        (asserts! (is-none (map-get? question-insertions insertion-id)) ERR_ALREADY_USED)
        
        ;; Record the question insertion
        (map-set question-insertions insertion-id {
            meeting-id: meeting-id,
            question-id: question-id,
            insertion-time: current-time,
            attention-before: attention-before,
            attention-after: u0, ;; To be updated later
            effectiveness-rating: none,
            context-match: context-match
        })
        
        ;; Update question usage statistics
        (map-set question-library question-id 
            (merge question-info { usage-count: (+ (get usage-count question-info) u1) })
        )
        
        ;; Update user participation score
        (update-participation-score tx-sender)
        
        (ok insertion-id)
    )
)

;; End meeting session
(define-public (end-meeting-session (meeting-id uint))
    (let (
        (meeting-info (unwrap! (map-get? meetings meeting-id) ERR_INVALID_MEETING))
        (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
        (asserts! (is-eq tx-sender (get host meeting-info)) ERR_NOT_AUTHORIZED)
        (asserts! (is-eq (get status meeting-info) "active") ERR_MEETING_NOT_ACTIVE)
        
        (map-set meetings meeting-id 
            (merge meeting-info { 
                end-time: (some current-time),
                status: "completed"
            })
        )
        
        (ok meeting-id)
    )
)

;; Rate question effectiveness after insertion
(define-public (rate-question-effectiveness (insertion-id uint) (effectiveness-rating uint) (attention-after uint))
    (let (
        (insertion-info (unwrap! (map-get? question-insertions insertion-id) ERR_QUESTION_NOT_FOUND))
        (question-id (get question-id insertion-info))
        (question-info (unwrap! (map-get? question-library question-id) ERR_QUESTION_NOT_FOUND))
    )
        (asserts! (<= effectiveness-rating u100) ERR_INVALID_TIMING)
        
        ;; Update insertion record
        (map-set question-insertions insertion-id 
            (merge insertion-info {
                attention-after: attention-after,
                effectiveness-rating: (some effectiveness-rating)
            })
        )
        
        ;; Update question effectiveness score
        (let ((new-score (/ (+ (get effectiveness-score question-info) effectiveness-rating) u2)))
            (map-set question-library question-id 
                (merge question-info { effectiveness-score: new-score })
            )
        )
        
        (ok true)
    )
)

;; Read-only Functions

;; Get meeting information
(define-read-only (get-meeting-info (meeting-id uint))
    (map-get? meetings meeting-id)
)

;; Get question from library
(define-read-only (get-question-info (question-id uint))
    (map-get? question-library question-id)
)

;; Get recommended questions for current context
(define-read-only (get-recommended-questions (meeting-id uint) (current-attention uint))
    (let (
        (meeting-info (unwrap! (map-get? meetings meeting-id) (list)))
        (meeting-keywords (get context-keywords meeting-info))
    )
        (if (< current-attention MIN_ATTENTION_THRESHOLD)
            (filter-questions-by-context meeting-keywords)
            (list)
        )
    )
)

;; Get attention analytics for meeting
(define-read-only (get-attention-analytics (meeting-id uint))
    (let (
        (meeting-info (unwrap! (map-get? meetings meeting-id) none))
    )
        (some {
            meeting-id: meeting-id,
            baseline-attention: (get attention-baseline meeting-info),
            current-status: (get status meeting-info),
            total-participants: (get participants meeting-info)
        })
    )
)

;; Get user participation statistics
(define-read-only (get-participation-stats (user principal))
    (default-to 
        { total-meetings: u0, questions-asked: u0, average-effectiveness: u0, engagement-rating: u0, last-updated: u0 }
        (map-get? participation-scores user)
    )
)

;; Check if optimal time for question insertion
(define-read-only (is-optimal-insertion-time (meeting-id uint) (current-attention uint))
    (let (
        (meeting-info (unwrap! (map-get? meetings meeting-id) false))
        (baseline (get attention-baseline meeting-info))
        (attention-drop (- baseline current-attention))
    )
        (and 
            (is-eq (get status meeting-info) "active")
            (>= attention-drop u15) ;; Significant attention drop
            (<= current-attention MIN_ATTENTION_THRESHOLD)
        )
    )
)

;; Private Functions

;; Calculate context match between meeting and question
(define-private (calculate-context-match (meeting-id uint) (question-id uint))
    (let (
        (meeting-info (unwrap! (map-get? meetings meeting-id) u0))
        (question-info (unwrap! (map-get? question-library question-id) u0))
        (meeting-keywords (get context-keywords meeting-info))
        (question-keywords (get relevance-keywords question-info))
    )
        ;; Simplified context matching - in real implementation would be more sophisticated
        (if (> (len meeting-keywords) u0)
            (+ u75 (len (filter is-keyword-match meeting-keywords))) ;; Base score + keyword matches
            u50 ;; Default moderate match
        )
    )
)

;; Check if keyword matches (simplified)
(define-private (is-keyword-match (keyword (string-ascii 50)))
    (> (len keyword) u0) ;; Simplified - would implement actual matching logic
)

;; Filter questions by context relevance
(define-private (filter-questions-by-context (keywords (list 10 (string-ascii 50))))
    ;; Simplified implementation - would return list of relevant question IDs
    (list u1 u2 u3) ;; Placeholder
)

;; Trigger question recommendation based on attention drop
(define-private (trigger-question-recommendation (meeting-id uint) (attention-score uint))
    (let (
        (recommended-questions (get-recommended-questions meeting-id attention-score))
    )
        ;; In a real implementation, this would trigger notifications or UI updates
        (ok true)
    )
)

;; Update user participation score
(define-private (update-participation-score (user principal))
    (let (
        (current-stats (get-participation-stats user))
        (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
        (map-set participation-scores user {
            total-meetings: (get total-meetings current-stats),
            questions-asked: (+ (get questions-asked current-stats) u1),
            average-effectiveness: (get average-effectiveness current-stats), ;; Would be recalculated
            engagement-rating: (if (<= (+ (get engagement-rating current-stats) u1) u100) 
                                  (+ (get engagement-rating current-stats) u1) 
                                  u100),
            last-updated: current-time
        })
    )
)
