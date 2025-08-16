(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-not-authorized (err u103))
(define-constant err-wrong-price (err u104))
(define-constant err-group-not-found (err u105))
(define-constant err-already-member (err u106))
(define-constant err-not-member (err u107))
(define-constant err-group-full (err u108))
(define-constant err-not-enrolled (err u109))
(define-constant err-invalid-rating (err u110))
(define-constant err-not-completed (err u111))
(define-constant err-already-certified (err u112))
(define-constant err-invalid-grade (err u113))
(define-constant err-certificate-not-found (err u114))
(define-constant err-certificate-revoked (err u115))

(define-non-fungible-token skillhub-course uint)
(define-non-fungible-token skillhub-enrollment uint)
(define-non-fungible-token skillhub-certificate uint)

(define-data-var next-course-id uint u1)
(define-data-var next-enrollment-id uint u1)
(define-data-var next-group-id uint u1)
(define-data-var next-discussion-id uint u1)
(define-data-var next-resource-id uint u1)
(define-data-var next-certificate-id uint u1)

(define-map courses
    uint 
    {
        creator: principal,
        title: (string-ascii 50),
        description: (string-ascii 500),
        price: uint,
        content-url: (string-ascii 255),
        active: bool
    }
)

(define-map enrollments
    uint
    {
        student: principal,
        course-id: uint,
        timestamp: uint
    }
)

(define-map learning-groups
    uint
    {
        creator: principal,
        course-id: uint,
        name: (string-ascii 50),
        description: (string-ascii 200),
        max-members: uint,
        current-members: uint,
        active: bool,
        created-at: uint
    }
)

(define-map group-memberships
    {group-id: uint, member: principal}
    {
        joined-at: uint,
        reputation-points: uint,
        is-moderator: bool
    }
)

(define-map group-discussions
    uint
    {
        group-id: uint,
        author: principal,
        title: (string-ascii 100),
        content: (string-ascii 500),
        created-at: uint,
        replies-count: uint
    }
)

(define-map group-resources
    uint
    {
        group-id: uint,
        contributor: principal,
        title: (string-ascii 100),
        resource-url: (string-ascii 255),
        resource-type: (string-ascii 20),
        rating: uint,
        votes-count: uint,
        created-at: uint
    }
)

(define-map resource-votes
    {resource-id: uint, voter: principal}
    {
        rating: uint,
        voted-at: uint
    }
)

(define-map certificates
    uint
    {
        student: principal,
        course-id: uint,
        instructor: principal,
        grade: uint,
        completion-date: uint,
        certificate-hash: (string-ascii 64),
        certificate-level: (string-ascii 20),
        active: bool,
        verification-code: (string-ascii 32)
    }
)

(define-map course-completions
    {student: principal, course-id: uint}
    {
        completed: bool,
        completion-date: uint,
        final-grade: uint,
        attempts: uint
    }
)

(define-map certificate-templates
    uint
    {
        course-id: uint,
        template-name: (string-ascii 50),
        min-grade-required: uint,
        certificate-level: (string-ascii 20),
        active: bool
    }
)

(define-map certificate-verifications
    {certificate-id: uint, verifier: principal}
    {
        verified-at: uint,
        verification-status: bool
    }
)

(define-public (create-course (title (string-ascii 50)) (description (string-ascii 500)) (price uint) (content-url (string-ascii 255)))
    (let ((course-id (var-get next-course-id)))
        (try! (nft-mint? skillhub-course course-id tx-sender))
        (map-set courses course-id {
            creator: tx-sender,
            title: title,
            description: description,
            price: price,
            content-url: content-url,
            active: true
        })
        (var-set next-course-id (+ course-id u1))
        (ok course-id)
    )
)

(define-public (update-course (course-id uint) (new-price uint) (new-content-url (string-ascii 255)))
    (let ((course (unwrap! (map-get? courses course-id) err-not-found)))
        (asserts! (is-eq tx-sender (get creator course)) err-owner-only)
        (map-set courses course-id (merge course {
            price: new-price,
            content-url: new-content-url
        }))
        (ok true)
    )
)

(define-public (deactivate-course (course-id uint))
    (let ((course (unwrap! (map-get? courses course-id) err-not-found)))
        (asserts! (is-eq tx-sender (get creator course)) err-owner-only)
        (map-set courses course-id (merge course {
            active: false
        }))
        (ok true)
    )
)

(define-public (enroll-in-course (course-id uint))
    (let 
        (
            (course (unwrap! (map-get? courses course-id) err-not-found))
            (enrollment-id (var-get next-enrollment-id))
        )
        (asserts! (get active course) err-not-authorized)
        (try! (stx-transfer? (get price course) tx-sender (get creator course)))
        (try! (nft-mint? skillhub-enrollment enrollment-id tx-sender))
        (map-set enrollments enrollment-id {
            student: tx-sender,
            course-id: course-id,
            timestamp: stacks-block-height
        })
        (var-set next-enrollment-id (+ enrollment-id u1))
        (ok enrollment-id)
    )
)

(define-read-only (get-course (course-id uint))
    (ok (map-get? courses course-id))
)

(define-read-only (get-enrollment (enrollment-id uint))
    (ok (map-get? enrollments enrollment-id))
)


(define-private (filter-student-enrollments (student-principal principal) (enrollment-id uint) (acc (list 100 {student: principal, course-id: uint, timestamp: uint})))
    (match (map-get? enrollments enrollment-id)
        enrollment (if (is-eq (get student enrollment) student-principal)
            (append acc enrollment)
            acc
        )
        acc
    )
)

;; (define-read-only (get-creator-courses (creator-principal principal))
;;     (ok (fold filter-creator-courses (list) (map-range u0 (var-get next-course-id))))
;; )

(define-private (filter-creator-courses (creator principal) (course-id uint) (acc (list 100 {creator: principal, title: (string-ascii 50), description: (string-ascii 500), price: uint, content-url: (string-ascii 255), active: bool})))
    (match (map-get? courses course-id)
        course (if (is-eq (get creator course) creator)
            (append acc course)
            acc
        )
        acc
    )
)

(define-private (is-member-of-group (group-id uint) (member principal))
    (is-some (map-get? group-memberships {group-id: group-id, member: member}))
)

(define-public (create-learning-group (course-id uint) (name (string-ascii 50)) (description (string-ascii 200)) (max-members uint))
    (let ((group-id (var-get next-group-id)))
        (asserts! (is-some (map-get? courses course-id)) err-not-found)
        (asserts! (> max-members u1) err-not-authorized)
        (try! (nft-mint? skillhub-enrollment group-id tx-sender))
        (map-set learning-groups group-id {
            creator: tx-sender,
            course-id: course-id,
            name: name,
            description: description,
            max-members: max-members,
            current-members: u1,
            active: true,
            created-at: stacks-block-height
        })
        (map-set group-memberships {group-id: group-id, member: tx-sender} {
            joined-at: stacks-block-height,
            reputation-points: u0,
            is-moderator: true
        })
        (var-set next-group-id (+ group-id u1))
        (ok group-id)
    )
)

(define-public (join-learning-group (group-id uint))
    (let ((group (unwrap! (map-get? learning-groups group-id) err-group-not-found)))
        (asserts! (get active group) err-not-authorized)
        (asserts! (< (get current-members group) (get max-members group)) err-group-full)
        (asserts! (not (is-member-of-group group-id tx-sender)) err-already-member)
        (map-set group-memberships {group-id: group-id, member: tx-sender} {
            joined-at: stacks-block-height,
            reputation-points: u0,
            is-moderator: false
        })
        (map-set learning-groups group-id (merge group {
            current-members: (+ (get current-members group) u1)
        }))
        (ok true)
    )
)

(define-public (leave-learning-group (group-id uint))
    (let ((group (unwrap! (map-get? learning-groups group-id) err-group-not-found)))
        (asserts! (is-member-of-group group-id tx-sender) err-not-member)
        (asserts! (not (is-eq tx-sender (get creator group))) err-not-authorized)
        (map-delete group-memberships {group-id: group-id, member: tx-sender})
        (map-set learning-groups group-id (merge group {
            current-members: (- (get current-members group) u1)
        }))
        (ok true)
    )
)

(define-public (create-group-discussion (group-id uint) (title (string-ascii 100)) (content (string-ascii 500)))
    (let ((discussion-id (var-get next-discussion-id)))
        (asserts! (is-some (map-get? learning-groups group-id)) err-group-not-found)
        (asserts! (is-member-of-group group-id tx-sender) err-not-member)
        (map-set group-discussions discussion-id {
            group-id: group-id,
            author: tx-sender,
            title: title,
            content: content,
            created-at: stacks-block-height,
            replies-count: u0
        })
        (var-set next-discussion-id (+ discussion-id u1))
        (ok discussion-id)
    )
)

(define-public (share-group-resource (group-id uint) (title (string-ascii 100)) (resource-url (string-ascii 255)) (resource-type (string-ascii 20)))
    (let ((resource-id (var-get next-resource-id)))
        (asserts! (is-some (map-get? learning-groups group-id)) err-group-not-found)
        (asserts! (is-member-of-group group-id tx-sender) err-not-member)
        (map-set group-resources resource-id {
            group-id: group-id,
            contributor: tx-sender,
            title: title,
            resource-url: resource-url,
            resource-type: resource-type,
            rating: u0,
            votes-count: u0,
            created-at: stacks-block-height
        })
        (var-set next-resource-id (+ resource-id u1))
        (ok resource-id)
    )
)

(define-public (rate-group-resource (resource-id uint) (rating uint))
    (let ((resource (unwrap! (map-get? group-resources resource-id) err-not-found)))
        (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-rating)
        (asserts! (is-member-of-group (get group-id resource) tx-sender) err-not-member)
        (asserts! (not (is-eq tx-sender (get contributor resource))) err-not-authorized)
        (let ((existing-vote (map-get? resource-votes {resource-id: resource-id, voter: tx-sender})))
            (match existing-vote
                vote (let ((old-rating (get rating vote))
                          (new-rating-sum (+ (- (* (get rating resource) (get votes-count resource)) old-rating) rating)))
                    (map-set resource-votes {resource-id: resource-id, voter: tx-sender} {
                        rating: rating,
                        voted-at: stacks-block-height
                    })
                    (map-set group-resources resource-id (merge resource {
                        rating: (/ new-rating-sum (get votes-count resource))
                    }))
                    (ok true)
                )
                (let ((new-votes-count (+ (get votes-count resource) u1))
                      (new-rating-sum (+ (* (get rating resource) (get votes-count resource)) rating)))
                    (map-set resource-votes {resource-id: resource-id, voter: tx-sender} {
                        rating: rating,
                        voted-at: stacks-block-height
                    })
                    (map-set group-resources resource-id (merge resource {
                        rating: (/ new-rating-sum new-votes-count),
                        votes-count: new-votes-count
                    }))
                    (let ((membership (unwrap! (map-get? group-memberships {group-id: (get group-id resource), member: (get contributor resource)}) err-not-found)))
                        (map-set group-memberships {group-id: (get group-id resource), member: (get contributor resource)} (merge membership {
                            reputation-points: (+ (get reputation-points membership) u1)
                        }))
                        (ok true)
                    )
                )
            )
        )
    )
)

(define-public (deactivate-learning-group (group-id uint))
    (let ((group (unwrap! (map-get? learning-groups group-id) err-group-not-found)))
        (asserts! (is-eq tx-sender (get creator group)) err-not-authorized)
        (map-set learning-groups group-id (merge group {
            active: false
        }))
        (ok true)
    )
)

(define-read-only (get-learning-group (group-id uint))
    (ok (map-get? learning-groups group-id))
)

(define-read-only (get-group-membership (group-id uint) (member principal))
    (ok (map-get? group-memberships {group-id: group-id, member: member}))
)

(define-read-only (get-group-discussion (discussion-id uint))
    (ok (map-get? group-discussions discussion-id))
)

(define-read-only (get-group-resource (resource-id uint))
    (ok (map-get? group-resources resource-id))
)

(define-read-only (get-resource-vote (resource-id uint) (voter principal))
    (ok (map-get? resource-votes {resource-id: resource-id, voter: voter}))
)

(define-private (generate-verification-code (student principal) (course-id uint))
    (unwrap-panic (as-max-len? (concat "CERT" (int-to-ascii course-id)) u32))
)



(define-public (mark-course-completion (student principal) (course-id uint) (final-grade uint))
    (let ((course (unwrap! (map-get? courses course-id) err-not-found)))
        (asserts! (is-eq tx-sender (get creator course)) err-not-authorized)
        (asserts! (and (>= final-grade u0) (<= final-grade u100)) err-invalid-grade)
        (let ((existing-completion (map-get? course-completions {student: student, course-id: course-id})))
            (match existing-completion
                completion (map-set course-completions {student: student, course-id: course-id} (merge completion {
                    final-grade: final-grade,
                    attempts: (+ (get attempts completion) u1)
                }))
                (map-set course-completions {student: student, course-id: course-id} {
                    completed: true,
                    completion-date: stacks-block-height,
                    final-grade: final-grade,
                    attempts: u1
                })
            )
        )
        (ok true)
    )
)

(define-public (create-certificate-template (course-id uint) (template-name (string-ascii 50)) (min-grade-required uint) (certificate-level (string-ascii 20)))
    (let ((course (unwrap! (map-get? courses course-id) err-not-found)))
        (asserts! (is-eq tx-sender (get creator course)) err-not-authorized)
        (asserts! (and (>= min-grade-required u0) (<= min-grade-required u100)) err-invalid-grade)
        (map-set certificate-templates course-id {
            course-id: course-id,
            template-name: template-name,
            min-grade-required: min-grade-required,
            certificate-level: certificate-level,
            active: true
        })
        (ok true)
    )
)

(define-public (issue-certificate (student principal) (course-id uint) (certificate-hash (string-ascii 64)))
    (let ((course (unwrap! (map-get? courses course-id) err-not-found))
          (completion (unwrap! (map-get? course-completions {student: student, course-id: course-id}) err-not-completed))
          (template (unwrap! (map-get? certificate-templates course-id) err-not-found))
          (certificate-id (var-get next-certificate-id)))
        (asserts! (is-eq tx-sender (get creator course)) err-not-authorized)
        (asserts! (get completed completion) err-not-completed)
        (asserts! (>= (get final-grade completion) (get min-grade-required template)) err-invalid-grade)
        (asserts! (get active template) err-not-authorized)
        (asserts! (is-none (map-get? certificates (- (var-get next-certificate-id) u1))) err-already-certified)
        (try! (nft-mint? skillhub-certificate certificate-id student))
        (map-set certificates certificate-id {
            student: student,
            course-id: course-id,
            instructor: tx-sender,
            grade: (get final-grade completion),
            completion-date: (get completion-date completion),
            certificate-hash: certificate-hash,
            certificate-level: (get certificate-level template),
            active: true,
            verification-code: (generate-verification-code student course-id)
        })
        (var-set next-certificate-id (+ certificate-id u1))
        (ok certificate-id)
    )
)



(define-public (verify-certificate (certificate-id uint) (verification-code (string-ascii 32)))
    (let ((certificate (unwrap! (map-get? certificates certificate-id) err-certificate-not-found)))
        (asserts! (get active certificate) err-certificate-revoked)
        (asserts! (is-eq verification-code (get verification-code certificate)) err-not-authorized)
        (map-set certificate-verifications {certificate-id: certificate-id, verifier: tx-sender} {
            verified-at: stacks-block-height,
            verification-status: true
        })
        (ok true)
    )
)

(define-public (revoke-certificate (certificate-id uint))
    (let ((certificate (unwrap! (map-get? certificates certificate-id) err-certificate-not-found))
          (course (unwrap! (map-get? courses (get course-id certificate)) err-not-found)))
        (asserts! (is-eq tx-sender (get creator course)) err-not-authorized)
        (map-set certificates certificate-id (merge certificate {
            active: false
        }))
        (ok true)
    )
)

(define-public (transfer-certificate (certificate-id uint) (new-owner principal))
    (let ((certificate (unwrap! (map-get? certificates certificate-id) err-certificate-not-found)))
        (asserts! (is-eq tx-sender (get student certificate)) err-not-authorized)
        (asserts! (get active certificate) err-certificate-revoked)
        (try! (nft-transfer? skillhub-certificate certificate-id tx-sender new-owner))
        (map-set certificates certificate-id (merge certificate {
            student: new-owner,
            verification-code: (generate-verification-code new-owner (get course-id certificate))
        }))
        (ok true)
    )
)

(define-public (update-certificate-template (course-id uint) (min-grade-required uint) (active bool))
    (let ((course (unwrap! (map-get? courses course-id) err-not-found))
          (template (unwrap! (map-get? certificate-templates course-id) err-not-found)))
        (asserts! (is-eq tx-sender (get creator course)) err-not-authorized)
        (asserts! (and (>= min-grade-required u0) (<= min-grade-required u100)) err-invalid-grade)
        (map-set certificate-templates course-id (merge template {
            min-grade-required: min-grade-required,
            active: active
        }))
        (ok true)
    )
)

(define-read-only (get-certificate (certificate-id uint))
    (ok (map-get? certificates certificate-id))
)

(define-read-only (get-course-completion (student principal) (course-id uint))
    (ok (map-get? course-completions {student: student, course-id: course-id}))
)

(define-read-only (get-certificate-template (course-id uint))
    (ok (map-get? certificate-templates course-id))
)

(define-read-only (get-certificate-verification (certificate-id uint) (verifier principal))
    (ok (map-get? certificate-verifications {certificate-id: certificate-id, verifier: verifier}))
)

(define-read-only (validate-certificate (certificate-id uint) (verification-code (string-ascii 32)))
    (match (map-get? certificates certificate-id)
        certificate (ok (and (get active certificate) (is-eq verification-code (get verification-code certificate))))
        (ok false)
    )
)





