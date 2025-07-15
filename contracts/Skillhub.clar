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

(define-non-fungible-token skillhub-course uint)
(define-non-fungible-token skillhub-enrollment uint)

(define-data-var next-course-id uint u1)
(define-data-var next-enrollment-id uint u1)
(define-data-var next-group-id uint u1)
(define-data-var next-discussion-id uint u1)
(define-data-var next-resource-id uint u1)

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