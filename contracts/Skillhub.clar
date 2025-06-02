(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-not-authorized (err u103))
(define-constant err-wrong-price (err u104))

(define-non-fungible-token skillhub-course uint)
(define-non-fungible-token skillhub-enrollment uint)

(define-data-var next-course-id uint u1)
(define-data-var next-enrollment-id uint u1)

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