# 🎓 Skillhub - Decentralized Course Marketplace

Welcome to Skillhub, a decentralized platform for creating and accessing token-gated educational content on the Stacks blockchain! 

## 🌟 Features

- Create and manage courses with token-gated content
- Set course prices in STX
- Enroll in courses using STX payments
- Track course enrollments with NFTs
- View course content through secure URLs

## 🚀 Smart Contract Functions

### For Course Creators

- `create-course`: Create a new course with title, description, price, and content URL
- `update-course`: Update course price and content URL
- `deactivate-course`: Disable enrollment for a course
- `get-creator-courses`: View all courses created by an address

### For Students

- `enroll-in-course`: Purchase access to a course
- `get-course`: View course details
- `get-enrollment`: Check enrollment details
- `get-student-enrollments`: View all courses enrolled by a student

## 💻 Usage

1. Deploy the contract using Clarinet
2. Create courses by calling `create-course`
3. Students can enroll using `enroll-in-course`
4. Access content through the provided content URLs

## 🔒 Security

- Course creation restricted to tx-sender
- Course updates restricted to course creator
- Enrollment NFTs track student access
- STX payments handled securely

## 🤝 Contributing

Feel free to submit issues and enhancement requests!

## 📜 License

MIT
```

