# 🔐 On-Chain Social Recovery for Lost Wallets

A Clarity smart contract that enables social recovery of lost wallets through a trusted network of guardians on the Stacks blockchain.

## 🌟 Features

- **Guardian Network**: Set up trusted contacts who can help recover your wallet
- **Configurable Threshold**: Define how many guardians need to approve recovery
- **Time-locked Recovery**: Built-in delays to prevent malicious recovery attempts
- **Secure Process**: Multi-step approval process with expiration timeouts

## 🚀 Quick Start

### Setup Recovery for Your Wallet

```clarity
;; Setup recovery with 3 guardians, requiring 2 approvals
(contract-call? .social-recovery setup-recovery 
  (list 'SP1... 'SP2... 'SP3...) 
  u2)
```

### Request Recovery (Anyone can initiate)

```clarity
;; Request recovery to transfer wallet to new owner
(contract-call? .social-recovery request-recovery 
  'SP-WALLET-TO-RECOVER... 
  'SP-NEW-OWNER...)
```

### Approve Recovery (Guardians only)

```clarity
;; Guardian approves the recovery request
(contract-call? .social-recovery approve-recovery 
  'SP-WALLET-TO-RECOVER...)
```

### Execute Recovery

```clarity
;; Execute recovery once threshold is met
(contract-call? .social-recovery execute-recovery 
  'SP-WALLET-TO-RECOVER...)
```

## 📋 Contract Functions

### Public Functions

| Function | Description | Who Can Call |
|----------|-------------|--------------|
| `setup-recovery` | Initialize recovery setup for wallet | Wallet Owner |
| `add-guardian` | Add new guardian to wallet | Wallet Owner |
| `remove-guardian` | Remove guardian from wallet | Wallet Owner |
| `request-recovery` | Start recovery process | Anyone |
| `approve-recovery` | Approve pending recovery | Guardians Only |
| `execute-recovery` | Complete recovery process | Anyone |
| `cancel-recovery` | Cancel ongoing recovery | Wallet Owner |

### Read-Only Functions

| Function | Description |
|----------|-------------|
| `get-wallet-info` | Get wallet configuration |
| `get-recovery-info` | Get active recovery details |
| `get-recovery-status` | Get recovery progress status |
| `has-guardian-approved` | Check if guardian approved |

## 🔧 Configuration

- **Maximum Guardians**: 10 per wallet
- **Recovery Delay**: 144 blocks (~24 hours) by default
- **Approval Threshold**: 1-10 guardians (configurable per wallet)

## 🛡️ Security Features

- ⏰ **Time Delays**: Recovery requests expire after set period
- 🔒 **Threshold Requirements**: Multiple guardian approvals required
- 🚫 **Owner Override**: Original owner can cancel recovery attempts
- 📝 **Audit Trail**: All actions recorded on-chain

## 💡 Use Cases

- 🔑 **Lost Private Keys**: Recover access when keys are lost
- 👥 **Family Inheritance**: Enable family members to recover wallets
- 🏢 **Corporate Accounts**: Multi-signature recovery for business wallets
- 🆘 **Emergency Access**: Quick recovery in critical situations

## 🧪 Testing

Run the test suite:

```bash
clarinet test
```

## 📄 License

MIT License - see LICENSE file for details.

## 🤝 Contributing

Contributions welcome! Please read contributing guidelines and submit PRs.

---

*Built with ❤️ for the Stacks ecosystem*
