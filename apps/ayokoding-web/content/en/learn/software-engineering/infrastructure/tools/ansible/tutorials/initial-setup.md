---
title: "Initial Setup"
date: 2025-12-30T06:12:21+07:00
draft: false
weight: 100001
description: "Install Ansible and execute your first playbook"
tags:
  - ansible
  - automation
  - devops
  - installation
---

Get Ansible installed and execute your first playbook. This guide walks you through installation and running your first automation.

## 🎯 What You'll Accomplish

By the end of this tutorial, you'll have:

- ✅ Ansible installed and verified
- ✅ Your first playbook created
- ✅ First automation executed

## 📋 Prerequisites

- Linux or macOS (or WSL2 on Windows)
- Python 3.8 or later
- SSH access to target machines (or localhost for testing)

## 💾 Step 1: Install Ansible

### Using pip (Recommended)

```bash
python3 -m pip install --user ansible
```

### macOS (Homebrew)

```bash
brew install ansible
```

### Ubuntu/Debian

```bash
sudo apt update
sudo apt install ansible
```

### Verify Installation

```bash
ansible --version
```

## 🚀 Step 2: Create Your First Playbook

Create `hello.yml`:

```yaml
---
- name: My First Playbook
  hosts: localhost
  connection: local
  tasks:
    - name: Print hello message
      ansible.builtin.debug:
        msg: "Hello, Ansible!"

    - name: Create a file
      ansible.builtin.file:
        path: /tmp/ansible-test.txt
        state: touch

    - name: Write to file
      ansible.builtin.copy:
        content: "Ansible was here!"
        dest: /tmp/ansible-test.txt
```

## ▶️ Step 3: Run Your Playbook

```bash
ansible-playbook hello.yml
```

Expected output shows tasks being executed and their status (ok, changed, failed).

## ✅ Verification Checklist

Before moving forward, verify:

- [ ] `ansible --version` shows Ansible installed
- [ ] Playbook executes without errors
- [ ] File created at `/tmp/ansible-test.txt`

## 🎉 You're Done!

You've successfully installed Ansible and run your first playbook. You're ready for more advanced automation.

## 📚 What's Next?

**Quick learner**: [Ansible Quick Start](/en/learn/software-engineering/infrastructure/tools/ansible/tutorials/quick-start)

**Code-first learner**: [Ansible By Example](/en/learn/software-engineering/infrastructure/tools/ansible/tutorials/by-example)
