---
title: "Quick Start"
date: 2025-12-30T06:12:21+07:00
draft: false
weight: 100002
description: "Learn core Ansible concepts and automation patterns"
tags:
  - ansible
  - automation
  - devops
  - quick-start
---

Learn core Ansible concepts and automation patterns to build infrastructure automation. This Quick Start teaches essential Ansible concepts.

## 🎯 What You'll Learn

By the end of this tutorial, you'll understand:

- Inventory management
- Playbooks, tasks, and modules
- Variables and templates
- Roles and best practices

## 📋 Prerequisites

- Ansible installed (see [Initial Setup](/en/learn/software-engineering/infrastructure/tools/ansible/tutorials/initial-setup))
- Basic Linux command line knowledge

## 📦 Inventory

Create `inventory.ini`:

```ini
[webservers]
web1 ansible_host=192.168.1.10
web2 ansible_host=192.168.1.11

[databases]
db1 ansible_host=192.168.1.20

[all:vars]
ansible_user=ubuntu
ansible_ssh_private_key_file=~/.ssh/id_rsa
```

## 🔧 Playbooks and Tasks

```yaml
---
- name: Configure Web Servers
  hosts: webservers
  become: yes
  tasks:
    - name: Install nginx
      ansible.builtin.apt:
        name: nginx
        state: present
        update_cache: yes

    - name: Start nginx service
      ansible.builtin.service:
        name: nginx
        state: started
        enabled: yes

    - name: Copy config file
      ansible.builtin.copy:
        src: nginx.conf
        dest: /etc/nginx/nginx.conf
        owner: root
        group: root
        mode: "0644"
      notify: Restart nginx

  handlers:
    - name: Restart nginx
      ansible.builtin.service:
        name: nginx
        state: restarted
```

## 📊 Variables and Templates

```yaml
---
- name: Deploy Application
  hosts: webservers
  vars:
    app_name: myapp
    app_port: 8080
  tasks:
    - name: Deploy from template
      ansible.builtin.template:
        src: app.conf.j2
        dest: "/etc/{{ app_name }}/config.conf"
```

## 🎨 Roles

```bash
ansible-galaxy init webserver

# Creates:
webserver/
├── tasks/
│   └── main.yml
├── handlers/
│   └── main.yml
├── templates/
├── files/
├── vars/
│   └── main.yml
└── defaults/
    └── main.yml
```

## ✅ Next Steps

You now understand Ansible essentials! To deepen your knowledge:

1. **Try the examples**: Execute each playbook
2. **Explore By Example**: [Ansible By Example](/en/learn/software-engineering/infrastructure/tools/ansible/tutorials/by-example)

## 🎯 Self-Assessment

After completing this Quick Start, you should be able to:

- [ ] Define inventory files
- [ ] Write playbooks with tasks and modules
- [ ] Use variables and templates
- [ ] Understand handlers and notifications
- [ ] Organize automation with roles
