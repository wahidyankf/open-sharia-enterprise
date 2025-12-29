---
title: "Intermediate"
date: 2025-12-29T22:43:50+07:00
draft: false
weight: 10000002
description: "Examples 28-54: Production patterns including roles, handlers, templates, vault, error handling, and task control (40-75% coverage)"
categories: ["learn"]
tags: ["ansible", "tutorial", "by-example", "intermediate", "roles", "handlers", "templates", "vault", "production"]
---

Learn Ansible production patterns through 27 annotated code examples. Each example demonstrates real-world automation techniques used in enterprise environments.

## Group 7: Roles & Galaxy

### Example 28: Basic Role Structure

Roles organize playbooks into reusable components with standardized directory structure. Each role encapsulates related tasks, variables, files, and templates for a specific function.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    A["Role<br/>webserver"] --> B["tasks/main.yml<br/>Task definitions"]
    A --> C["handlers/main.yml<br/>Service handlers"]
    A --> D["templates/<br/>Jinja2 templates"]
    A --> E["files/<br/>Static files"]
    A --> F["vars/main.yml<br/>Role variables"]
    A --> G["defaults/main.yml<br/>Default variables"]
    A --> H["meta/main.yml<br/>Dependencies"]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#fff
    style C fill:#029E73,color:#fff
    style D fill:#CC78BC,color:#fff
    style E fill:#CA9161,color:#fff
    style F fill:#029E73,color:#fff
    style G fill:#DE8F05,color:#fff
    style H fill:#CC78BC,color:#fff
```

**Code**:

Create role directory structure:

```bash
# ansible-galaxy init webserver
# Creates standard role structure
```

**`roles/webserver/defaults/main.yml`**:

```yaml
---
# Default variables (lowest precedence)
http_port: 80 # => Can be overridden by playbook vars
https_port: 443
document_root: /var/www/html
server_name: localhost
```

**`roles/webserver/vars/main.yml`**:

```yaml
---
# Role variables (higher precedence than defaults)
nginx_package: nginx # => Package name
nginx_service: nginx # => Service name
config_file: /etc/nginx/sites-available/default
```

**`roles/webserver/tasks/main.yml`**:

```yaml
---
# Main task file for webserver role
- name: Install web server
  ansible.builtin.package:
    name: "{{ nginx_package }}" # => Uses var from vars/main.yml
    state: present
  # => changed: [host] (installs nginx package)

- name: Create document root
  ansible.builtin.file:
    path: "{{ document_root }}" # => Uses var from defaults/main.yml
    state: directory
    mode: "0755"
  # => changed: [host] (creates web root directory)

- name: Deploy configuration
  ansible.builtin.template:
    src: nginx.conf.j2 # => Template from templates/ directory
    dest: "{{ config_file }}"
    mode: "0644"
  notify: restart nginx # => Trigger handler if config changes
  # => changed: [host] (renders and copies template)

- name: Ensure service is running
  ansible.builtin.service:
    name: "{{ nginx_service }}"
    state: started
    enabled: true
  # => ok: [host] (service already running and enabled)
```

**`roles/webserver/handlers/main.yml`**:

```yaml
---
# Handlers for service management
- name: restart nginx
  ansible.builtin.service:
    name: "{{ nginx_service }}"
    state: restarted
  # => Executes only if notified by task changes
```

**`roles/webserver/templates/nginx.conf.j2`**:

```nginx
server {
    listen {{ http_port }};
    server_name {{ server_name }};
    root {{ document_root }};
    index index.html;
}
```

**Playbook using role**:

```yaml
---
# webserver_playbook.yml
- name: Deploy Web Server
  hosts: webservers
  become: true

  roles:
    - webserver # => Apply webserver role
  # => Executes all tasks from roles/webserver/tasks/main.yml
```

**Run**: `ansible-playbook webserver_playbook.yml`

**Key Takeaway**: Roles enable code reuse and logical organization. Use `defaults/main.yml` for overridable defaults, `vars/main.yml` for fixed values. The `roles` keyword automatically includes tasks, handlers, variables, and files from role directories. Role structure is standardized—any Ansible user recognizes the layout.

---

### Example 29: Role Variables and Precedence

Role variables come from multiple sources with specific precedence rules. Understanding precedence prevents unexpected values in complex playbooks.

**Code**:

**`roles/app/defaults/main.yml`**:

```yaml
---
# Defaults (precedence: 2 - lowest for roles)
app_port: 8080
app_env: development
debug_enabled: false
```

**`roles/app/vars/main.yml`**:

```yaml
---
# Role vars (precedence: 18)
app_name: MyApplication # => Fixed role variable
config_dir: /etc/myapp
```

**Playbook**:

```yaml
---
# role_precedence.yml
- name: Role Variable Precedence
  hosts: localhost
  gather_facts: false

  # Play vars (precedence: 15)
  vars:
    app_port: 9090 # => Overrides defaults but not vars
    app_env: staging

  roles:
    - role: app
      # Role parameters (precedence: 17)
      vars:
        debug_enabled: true # => Overrides defaults but not role vars
  # => Final values:
  # => app_name: MyApplication (from vars/main.yml - precedence 18)
  # => app_port: 9090 (from play vars - precedence 15)
  # => app_env: staging (from play vars - precedence 15)
  # => debug_enabled: true (from role parameters - precedence 17)
  # => config_dir: /etc/myapp (from vars/main.yml - precedence 18)

  tasks:
    - name: Display final configuration
      ansible.builtin.debug:
        msg: |
          App: {{ app_name }}
          Port: {{ app_port }}
          Environment: {{ app_env }}
          Debug: {{ debug_enabled }}
          Config Dir: {{ config_dir }}
      # => Shows merged configuration from all sources
```

**Run with extra-vars (precedence: 22 - highest)**:

```bash
ansible-playbook role_precedence.yml -e "app_port=3000"
# => app_port becomes 3000 (extra-vars override everything)
```

**Key Takeaway**: Variable precedence from lowest to highest: role defaults < inventory vars < play vars < role vars < extra-vars. Use `defaults/main.yml` for overridable configuration, `vars/main.yml` for fixed role internals. Pass role-specific overrides using role parameters (`vars:` under role declaration).

---

### Example 30: Role Dependencies

Roles can depend on other roles using `meta/main.yml`. Dependencies install automatically and execute before the dependent role.

**Code**:

**`roles/database/meta/main.yml`**:

```yaml
---
# Database role dependencies
dependencies:
  - role: common # => Always execute common role first
    vars:
      ntp_server: time.example.com # => Pass variables to dependency

  - role: firewall # => Execute firewall role second
    vars:
      allowed_ports:
        - 5432 # => Open PostgreSQL port
```

**`roles/common/tasks/main.yml`**:

```yaml
---
# Common role tasks
- name: Update package cache
  ansible.builtin.apt:
    update_cache: true
    cache_valid_time: 3600
  when: ansible_os_family == "Debian"
  # => Updates apt cache if stale

- name: Install common packages
  ansible.builtin.package:
    name:
      - curl
      - vim
      - git
    state: present
  # => Installs base utilities
```

**`roles/firewall/tasks/main.yml`**:

```yaml
---
# Firewall role tasks
- name: Install UFW
  ansible.builtin.package:
    name: ufw
    state: present
  when: ansible_os_family == "Debian"

- name: Allow SSH
  community.general.ufw:
    rule: allow
    port: "22"
    proto: tcp
  # => Ensure SSH access before enabling firewall

- name: Allow application ports
  community.general.ufw:
    rule: allow
    port: "{{ item }}"
    proto: tcp
  loop: "{{ allowed_ports }}" # => Ports from role dependency vars
  # => Opens ports passed from database role

- name: Enable UFW
  community.general.ufw:
    state: enabled
  # => Activates firewall with configured rules
```

**`roles/database/tasks/main.yml`**:

```yaml
---
# Database role tasks (executes AFTER dependencies)
- name: Install PostgreSQL
  ansible.builtin.package:
    name: postgresql
    state: present
  # => Installs database after common packages and firewall config

- name: Ensure PostgreSQL is running
  ansible.builtin.service:
    name: postgresql
    state: started
    enabled: true
  # => Starts database service
```

**Playbook**:

```yaml
---
# database_playbook.yml
- name: Deploy Database Server
  hosts: dbservers
  become: true

  roles:
    - database # => Applies database role
  # => Execution order: common -> firewall -> database
  # => Dependencies execute first automatically
```

**Run**: `ansible-playbook database_playbook.yml`

**Key Takeaway**: Role dependencies in `meta/main.yml` ensure prerequisite roles execute first. Dependencies run once even if multiple roles depend on the same role (unless `allow_duplicates: true`). Pass variables to dependencies using `vars:` parameter. This pattern enables composable, layered automation.

---

### Example 31: Ansible Galaxy - Using Community Roles

Ansible Galaxy hosts thousands of community-maintained roles. Use `ansible-galaxy` command to install, list, and manage roles from Galaxy or private repositories.

**Code**:

**Install role from Galaxy**:

```bash
# Install specific role
ansible-galaxy install geerlingguy.nginx

# Install with version constraint
ansible-galaxy install geerlingguy.nginx,2.8.0

# Install multiple roles from requirements file
ansible-galaxy install -r requirements.yml
```

**`requirements.yml`**:

```yaml
---
# Galaxy roles
- name: geerlingguy.nginx # => Role from Ansible Galaxy
  version: 2.8.0 # => Specific version (optional)

- name: geerlingguy.postgresql
  version: 3.4.1

# Git repository roles
- src: https://github.com/example/ansible-role-custom.git
  name: custom_role # => Local name for role
  version: main # => Git branch/tag

# Local roles path override
- src: ../local-roles/internal-role # => Relative path to local role
  name: internal_role
```

**Playbook using Galaxy role**:

```yaml
---
# galaxy_roles.yml
- name: Use Galaxy Roles
  hosts: webservers
  become: true

  roles:
    - role: geerlingguy.nginx # => Galaxy role (namespace.rolename)
      vars:
        nginx_vhosts: # => Variables expected by role
          - listen: "80"
            server_name: "example.com"
            root: "/var/www/html"
  # => Role tasks execute with provided configuration

  tasks:
    - name: Verify nginx is running
      ansible.builtin.service:
        name: nginx
        state: started
      # => Check service state after role execution
```

**List installed roles**:

```bash
# List all installed roles
ansible-galaxy list

# Show role information
ansible-galaxy info geerlingguy.nginx

# Remove installed role
ansible-galaxy remove geerlingguy.nginx
```

**Key Takeaway**: Ansible Galaxy accelerates automation by providing pre-built, tested roles for common software stacks. Use `requirements.yml` for version-pinned, reproducible role installations. Always review Galaxy role documentation for required variables and dependencies. Popular roles like `geerlingguy.*` are production-ready and well-maintained.

---

### Example 32: Creating Distributable Roles

Create reusable roles for sharing via Galaxy or private repositories. Follow role structure conventions and include comprehensive metadata.

**Code**:

**Initialize role with Galaxy template**:

```bash
ansible-galaxy init --init-path roles/ myapp
# Creates: roles/myapp/{tasks,handlers,templates,files,vars,defaults,meta,tests}
```

**`roles/myapp/meta/main.yml`**:

```yaml
---
galaxy_info:
  author: Your Name
  description: Deploys and configures MyApp
  license: MIT
  min_ansible_version: 2.14

  platforms: # => Supported platforms
    - name: Ubuntu
      versions:
        - focal # => 20.04
        - jammy # => 22.04
    - name: Debian
      versions:
        - bullseye
        - bookworm

  galaxy_tags: # => Galaxy search tags
    - web
    - application
    - deployment

dependencies: [] # => List role dependencies
```

**`roles/myapp/README.md`**:

````markdown
# Ansible Role: myapp

Deploys and configures MyApp web application.

## Requirements

- Ansible 2.14+
- Ubuntu 20.04+ or Debian 11+
- Python 3.8+

## Role Variables

Available variables with defaults (defaults/main.yml):

```yaml
myapp_version: "1.0.0" # Application version
myapp_port: 8080 # HTTP port
myapp_user: myapp # System user
myapp_install_dir: /opt/myapp # Installation directory
```
````

## Dependencies

None.

## Example Playbook

```yaml
- hosts: appservers
  roles:
    - role: myapp
      vars:
        myapp_version: "2.0.0"
        myapp_port: 9090
```

## License

MIT

## Author

Your Name

````

**`roles/myapp/defaults/main.yml`**:
```yaml
---
# Overridable defaults
myapp_version: "1.0.0"
myapp_port: 8080
myapp_user: myapp
myapp_group: myapp
myapp_install_dir: /opt/myapp
myapp_config_file: /etc/myapp/config.yml
````

**`roles/myapp/tasks/main.yml`**:

```yaml
---
# Main tasks
- name: Include OS-specific variables
  ansible.builtin.include_vars: "{{ ansible_os_family }}.yml"
  # => Loads vars/Debian.yml or vars/RedHat.yml based on OS

- name: Create application user
  ansible.builtin.user:
    name: "{{ myapp_user }}"
    group: "{{ myapp_group }}"
    system: true
    create_home: false
  # => Creates system user for application

- name: Create installation directory
  ansible.builtin.file:
    path: "{{ myapp_install_dir }}"
    state: directory
    owner: "{{ myapp_user }}"
    group: "{{ myapp_group }}"
    mode: "0755"
  # => Creates app installation directory

- name: Deploy application
  ansible.builtin.get_url:
    url: "https://releases.example.com/myapp-{{ myapp_version }}.tar.gz"
    dest: "/tmp/myapp-{{ myapp_version }}.tar.gz"
  # => Downloads application release

- name: Extract application
  ansible.builtin.unarchive:
    src: "/tmp/myapp-{{ myapp_version }}.tar.gz"
    dest: "{{ myapp_install_dir }}"
    remote_src: true
    owner: "{{ myapp_user }}"
    group: "{{ myapp_group }}"
  # => Extracts to installation directory

- name: Deploy configuration
  ansible.builtin.template:
    src: config.yml.j2
    dest: "{{ myapp_config_file }}"
    owner: "{{ myapp_user }}"
    group: "{{ myapp_group }}"
    mode: "0640"
  notify: restart myapp
  # => Renders configuration template
```

**Test role locally**:

```yaml
---
# tests/test.yml
- hosts: localhost
  become: true
  roles:
    - myapp
```

**Run test**: `ansible-playbook tests/test.yml`

**Key Takeaway**: Distributable roles require comprehensive metadata in `meta/main.yml` and clear documentation in `README.md`. Use `defaults/main.yml` for all configurable parameters with sensible defaults. Include OS-specific variable files for cross-platform compatibility. Test roles thoroughly before publishing to Galaxy.

---

### Example 33: Role Include and Import

Dynamic role inclusion enables conditional role application and runtime role selection. `include_role` loads at runtime, `import_role` loads at parse time.

**Code**:

```yaml
---
# dynamic_roles.yml
- name: Dynamic Role Inclusion
  hosts: localhost
  gather_facts: true

  vars:
    server_type: webserver # => Dynamic role selector
    enable_monitoring: true

  tasks:
    # Static import (parse-time)
    - name: Import role statically
      ansible.builtin.import_role:
        name: common # => Always imported (even if skipped)
      # => Tasks parsed immediately, supports tags

    # Dynamic include (runtime)
    - name: Include role dynamically
      ansible.builtin.include_role:
        name: "{{ server_type }}" # => Role name from variable
      # => Role loaded at runtime based on variable value

    # Conditional role inclusion
    - name: Include monitoring role if enabled
      ansible.builtin.include_role:
        name: monitoring
      when: enable_monitoring # => Include only if condition true
      # => Skipped if enable_monitoring is false

    # Include specific tasks from role
    - name: Include specific role tasks
      ansible.builtin.include_role:
        name: webserver
        tasks_from: install # => Include tasks/install.yml only
      # => Executes roles/webserver/tasks/install.yml (not main.yml)

    # Apply role to subset of hosts
    - name: Include role for specific hosts
      ansible.builtin.include_role:
        name: database
      when: "'dbservers' in group_names" # => Only on hosts in dbservers group
      # => Role applies selectively based on host membership

    # Include role with custom variables
    - name: Include role with vars
      ansible.builtin.include_role:
        name: application
      vars:
        app_version: "2.0.0" # => Override role defaults
        app_port: 9090
      # => Variables scoped to this role inclusion

    # Loop over roles
    - name: Include multiple roles dynamically
      ansible.builtin.include_role:
        name: "{{ item }}"
      loop:
        - logging
        - metrics
        - tracing
      # => Applies each role in sequence
```

**Run**: `ansible-playbook dynamic_roles.yml`

**Key Takeaway**: Use `include_role` for conditional or dynamic role application at runtime. Use `import_role` for static inclusion with tag support. The `tasks_from` parameter allows partial role inclusion—useful for breaking large roles into logical task files. Dynamic role selection enables playbooks that adapt to host groups or runtime conditions.

---

## Group 8: Handlers & Notifications

### Example 34: Handler Basics

Handlers execute once at the end of a play, triggered by task changes. Prevent redundant service restarts and enable efficient orchestration.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph LR
    A["Task 1<br/>Change Config"] -->|notify| B["Handler Queue"]
    C["Task 2<br/>Change Template"] -->|notify| B
    D["Task 3<br/>No Change"] -.->|skip| B
    B --> E["Play End"]
    E --> F["Execute Handler Once"]

    style A fill:#0173B2,color:#fff
    style C fill:#0173B2,color:#fff
    style D fill:#CA9161,color:#fff
    style B fill:#DE8F05,color:#fff
    style E fill:#029E73,color:#fff
    style F fill:#CC78BC,color:#fff
```

**Code**:

```yaml
---
# handlers_basic.yml
- name: Handler Basics
  hosts: localhost
  become: true
  gather_facts: false

  handlers:
    # Handler definition
    - name: restart nginx # => Handler name (used in notify)
      ansible.builtin.service:
        name: nginx
        state: restarted
      # => Executes ONLY if notified and ONLY ONCE per play

  tasks:
    # Task that notifies handler
    - name: Update nginx configuration
      ansible.builtin.copy:
        dest: /etc/nginx/nginx.conf
        content: |
          events {}
          http {
            server {
              listen 8080;
            }
          }
      notify: restart nginx # => Queue handler if task changes
      # => changed: [localhost] -> handler queued
      # => ok: [localhost] -> handler not queued

    # Another task notifying same handler
    - name: Update site configuration
      ansible.builtin.copy:
        dest: /etc/nginx/sites-available/default
        content: |
          server {
            listen 80;
            root /var/www/html;
          }
      notify: restart nginx # => Queue same handler again
      # => Handler still executes only ONCE at end of play

    # Task without notification
    - name: Create web root
      ansible.builtin.file:
        path: /var/www/html
        state: directory
        mode: "0755"
      # => No notify, handler not affected

  # Handler execution happens HERE (after all tasks complete)
  # => restart nginx executes once if ANY notifying task changed
```

**Run**: `ansible-playbook handlers_basic.yml --ask-become-pass`

**Key Takeaway**: Handlers execute once at play end even if notified multiple times. This prevents redundant service restarts when multiple configuration changes occur. Handlers only execute if notifying tasks report "changed" status—idempotent tasks won't trigger handlers unnecessarily.

---

### Example 35: Handler Notification Patterns

Multiple handlers can be triggered by a single task. Handlers execute in definition order, not notification order.

**Code**:

```yaml
---
# handler_patterns.yml
- name: Handler Notification Patterns
  hosts: localhost
  become: true
  gather_facts: false

  handlers:
    # Multiple handlers
    - name: restart nginx
      ansible.builtin.service:
        name: nginx
        state: restarted
      # => Handler 1

    - name: reload nginx
      ansible.builtin.service:
        name: nginx
        state: reloaded
      # => Handler 2

    - name: restart php-fpm
      ansible.builtin.service:
        name: php-fpm
        state: restarted
      # => Handler 3

    - name: clear cache
      ansible.builtin.file:
        path: /var/cache/app
        state: absent
      # => Handler 4

  tasks:
    # Single notification
    - name: Update nginx config
      ansible.builtin.copy:
        dest: /etc/nginx/nginx.conf
        content: "events {}\nhttp {}\n"
      notify: restart nginx # => Notify one handler
      # => Queues restart nginx only

    # Multiple notifications
    - name: Update PHP config
      ansible.builtin.copy:
        dest: /etc/php/php.ini
        content: |
          [PHP]
          memory_limit = 256M
      notify:
        - restart php-fpm # => Notify first handler
        - clear cache # => Notify second handler
      # => Queues both handlers if task changes

    # Notification with list syntax
    - name: Update application code
      ansible.builtin.copy:
        dest: /var/www/app/index.php
        content: "<?php phpinfo(); ?>"
      notify:
        - reload nginx # => Reload instead of restart
        - clear cache
      # => Queues reload nginx and clear cache

  # Handlers execute in DEFINITION order (not notification order):
  # 1. restart nginx (if queued)
  # 2. reload nginx (if queued)
  # 3. restart php-fpm (if queued)
  # 4. clear cache (if queued)
```

**Run**: `ansible-playbook handler_patterns.yml --ask-become-pass`

**Key Takeaway**: Tasks can notify multiple handlers using list syntax under `notify:`. Handlers execute in the order they're defined in the `handlers:` section, not in notification order. Use `reload` handlers for configuration changes that don't require full restarts, reducing downtime.

---

### Example 36: Flush Handlers and Listen

Force handler execution mid-play with `meta: flush_handlers`. Use `listen` to group handlers under common topic.

**Code**:

```yaml
---
# flush_handlers.yml
- name: Flush Handlers and Listen
  hosts: localhost
  become: true
  gather_facts: false

  handlers:
    # Traditional handler with listen
    - name: restart nginx service
      ansible.builtin.service:
        name: nginx
        state: restarted
      listen: restart web services # => Handler listens to topic
      # => Triggered by notify: restart web services

    - name: restart apache service
      ansible.builtin.service:
        name: apache2
        state: restarted
      listen: restart web services # => Same topic, different handler
      # => BOTH handlers execute when topic notified

    - name: reload configuration
      ansible.builtin.command:
        cmd: /usr/local/bin/reload-config.sh
      listen: restart web services # => Third handler on same topic
      # => All three execute for one notification

  tasks:
    # Task notifying topic
    - name: Update shared configuration
      ansible.builtin.copy:
        dest: /etc/shared/config.conf
        content: "setting=value\n"
      notify: restart web services # => Notifies topic, not specific handler
      # => Queues ALL handlers listening to this topic

    # Force handler execution NOW
    - name: Flush handlers immediately
      ansible.builtin.meta: flush_handlers
      # => Executes all queued handlers immediately
      # => Handlers: restart nginx, restart apache, reload configuration

    # Task after flush
    - name: Verify services are running
      ansible.builtin.service:
        name: nginx
        state: started
      # => Runs AFTER handlers execute (nginx already restarted)

    # Another notification
    - name: Update another config
      ansible.builtin.copy:
        dest: /etc/shared/other.conf
        content: "other=value\n"
      notify: restart web services
      # => Queues handlers again

  # Final handlers execute here at play end
  # => restart web services handlers run second time if notified after flush
```

**Run**: `ansible-playbook flush_handlers.yml --ask-become-pass`

**Key Takeaway**: Use `listen` to group related handlers under topic names—one notification triggers multiple handlers. Use `meta: flush_handlers` to force immediate handler execution mid-play—critical when subsequent tasks depend on handler changes. Handlers can execute multiple times per play if flushed and notified again.

---

### Example 37: Handler Conditionals and Error Handling

Handlers support conditionals and error handling like regular tasks. Control handler execution based on facts or variables.

**Code**:

```yaml
---
# handler_conditionals.yml
- name: Handler Conditionals and Error Handling
  hosts: localhost
  become: true
  gather_facts: true

  vars:
    production_mode: true
    enable_service_restart: true

  handlers:
    # Conditional handler
    - name: restart nginx
      ansible.builtin.service:
        name: nginx
        state: restarted
      when: enable_service_restart # => Handler runs only if variable true
      # => Skipped if enable_service_restart is false

    # OS-specific handler
    - name: restart web server
      ansible.builtin.service:
        name: "{{ 'apache2' if ansible_os_family == 'Debian' else 'httpd' }}"
        state: restarted
      when: ansible_os_family in ['Debian', 'RedHat']
      # => Executes only on Debian or RedHat systems

    # Handler with error handling
    - name: reload application
      ansible.builtin.command:
        cmd: /usr/local/bin/app reload
      register: reload_result
      failed_when: false # => Never fail (always report ok)
      # => Continues even if reload command fails

    - name: report reload failure
      ansible.builtin.debug:
        msg: "WARNING: Application reload failed"
      when: reload_result is defined and reload_result.rc != 0
      # => Executes only if previous handler registered failure

    # Production-only handler
    - name: send notification
      ansible.builtin.uri:
        url: https://hooks.slack.com/services/YOUR/WEBHOOK/URL
        method: POST
        body_format: json
        body:
          text: "Configuration updated on {{ inventory_hostname }}"
      when: production_mode # => Only in production
      # => Sends Slack notification in production environment

  tasks:
    - name: Update configuration
      ansible.builtin.copy:
        dest: /etc/app/config.yml
        content: |
          port: 8080
          debug: false
      notify:
        - restart nginx # => Conditional handler
        - reload application # => Error-tolerant handler
        - report reload failure # => Conditional on previous handler
        - send notification # => Production-only handler
      # => All handlers queued, conditions evaluated at handler execution

    # Demonstrate conditional handler skip
    - name: Update non-critical config
      ansible.builtin.copy:
        dest: /tmp/test.conf
        content: "test=value\n"
      notify: restart nginx
      vars:
        enable_service_restart: false # => Task-level var disables handler
      # => Handler queued but skipped due to condition
```

**Run**: `ansible-playbook handler_conditionals.yml --ask-become-pass`

**Key Takeaway**: Handlers support `when` conditionals for environment-aware execution. Use `failed_when: false` in handlers to prevent failures from stopping playbook. Chain handlers with `register` and conditionals for error recovery. Handler conditionals enable safe rollouts where restarts are conditional on environment or success state.

---

## Group 9: Templates (Jinja2)

### Example 38: Jinja2 Template Basics

Jinja2 templates combine static text with dynamic variables. The `template` module renders templates on control node and copies to managed hosts.

**Code**:

**`templates/app_config.yml.j2`**:

```jinja2
# Application Configuration
# Generated by Ansible on {{ ansible_date_time.iso8601 }}

application:
  name: {{ app_name }}                   # => Variable substitution
  version: {{ app_version }}
  environment: {{ app_environment }}

server:
  host: {{ server_host }}
  port: {{ server_port }}
  workers: {{ worker_count }}

database:
  host: {{ db_host }}
  port: {{ db_port }}
  name: {{ db_name }}
  user: {{ db_user }}
  # Password managed by Ansible Vault

logging:
  level: {{ log_level | upper }}         # => Filter: convert to uppercase
  file: {{ log_file | default('/var/log/app.log') }}  # => Default value
```

**Playbook**:

```yaml
---
# template_basics.yml
- name: Jinja2 Template Basics
  hosts: localhost
  gather_facts: true

  vars:
    app_name: MyApplication
    app_version: "2.1.0"
    app_environment: production
    server_host: 0.0.0.0
    server_port: 8080
    worker_count: 4
    db_host: db.example.com
    db_port: 5432
    db_name: appdb
    db_user: appuser
    log_level: info
    # log_file not defined -> will use default

  tasks:
    - name: Render application configuration
      ansible.builtin.template:
        src: app_config.yml.j2 # => Template file (relative to playbook)
        dest: /tmp/app_config.yml # => Destination on target
        mode: "0644"
      # => Renders template with variables and copies to destination
      # => changed: [localhost] (if content differs)

    - name: Display rendered configuration
      ansible.builtin.command:
        cmd: cat /tmp/app_config.yml
      register: config_content

    - name: Show configuration
      ansible.builtin.debug:
        msg: "{{ config_content.stdout }}"
      # => Shows fully rendered configuration file
```

**Run**: `ansible-playbook template_basics.yml`

**Rendered output**:

```yaml
# Application Configuration
# Generated by Ansible on 2024-01-15T10:30:00Z

application:
  name: MyApplication
  version: 2.1.0
  environment: production

server:
  host: 0.0.0.0
  port: 8080
  workers: 4

database:
  host: db.example.com
  port: 5432
  name: appdb
  user: appuser

logging:
  level: INFO
  file: /var/log/app.log
```

**Key Takeaway**: Jinja2 templates use `{{ variable }}` for substitution, `{{ var | filter }}` for transformations. The `default()` filter provides fallback values for undefined variables. Templates render on control node, so targets don't need Jinja2 installed. Use templates for configuration files that vary by environment or host.

---

### Example 39: Jinja2 Conditionals and Loops

Jinja2 supports control structures: conditionals (`{% if %}`), loops (`{% for %}`), and blocks. Generate dynamic configuration based on variables and facts.

**Code**:

**`templates/nginx_site.conf.j2`**:

```jinja2
# Nginx Site Configuration for {{ site_name }}

server {
    listen {{ http_port }};
    server_name {{ server_name }};

    {% if enable_ssl %}
    # SSL Configuration
    listen {{ https_port }} ssl;
    ssl_certificate {{ ssl_cert_path }};
    ssl_certificate_key {{ ssl_key_path }};
    ssl_protocols TLSv1.2 TLSv1.3;
    {% endif %}

    root {{ document_root }};
    index index.html index.php;

    # Custom locations
    {% for location in custom_locations %}
    location {{ location.path }} {
        {% if location.type == 'proxy' %}
        proxy_pass {{ location.backend }};
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        {% elif location.type == 'static' %}
        alias {{ location.root }};
        {% endif %}
    }
    {% endfor %}

    # Default location
    location / {
        try_files $uri $uri/ =404;
    }

    # PHP processing (conditional)
    {% if enable_php %}
    location ~ \.php$ {
        fastcgi_pass unix:/var/run/php/php{{ php_version }}-fpm.sock;
        fastcgi_index index.php;
        include fastcgi_params;
        fastcgi_param SCRIPT_FILENAME $document_root$fastcgi_script_name;
    }
    {% endif %}

    # Access log
    access_log /var/log/nginx/{{ site_name }}_access.log;
    error_log /var/log/nginx/{{ site_name }}_error.log;
}
```

**Playbook**:

```yaml
---
# template_conditionals.yml
- name: Jinja2 Conditionals and Loops
  hosts: localhost
  gather_facts: false

  vars:
    site_name: example.com
    server_name: www.example.com
    http_port: 80
    https_port: 443
    enable_ssl: true
    ssl_cert_path: /etc/ssl/certs/example.com.crt
    ssl_key_path: /etc/ssl/private/example.com.key
    document_root: /var/www/example.com
    enable_php: true
    php_version: "8.1"

    custom_locations:
      - path: /api
        type: proxy
        backend: http://localhost:3000
      - path: /admin
        type: proxy
        backend: http://localhost:4000
      - path: /static
        type: static
        root: /var/www/static

  tasks:
    - name: Render nginx site configuration
      ansible.builtin.template:
        src: nginx_site.conf.j2
        dest: /tmp/example.com.conf
        mode: "0644"
      # => Renders template with conditionals and loops

    - name: Display configuration
      ansible.builtin.command:
        cmd: cat /tmp/example.com.conf
      register: nginx_config

    - name: Show nginx config
      ansible.builtin.debug:
        msg: "{{ nginx_config.stdout }}"
```

**Run**: `ansible-playbook template_conditionals.yml`

**Key Takeaway**: Use `{% if condition %}` for conditional blocks, `{% for item in list %}` for loops. Jinja2 control structures use `{% %}` delimiters. Support nested conditionals and loops for complex configuration generation. Template logic enables single template file to generate configurations for multiple environments.

---

### Example 40: Jinja2 Filters and Tests

Jinja2 filters transform data (string manipulation, list operations, math). Tests check conditions (variable type, existence, truthiness).

**Code**:

**`templates/advanced_config.yml.j2`**:

```jinja2
# Advanced Configuration with Filters and Tests

# String filters
app_name_upper: {{ app_name | upper }}                    # => Convert to uppercase
app_name_lower: {{ app_name | lower }}                    # => Convert to lowercase
app_slug: {{ app_name | lower | replace(' ', '-') }}     # => Chain filters

# Default values
optional_value: {{ optional_var | default('fallback') }}  # => Use default if undefined
database_port: {{ db_port | default(5432) }}              # => Default for number

# List filters
total_hosts: {{ groups['webservers'] | length }}          # => Count list items
first_host: {{ groups['webservers'] | first }}            # => First item
last_host: {{ groups['webservers'] | last }}              # => Last item
sorted_hosts: {{ groups['webservers'] | sort }}           # => Sort list

# Dictionary filters
all_vars: {{ hostvars[inventory_hostname] | to_nice_json }}  # => Pretty JSON
compact_vars: {{ hostvars[inventory_hostname] | to_json }}   # => Compact JSON
yaml_vars: {{ hostvars[inventory_hostname] | to_nice_yaml }} # => YAML format

# Math filters
double_port: {{ server_port | int * 2 }}                  # => Integer math
memory_gb: {{ (total_memory_mb | int / 1024) | round(2) }}  # => Division and rounding

# Type conversion
port_string: "{{ server_port | string }}"                 # => Convert to string
bool_value: {{ "yes" | bool }}                            # => String to boolean

# Tests (conditional checks)
{% if optional_var is defined %}
optional_is_defined: true
{% endif %}

{% if optional_var is undefined %}
optional_is_undefined: true
{% endif %}

{% if server_port is number %}
port_is_number: true
{% endif %}

{% if app_name is string %}
name_is_string: true
{% endif %}

{% if worker_list is iterable %}
workers_is_list: true
{% endif %}

# Version comparison (test)
{% if app_version is version('2.0', '>=') %}
version_meets_requirement: true
{% endif %}

# List operations
unique_list: {{ duplicate_list | unique }}                # => Remove duplicates
joined_string: {{ string_list | join(', ') }}            # => Join with separator
```

**Playbook**:

```yaml
---
# template_filters.yml
- name: Jinja2 Filters and Tests
  hosts: localhost
  gather_facts: false

  vars:
    app_name: "My Application"
    app_version: "2.5.0"
    server_port: 8080
    total_memory_mb: 8192
    worker_list: [1, 2, 3, 4]
    duplicate_list: [1, 2, 2, 3, 3, 3]
    string_list: ["apple", "banana", "cherry"]
    # optional_var intentionally not defined

  tasks:
    - name: Render configuration with filters
      ansible.builtin.template:
        src: advanced_config.yml.j2
        dest: /tmp/advanced_config.yml
        mode: "0644"

    - name: Display rendered configuration
      ansible.builtin.command:
        cmd: cat /tmp/advanced_config.yml
      register: config

    - name: Show configuration
      ansible.builtin.debug:
        msg: "{{ config.stdout }}"
```

**Run**: `ansible-playbook template_filters.yml`

**Common filters**:

- String: `upper`, `lower`, `capitalize`, `title`, `replace`
- List: `first`, `last`, `length`, `sort`, `unique`, `join`
- Dict: `to_json`, `to_nice_json`, `to_yaml`, `to_nice_yaml`
- Math: `int`, `float`, `round`, `abs`
- Default: `default(value)`, `default(value, true)` (default even if empty)

**Common tests**:

- Existence: `is defined`, `is undefined`
- Type: `is string`, `is number`, `is iterable`, `is mapping`
- Value: `is true`, `is false`, `is none`
- Comparison: `is version('1.0', '>=')`, `is equalto(value)`

**Key Takeaway**: Filters transform data inline (`{{ var | filter }}`), tests check conditions (`{% if var is test %}`). Chain filters with `|` for complex transformations. Use `default()` filter extensively to handle undefined variables gracefully. Filters and tests enable sophisticated template logic without external preprocessing.

---

### Example 41: Template Whitespace Control

Jinja2 whitespace control prevents unwanted blank lines and indentation in generated files. Use `-` trimming operators for clean output.

**Code**:

**`templates/whitespace_example.j2`**:

```jinja2
# Without whitespace control (creates blank lines)
{% for item in items %}
{{ item }}
{% endfor %}
# => Output has blank lines between items and after loop

# With whitespace control (removes blank lines)
{% for item in items -%}
{{ item }}
{% endfor -%}
# => Output has no blank lines (- trims newlines)

# Trim left whitespace
{%- if condition %}
content here
{% endif %}
# => Removes newline before if block

# Trim right whitespace
{% if condition -%}
content here
{% endif %}
# => Removes newline after if block

# Trim both sides
{%- if condition -%}
content here
{%- endif -%}
# => Removes newlines on both sides of block

# Practical example: clean list generation
servers:
{%- for server in server_list %}
  - name: {{ server.name }}
    ip: {{ server.ip }}
{%- endfor %}
# => No blank line after list (- after endfor)

# Example: conditional with clean formatting
database:
  host: {{ db_host }}
  port: {{ db_port }}
{%- if db_ssl_enabled %}
  ssl: true
{%- endif %}
# => No blank line if ssl block is skipped
```

**Playbook**:

```yaml
---
# template_whitespace.yml
- name: Template Whitespace Control
  hosts: localhost
  gather_facts: false

  vars:
    items: [item1, item2, item3]
    condition: true
    server_list:
      - name: web1
        ip: 192.168.1.10
      - name: web2
        ip: 192.168.1.11
    db_host: localhost
    db_port: 5432
    db_ssl_enabled: true

  tasks:
    - name: Render template with whitespace control
      ansible.builtin.template:
        src: whitespace_example.j2
        dest: /tmp/whitespace_demo.txt
        mode: "0644"
      # => Renders clean output without extra blank lines

    - name: Display rendered file
      ansible.builtin.command:
        cmd: cat /tmp/whitespace_demo.txt
      register: output

    - name: Show output
      ansible.builtin.debug:
        msg: "{{ output.stdout }}"
```

**Whitespace control syntax**:

- `{%- ... %}` → Trim whitespace before block
- `{% ... -%}` → Trim whitespace after block
- `{%- ... -%}` → Trim whitespace on both sides
- `{{- ... }}` → Trim before variable
- `{{ ... -}}` → Trim after variable

**Run**: `ansible-playbook template_whitespace.yml`

**Key Takeaway**: Use `-` operator to control whitespace in template output. Place `-` inside delimiters adjacent to where trimming should occur. Critical for generating clean configuration files like YAML or JSON where whitespace affects parsing. Without whitespace control, templates produce files with excessive blank lines.

---

### Example 42: Template Macros and Inheritance

Jinja2 macros define reusable template fragments. Template inheritance enables base templates extended by child templates.

**Code**:

**`templates/macros.j2`**:

```jinja2
{# Define reusable macros #}

{# Macro: Generate server block #}
{% macro server_block(name, host, port) -%}
server {
    listen {{ port }};
    server_name {{ name }};
    location / {
        proxy_pass http://{{ host }}:{{ port }};
    }
}
{%- endmacro %}

{# Macro: Generate database connection string #}
{% macro db_connection(type, host, port, name, user) -%}
{{ type }}://{{ user }}@{{ host }}:{{ port }}/{{ name }}
{%- endmacro %}

{# Macro: Generate logging configuration #}
{% macro logging_config(level, file) -%}
logging:
  level: {{ level | upper }}
  handlers:
    file:
      filename: {{ file }}
      maxBytes: 10485760
      backupCount: 5
{%- endmacro %}
```

**`templates/base.conf.j2`** (base template):

```jinja2
# Base Configuration Template
# Environment: {{ environment }}

{% block header %}
# Default Header
# Generated: {{ ansible_date_time.iso8601 }}
{% endblock %}

{% block application %}
# Application section must be defined in child template
{% endblock %}

{% block database %}
# Database section must be defined in child template
{% endblock %}

{% block footer %}
# Default Footer
{% endblock %}
```

**`templates/app.conf.j2`** (child template):

```jinja2
{% extends "base.conf.j2" %}                              {# Inherit from base #}

{% import "macros.j2" as macros %}                        {# Import macros #}

{% block header %}
# Application Configuration
# Name: {{ app_name }}
# Version: {{ app_version }}
{% endblock %}

{% block application %}
application:
  name: {{ app_name }}
  port: {{ app_port }}

  # Use macro for server blocks
  {% for server in app_servers %}
  {{ macros.server_block(server.name, server.host, server.port) }}
  {% endfor %}
{% endblock %}

{% block database %}
database:
  # Use macro for connection string
  url: {{ macros.db_connection('postgresql', db_host, db_port, db_name, db_user) }}

  # Use macro for logging
  {{ macros.logging_config('info', '/var/log/app.log') }}
{% endblock %}

{% block footer %}
# End of {{ app_name }} Configuration
{% endblock %}
```

**Playbook**:

```yaml
---
# template_macros.yml
- name: Template Macros and Inheritance
  hosts: localhost
  gather_facts: true

  vars:
    environment: production
    app_name: MyApp
    app_version: "3.0.0"
    app_port: 8080
    app_servers:
      - name: api.example.com
        host: localhost
        port: 3000
      - name: web.example.com
        host: localhost
        port: 4000
    db_host: db.example.com
    db_port: 5432
    db_name: appdb
    db_user: appuser

  tasks:
    - name: Render configuration with macros and inheritance
      ansible.builtin.template:
        src: app.conf.j2 # => Child template
        dest: /tmp/app.conf
        mode: "0644"
      # => Extends base template and imports macros

    - name: Display rendered configuration
      ansible.builtin.command:
        cmd: cat /tmp/app.conf
      register: config

    - name: Show configuration
      ansible.builtin.debug:
        msg: "{{ config.stdout }}"
```

**Run**: `ansible-playbook template_macros.yml`

**Key Takeaway**: Macros (`{% macro name(args) %}`) define reusable template functions—reduce duplication and improve maintainability. Template inheritance (`{% extends %}` and `{% block %}`) enables base templates with overridable sections. Import macros with `{% import "file.j2" as namespace %}`. This pattern scales to complex multi-environment configurations.

---

## Group 10: Ansible Vault

### Example 43: Vault Basics

Ansible Vault encrypts sensitive data (passwords, API keys, certificates) in version control. Encrypted files remain YAML-readable but content is protected.

**Code**:

**Create encrypted file**:

```bash
# Create new encrypted file with password prompt
ansible-vault create secrets.yml

# Enter vault password, then edit content:
```

**`secrets.yml` (encrypted)**:

```yaml
---
db_password: SuperSecretPassword123
api_key: sk-1234567890abcdef
ssl_cert_password: CertPassword456
admin_password: AdminPass789
```

**View encrypted file**:

```bash
# View encrypted file content
ansible-vault view secrets.yml
# => Prompts for password, displays decrypted content

# Edit encrypted file
ansible-vault edit secrets.yml
# => Opens editor with decrypted content, re-encrypts on save
```

**Playbook using vault**:

```yaml
---
# vault_basic.yml
- name: Use Ansible Vault
  hosts: localhost
  gather_facts: false

  vars_files:
    - secrets.yml # => Load encrypted variables file
  # => Variables decrypted automatically during playbook execution

  vars:
    db_host: localhost # => Non-sensitive variables in playbook
    db_port: 5432
    db_name: appdb
    db_user: appuser
    # db_password loaded from secrets.yml (encrypted)

  tasks:
    - name: Display database connection info (password masked)
      ansible.builtin.debug:
        msg: |
          Host: {{ db_host }}:{{ db_port }}
          Database: {{ db_name }}
          User: {{ db_user }}
      # => Shows non-sensitive info only

    - name: Use password in connection (example)
      ansible.builtin.debug:
        msg: "Connecting with password {{ db_password }}"
      no_log: true # => Prevent password from appearing in logs
      # => Task executes but output suppressed
```

**Run with vault password**:

```bash
# Prompt for vault password
ansible-playbook vault_basic.yml --ask-vault-pass

# Use password file (avoid interactive prompt)
ansible-playbook vault_basic.yml --vault-password-file ~/.vault_pass

# Use environment variable
export ANSIBLE_VAULT_PASSWORD_FILE=~/.vault_pass
ansible-playbook vault_basic.yml
```

**Vault operations**:

```bash
# Encrypt existing file
ansible-vault encrypt existing_file.yml

# Decrypt file (remove encryption)
ansible-vault decrypt secrets.yml

# Change vault password
ansible-vault rekey secrets.yml

# Show encrypted file content
cat secrets.yml  # => Shows encrypted blob (safe to commit)
```

**Key Takeaway**: Ansible Vault encrypts sensitive variables in YAML files—safe to commit to version control. Use `--ask-vault-pass` or password files for decryption. Always use `no_log: true` on tasks displaying sensitive data to prevent password leaks in logs. Vault files remain YAML-parseable but content is AES256-encrypted.

---

### Example 44: Vault IDs for Multiple Passwords

Vault IDs enable multiple vault passwords in one playbook—separate credentials for different environments or security domains.

**Code**:

**Create files with different vault IDs**:

```bash
# Production secrets with 'prod' vault ID
ansible-vault create --vault-id prod@prompt prod_secrets.yml

# Staging secrets with 'staging' vault ID
ansible-vault create --vault-id staging@prompt staging_secrets.yml

# Database secrets with 'database' vault ID
ansible-vault create --vault-id database@prompt db_secrets.yml
```

**`prod_secrets.yml`** (encrypted with prod ID):

```yaml
---
environment: production
api_endpoint: https://api.prod.example.com
api_key: prod-key-abc123
```

**`staging_secrets.yml`** (encrypted with staging ID):

```yaml
---
environment: staging
api_endpoint: https://api.staging.example.com
api_key: staging-key-xyz789
```

**`db_secrets.yml`** (encrypted with database ID):

```yaml
---
db_master_password: MasterDBPass123
db_replication_key: ReplKey456
```

**Playbook**:

```yaml
---
# vault_ids.yml
- name: Multiple Vault IDs
  hosts: localhost
  gather_facts: false

  vars_files:
    - prod_secrets.yml # => Requires 'prod' vault password
    - staging_secrets.yml # => Requires 'staging' vault password
    - db_secrets.yml # => Requires 'database' vault password

  tasks:
    - name: Display environment configuration
      ansible.builtin.debug:
        msg: |
          Prod API: {{ api_endpoint }} (from prod_secrets.yml)
          DB Password: {{ db_master_password }} (from db_secrets.yml)
      no_log: true
```

**Create password files**:

```bash
# ~/.vault_pass_prod
echo "prod-password" > ~/.vault_pass_prod
chmod 600 ~/.vault_pass_prod

# ~/.vault_pass_staging
echo "staging-password" > ~/.vault_pass_staging
chmod 600 ~/.vault_pass_staging

# ~/.vault_pass_database
echo "database-password" > ~/.vault_pass_database
chmod 600 ~/.vault_pass_database
```

**Run with multiple vault IDs**:

```bash
# Prompt for each vault password
ansible-playbook vault_ids.yml \
  --vault-id prod@prompt \
  --vault-id staging@prompt \
  --vault-id database@prompt

# Use password files
ansible-playbook vault_ids.yml \
  --vault-id prod@~/.vault_pass_prod \
  --vault-id staging@~/.vault_pass_staging \
  --vault-id database@~/.vault_pass_database

# Mix prompt and file
ansible-playbook vault_ids.yml \
  --vault-id prod@prompt \
  --vault-id staging@~/.vault_pass_staging
```

**View file with specific vault ID**:

```bash
ansible-vault view --vault-id prod@prompt prod_secrets.yml
ansible-vault edit --vault-id database@~/.vault_pass_database db_secrets.yml
```

**Key Takeaway**: Vault IDs enable multiple passwords in one playbook—useful for separating prod/staging credentials or security domains. Use `--vault-id <label>@<source>` format where source is `prompt` or path to password file. Each encrypted file tagged with vault ID during creation. This pattern enables role-based access control where different teams have different vault passwords.

---

### Example 45: Inline Encrypted Variables

Encrypt individual variables inline instead of entire files. Useful for mixed sensitive/non-sensitive variable files.

**Code**:

**Encrypt single string**:

```bash
# Encrypt string value
ansible-vault encrypt_string 'SuperSecretPassword123' --name 'db_password'
# => Outputs encrypted string in YAML format

# Output:
# db_password: !vault |
#           $ANSIBLE_VAULT;1.1;AES256
#           66386439653264336462626566653063336164663966303231363934653561363964363833313662
#           ...encrypted content...
```

**`vars.yml`** (mixed encrypted and plaintext):

```yaml
---
# Non-sensitive variables (plaintext)
db_host: localhost
db_port: 5432
db_name: appdb
db_user: appuser

# Sensitive variable (encrypted inline)
db_password: !vault |
  $ANSIBLE_VAULT;1.1;AES256
  66386439653264336462626566653063336164663966303231363934653561363964363833313662
  3431626338623034303037646233396431346564663936310a613034343933343462373465373738
  62376662626533643732333461303639626533633131373635373832336531373162366534363464
  3561373633613763360a313331613031656561623332353332613235376565353966383334646364
  3566

# Another encrypted variable
api_key: !vault |
  $ANSIBLE_VAULT;1.1;AES256
  39653561363964363833313662626566653063336164663966303231363934653561363964363833
  ...encrypted content...
```

**Playbook**:

```yaml
---
# inline_vault.yml
- name: Inline Encrypted Variables
  hosts: localhost
  gather_facts: false

  vars_files:
    - vars.yml # => Mixed plaintext and encrypted vars

  tasks:
    - name: Display configuration
      ansible.builtin.debug:
        msg: |
          Host: {{ db_host }}:{{ db_port }}
          Database: {{ db_name }}
          User: {{ db_user }}
      # => Shows plaintext variables

    - name: Use encrypted password
      ansible.builtin.debug:
        msg: "Password is {{ db_password | length }} characters"
      no_log: true # => Don't log actual password
      # => Uses decrypted password in task
```

**Create inline encrypted variable**:

```bash
# Encrypt string and copy to clipboard
ansible-vault encrypt_string 'MySecret123' --name 'secret_var' \
  --vault-id prod@~/.vault_pass_prod

# Encrypt from stdin (for long values)
echo -n 'LongSecretValue' | \
  ansible-vault encrypt_string --stdin-name 'long_secret'
```

**Run**: `ansible-playbook inline_vault.yml --ask-vault-pass`

**Key Takeaway**: Inline encrypted variables (`!vault |`) enable mixing sensitive and non-sensitive data in one file—improves readability compared to fully encrypted files. Use `encrypt_string` to generate encrypted values. Inline encryption works with any vault ID. This pattern is ideal for variable files where only a few values are sensitive.

---

### Example 46: Vault Best Practices

Production-ready vault usage patterns: password management, file organization, access control, and security.

**Code**:

**Directory structure**:

```
inventory/
  production/
    group_vars/
      all/
        vars.yml                         # => Non-sensitive variables
        vault.yml                        # => Encrypted sensitive variables
      webservers/
        vars.yml
        vault.yml
      databases/
        vars.yml
        vault.yml

.vault_pass_prod                         # => Production vault password file
.vault_pass_staging                      # => Staging vault password file
.gitignore                               # => Exclude password files from git
```

**`.gitignore`**:

```
# Vault password files
.vault_pass*
vault_password.txt
*.vault_pass

# Backup files from vault edit
*.yml.backup
*~
```

**`group_vars/all/vars.yml`** (non-sensitive):

```yaml
---
# Non-sensitive configuration
ntp_server: time.example.com
log_level: info
backup_enabled: true
monitoring_enabled: true
```

**`group_vars/all/vault.yml`** (encrypted):

```yaml
---
# Encrypted sensitive variables (prefix with vault_)
vault_db_password: SuperSecret123
vault_api_key: sk-1234567890
vault_ssl_key: |
  -----BEGIN PRIVATE KEY-----
  MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQC...
  -----END PRIVATE KEY-----
```

**`group_vars/all/all.yml`** (variable mapping):

```yaml
---
# Map vault variables to usable names
db_password: "{{ vault_db_password }}" # => Indirect reference
api_key: "{{ vault_api_key }}"
ssl_private_key: "{{ vault_ssl_key }}"
```

**Password file setup**:

```bash
# Create password file with restricted permissions
echo "production-vault-password" > .vault_pass_prod
chmod 600 .vault_pass_prod

# Configure Ansible to use password file by default
cat >> ansible.cfg <<EOF
[defaults]
vault_password_file = .vault_pass_prod
EOF
```

**`ansible.cfg`**:

```ini
[defaults]
# Default vault password file (override with --vault-password-file)
vault_password_file = .vault_pass_prod

# Require vault password for encrypted files
vault_encrypt_identity_list = default

# Log encryption
no_log = True                            # => Global no_log for sensitive tasks
```

**Playbook with vault best practices**:

```yaml
---
# vault_best_practices.yml
- name: Vault Best Practices
  hosts: localhost
  gather_facts: false

  # Load variables (vars.yml + vault.yml automatically loaded from group_vars)

  tasks:
    - name: Use sensitive variable
      ansible.builtin.debug:
        msg: "Connecting to database"
      no_log: true # => Always use no_log with sensitive data
      # => Task uses db_password from vault.yml indirectly

    - name: Deploy configuration with secrets
      ansible.builtin.template:
        src: app_config.j2
        dest: /tmp/app_config.yml
        mode: "0600" # => Restrictive permissions for config with secrets
      no_log: true # => Don't log file content
```

**Vault rotation procedure**:

```bash
# 1. Create new vault password file
echo "new-production-password" > .vault_pass_prod_new

# 2. Rekey all vault files
find inventory/ -name 'vault.yml' -exec \
  ansible-vault rekey --vault-password-file .vault_pass_prod \
  --new-vault-password-file .vault_pass_prod_new {} \;

# 3. Replace old password file
mv .vault_pass_prod_new .vault_pass_prod
chmod 600 .vault_pass_prod

# 4. Test decryption
ansible-vault view inventory/production/group_vars/all/vault.yml
```

**Key Takeaway**: Separate sensitive (`vault.yml`) from non-sensitive (`vars.yml`) variables for clarity. Prefix vault variables with `vault_` and map to clean names in `all.yml`. Store vault password files outside repository with 600 permissions. Use `no_log: true` on all tasks handling sensitive data. Implement vault password rotation procedure. Use group_vars hierarchy for environment-specific secrets. This structure scales to hundreds of hosts and multiple environments.

---

## Group 11: Error Handling & Blocks

### Example 47: Failed When and Changed When

Control task success/failure criteria and changed status reporting with `failed_when` and `changed_when` directives.

**Code**:

```yaml
---
# failed_changed_when.yml
- name: Failed When and Changed When
  hosts: localhost
  gather_facts: false

  tasks:
    # Default behavior: command always reports changed
    - name: Run command (always changed)
      ansible.builtin.command:
        cmd: echo "test"
      register: result
      # => changed: [localhost] (command module default)

    - name: Check default status
      ansible.builtin.debug:
        msg: "Task changed: {{ result.changed }}"
      # => Output: Task changed: True

    # Control changed status with changed_when
    - name: Command with changed_when never
      ansible.builtin.command:
        cmd: uptime
      changed_when: false # => Always report OK (never changed)
      # => ok: [localhost] (forced to not changed)

    # Conditional changed status
    - name: Command with conditional changed
      ansible.builtin.command:
        cmd: grep "pattern" /tmp/file.txt
      register: grep_result
      changed_when: grep_result.rc == 0 # => Changed only if pattern found
      failed_when: grep_result.rc not in [0, 1] # => Fail on errors (not "not found")
      # => ok: [localhost] if pattern not found (rc=1)
      # => changed: [localhost] if pattern found (rc=0)
      # => failed: [localhost] if error (rc=2)

    # Complex failed_when condition
    - name: Check application status
      ansible.builtin.shell:
        cmd: |
          curl -s http://localhost:8080/health || echo "DOWN"
      register: health_check
      changed_when: false # => Health check never changes system
      failed_when: >
        health_check.rc != 0 or
        'DOWN' in health_check.stdout or
        'error' in health_check.stdout | lower
      # => Fail if curl fails OR output contains "DOWN" or "error"

    # Never fail (always succeed)
    - name: Task that always succeeds
      ansible.builtin.command:
        cmd: /usr/local/bin/optional-script.sh
      register: optional_result
      failed_when: false # => Never fail regardless of return code
      changed_when: optional_result.rc == 0 # => Changed only if successful
      # => ok: [localhost] even if script returns non-zero

    # Validate output content
    - name: Validate configuration file
      ansible.builtin.command:
        cmd: cat /etc/app/config.yml
      register: config_content
      changed_when: false
      failed_when: "'debug: true' in config_content.stdout"
      # => Fail if debug mode enabled in config (safety check)

    # Multiple failure conditions
    - name: Check disk space
      ansible.builtin.shell:
        cmd: df -h / | awk 'NR==2 {print $5}' | sed 's/%//'
      register: disk_usage
      changed_when: false
      failed_when:
        - disk_usage.stdout | int > 90 # => Fail if > 90% used
        - disk_usage.rc != 0 # => Also fail if command errors
      # => Combines multiple failure conditions with implicit AND
```

**Run**: `ansible-playbook failed_changed_when.yml`

**Key Takeaway**: Use `changed_when: false` for read-only tasks (checks, queries) to prevent misleading change reports. Use `failed_when` to define success criteria based on output content or return codes—enables validation and safety checks. Combine both directives for precise task status control. This pattern enables idempotent playbooks where task status accurately reflects system state changes.

---

### Example 48: Ignore Errors and Error Recovery

Handle task failures gracefully with `ignore_errors`, conditional error handling, and recovery patterns.

**Code**:

```yaml
---
# error_handling.yml
- name: Ignore Errors and Error Recovery
  hosts: localhost
  gather_facts: false

  tasks:
    # Ignore errors (continue playbook)
    - name: Optional task that might fail
      ansible.builtin.command:
        cmd: /usr/local/bin/optional-tool --check
      ignore_errors: true # => Continue even if task fails
      register: optional_result
      # => failed: [localhost] (task fails but playbook continues)

    - name: Check if optional task succeeded
      ansible.builtin.debug:
        msg: "Optional tool {{ 'succeeded' if optional_result.rc == 0 else 'failed' }}"
      # => Shows task result without stopping playbook

    # Conditional error ignore
    - name: Task with conditional error handling
      ansible.builtin.command:
        cmd: test -f /tmp/important_file.txt
      register: file_check
      failed_when: false # => Never fail (alternative to ignore_errors)
      # => ok: [localhost] regardless of file existence

    - name: Create file if missing
      ansible.builtin.file:
        path: /tmp/important_file.txt
        state: touch
      when: file_check.rc != 0 # => Create only if previous check failed
      # => Recovery action based on check result

    # Error recovery pattern
    - name: Try primary service
      ansible.builtin.uri:
        url: https://primary.example.com/api
        method: GET
      register: primary_result
      ignore_errors: true
      # => Try primary endpoint, don't fail if unreachable

    - name: Fallback to secondary service
      ansible.builtin.uri:
        url: https://secondary.example.com/api
        method: GET
      when: primary_result is failed # => Execute only if primary failed
      register: secondary_result
      # => Fallback to backup service

    - name: Fallback to tertiary service
      ansible.builtin.uri:
        url: https://tertiary.example.com/api
        method: GET
      when:
        - primary_result is failed # => Primary failed
        - secondary_result is failed # => Secondary also failed
      # => Last resort fallback

    # Fail playbook after recovery attempts
    - name: Fail if all services unavailable
      ansible.builtin.fail:
        msg: "All API endpoints are unavailable"
      when:
        - primary_result is failed
        - secondary_result is failed
        - tertiary_result is failed
      # => Explicit failure after exhausting recovery options

    # Retry pattern with ignore_errors
    - name: Attempt flaky operation
      ansible.builtin.command:
        cmd: /usr/local/bin/flaky-script.sh
      register: flaky_result
      ignore_errors: true
      # => First attempt (might fail)

    - name: Retry flaky operation
      ansible.builtin.command:
        cmd: /usr/local/bin/flaky-script.sh
      when: flaky_result is failed # => Retry if first attempt failed
      register: retry_result
      # => Second attempt

    - name: Final status
      ansible.builtin.debug:
        msg: "Operation {{ 'succeeded' if (flaky_result is success or retry_result is success) else 'failed after retry' }}"
```

**Run**: `ansible-playbook error_handling.yml`

**Key Takeaway**: Use `ignore_errors: true` to continue playbook execution after failures—essential for optional tasks or multi-path workflows. Combine with `register` and `is failed` test for error recovery patterns. Use `failed_when: false` as alternative to `ignore_errors` for more explicit control. Implement fallback chains for resilient automation. Always explicitly fail with `ansible.builtin.fail` after exhausting recovery options to prevent silent failures.

---

### Example 49: Block, Rescue, and Always

Blocks group tasks with unified error handling. `rescue` executes on failure, `always` executes regardless of outcome—similar to try/catch/finally.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    A["Block Section"] --> B{Success?}
    B -->|Success| C["Always Section"]
    B -->|Failure| D["Rescue Section"]
    D --> E{Rescue Success?}
    E -->|Success| C
    E -->|Failure| F["Playbook Fails"]
    C --> G["Continue Playbook"]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#fff
    style D fill:#CC78BC,color:#fff
    style C fill:#029E73,color:#fff
    style E fill:#DE8F05,color:#fff
    style F fill:#CA9161,color:#fff
    style G fill:#029E73,color:#fff
```

**Code**:

```yaml
---
# block_rescue_always.yml
- name: Block, Rescue, and Always
  hosts: localhost
  gather_facts: false

  tasks:
    # Basic block with rescue
    - name: Block example with error handling
      block:
        - name: Task that might fail
          ansible.builtin.command:
            cmd: /usr/bin/false # => Always fails (exit 1)
          # => failed: [localhost] (triggers rescue)

        - name: This task won't execute
          ansible.builtin.debug:
            msg: "Skipped due to previous failure"
          # => Skipped (block execution stops at first failure)

      rescue:
        - name: Handle failure
          ansible.builtin.debug:
            msg: "Block failed, executing rescue tasks"
          # => Executes when any block task fails

        - name: Recovery action
          ansible.builtin.file:
            path: /tmp/error_recovery.log
            state: touch
          # => Create error log file

      always:
        - name: Cleanup (always executes)
          ansible.builtin.debug:
            msg: "Cleanup executing regardless of success/failure"
          # => Always executes (even if rescue fails)

    # Practical example: Database backup with rollback
    - name: Database operation with rollback
      block:
        - name: Create backup snapshot
          ansible.builtin.command:
            cmd: pg_dump appdb > /tmp/backup.sql
          # => Backup before risky operation

        - name: Run database migration
          ansible.builtin.command:
            cmd: /usr/local/bin/migrate-database.sh
          register: migration_result
          # => Risky operation that might fail

        - name: Verify migration
          ansible.builtin.command:
            cmd: /usr/local/bin/verify-migration.sh
          # => Validation step

      rescue:
        - name: Restore from backup
          ansible.builtin.command:
            cmd: psql appdb < /tmp/backup.sql
          # => Rollback on failure

        - name: Notify failure
          ansible.builtin.debug:
            msg: "Migration failed, database restored from backup"

      always:
        - name: Remove backup file
          ansible.builtin.file:
            path: /tmp/backup.sql
            state: absent
          # => Cleanup backup regardless of outcome

    # Nested blocks
    - name: Nested block example
      block:
        - name: Outer block task
          ansible.builtin.debug:
            msg: "Outer block executing"

        - name: Nested block with own error handling
          block:
            - name: Inner task
              ansible.builtin.command:
                cmd: /usr/bin/false
              # => Fails, triggers inner rescue

          rescue:
            - name: Inner rescue
              ansible.builtin.debug:
                msg: "Inner rescue executing"
              # => Handles inner block failure

          # Note: Inner block failure does NOT trigger outer rescue
          # because inner rescue handled it successfully

      rescue:
        - name: Outer rescue (won't execute)
          ansible.builtin.debug:
            msg: "Outer rescue (not reached)"

      always:
        - name: Outer always
          ansible.builtin.debug:
            msg: "Outer always executing"
          # => Executes regardless of nested block outcome

    # Block with variables and conditions
    - name: Conditional block execution
      block:
        - name: Production-only task 1
          ansible.builtin.debug:
            msg: "Production task executing"

        - name: Production-only task 2
          ansible.builtin.debug:
            msg: "Another production task"

      when: environment == "production" # => Entire block conditional
      # => All block tasks skip if condition false

      vars:
        environment: staging # => Block-scoped variable
```

**Run**: `ansible-playbook block_rescue_always.yml`

**Key Takeaway**: Use `block`/`rescue`/`always` for structured error handling similar to try/catch/finally. Rescue section executes only on block failure, always section executes regardless of outcome. Blocks enable atomic operations with rollback—critical for database migrations, configuration updates, and deployments. Apply `when` conditions to entire blocks for efficiency. Nested blocks have independent error handling contexts.

---

### Example 50: Assertions and Validations

Use assertions to validate prerequisites, enforce invariants, and implement safety checks before dangerous operations.

**Code**:

```yaml
---
# assertions.yml
- name: Assertions and Validations
  hosts: localhost
  gather_facts: true

  vars:
    required_memory_mb: 2048
    required_disk_gb: 20
    allowed_environments: [development, staging, production]
    current_environment: production

  tasks:
    # Simple assertion
    - name: Assert Ansible version
      ansible.builtin.assert:
        that:
          - ansible_version.full is version('2.14', '>=')
        fail_msg: "Ansible 2.14+ required, found {{ ansible_version.full }}"
        success_msg: "Ansible version check passed"
      # => Fails playbook if condition not met

    # Multiple conditions (all must be true)
    - name: Assert system requirements
      ansible.builtin.assert:
        that:
          - ansible_memtotal_mb >= required_memory_mb
          - ansible_mounts | selectattr('mount', 'equalto', '/') | map(attribute='size_available') | first | int > required_disk_gb * 1024 * 1024 * 1024
          - ansible_processor_vcpus >= 2
        fail_msg: |
          System requirements not met:
          - Memory: {{ ansible_memtotal_mb }}MB (required: {{ required_memory_mb }}MB)
          - CPUs: {{ ansible_processor_vcpus }} (required: 2+)
        success_msg: "System requirements validated"
      # => Validates hardware prerequisites before deployment

    # Validate environment variable
    - name: Assert valid environment
      ansible.builtin.assert:
        that:
          - current_environment in allowed_environments
        fail_msg: "Invalid environment: {{ current_environment }}"
      # => Prevents deployment to unknown environments

    # Validate file exists before operation
    - name: Check if configuration exists
      ansible.builtin.stat:
        path: /etc/app/config.yml
      register: config_file

    - name: Assert configuration file exists
      ansible.builtin.assert:
        that:
          - config_file.stat.exists
          - config_file.stat.size > 0
        fail_msg: "Configuration file missing or empty"
      # => Fails early if config missing (before app deployment)

    # Validate service state
    - name: Check current service status
      ansible.builtin.systemd:
        name: nginx
      register: nginx_status
      check_mode: true # => Read-only check

    - name: Assert service requirements
      ansible.builtin.assert:
        that:
          - nginx_status.status.LoadState == "loaded"
          - nginx_status.status.SubState == "running"
        fail_msg: "Nginx not running properly"
      # => Validates service state before dependent operations

    # Validate database connection
    - name: Test database connectivity
      ansible.builtin.command:
        cmd: pg_isready -h localhost -p 5432
      register: db_check
      failed_when: false
      changed_when: false

    - name: Assert database accessible
      ansible.builtin.assert:
        that:
          - db_check.rc == 0
        fail_msg: "Database not accessible on localhost:5432"
      # => Validates connectivity before schema migrations

    # Validate variable types and format
    - name: Assert variable types
      ansible.builtin.assert:
        that:
          - app_port is defined
          - app_port is number
          - app_port >= 1024
          - app_port <= 65535
          - app_name is string
          - app_name | length > 0
        fail_msg: "Invalid variable types or values"
      vars:
        app_port: 8080
        app_name: "MyApp"
      # => Validates variable schemas before usage

    # Conditional assertions (only in production)
    - name: Production-specific checks
      ansible.builtin.assert:
        that:
          - ssl_enabled == true
          - debug_mode == false
          - monitoring_enabled == true
        fail_msg: "Production safety checks failed"
      when: current_environment == "production"
      vars:
        ssl_enabled: true
        debug_mode: false
        monitoring_enabled: true
      # => Enforces production safety requirements

    # Assert with quiet mode
    - name: Silent validation
      ansible.builtin.assert:
        that:
          - ansible_distribution in ['Ubuntu', 'Debian', 'RedHat', 'CentOS']
        quiet: true # => Only show output on failure
      # => Reduces output noise for successful validations
```

**Run**: `ansible-playbook assertions.yml`

**Key Takeaway**: Use `ansible.builtin.assert` to validate prerequisites before dangerous operations—prevents partial failures and corrupted state. Assertions fail fast, stopping playbook execution immediately when conditions aren't met. Use `that:` list for multiple conditions (implicit AND). Provide clear `fail_msg` for troubleshooting. Assertions document assumptions and make playbooks self-validating—critical for production automation.

---

## Group 12: Tags & Task Control

### Example 51: Task Tagging Basics

Tags enable selective task execution without modifying playbooks. Run subsets of tasks using `--tags` and `--skip-tags`.

**Code**:

```yaml
---
# task_tags.yml
- name: Task Tagging Basics
  hosts: localhost
  gather_facts: false

  tasks:
    # Single tag
    - name: Install packages
      ansible.builtin.package:
        name: nginx
        state: present
      tags: install # => Run with --tags install
      # => ansible-playbook playbook.yml --tags install

    # Multiple tags
    - name: Configure nginx
      ansible.builtin.template:
        src: nginx.conf.j2
        dest: /etc/nginx/nginx.conf
      tags:
        - configure # => First tag
        - nginx # => Second tag
      # => ansible-playbook playbook.yml --tags configure
      # => ansible-playbook playbook.yml --tags nginx (both work)

    # Tagged task with notification
    - name: Deploy application
      ansible.builtin.copy:
        src: app.tar.gz
        dest: /opt/app/
      tags: deploy
      notify: restart app

    # Special tag: always
    - name: Verify system requirements
      ansible.builtin.assert:
        that:
          - ansible_memtotal_mb >= 1024
      tags: always # => Always runs (unless --skip-tags always)
      # => Executes even with --tags deploy

    # Special tag: never
    - name: Dangerous operation
      ansible.builtin.command:
        cmd: rm -rf /tmp/cache/*
      tags: never # => Never runs (unless explicitly --tags never)
      # => Skipped by default, requires --tags never

    # Untagged task
    - name: Create log directory
      ansible.builtin.file:
        path: /var/log/app
        state: directory
      # => Runs when no --tags specified (runs all untagged + always)

    # Multiple roles of same tag
    - name: Update configuration
      ansible.builtin.template:
        src: app.conf.j2
        dest: /etc/app/config.conf
      tags:
        - configure
        - update
      # => Runs with either --tags configure OR --tags update

  handlers:
    - name: restart app
      ansible.builtin.service:
        name: app
        state: restarted
      tags: always # => Handler inherits tags from notifying task
      # => Executes if notifying task runs and changes
```

**Run with tags**:

```bash
# Run only install tasks
ansible-playbook task_tags.yml --tags install

# Run multiple tags
ansible-playbook task_tags.yml --tags "install,configure"

# Run all except specific tags
ansible-playbook task_tags.yml --skip-tags deploy

# Run only never tasks
ansible-playbook task_tags.yml --tags never

# List available tags
ansible-playbook task_tags.yml --list-tags

# List tasks with specific tag
ansible-playbook task_tags.yml --tags deploy --list-tasks

# Combine tags and skip-tags
ansible-playbook task_tags.yml --tags configure --skip-tags never
```

**Key Takeaway**: Tags enable surgical playbook execution without code changes. Use `always` tag for tasks that must run every time (validations, checks). Use `never` tag for dangerous operations requiring explicit opt-in. Tasks can have multiple tags for flexible grouping. Handlers inherit tags from notifying tasks. Tags are a runtime feature—playbook code remains unchanged.

---

### Example 52: Role and Play Tagging

Apply tags to entire plays or roles for coarse-grained control. Tags cascade to all tasks within tagged play/role.

**Code**:

**Role with tags** (`roles/webserver/tasks/main.yml`):

```yaml
---
# Webserver role tasks
- name: Install web server
  ansible.builtin.package:
    name: nginx
    state: present
  tags: install # => Task-level tag

- name: Configure web server
  ansible.builtin.template:
    src: nginx.conf.j2
    dest: /etc/nginx/nginx.conf
  tags: configure

- name: Start web server
  ansible.builtin.service:
    name: nginx
    state: started
  tags: service
```

**Playbook with role and play tags**:

```yaml
---
# role_play_tags.yml
# Play 1: Tagged play
- name: Setup Phase
  hosts: localhost
  gather_facts: true
  tags: setup # => Play-level tag (applies to all tasks)
  # => All tasks in this play inherit 'setup' tag

  tasks:
    - name: Update package cache
      ansible.builtin.apt:
        update_cache: true
      when: ansible_os_family == "Debian"
      # => Inherits 'setup' tag from play

    - name: Install common packages
      ansible.builtin.package:
        name:
          - curl
          - vim
        state: present
      # => Also inherits 'setup' tag

# Play 2: Role with tags
- name: Deploy Web Server
  hosts: localhost
  become: true
  tags: webserver # => Play-level tag

  roles:
    - role: webserver
      tags: web # => Role-level tag (additional to play tag)
  # => All role tasks have both 'webserver' (play) and 'web' (role) tags
  # => Can run with --tags webserver OR --tags web

# Play 3: Multiple roles with different tags
- name: Deploy Application Stack
  hosts: localhost
  become: true

  roles:
    - role: database
      tags: database # => Only database role

    - role: cache
      tags: cache # => Only cache role

    - role: application
      tags:
        - app # => Multiple role tags
        - deploy

# Play 4: Mixed tagged and untagged tasks
- name: Configuration Phase
  hosts: localhost
  tags: config

  tasks:
    - name: Tagged task within tagged play
      ansible.builtin.debug:
        msg: "Has both 'config' (play) and 'important' (task) tags"
      tags: important
      # => Has tags: config, important

    - name: Untagged task in tagged play
      ansible.builtin.debug:
        msg: "Only has 'config' tag from play"
      # => Has tags: config

# Play 5: Tag inheritance demonstration
- name: Tag Inheritance
  hosts: localhost
  tags:
    - phase1
    - initialization

  tasks:
    - name: Task with additional tags
      ansible.builtin.debug:
        msg: "Multiple inherited and task tags"
      tags:
        - step1
        - critical
      # => Has tags: phase1, initialization, step1, critical
```

**Run with various tag combinations**:

```bash
# Run entire setup phase
ansible-playbook role_play_tags.yml --tags setup

# Run webserver role (via play tag OR role tag)
ansible-playbook role_play_tags.yml --tags webserver
ansible-playbook role_play_tags.yml --tags web

# Run only database deployment
ansible-playbook role_play_tags.yml --tags database

# Run multiple roles
ansible-playbook role_play_tags.yml --tags "database,cache,app"

# Skip entire phase
ansible-playbook role_play_tags.yml --skip-tags config

# Run specific task tag across all plays
ansible-playbook role_play_tags.yml --tags install
```

**Tag inheritance hierarchy**:

1. Play tags → Apply to all tasks in play
2. Role tags → Apply to all tasks in role
3. Task tags → Apply to specific task
4. **Combined**: Task has all three levels

**Key Takeaway**: Tag plays for phase-level control (setup, deploy, cleanup), tag roles for component-level control (webserver, database), tag tasks for operation-level control (install, configure). Tags are additive—tasks inherit play and role tags plus their own task tags. This multi-level tagging enables flexible execution patterns from coarse-grained (entire plays) to fine-grained (specific tasks).

---

### Example 53: Tag-Based Workflows

Design playbooks with tag-based workflows for common operational scenarios: deployment, rollback, health checks, maintenance.

**Code**:

```yaml
---
# tag_workflows.yml
- name: Application Deployment Workflow
  hosts: localhost
  become: true
  gather_facts: true

  vars:
    app_version: "2.1.0"
    app_user: appuser

  tasks:
    # Pre-deployment checks (always run)
    - name: Verify system requirements
      ansible.builtin.assert:
        that:
          - ansible_memtotal_mb >= 2048
          - ansible_mounts | selectattr('mount', 'equalto', '/') | map(attribute='size_available') | first | int > 10000000000
      tags: always
      # => Runs with any tag combination

    # Backup phase
    - name: Create backup directory
      ansible.builtin.file:
        path: /backup/{{ ansible_date_time.date }}
        state: directory
      tags:
        - backup
        - rollback # => Also needed for rollback

    - name: Backup application
      ansible.builtin.command:
        cmd: tar czf /backup/{{ ansible_date_time.date }}/app-backup.tar.gz /opt/app
      tags:
        - backup
        - rollback
      # => ansible-playbook playbook.yml --tags backup

    # Deployment phase
    - name: Stop application
      ansible.builtin.service:
        name: app
        state: stopped
      tags: deploy
      # => ansible-playbook playbook.yml --tags deploy

    - name: Deploy new version
      ansible.builtin.unarchive:
        src: /tmp/app-{{ app_version }}.tar.gz
        dest: /opt/app
        owner: "{{ app_user }}"
      tags: deploy

    - name: Update configuration
      ansible.builtin.template:
        src: app.conf.j2
        dest: /etc/app/config.conf
      tags:
        - deploy
        - config # => Can update config independently

    - name: Run database migrations
      ansible.builtin.command:
        cmd: /opt/app/bin/migrate
      tags:
        - deploy
        - migration
      # => ansible-playbook playbook.yml --tags migration (run migrations only)

    - name: Start application
      ansible.builtin.service:
        name: app
        state: started
      tags:
        - deploy
        - restart # => Quick restart without full deploy
      # => ansible-playbook playbook.yml --tags restart

    # Health check phase
    - name: Wait for application startup
      ansible.builtin.wait_for:
        host: localhost
        port: 8080
        delay: 5
        timeout: 60
      tags:
        - deploy
        - health_check
        - verify
      # => ansible-playbook playbook.yml --tags health_check

    - name: Verify application health
      ansible.builtin.uri:
        url: http://localhost:8080/health
        return_content: true
      register: health_response
      tags:
        - deploy
        - health_check
        - verify

    - name: Assert healthy status
      ansible.builtin.assert:
        that:
          - health_response.status == 200
          - "'healthy' in health_response.content"
      tags:
        - deploy
        - health_check
        - verify

    # Rollback phase (never runs unless explicitly tagged)
    - name: Stop application (rollback)
      ansible.builtin.service:
        name: app
        state: stopped
      tags:
        - rollback
        - never # => Requires --tags rollback

    - name: Restore from backup
      ansible.builtin.unarchive:
        src: /backup/{{ ansible_date_time.date }}/app-backup.tar.gz
        dest: /
        remote_src: true
      tags:
        - rollback
        - never

    - name: Start application (rollback)
      ansible.builtin.service:
        name: app
        state: started
      tags:
        - rollback
        - never

    # Maintenance mode
    - name: Enable maintenance page
      ansible.builtin.copy:
        src: maintenance.html
        dest: /var/www/html/index.html
      tags:
        - maintenance
        - never
      # => ansible-playbook playbook.yml --tags maintenance

    - name: Disable maintenance page
      ansible.builtin.file:
        path: /var/www/html/index.html
        state: absent
      tags:
        - unmaintenance
        - never

    # Cleanup phase
    - name: Remove old backups
      ansible.builtin.find:
        paths: /backup
        age: 7d
        file_type: directory
      register: old_backups
      tags:
        - cleanup
        - never

    - name: Delete old backups
      ansible.builtin.file:
        path: "{{ item.path }}"
        state: absent
      loop: "{{ old_backups.files }}"
      tags:
        - cleanup
        - never
```

**Workflow execution scenarios**:

```bash
# Full deployment (backup + deploy + verify)
ansible-playbook tag_workflows.yml --tags "backup,deploy"

# Quick config update without full deploy
ansible-playbook tag_workflows.yml --tags config

# Restart application only
ansible-playbook tag_workflows.yml --tags restart

# Run health checks only
ansible-playbook tag_workflows.yml --tags health_check

# Emergency rollback
ansible-playbook tag_workflows.yml --tags rollback

# Enable maintenance mode
ansible-playbook tag_workflows.yml --tags maintenance

# Database migration only
ansible-playbook tag_workflows.yml --tags migration

# Cleanup old backups
ansible-playbook tag_workflows.yml --tags cleanup

# Deployment skipping health checks
ansible-playbook tag_workflows.yml --tags deploy --skip-tags health_check
```

**Key Takeaway**: Design playbooks with overlapping tags for different workflows. Use `never` tag for destructive operations (rollback, maintenance, cleanup). Combine `always` for prerequisites and validation. Tag grouping enables: full deployments (`backup,deploy`), quick operations (`restart`, `config`), maintenance tasks (`cleanup`, `maintenance`), and emergency procedures (`rollback`). This pattern creates playbooks that serve multiple operational needs without duplication.

---

### Example 54: Task Delegation and Run Once

Delegate tasks to different hosts or execute once across group. Control task execution context beyond inventory targeting.

**Code**:

```yaml
---
# delegation.yml
- name: Task Delegation and Run Once
  hosts: webservers # => Target host group
  gather_facts: true

  tasks:
    # Regular task (runs on all webservers)
    - name: Install nginx on webservers
      ansible.builtin.package:
        name: nginx
        state: present
      # => Executes on web1, web2, web3, ... (all webservers)

    # Delegate to specific host
    - name: Update load balancer configuration
      ansible.builtin.template:
        src: backend.conf.j2
        dest: /etc/haproxy/backends.conf
      delegate_to: loadbalancer.example.com # => Runs on loadbalancer, not webservers
      # => Executes once on loadbalancer for each webserver
      # => Has access to webserver variables/facts

    # Delegate to localhost (control node)
    - name: Generate report on control node
      ansible.builtin.copy:
        dest: /tmp/webserver_report.txt
        content: |
          Webserver: {{ inventory_hostname }}
          IP: {{ ansible_default_ipv4.address }}
          Memory: {{ ansible_memtotal_mb }}MB
      delegate_to: localhost # => Runs on control machine
      # => Creates report file locally for each webserver

    # Run once across all hosts
    - name: Database migration (run once)
      ansible.builtin.command:
        cmd: /usr/local/bin/migrate-database.sh
      run_once: true # => Executes on first host only
      # => Runs on web1 (first in group), skips web2, web3, ...

    # Run once with delegation
    - name: Notify external service (run once)
      ansible.builtin.uri:
        url: https://api.example.com/deployment
        method: POST
        body_format: json
        body:
          event: deployment_started
          timestamp: "{{ ansible_date_time.iso8601 }}"
      run_once: true
      delegate_to: localhost # => Runs once on control node
      # => Single API call regardless of webserver count

    # Local action (shorthand for delegate_to: localhost)
    - name: Create local backup directory
      ansible.builtin.file:
        path: /tmp/backups/{{ inventory_hostname }}
        state: directory
      delegate_to: localhost
      # => Creates directory on control node for each webserver

    # Delegate facts gathering
    - name: Gather facts from database server
      ansible.builtin.setup:
      delegate_to: db.example.com
      delegate_facts: true # => Store facts under db.example.com, not webserver
      # => Collects db.example.com facts accessible via hostvars['db.example.com']

    # Use delegated facts
    - name: Configure database connection
      ansible.builtin.template:
        src: db_config.j2
        dest: /etc/app/database.conf
        content: |
          host={{ hostvars['db.example.com'].ansible_default_ipv4.address }}
          port=5432
      # => Uses database server IP from delegated facts

    # Delegate with connection override
    - name: Execute on remote via specific connection
      ansible.builtin.command:
        cmd: kubectl get pods
      delegate_to: k8s-master.example.com
      vars:
        ansible_connection: ssh
        ansible_user: k8s-admin
      # => Overrides connection parameters for delegation

    # Run once with conditional
    - name: Conditional run-once task
      ansible.builtin.debug:
        msg: "First production webserver: {{ inventory_hostname }}"
      run_once: true
      when: "'production' in group_names"
      # => Runs once on first host matching condition

    # Delegate to inventory group member
    - name: Update monitoring server
      ansible.builtin.uri:
        url: http://{{ item }}/api/register
        method: POST
        body_format: json
        body:
          hostname: "{{ inventory_hostname }}"
          ip: "{{ ansible_default_ipv4.address }}"
      delegate_to: "{{ item }}"
      loop: "{{ groups['monitoring'] }}" # => Delegate to each monitoring server
      # => Registers this webserver with all monitoring servers
```

**Practical scenarios**:

```yaml
---
# Rolling deployment with load balancer
- hosts: webservers
  serial: 1 # => One host at a time

  tasks:
    # Remove from load balancer
    - name: Disable server in load balancer
      ansible.builtin.command:
        cmd: /usr/local/bin/lb-disable {{ inventory_hostname }}
      delegate_to: loadbalancer.example.com
      # => Runs on LB for current webserver

    # Update application
    - name: Deploy new version
      ansible.builtin.copy:
        src: /tmp/app.tar.gz
        dest: /opt/app/

    # Add back to load balancer
    - name: Enable server in load balancer
      ansible.builtin.command:
        cmd: /usr/local/bin/lb-enable {{ inventory_hostname }}
      delegate_to: loadbalancer.example.com
```

**Run**: `ansible-playbook -i inventory delegation.yml`

**Key Takeaway**: Use `delegate_to` to execute tasks on different hosts while maintaining access to target host variables—critical for load balancer updates, external API calls, and centralized operations. Use `run_once` to avoid redundant operations like database migrations or API notifications. Combine `run_once` with `delegate_to: localhost` for single-execution control node tasks. Use `delegate_facts: true` when gathering facts from delegated hosts. Delegation is the mechanism for cross-host orchestration and external system integration.

---

**🎯 Intermediate level complete!** You've covered roles, handlers, templates, vault, error handling, and task control. These patterns enable production-ready automation with reusability, security, and resilience. Proceed to [Advanced](/en/learn/software-engineering/infrastructure/tools/ansible/tutorials/by-example/advanced) for custom modules, collections, testing, and CI/CD integration.
