---
title: "Advanced"
date: 2025-12-29T00:00:00+07:00
draft: false
weight: 10000003
description: "Examples 55-80: Advanced Ansible covering custom modules, collections, testing, performance optimization, CI/CD integration, and production patterns (75-95% coverage)"
categories: ["learn"]
tags: ["ansible", "automation", "by-example", "advanced", "testing", "ci-cd", "custom-modules"]
---

## Example 55: Custom Module - Hello Module

Custom modules extend Ansible's functionality using Python. This simple module demonstrates the basic structure: argument spec definition, input validation, and result return with changed status.

```python
# library/hello.py
#!/usr/bin/python
from ansible.module_utils.basic import AnsibleModule

def run_module():
    module_args = dict(
        name=dict(type='str', required=True)    # => Module argument definition
    )

    module = AnsibleModule(
        argument_spec=module_args,
        supports_check_mode=True
    )

    result = dict(
        changed=False,
        message=f"Hello, {module.params['name']}!"
    )

    module.exit_json(**result)                  # => Return results to Ansible

if __name__ == '__main__':
    run_module()
# => Usage: ansible localhost -m hello -a "name=World"
```

**Key Takeaway**: Custom modules are Python scripts that use `AnsibleModule` for argument parsing and `exit_json()` for result return.

## Example 56: Custom Module with State Management

Production modules manage resources with state (present/absent). This pattern checks current state, calculates necessary changes, and reports accurate changed status for idempotency.

```python
# library/user_quota.py
#!/usr/bin/python
from ansible.module_utils.basic import AnsibleModule
import os

def main():
    module = AnsibleModule(
        argument_spec=dict(
            username=dict(required=True),
            quota_mb=dict(type='int', default=1000),
            state=dict(choices=['present', 'absent'], default='present')
        )
    )

    username = module.params['username']
    quota = module.params['quota_mb']
    state = module.params['state']

    quota_file = f"/etc/quotas/{username}"
    exists = os.path.exists(quota_file)

    changed = False

    if state == 'present' and not exists:
        # Create quota
        with open(quota_file, 'w') as f:
            f.write(str(quota))
        changed = True                           # => Resource created
        msg = f"Created quota {quota}MB for {username}"
    elif state == 'absent' and exists:
        os.remove(quota_file)
        changed = True                           # => Resource removed
        msg = f"Removed quota for {username}"
    else:
        msg = f"Quota already in desired state"  # => No change needed

    module.exit_json(changed=changed, msg=msg)

if __name__ == '__main__':
    main()
```

**Key Takeaway**: Idempotent modules check current state before making changes and accurately report `changed` status.

## Example 57: Ansible Collections - Using Collections

Collections bundle modules, plugins, and roles into distributable packages. Install from Ansible Galaxy and reference modules with FQCN (Fully Qualified Collection Name).

```yaml
# requirements.yml
---
collections:
  - name: community.general
    version: ">=8.0.0" # => Minimum version constraint
  - name: ansible.posix
    version: "9.0.0"
# => Install with: ansible-galaxy collection install -r requirements.yml
```

```yaml
# use_collection.yml
---
- name: Using Collection Modules
  hosts: localhost
  tasks:
    - name: Archive files with community.general
      community.general.archive:
        path: /tmp/mydir
        dest: /tmp/archive.tar.gz
        format: gz
      # => Uses FQCN: namespace.collection.module

    - name: Mount filesystem with ansible.posix
      ansible.posix.mount:
        path: /mnt/data
        src: /dev/sdb1
        fstype: ext4
        state: mounted
      # => FQCN ensures no module name conflicts
```

**Key Takeaway**: Collections provide namespaced modules via FQCN (`namespace.collection.module`). Install via `requirements.yml` for reproducible environments.

## Example 58: Testing with Molecule - Scenario

Molecule automates role testing across multiple platforms. It creates test instances, applies roles, runs verifiers, and cleans up. Essential for role development.

```yaml
# molecule/default/molecule.yml
---
driver:
  name: docker # => Use Docker for test instances
platforms:
  - name: ubuntu-test
    image: ubuntu:22.04
    pre_build_image: true
provisioner:
  name: ansible
  playbooks:
    converge: converge.yml # => Playbook that applies role
verifier:
  name: ansible
  playbooks:
    verify: verify.yml # => Playbook that tests results
# => Run with: molecule test
```

```yaml
# molecule/default/converge.yml
---
- name: Converge
  hosts: all
  roles:
    - role: my_role
      vars:
        app_port: 8080
# => Applies role to test instance
```

```yaml
# molecule/default/verify.yml
---
- name: Verify
  hosts: all
  tasks:
    - name: Check service is running
      service:
        name: myapp
        state: started
      check_mode: yes
      register: result
      failed_when: result.changed # => Fail if service not running
```

**Key Takeaway**: Molecule provides full role testing lifecycle: create → converge → verify → destroy. Use for TDD (Test-Driven Development) of roles.

## Example 59: Ansible-Lint Configuration

Ansible-lint enforces best practices and catches common errors. Configure via `.ansible-lint` for project-specific rules and skip patterns.

```yaml
# .ansible-lint
---
profile: production # => Use production rule profile

skip_list:
  - yaml[line-length] # => Allow long lines
  - name[casing] # => Allow any task name casing

warn_list:
  - experimental # => Warn on experimental features

exclude_paths:
  - .cache/
  - test/fixtures/
  - molecule/
# => Run with: ansible-lint site.yml
```

```bash
# CI pipeline integration
ansible-lint playbooks/*.yml --force-color --format pep8 > lint-results.txt
# => Returns non-zero exit code on failures
# => Integration with CI/CD for automated quality checks
```

**Key Takeaway**: Ansible-lint automates best practice enforcement. Configure via `.ansible-lint` file. Integrate in CI/CD pipelines for quality gates.

## Example 60: Performance - Fact Caching

Fact gathering is slow on large inventories. Enable fact caching to store facts between runs. Supports memory, file, Redis, and Memcached backends.

```ini
# ansible.cfg
[defaults]
gathering = smart                    # => Only gather if facts not cached
fact_caching = jsonfile              # => Use JSON file backend
fact_caching_connection = /tmp/ansible_facts
fact_caching_timeout = 86400         # => Cache for 24 hours
```

```yaml
# playbook.yml
---
- name: Use Cached Facts
  hosts: all
  gather_facts: yes # => Will use cache if available
  tasks:
    - name: Print cached IP
      debug:
        msg: "IP: {{ ansible_default_ipv4.address }}"
      # => First run: gathers facts (slow)
      # => Subsequent runs: uses cache (fast)
```

**Key Takeaway**: Fact caching dramatically speeds up playbooks on large inventories. Configure in `ansible.cfg` with appropriate timeout.

## Example 61: Performance - Pipelining

Pipelining reduces SSH overhead by executing modules without creating temporary files on target. Requires `requiretty` disabled in sudoers.

```ini
# ansible.cfg
[defaults]
pipelining = True                    # => Enable SSH pipelining

[ssh_connection]
pipelining = True
ssh_args = -o ControlMaster=auto -o ControlPersist=60s
# => Reuse SSH connections for 60 seconds
```

```yaml
# playbook.yml
---
- name: Fast Execution with Pipelining
  hosts: webservers
  tasks:
    - name: Install 10 packages
      apt:
        name:
          - pkg1
          - pkg2
          # ... 10 packages
        state: present
      # => With pipelining: ~30% faster execution
      # => Without: creates temp file for each module
```

**Key Takeaway**: Pipelining reduces SSH overhead significantly. Enable in `ansible.cfg`. Requires sudoers without `requiretty`.

## Example 62: CI/CD - GitHub Actions Pipeline

Automate Ansible execution in CI/CD pipelines. This GitHub Actions workflow validates syntax, runs linting, executes playbooks, and tests idempotency.

```yaml
# .github/workflows/ansible-ci.yml
name: Ansible CI
on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Setup Python
        uses: actions/setup-python@v4
        with:
          python-version: "3.11"

      - name: Install Ansible
        run: pip install ansible ansible-lint
        # => Install tools in CI environment

      - name: Syntax check
        run: ansible-playbook site.yml --syntax-check
        # => Validate YAML syntax

      - name: Lint playbooks
        run: ansible-lint site.yml
        # => Check best practices

      - name: Run playbook
        run: ansible-playbook site.yml -i inventory/ci
        # => Execute against CI inventory

      - name: Test idempotency
        run: |
          ansible-playbook site.yml -i inventory/ci | tee first-run.txt
          ansible-playbook site.yml -i inventory/ci | tee second-run.txt
          grep -q 'changed=0' second-run.txt
        # => Verify playbook is idempotent
```

**Key Takeaway**: CI/CD pipelines automate validation, linting, execution, and idempotency testing. Essential for production Ansible workflows.

## Example 63: Production Pattern - Rolling Updates

Rolling updates deploy changes gradually to avoid downtime. Use `serial` to control batch size and `max_fail_percentage` for automatic rollback triggers.

```yaml
# rolling_update.yml
---
- name: Rolling Update Web Servers
  hosts: webservers
  serial: 2 # => Update 2 hosts at a time
  max_fail_percentage: 25 # => Abort if >25% hosts fail

  pre_tasks:
    - name: Remove from load balancer
      uri:
        url: "http://lb.example.com/api/hosts/{{ inventory_hostname }}/disable"
        method: POST
      delegate_to: localhost
      # => Remove host from LB before update

  tasks:
    - name: Deploy new version
      copy:
        src: "app-{{ app_version }}.jar"
        dest: /opt/myapp/app.jar
      notify: Restart application

    - name: Wait for application health
      uri:
        url: "http://{{ inventory_hostname }}:8080/health"
        status_code: 200
      retries: 10
      delay: 3
      # => Verify app is healthy before continuing

  post_tasks:
    - name: Add back to load balancer
      uri:
        url: "http://lb.example.com/api/hosts/{{ inventory_hostname }}/enable"
        method: POST
      delegate_to: localhost
      # => Re-add host to LB after successful update

  handlers:
    - name: Restart application
      service:
        name: myapp
        state: restarted
```

**Key Takeaway**: Rolling updates use `serial` for batch control and health checks between batches. Pre/post tasks manage load balancer integration.

## Example 64: Production Pattern - Canary Deployment

Canary deployments test new versions on a subset of servers before full rollout. Combine with monitoring to validate changes before proceeding.

```yaml
# canary_deploy.yml
---
- name: Canary Deployment
  hosts: webservers
  tasks:
    - name: Deploy to canary hosts
      copy:
        src: "app-{{ new_version }}.jar"
        dest: /opt/myapp/app.jar
      when: "'canary' in group_names"
      notify: Restart application
      # => Only deploy to canary group first

    - name: Wait for canary validation
      pause:
        prompt: "Check metrics. Press enter to continue or Ctrl-C to abort"
      when: "'canary' in group_names"
      # => Manual validation checkpoint

    - name: Deploy to production
      copy:
        src: "app-{{ new_version }}.jar"
        dest: /opt/myapp/app.jar
      when: "'production' in group_names"
      notify: Restart application
      # => Deploy to all production after canary success
```

```ini
# inventory.ini
[canary]
web1.example.com

[production]
web2.example.com
web3.example.com
web4.example.com

[webservers:children]
canary
production
```

**Key Takeaway**: Canary deployments reduce risk by testing on subset. Use inventory groups and conditionals to control deployment stages.

## Example 65: Production Pattern - Blue-Green Deployment

Blue-green deployments maintain two identical environments. Deploy to inactive environment, verify, then switch traffic. Enables instant rollback.

```yaml
# blue_green.yml
---
- name: Blue-Green Deployment
  hosts: localhost
  vars:
    active_color: "{{ lookup('file', '/etc/active_color.txt') }}" # => Current: blue or green
    inactive_color: "{{ 'green' if active_color == 'blue' else 'blue' }}"

  tasks:
    - name: Deploy to inactive environment
      include_tasks: deploy.yml
      vars:
        target_hosts: "{{ inactive_color }}_webservers"
      # => Deploy to inactive (green if blue is active)

    - name: Run smoke tests
      uri:
        url: "http://{{ inactive_color }}-lb.example.com/health"
        status_code: 200
      # => Verify inactive environment is healthy

    - name: Switch load balancer
      uri:
        url: "http://lb.example.com/api/switch"
        method: POST
        body_format: json
        body:
          active: "{{ inactive_color }}"
      # => Switch traffic to newly deployed environment

    - name: Update active color file
      copy:
        content: "{{ inactive_color }}"
        dest: /etc/active_color.txt
      # => Record new active environment
```

**Key Takeaway**: Blue-green deployments enable zero-downtime releases and instant rollback by switching between two complete environments.

## Example 66: Production Pattern - Immutable Infrastructure

Immutable infrastructure replaces servers rather than modifying them. Build new AMIs/images, launch new instances, then terminate old ones.

```yaml
# immutable_deploy.yml
---
- name: Build Golden AMI
  hosts: packer_builder
  tasks:
    - name: Launch Packer build
      command: packer build -var 'version={{ app_version }}' ami-template.json
      register: packer_result

    - name: Extract AMI ID
      set_fact:
        new_ami: "{{ packer_result.stdout | regex_search('ami-[a-z0-9]+') }}"
      # => Captures: ami-0abc123def456

- name: Deploy New Auto Scaling Group
  hosts: localhost
  tasks:
    - name: Create launch configuration
      ec2_lc:
        name: "myapp-{{ app_version }}"
        image_id: "{{ new_ami }}"
        instance_type: t3.medium
        security_groups: ["sg-123456"]
      # => New launch config with new AMI

    - name: Update Auto Scaling Group
      ec2_asg:
        name: myapp-asg
        launch_config_name: "myapp-{{ app_version }}"
        min_size: 3
        max_size: 6
        desired_capacity: 3
      # => Triggers instance replacement

    - name: Wait for new instances healthy
      ec2_instance_facts:
        filters:
          "tag:Version": "{{ app_version }}"
          "instance-state-name": running
      register: instances
      until: instances.instances | length == 3
      retries: 20
      delay: 30
```

**Key Takeaway**: Immutable infrastructure builds new images and replaces instances entirely. Eliminates configuration drift and enables reliable rollbacks.

## Example 67: Zero-Downtime Deployment Pattern

Combine health checks, load balancer management, and serial execution for truly zero-downtime deployments. Each server is updated while others handle traffic.

```yaml
# zero_downtime.yml
---
- name: Zero-Downtime Deployment
  hosts: webservers
  serial: 1 # => One host at a time
  max_fail_percentage: 0 # => Abort on any failure

  tasks:
    - name: Pre-deployment health check
      uri:
        url: "http://{{ inventory_hostname }}:8080/health"
        status_code: 200
      # => Ensure host healthy before starting

    - name: Disable host in load balancer
      haproxy:
        backend: web_backend
        host: "{{ inventory_hostname }}"
        state: disabled
        socket: /run/haproxy/admin.sock
      delegate_to: lb.example.com
      # => Remove from LB pool

    - name: Wait for connections to drain
      wait_for:
        timeout: 30
      # => Allow active requests to complete

    - name: Deploy application
      copy:
        src: "myapp-{{ version }}.jar"
        dest: /opt/myapp/app.jar
      notify: Restart application

    - name: Flush handlers now
      meta: flush_handlers
      # => Ensure restart happens before health check

    - name: Wait for application startup
      wait_for:
        port: 8080
        delay: 5
        timeout: 120
      # => Wait for app to bind port

    - name: Application health check
      uri:
        url: "http://{{ inventory_hostname }}:8080/health"
        status_code: 200
      retries: 12
      delay: 5
      # => Verify app is healthy

    - name: Enable host in load balancer
      haproxy:
        backend: web_backend
        host: "{{ inventory_hostname }}"
        state: enabled
        socket: /run/haproxy/admin.sock
      delegate_to: lb.example.com
      # => Add back to LB pool

    - name: Wait for host to receive traffic
      pause:
        seconds: 10
      # => Allow LB health checks to pass

  handlers:
    - name: Restart application
      service:
        name: myapp
        state: restarted
```

**Key Takeaway**: Zero-downtime deployments require serial execution, LB integration, connection draining, and comprehensive health checks at each stage.

## Example 68: Monitoring Integration

Integrate Ansible with monitoring systems to track deployment progress and trigger alerts. Send notifications to Slack, DataDog, or PagerDuty during critical phases.

```yaml
# monitored_deploy.yml
---
- name: Deployment with Monitoring
  hosts: webservers
  tasks:
    - name: Send deployment start notification
      uri:
        url: "{{ slack_webhook_url }}"
        method: POST
        body_format: json
        body:
          text: "Starting deployment of {{ app_version }} to {{ inventory_hostname }}"
      delegate_to: localhost
      # => Notify team in Slack

    - name: Create deployment marker in DataDog
      uri:
        url: "https://api.datadoghq.com/api/v1/events"
        method: POST
        headers:
          DD-API-KEY: "{{ datadog_api_key }}"
        body_format: json
        body:
          title: "Deployment Started"
          text: "{{ app_version }} deploying to {{ inventory_hostname }}"
          tags:
            - "environment:production"
            - "version:{{ app_version }}"
      delegate_to: localhost
      # => Creates event marker in DataDog dashboard

    - name: Deploy application
      copy:
        src: "app-{{ app_version }}.jar"
        dest: /opt/myapp/app.jar
      notify: Restart application

    - name: Check error rate post-deployment
      uri:
        url: "{{ metrics_api }}/error_rate?host={{ inventory_hostname }}"
        return_content: yes
      register: error_rate
      delegate_to: localhost
      # => Query metrics API

    - name: Trigger alert if error rate high
      uri:
        url: "{{ pagerduty_events_url }}"
        method: POST
        body_format: json
        body:
          routing_key: "{{ pagerduty_key }}"
          event_action: trigger
          payload:
            summary: "High error rate after deployment"
            severity: critical
      when: error_rate.json.value > 5.0
      delegate_to: localhost
      # => Create PagerDuty incident if errors spike
```

**Key Takeaway**: Monitor deployments by integrating with Slack, DataDog, PagerDuty. Send notifications at key phases and trigger alerts on anomalies.

## Example 69: Disaster Recovery Pattern

Automate disaster recovery with playbooks that restore from backups, recreate infrastructure, and verify system integrity. Test DR playbooks regularly.

```yaml
# disaster_recovery.yml
---
- name: Disaster Recovery Procedure
  hosts: localhost
  vars:
    backup_date: "{{ lookup('pipe', 'date +%Y-%m-%d') }}"

  tasks:
    - name: Provision new infrastructure
      include_role:
        name: provision_infrastructure
      vars:
        environment: dr_recovery
      # => Recreate VMs/cloud resources

    - name: Restore database from backup
      postgresql_db:
        name: myapp
        state: restore
        target: "s3://backups/db-{{ backup_date }}.dump"
      # => Restore DB from S3

    - name: Restore application files
      aws_s3:
        bucket: backups
        object: "app-{{ backup_date }}.tar.gz"
        dest: /tmp/app-restore.tar.gz
        mode: get
      # => Download app backup

    - name: Extract application
      unarchive:
        src: /tmp/app-restore.tar.gz
        dest: /opt/myapp
        remote_src: yes
      # => Restore application code

    - name: Verify data integrity
      command: /opt/myapp/bin/verify-data.sh
      register: integrity_check
      failed_when: "'PASS' not in integrity_check.stdout"
      # => Validate restored data

    - name: Update DNS to DR site
      route53:
        state: present
        zone: example.com
        record: app.example.com
        type: A
        value: "{{ dr_lb_ip }}"
        ttl: 60
      # => Point DNS to DR environment

    - name: Send recovery notification
      uri:
        url: "{{ slack_webhook_url }}"
        method: POST
        body:
          text: "DR completed. Services running at DR site."
      # => Notify team of DR completion
```

**Key Takeaway**: DR playbooks automate infrastructure recreation, data restoration, and traffic cutover. Test regularly to ensure RTO/RPO targets.

## Example 70: Configuration Drift Detection

Detect configuration drift by comparing desired state (playbooks) against actual state (target hosts). Run in check mode and alert on differences.

```yaml
# drift_detection.yml
---
- name: Detect Configuration Drift
  hosts: production
  check_mode: yes # => Don't make changes, only check
  diff: yes # => Show differences
  tasks:
    - name: Check nginx configuration
      template:
        src: nginx.conf.j2
        dest: /etc/nginx/nginx.conf
      register: nginx_drift
      # => In check mode: reports if file would change

    - name: Check service state
      service:
        name: nginx
        state: started
        enabled: yes
      register: service_drift

    - name: Check package versions
      package:
        name:
          - nginx=1.18*
          - postgresql=14*
        state: present
      register: package_drift

    - name: Collect drift report
      set_fact:
        drift_detected: >-
          {{
            nginx_drift.changed or
            service_drift.changed or
            package_drift.changed
          }}

    - name: Alert on drift
      uri:
        url: "{{ alerting_webhook }}"
        method: POST
        body:
          host: "{{ inventory_hostname }}"
          drift: "{{ drift_detected }}"
          details:
            nginx: "{{ nginx_drift.changed }}"
            service: "{{ service_drift.changed }}"
            packages: "{{ package_drift.changed }}"
      when: drift_detected
      delegate_to: localhost
      # => Send alert if any drift detected
```

**Key Takeaway**: Run playbooks in check mode to detect drift without changing systems. Schedule drift detection jobs to catch manual changes.

## Example 71: Multi-Stage Deployment Pipeline

Orchestrate multi-stage deployments (dev → staging → production) with approval gates and environment-specific configurations.

```yaml
# pipeline_deploy.yml
---
- name: Deploy to Development
  hosts: dev_webservers
  vars_files:
    - vars/dev.yml
  tasks:
    - include_tasks: deploy_tasks.yml

- name: Run Integration Tests
  hosts: dev_webservers
  tasks:
    - name: Execute test suite
      command: /opt/tests/run-integration-tests.sh
      register: tests
      failed_when: tests.rc != 0
      # => Fail pipeline if tests fail

- name: Deploy to Staging
  hosts: staging_webservers
  vars_files:
    - vars/staging.yml
  tasks:
    - include_tasks: deploy_tasks.yml

- name: Staging Smoke Tests
  hosts: staging_webservers
  tasks:
    - name: Check critical endpoints
      uri:
        url: "http://{{ inventory_hostname }}/{{ item }}"
        status_code: 200
      loop:
        - health
        - api/users
        - api/orders
      # => Verify staging is functional

- name: Production Approval Gate
  hosts: localhost
  tasks:
    - name: Wait for approval
      pause:
        prompt: "Approve production deployment? (Enter to continue)"
      # => Manual approval before production

- name: Deploy to Production
  hosts: prod_webservers
  serial: 3 # => Rolling update
  vars_files:
    - vars/production.yml
  tasks:
    - include_tasks: deploy_tasks.yml
```

**Key Takeaway**: Multi-stage pipelines use separate plays for each environment with tests and approval gates between stages.

## Example 72: Secrets Management with HashiCorp Vault

Integrate Ansible with HashiCorp Vault for dynamic secrets. Fetch credentials at runtime instead of storing in Ansible Vault or vars files.

```yaml
# vault_integration.yml
---
- name: Dynamic Secrets from Vault
  hosts: webservers
  vars:
    vault_addr: "https://vault.example.com:8200"
  tasks:
    - name: Get database credentials from Vault
      uri:
        url: "{{ vault_addr }}/v1/database/creds/myapp"
        method: GET
        headers:
          X-Vault-Token: "{{ lookup('env', 'VAULT_TOKEN') }}"
        return_content: yes
      register: db_creds
      delegate_to: localhost
      no_log: true # => Don't log credentials
      # => Fetches dynamic DB credentials

    - name: Configure application with Vault credentials
      template:
        src: app-config.j2
        dest: /opt/myapp/config.yml
        mode: "0600"
      vars:
        db_username: "{{ db_creds.json.data.username }}"
        db_password: "{{ db_creds.json.data.password }}"
      no_log: true
      # => Credentials never stored in playbooks

    - name: Revoke credentials on failure
      uri:
        url: "{{ vault_addr }}/v1/sys/leases/revoke"
        method: PUT
        headers:
          X-Vault-Token: "{{ lookup('env', 'VAULT_TOKEN') }}"
        body:
          lease_id: "{{ db_creds.json.lease_id }}"
      delegate_to: localhost
      when: ansible_failed_task is defined
      # => Clean up credentials on failure
```

**Key Takeaway**: HashiCorp Vault integration provides dynamic secrets that auto-expire. Use `no_log` to prevent credential exposure in logs.

## Example 73: Compliance Auditing

Automate compliance checks (CIS benchmarks, STIG) and generate audit reports. Compare actual configuration against security baselines.

```yaml
# compliance_audit.yml
---
- name: CIS Ubuntu 22.04 Compliance Audit
  hosts: all
  become: yes
  tasks:
    - name: Check SSH configuration
      block:
        - name: Verify PermitRootLogin is disabled
          lineinfile:
            path: /etc/ssh/sshd_config
            regexp: "^PermitRootLogin"
            line: "PermitRootLogin no"
          check_mode: yes
          register: ssh_root
          # => Check without changing

        - name: Record compliance status
          set_fact:
            compliance_ssh_root: "{{ not ssh_root.changed }}"

    - name: Check firewall status
      command: ufw status
      register: firewall
      changed_when: false
      failed_when: "'Status: active' not in firewall.stdout"

    - name: Check password policy
      command: grep -E '^PASS_MAX_DAYS' /etc/login.defs
      register: pass_policy
      changed_when: false
      failed_when: pass_policy.stdout.split()[1] | int > 90

    - name: Generate compliance report
      template:
        src: compliance-report.j2
        dest: "/var/log/compliance-{{ ansible_date_time.date }}.json"
      vars:
        checks:
          ssh_root_disabled: "{{ compliance_ssh_root }}"
          firewall_active: "{{ 'active' in firewall.stdout }}"
          password_max_days: "{{ pass_policy.stdout.split()[1] }}"
      delegate_to: localhost
      # => JSON report for SIEM ingestion
```

**Key Takeaway**: Compliance audits use check mode and assertions to verify security baselines. Generate structured reports for audit trails.

## Example 74: Network Automation - VLAN Configuration

Automate network device configuration using vendor-specific modules. This example configures VLANs on Cisco switches.

```yaml
# network_vlans.yml
---
- name: Configure VLANs on Cisco Switches
  hosts: cisco_switches
  gather_facts: no
  tasks:
    - name: Create VLANs
      cisco.ios.ios_vlans:
        config:
          - vlan_id: 10
            name: ENGINEERING
            state: active
          - vlan_id: 20
            name: SALES
            state: active
          - vlan_id: 30
            name: GUEST
            state: active
        state: merged
      # => Creates VLANs if missing, updates if exist

    - name: Configure trunk port
      cisco.ios.ios_l2_interfaces:
        config:
          - name: GigabitEthernet0/1
            mode: trunk
            trunk:
              allowed_vlans: 10,20,30
        state: replaced
      # => Configures port as trunk with allowed VLANs

    - name: Save configuration
      cisco.ios.ios_config:
        save_when: modified
      # => Writes config to startup-config
```

**Key Takeaway**: Network modules provide declarative interface to network devices. Use vendor collections (`cisco.ios`, `arista.eos`) for device-specific operations.

## Example 75: Container Orchestration - Docker Deployment

Manage Docker containers with Ansible. Deploy multi-container applications with proper networking and volume configuration.

```yaml
# docker_deploy.yml
---
- name: Deploy Docker Application
  hosts: docker_hosts
  tasks:
    - name: Create application network
      docker_network:
        name: myapp_network
        driver: bridge
      # => Creates isolated network for containers

    - name: Deploy PostgreSQL container
      docker_container:
        name: postgres
        image: postgres:15
        state: started
        restart_policy: always
        networks:
          - name: myapp_network
        env:
          POSTGRES_DB: myapp
          POSTGRES_PASSWORD: "{{ db_password }}"
        volumes:
          - postgres_data:/var/lib/postgresql/data
      # => Database container with persistent volume

    - name: Deploy application container
      docker_container:
        name: myapp
        image: "myapp:{{ version }}"
        state: started
        restart_policy: always
        networks:
          - name: myapp_network
        env:
          DB_HOST: postgres
          DB_NAME: myapp
        ports:
          - "8080:8080"
      # => App container linked to database

    - name: Wait for application health
      uri:
        url: "http://{{ inventory_hostname }}:8080/health"
        status_code: 200
      retries: 10
      delay: 3
```

**Key Takeaway**: Docker modules manage containers declaratively. Use networks for container communication and volumes for data persistence.

## Example 76: Kubernetes Deployment

Deploy applications to Kubernetes using Ansible. Apply manifests, wait for rollout completion, and verify pod health.

```yaml
# k8s_deploy.yml
---
- name: Deploy to Kubernetes
  hosts: localhost
  tasks:
    - name: Create namespace
      kubernetes.core.k8s:
        state: present
        definition:
          apiVersion: v1
          kind: Namespace
          metadata:
            name: myapp
      # => Creates namespace if missing

    - name: Deploy application
      kubernetes.core.k8s:
        state: present
        namespace: myapp
        definition: "{{ lookup('file', 'k8s/deployment.yml') }}"
      # => Applies deployment manifest

    - name: Wait for deployment rollout
      kubernetes.core.k8s_info:
        kind: Deployment
        namespace: myapp
        name: myapp
      register: deployment
      until: deployment.resources[0].status.readyReplicas == 3
      retries: 20
      delay: 10
      # => Waits for all replicas ready

    - name: Expose service
      kubernetes.core.k8s:
        state: present
        namespace: myapp
        definition:
          apiVersion: v1
          kind: Service
          metadata:
            name: myapp
          spec:
            type: LoadBalancer
            selector:
              app: myapp
            ports:
              - port: 80
                targetPort: 8080
      # => Creates LoadBalancer service
```

**Key Takeaway**: Kubernetes modules enable GitOps workflows. Use `k8s_info` to wait for resources to reach desired state before proceeding.

## Example 77: Database Migration Automation

Automate database schema migrations as part of deployment pipelines. Run migrations, verify success, and rollback on failure.

```yaml
# db_migration.yml
---
- name: Database Migration
  hosts: db_servers
  tasks:
    - name: Backup database before migration
      postgresql_db:
        name: myapp
        state: dump
        target: "/backups/pre-migration-{{ ansible_date_time.epoch }}.sql"
      # => Safety backup before schema changes

    - name: Run database migrations
      command: /opt/myapp/bin/migrate up
      register: migration
      failed_when: migration.rc != 0
      # => Execute migration scripts

    - name: Verify migration success
      postgresql_query:
        db: myapp
        query: "SELECT version FROM schema_migrations ORDER BY version DESC LIMIT 1"
      register: current_version
      # => Check current schema version

    - name: Rollback on failure
      block:
        - name: Restore from backup
          postgresql_db:
            name: myapp
            state: restore
            target: "/backups/pre-migration-{{ ansible_date_time.epoch }}.sql"
      rescue:
        - name: Alert on rollback failure
          uri:
            url: "{{ pagerduty_url }}"
            method: POST
            body:
              message: "CRITICAL: Migration rollback failed"
          delegate_to: localhost
      when: migration.failed
```

**Key Takeaway**: Automate migrations with pre-migration backups and rollback procedures. Use blocks for error handling and recovery.

## Example 78: Self-Healing Infrastructure

Implement self-healing by detecting failures and automatically remediating. Monitor service health and restart failed services.

```yaml
# self_healing.yml
---
- name: Self-Healing Monitor
  hosts: all
  tasks:
    - name: Check critical services
      service_facts:
      # => Gathers service status facts

    - name: Restart failed nginx
      service:
        name: nginx
        state: restarted
      when: ansible_facts.services['nginx.service'].state != 'running'
      # => Auto-restart if stopped

    - name: Check disk space
      shell: df -h / | tail -1 | awk '{print $5}' | sed 's/%//'
      register: disk_usage
      changed_when: false
      # => Get root filesystem usage percentage

    - name: Clean logs if disk full
      file:
        path: /var/log/old-logs
        state: absent
      when: disk_usage.stdout | int > 85
      # => Remove old logs if >85% full

    - name: Verify database connectivity
      postgresql_ping:
        db: myapp
      register: db_ping
      ignore_errors: yes
      # => Test DB connection

    - name: Restart database on failure
      service:
        name: postgresql
        state: restarted
      when: db_ping.failed
      # => Auto-remediate DB failures

    - name: Alert if remediation fails
      uri:
        url: "{{ alerting_webhook }}"
        method: POST
        body:
          host: "{{ inventory_hostname }}"
          issue: "Self-healing failed"
      when: db_ping.failed
      delegate_to: localhost
```

**Key Takeaway**: Self-healing playbooks run periodically (cron/systemd timers) to detect and remediate common failures automatically.

## Example 79: Infrastructure Cost Optimization

Automate cost optimization by identifying and remediating wasteful resource usage (unused volumes, stopped instances, oversized VMs).

```yaml
# cost_optimization.yml
---
- name: Identify Unused Resources
  hosts: localhost
  tasks:
    - name: Find unattached EBS volumes
      ec2_vol_info:
        region: us-east-1
        filters:
          status: available # => Unattached volumes
      register: unused_volumes
      # => Lists orphaned volumes

    - name: Delete old unattached volumes
      ec2_vol:
        id: "{{ item.id }}"
        state: absent
      loop: "{{ unused_volumes.volumes }}"
      when: item.create_time | to_datetime < (ansible_date_time.epoch | int - 2592000)
      # => Delete volumes older than 30 days

    - name: Find stopped instances running >7 days
      ec2_instance_info:
        region: us-east-1
        filters:
          instance-state-name: stopped
      register: stopped_instances

    - name: Terminate long-stopped instances
      ec2_instance:
        instance_ids: "{{ item.instance_id }}"
        state: absent
      loop: "{{ stopped_instances.instances }}"
      when: item.launch_time | to_datetime < (ansible_date_time.epoch | int - 604800)
      # => Terminate stopped >7 days

    - name: Generate cost report
      template:
        src: cost-report.j2
        dest: "/reports/cost-optimization-{{ ansible_date_time.date }}.html"
      vars:
        deleted_volumes: "{{ unused_volumes.volumes | length }}"
        terminated_instances: "{{ stopped_instances.instances | length }}"
```

**Key Takeaway**: Automate cost optimization by periodically identifying and removing unused cloud resources.

## Example 80: Chaos Engineering with Ansible

Implement chaos engineering experiments to test system resilience. Inject failures and verify recovery mechanisms.

```yaml
# chaos_experiment.yml
---
- name: Chaos Engineering - Random Service Failure
  hosts: production
  serial: 1
  tasks:
    - name: Select random service to disrupt
      set_fact:
        chaos_target: "{{ ['nginx', 'myapp', 'postgres'] | random }}"
      # => Pick random service

    - name: Record experiment start
      uri:
        url: "{{ metrics_api }}/chaos/start"
        method: POST
        body:
          host: "{{ inventory_hostname }}"
          service: "{{ chaos_target }}"
      delegate_to: localhost

    - name: Stop service
      service:
        name: "{{ chaos_target }}"
        state: stopped
      # => Inject failure

    - name: Wait for monitoring to detect failure
      pause:
        seconds: 30
      # => Give monitoring time to alert

    - name: Verify alerting fired
      uri:
        url: "{{ alerting_api }}/check"
        method: GET
      register: alerts
      failed_when: chaos_target not in alerts.json.active_alerts
      delegate_to: localhost
      # => Ensure monitoring detected failure

    - name: Allow self-healing to trigger
      pause:
        seconds: 60
      # => Wait for auto-remediation

    - name: Verify service recovered
      service_facts:
      failed_when: ansible_facts.services[chaos_target + '.service'].state != 'running'
      # => Ensure auto-remediation worked

    - name: Record experiment completion
      uri:
        url: "{{ metrics_api }}/chaos/complete"
        method: POST
        body:
          host: "{{ inventory_hostname }}"
          service: "{{ chaos_target }}"
          outcome: "{{ 'success' if ansible_failed_result is not defined else 'failure' }}"
      delegate_to: localhost
```

**Key Takeaway**: Chaos engineering validates monitoring and auto-remediation. Run experiments in controlled manner to test system resilience.
