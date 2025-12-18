---
title: "{{ replace .Name "-" " " | title }}"
date: {{ .Date }}
draft: true
description: ""
weight: 10
tags: ["example-tag"]
# Note: No categories field - causes raw text leak in Hextra theme
# Note: No author field - uses site-level config (params.author in hugo.yaml)
---

## Pengantar

[Introduce the topic here]

## Konsep Kunci

- Point 1
- Point 2
- Point 3

## Contoh

```[language]
// Code example here
```

## Rangkuman

[Summarize key learnings]
