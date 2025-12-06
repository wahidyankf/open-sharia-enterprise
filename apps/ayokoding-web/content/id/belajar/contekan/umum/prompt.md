---
title: "ChatGPT"
date: 2025-03-16T07:20:00+07:00
draft: false
---

Ini adalah daftar kemungkinan prompt yang bisa kamu gunakan di ChatGPT. Untuk menggunakannya, kamu bisa mengganti `{{}}` dengan apa yang kamu butuhkan.

## Belajar

### Daftar Topik Penting

```yaml
- Give the list of the most important topics to learn, use the Pareto principle, and smooth the learning curve. Format the result as the list in markdown format, and make a heading for each topic.
  - {{topic}} essentials
- Get straight to the point, skip the Pareto principle section/intro
- Add the further readings section with links (if available), and ensure it includes all the items.
```

### Daftar Topik 3-Level

```yaml
- Give the list of the most important topics to learn with its subtopics, use the Pareto principle, and smoothen its learning curve. Format the result as the list in markdown format, and make a heading for each topic.
  - essentials, intermediate, and advanced topics of {{topic}}
- Get straight to the point, skip the Pareto principle section/intro
- Add the further readings section with links (if available), and ensure it includes all the items.
```

### Detail Topik

```yaml
- Teach me about this topic, consider the Pareto principles, and don't forget to make the explanation comprehensive and give many examples. Format the result in markdown format.
  - {{topic}}
- Get straight to the point, skip the Pareto principle section/intro
- Add the further readings section with links (if available), and ensure it includes all the items.
- Make the title H1
```

### Penjelasan Simpel

```clojure
Explain Like I am 5 years old and dumb about this {{topic}}: {{subtopic}}.

Also, give the benefits and downsides of this approach and further readings.

Format the result in markdown, with H1 as the title.
```
