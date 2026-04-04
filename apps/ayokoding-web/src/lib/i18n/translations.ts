import type { Locale } from "./config";

const translations: Record<Locale, Record<string, string>> = {
  en: {
    readMore: "Read More",
    lastUpdated: "Last updated",
    publishedOn: "Published on",
    author: "Author",
    tags: "Tags",
    categories: "Categories",
    share: "Share",
    relatedContent: "Related Content",
    openSourceProject: "Source-Available Project",
    search: "Search...",
    onThisPage: "On this page",
    previous: "Previous",
    next: "Next",
    noResults: "No results found",
    toggleTheme: "Toggle theme",
    skipToContent: "Skip to content",
  },
  id: {
    readMore: "Baca Selengkapnya",
    lastUpdated: "Terakhir diperbarui",
    publishedOn: "Dipublikasikan pada",
    author: "Penulis",
    tags: "Tag",
    categories: "Kategori",
    share: "Bagikan",
    relatedContent: "Konten Terkait",
    openSourceProject: "Proyek Source-Available",
    search: "Cari...",
    onThisPage: "Di halaman ini",
    previous: "Sebelumnya",
    next: "Selanjutnya",
    noResults: "Tidak ada hasil",
    toggleTheme: "Ubah tema",
    skipToContent: "Langsung ke konten",
  },
};

export function t(locale: Locale, key: string): string {
  return translations[locale]?.[key] ?? key;
}
