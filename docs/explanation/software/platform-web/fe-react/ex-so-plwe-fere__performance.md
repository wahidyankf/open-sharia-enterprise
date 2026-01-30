---
title: React Performance
description: Comprehensive guide to performance optimization techniques for React applications including profiling, code splitting, memoization, virtual lists, image optimization, and Web Vitals
category: explanation
tags:
  - react
  - performance
  - optimization
  - frontend
  - web-vitals
created: 2026-01-29
updated: 2026-01-29
---

# React Performance

This guide provides comprehensive coverage of performance optimization techniques for React applications. We'll explore profiling tools, optimization strategies, and best practices using Sharia-compliant business scenarios like zakat calculators and donation platforms.

## 📋 Overview

Performance optimization is critical for user experience and business success. Studies show:

- 53% of mobile users abandon sites that take over 3 seconds to load
- 1 second delay in page response can result in 7% reduction in conversions
- Fast sites rank higher in search results

**Performance optimization priorities**:

1. **Measure first** - Profile before optimizing
2. **Focus on user experience** - Optimize what users perceive
3. **Optimize strategically** - Address bottlenecks, not symptoms
4. **Test results** - Verify improvements with real metrics

## 🔍 Performance Profiling

### React DevTools Profiler

The React DevTools Profiler helps identify performance bottlenecks by recording component render times.

**Installation**:

- Browser extension: [Chrome](https://chrome.google.com/webstore/detail/react-developer-tools/) | [Firefox](https://addons.mozilla.org/en-US/firefox/addon/react-devtools/)
- Standalone: `npm install -g react-devtools`

**Using the Profiler**:

```typescript
// 1. Open React DevTools in browser
// 2. Click "Profiler" tab
// 3. Click record button (red circle)
// 4. Interact with your app
// 5. Stop recording
// 6. Analyze flamegraph and ranked chart

// Enable profiling in development
// vite.config.ts
export default defineConfig({
  plugins: [
    react({
      // Enable profiling in development
      jsxRuntime: "automatic",
    }),
  ],
  define: {
    "process.env.NODE_ENV": JSON.stringify(process.env.NODE_ENV || "development"),
  },
});
```

**Interpreting Results**:

- **Flamegraph**: Shows component hierarchy and render time
- **Ranked chart**: Lists components by render duration
- **Yellow/red bars**: Indicate slow renders (investigate these first)
- **Gray bars**: Component didn't render (optimized)

**Example - Profiling Zakat Calculator**:

```typescript
import { Profiler, ProfilerOnRenderCallback } from 'react';

// Profiler callback
const onRenderCallback: ProfilerOnRenderCallback = (
  id, // Profiler id
  phase, // "mount" or "update"
  actualDuration, // Time spent rendering
  baseDuration, // Estimated time without memoization
  startTime, // When render started
  commitTime, // When React committed
  interactions // Set of interactions tracked
) => {
  console.log(`${id} ${phase} phase:`, {
    actualDuration: `${actualDuration.toFixed(2)}ms`,
    baseDuration: `${baseDuration.toFixed(2)}ms`,
    improvement: `${((1 - actualDuration / baseDuration) * 100).toFixed(1)}%`,
  });
};

// Wrap component in Profiler
export function ZakatCalculatorPage() {
  return (
    <Profiler id="ZakatCalculator" onRender={onRenderCallback}>
      <ZakatCalculator />
    </Profiler>
  );
}
```

### Chrome DevTools Performance Tab

Chrome DevTools provides deeper performance insights beyond React.

**Using Performance Tab**:

1. Open DevTools (F12)
2. Navigate to "Performance" tab
3. Click record button
4. Interact with your app
5. Stop recording
6. Analyze timeline

**Key Metrics**:

- **FPS (Frames Per Second)**: Target 60 FPS for smooth animations
- **CPU usage**: Identify expensive JavaScript operations
- **Network activity**: Monitor API calls and resource loading
- **Main thread activity**: Detect long tasks blocking UI

**Performance Recording Tips**:

```typescript
// Mark custom performance events
performance.mark("zakat-calculation-start");

// Expensive calculation
const zakatAmount = calculateComplexZakat(assets, rates, adjustments);

performance.mark("zakat-calculation-end");
performance.measure("zakat-calculation", "zakat-calculation-start", "zakat-calculation-end");

// View measurements in Performance tab
const measure = performance.getEntriesByName("zakat-calculation")[0];
console.log(`Zakat calculation took ${measure.duration.toFixed(2)}ms`);
```

## 📦 Code Splitting

Code splitting reduces initial bundle size by loading code on demand.

### Route-Based Splitting

Split code by routes to load only necessary code for each page.

```typescript
// App.tsx
import { lazy, Suspense } from 'react';
import { Routes, Route } from 'react-router-dom';

// Lazy load route components
const ZakatCalculator = lazy(() => import('./features/zakat-calculation'));
const DonationCampaigns = lazy(() => import('./features/donation-campaigns'));
const MurabahaContracts = lazy(() => import('./features/murabaha-contracts'));
const IslamicCalendar = lazy(() => import('./features/islamic-calendar'));
const AdminDashboard = lazy(() => import('./features/admin-dashboard'));

// Loading fallback
function LoadingFallback() {
  return (
    <div className="page-loading">
      <div className="spinner" />
      <p>Loading...</p>
    </div>
  );
}

export function App() {
  return (
    <Suspense fallback={<LoadingFallback />}>
      <Routes>
        <Route path="/" element={<HomePage />} />
        <Route path="/zakat" element={<ZakatCalculator />} />
        <Route path="/donations" element={<DonationCampaigns />} />
        <Route path="/contracts" element={<MurabahaContracts />} />
        <Route path="/calendar" element={<IslamicCalendar />} />
        <Route path="/admin/*" element={<AdminDashboard />} />
      </Routes>
    </Suspense>
  );
}
```

**Benefits**:

- Initial bundle reduced by ~60-80%
- Faster first page load
- Users only download code they use

### Component-Based Splitting

Split large components or libraries into separate chunks.

```typescript
// Heavy chart library loaded only when needed
import { lazy, Suspense, useState } from 'react';

const DonationChart = lazy(() => import('./DonationChart'));

export function DonationAnalytics() {
  const [showChart, setShowChart] = useState(false);

  return (
    <div className="donation-analytics">
      <h2>Donation Analytics</h2>

      {/* Summary always loaded */}
      <DonationSummary />

      {/* Chart loaded on demand */}
      {!showChart ? (
        <button onClick={() => setShowChart(true)}>
          Show Detailed Chart
        </button>
      ) : (
        <Suspense fallback={<ChartSkeleton />}>
          <DonationChart />
        </Suspense>
      )}
    </div>
  );
}
```

### Dynamic Imports

Use dynamic imports for conditional features.

```typescript
// Import heavy libraries only when needed
async function exportDonationReport(format: "pdf" | "excel") {
  if (format === "pdf") {
    const { generatePDF } = await import("./pdf-generator");
    return generatePDF();
  } else {
    const { generateExcel } = await import("./excel-generator");
    return generateExcel();
  }
}

// Load polyfills conditionally
async function initializeApp() {
  // Load IntersectionObserver polyfill for older browsers
  if (!("IntersectionObserver" in window)) {
    await import("intersection-observer");
  }

  // Continue app initialization
  renderApp();
}
```

## 🧠 Memoization

Memoization prevents unnecessary re-computations and re-renders.

### React.memo

Memoize components to prevent re-renders when props haven't changed.

```typescript
import { memo } from 'react';

interface CampaignCardProps {
  id: string;
  title: string;
  goal: number;
  raised: number;
  category: 'zakat' | 'sadaqah' | 'emergency';
  onDonate: (id: string) => void;
}

// Without memo - re-renders every time parent renders
export function CampaignCard({
  id,
  title,
  goal,
  raised,
  category,
  onDonate,
}: CampaignCardProps) {
  console.log('CampaignCard rendered:', title);

  return (
    <div className="campaign-card">
      <h3>{title}</h3>
      <div className="progress-bar">
        <div
          className="progress-fill"
          style={{ width: `${(raised / goal) * 100}%` }}
        />
      </div>
      <p>
        ${raised.toLocaleString()} of ${goal.toLocaleString()}
      </p>
      <button onClick={() => onDonate(id)}>Donate Now</button>
    </div>
  );
}

// With memo - only re-renders when props change
export const MemoizedCampaignCard = memo(CampaignCard);

// Custom comparison function for complex props
export const MemoizedCampaignCardCustom = memo(
  CampaignCard,
  (prevProps, nextProps) => {
    // Return true if props are equal (skip re-render)
    return (
      prevProps.id === nextProps.id &&
      prevProps.raised === nextProps.raised &&
      prevProps.onDonate === nextProps.onDonate
    );
  }
);
```

### useMemo

Memoize expensive calculations.

```typescript
import { useMemo } from 'react';

interface Asset {
  type: 'cash' | 'gold' | 'silver' | 'stocks' | 'property';
  value: number;
  held_days: number;
}

interface ZakatCalculatorProps {
  assets: Asset[];
  nisabThreshold: number;
  exchangeRates: Record<string, number>;
}

export function ZakatCalculator({
  assets,
  nisabThreshold,
  exchangeRates,
}: ZakatCalculatorProps) {
  // Expensive calculation - only runs when dependencies change
  const zakatCalculation = useMemo(() => {
    console.log('Calculating zakat...');

    // Convert all assets to base currency
    const totalValue = assets.reduce((sum, asset) => {
      const rate = exchangeRates[asset.type] || 1;
      return sum + asset.value * rate;
    }, 0);

    // Check if eligible (held for 1 lunar year)
    const eligibleAssets = assets.filter(
      (asset) => asset.held_days >= 354
    );

    const eligibleValue = eligibleAssets.reduce((sum, asset) => {
      const rate = exchangeRates[asset.type] || 1;
      return sum + asset.value * rate;
    }, 0);

    // Calculate zakat (2.5% of eligible wealth above nisab)
    const isEligible = eligibleValue >= nisabThreshold;
    const zakatDue = isEligible ? eligibleValue * 0.025 : 0;

    return {
      totalValue,
      eligibleValue,
      nisabThreshold,
      zakatDue,
      isEligible,
      assetBreakdown: eligibleAssets.map((asset) => ({
        type: asset.type,
        value: asset.value,
        zakatAmount: asset.value * 0.025,
      })),
    };
  }, [assets, nisabThreshold, exchangeRates]); // Only recalculate when these change

  return (
    <div className="zakat-calculator">
      <h2>Zakat Calculation Results</h2>

      <div className="summary">
        <p>Total Assets: ${zakatCalculation.totalValue.toFixed(2)}</p>
        <p>
          Eligible Assets: ${zakatCalculation.eligibleValue.toFixed(2)}
        </p>
        <p>
          Nisab Threshold: ${zakatCalculation.nisabThreshold.toFixed(2)}
        </p>

        {zakatCalculation.isEligible ? (
          <div className="zakat-due">
            <h3>Zakat Due: ${zakatCalculation.zakatDue.toFixed(2)}</h3>
            <p>2.5% of eligible wealth</p>
          </div>
        ) : (
          <p>
            Not eligible: Assets below nisab threshold
          </p>
        )}
      </div>

      <h3>Asset Breakdown</h3>
      <ul>
        {zakatCalculation.assetBreakdown.map((item, index) => (
          <li key={index}>
            {item.type}: ${item.value.toFixed(2)} → Zakat: $
            {item.zakatAmount.toFixed(2)}
          </li>
        ))}
      </ul>
    </div>
  );
}
```

**When to use useMemo**:

✅ **Good use cases**:

- Expensive calculations (loops over large arrays, complex math)
- Filtering/sorting large datasets
- Creating objects/arrays passed to memoized child components
- Derived state that's expensive to compute

❌ **Avoid useMemo for**:

- Simple arithmetic (`count * 2`)
- String concatenation
- Object creation if not passed to children
- Premature optimization

### useCallback

Memoize function references to prevent unnecessary child re-renders.

```typescript
import { useState, useCallback, memo } from 'react';

interface DonationItemProps {
  id: string;
  amount: number;
  campaignTitle: string;
  onDelete: (id: string) => void;
}

// Memoized child component
const DonationItem = memo(function DonationItem({
  id,
  amount,
  campaignTitle,
  onDelete,
}: DonationItemProps) {
  console.log('DonationItem rendered:', id);

  return (
    <div className="donation-item">
      <span>{campaignTitle}</span>
      <span>${amount}</span>
      <button onClick={() => onDelete(id)}>Delete</button>
    </div>
  );
});

interface Donation {
  id: string;
  amount: number;
  campaignTitle: string;
}

export function DonationHistory() {
  const [donations, setDonations] = useState<Donation[]>([
    { id: '1', amount: 100, campaignTitle: 'Emergency Relief' },
    { id: '2', amount: 250, campaignTitle: 'Education Fund' },
    { id: '3', amount: 500, campaignTitle: 'Zakat Distribution' },
  ]);

  // Without useCallback - new function on every render
  // Child components re-render even with memo
  const handleDeleteBad = (id: string) => {
    setDonations((prev) => prev.filter((d) => d.id !== id));
  };

  // With useCallback - stable function reference
  // Child components only re-render when their props change
  const handleDelete = useCallback((id: string) => {
    setDonations((prev) => prev.filter((d) => d.id !== id));
  }, []); // Empty deps - callback never changes

  return (
    <div className="donation-history">
      <h2>Your Donations</h2>
      <div className="donation-list">
        {donations.map((donation) => (
          <DonationItem
            key={donation.id}
            id={donation.id}
            amount={donation.amount}
            campaignTitle={donation.campaignTitle}
            onDelete={handleDelete}
          />
        ))}
      </div>
    </div>
  );
}
```

**When to use useCallback**:

✅ **Good use cases**:

- Callbacks passed to memoized child components
- Dependencies for other hooks (useEffect, useMemo)
- Event handlers in large lists
- Callbacks passed to context providers

❌ **Avoid useCallback for**:

- Event handlers in single components
- Callbacks not passed to children
- Functions without dependencies

## 📜 Virtual Lists

Virtual lists render only visible items, dramatically improving performance for long lists.

### React Virtual (TanStack Virtual)

Modern virtualization library with excellent TypeScript support.

**Installation**:

```bash
npm install @tanstack/react-virtual
```

**Example - Donation History Virtualization**:

```typescript
import { useRef } from 'react';
import { useVirtualizer } from '@tanstack/react-virtual';

interface Donation {
  id: string;
  amount: number;
  date: string;
  campaignTitle: string;
  status: 'completed' | 'pending' | 'failed';
}

interface VirtualDonationListProps {
  donations: Donation[];
}

export function VirtualDonationList({
  donations,
}: VirtualDonationListProps) {
  const parentRef = useRef<HTMLDivElement>(null);

  const virtualizer = useVirtualizer({
    count: donations.length,
    getScrollElement: () => parentRef.current,
    estimateSize: () => 80, // Estimated item height in pixels
    overscan: 5, // Render 5 extra items above/below viewport
  });

  return (
    <div
      ref={parentRef}
      className="virtual-list-container"
      style={{
        height: '600px',
        overflow: 'auto',
        border: '1px solid #ccc',
      }}
    >
      <div
        style={{
          height: `${virtualizer.getTotalSize()}px`,
          width: '100%',
          position: 'relative',
        }}
      >
        {virtualizer.getVirtualItems().map((virtualItem) => {
          const donation = donations[virtualItem.index];

          return (
            <div
              key={donation.id}
              data-index={virtualItem.index}
              ref={virtualizer.measureElement}
              style={{
                position: 'absolute',
                top: 0,
                left: 0,
                width: '100%',
                transform: `translateY(${virtualItem.start}px)`,
              }}
            >
              <div className="donation-card">
                <div className="donation-header">
                  <h3>{donation.campaignTitle}</h3>
                  <span className={`status ${donation.status}`}>
                    {donation.status}
                  </span>
                </div>
                <div className="donation-details">
                  <p className="amount">${donation.amount}</p>
                  <p className="date">
                    {new Date(donation.date).toLocaleDateString()}
                  </p>
                </div>
              </div>
            </div>
          );
        })}
      </div>
    </div>
  );
}
```

**Performance Comparison**:

```typescript
// Without virtualization: Renders all 10,000 items
// DOM nodes: 10,000
// Initial render: ~2000ms
// Scroll FPS: ~30 FPS

// With virtualization: Renders ~15 visible items
// DOM nodes: ~15
// Initial render: ~50ms
// Scroll FPS: 60 FPS
```

### Variable Height Items

Handle dynamically sized items.

```typescript
import { useVirtualizer } from '@tanstack/react-virtual';

export function VariableHeightList({ items }: { items: Item[] }) {
  const parentRef = useRef<HTMLDivElement>(null);

  const virtualizer = useVirtualizer({
    count: items.length,
    getScrollElement: () => parentRef.current,
    estimateSize: () => 100, // Initial estimate
    // measureElement automatically updates size after render
  });

  return (
    <div ref={parentRef} style={{ height: '600px', overflow: 'auto' }}>
      <div
        style={{
          height: `${virtualizer.getTotalSize()}px`,
          position: 'relative',
        }}
      >
        {virtualizer.getVirtualItems().map((virtualItem) => {
          const item = items[virtualItem.index];

          return (
            <div
              key={item.id}
              data-index={virtualItem.index}
              ref={virtualizer.measureElement} // Measures actual height
              style={{
                position: 'absolute',
                top: 0,
                left: 0,
                width: '100%',
                transform: `translateY(${virtualItem.start}px)`,
              }}
            >
              {/* Variable height content */}
              <ItemCard item={item} />
            </div>
          );
        })}
      </div>
    </div>
  );
}
```

## 🖼️ Image Optimization

Images often account for 50%+ of page weight. Optimization is critical.

### Lazy Loading

Load images only when they enter the viewport.

```typescript
import { useState, useEffect, useRef } from 'react';

interface LazyImageProps {
  src: string;
  alt: string;
  placeholder?: string;
  className?: string;
}

export function LazyImage({
  src,
  alt,
  placeholder = '/placeholder.jpg',
  className,
}: LazyImageProps) {
  const [imageSrc, setImageSrc] = useState(placeholder);
  const [isLoaded, setIsLoaded] = useState(false);
  const imgRef = useRef<HTMLImageElement>(null);

  useEffect(() => {
    const observer = new IntersectionObserver(
      (entries) => {
        entries.forEach((entry) => {
          if (entry.isIntersecting) {
            // Load actual image when visible
            setImageSrc(src);
            observer.disconnect();
          }
        });
      },
      {
        rootMargin: '50px', // Start loading 50px before entering viewport
      }
    );

    if (imgRef.current) {
      observer.observe(imgRef.current);
    }

    return () => {
      observer.disconnect();
    };
  }, [src]);

  return (
    <img
      ref={imgRef}
      src={imageSrc}
      alt={alt}
      className={`${className} ${isLoaded ? 'loaded' : 'loading'}`}
      onLoad={() => setIsLoaded(true)}
    />
  );
}

// Usage
export function CampaignGallery({ campaigns }: { campaigns: Campaign[] }) {
  return (
    <div className="campaign-gallery">
      {campaigns.map((campaign) => (
        <div key={campaign.id} className="campaign-card">
          <LazyImage
            src={campaign.imageUrl}
            alt={campaign.title}
            placeholder="/campaign-placeholder.jpg"
          />
          <h3>{campaign.title}</h3>
        </div>
      ))}
    </div>
  );
}
```

### Responsive Images

Serve appropriately sized images for different devices.

```typescript
interface ResponsiveImageProps {
  src: string;
  alt: string;
  sizes?: string;
}

export function ResponsiveImage({
  src,
  alt,
  sizes = '(max-width: 768px) 100vw, (max-width: 1200px) 50vw, 800px',
}: ResponsiveImageProps) {
  // Generate srcset for different sizes
  const srcset = [
    `${src}?w=400 400w`,
    `${src}?w=800 800w`,
    `${src}?w=1200 1200w`,
    `${src}?w=1600 1600w`,
  ].join(', ');

  return (
    <img
      src={`${src}?w=800`} // Default size
      srcSet={srcset}
      sizes={sizes}
      alt={alt}
      loading="lazy" // Native lazy loading
    />
  );
}
```

### WebP Format

WebP provides 25-35% better compression than JPEG/PNG.

```typescript
interface OptimizedImageProps {
  src: string;
  alt: string;
  width?: number;
  height?: number;
}

export function OptimizedImage({
  src,
  alt,
  width,
  height,
}: OptimizedImageProps) {
  // Serve WebP with fallback
  return (
    <picture>
      <source
        srcSet={`${src}?format=webp&w=400 400w, ${src}?format=webp&w=800 800w`}
        type="image/webp"
      />
      <source
        srcSet={`${src}?format=jpeg&w=400 400w, ${src}?format=jpeg&w=800 800w`}
        type="image/jpeg"
      />
      <img
        src={`${src}?format=jpeg&w=800`}
        alt={alt}
        width={width}
        height={height}
        loading="lazy"
      />
    </picture>
  );
}
```

### Image CDN Integration

Use image CDN for automatic optimization.

```typescript
// Image CDN helper (Cloudinary example)
export function generateImageUrl(
  publicId: string,
  options: {
    width?: number;
    height?: number;
    format?: 'auto' | 'webp' | 'jpeg' | 'png';
    quality?: number;
  } = {}
) {
  const {
    width,
    height,
    format = 'auto',
    quality = 80,
  } = options;

  const transformations = [
    width && `w_${width}`,
    height && `h_${height}`,
    `q_${quality}`,
    `f_${format}`,
  ]
    .filter(Boolean)
    .join(',');

  return `https://res.cloudinary.com/your-cloud/image/upload/${transformations}/${publicId}`;
}

// Usage
export function CampaignImage({ publicId, alt }: { publicId: string; alt: string }) {
  return (
    <img
      src={generateImageUrl(publicId, { width: 800 })}
      srcSet={`
        ${generateImageUrl(publicId, { width: 400 })} 400w,
        ${generateImageUrl(publicId, { width: 800 })} 800w,
        ${generateImageUrl(publicId, { width: 1200 })} 1200w
      `}
      sizes="(max-width: 768px) 100vw, 800px"
      alt={alt}
      loading="lazy"
    />
  );
}
```

## 📦 Bundle Optimization

Reduce JavaScript bundle size for faster downloads and parsing.

### Tree Shaking

Remove unused code from bundles.

```typescript
// ❌ BAD - Imports entire library (100KB)
import _ from "lodash";
const result = _.sortBy(items, "date");

// ✅ GOOD - Imports only needed function (5KB)
import sortBy from "lodash/sortBy";
const result = sortBy(items, "date");

// ✅ BETTER - Use modern JavaScript
const result = items.sort((a, b) => a.date - b.date);
```

### Bundle Analysis

Analyze bundle size to identify optimization opportunities.

```bash
# Install bundle analyzer
npm install -D rollup-plugin-visualizer

# vite.config.ts
import { visualizer } from 'rollup-plugin-visualizer';

export default defineConfig({
  plugins: [
    react(),
    visualizer({
      open: true, // Opens report in browser after build
      gzipSize: true,
      brotliSize: true,
    }),
  ],
});

# Build and view report
npm run build
# Opens stats.html showing bundle composition
```

### Manual Chunking

Split bundles strategically.

```typescript
// vite.config.ts
export default defineConfig({
  build: {
    rollupOptions: {
      output: {
        manualChunks: {
          // React ecosystem
          "vendor-react": ["react", "react-dom", "react-router-dom"],

          // UI libraries
          "vendor-ui": ["@radix-ui/react-dialog", "@radix-ui/react-dropdown-menu", "@radix-ui/react-select"],

          // Data fetching
          "vendor-query": ["@tanstack/react-query"],

          // Form handling
          "vendor-form": ["react-hook-form", "@hookform/resolvers", "zod"],

          // Charts (heavy library)
          "vendor-charts": ["recharts"],
        },
      },
    },
  },
});
```

## 🎯 Render Optimization

Minimize unnecessary component re-renders.

### Identifying Render Problems

```typescript
import { useEffect, useRef } from 'react';

// Hook to track component renders
export function useRenderCount(componentName: string) {
  const renderCount = useRef(0);

  useEffect(() => {
    renderCount.current += 1;
    console.log(`${componentName} rendered ${renderCount.current} times`);
  });
}

// Hook to track why component re-rendered
export function useWhyDidYouUpdate(
  componentName: string,
  props: Record<string, any>
) {
  const previousProps = useRef<Record<string, any>>();

  useEffect(() => {
    if (previousProps.current) {
      const allKeys = Object.keys({ ...previousProps.current, ...props });
      const changedProps: Record<string, any> = {};

      allKeys.forEach((key) => {
        if (previousProps.current![key] !== props[key]) {
          changedProps[key] = {
            from: previousProps.current![key],
            to: props[key],
          };
        }
      });

      if (Object.keys(changedProps).length > 0) {
        console.log(`${componentName} re-rendered due to:`, changedProps);
      }
    }

    previousProps.current = props;
  });
}

// Usage
export function DonationCard({ campaign, onDonate }: DonationCardProps) {
  useRenderCount('DonationCard');
  useWhyDidYouUpdate('DonationCard', { campaign, onDonate });

  return <div>{/* ... */}</div>;
}
```

### Preventing Unnecessary Renders

**Strategy 1: State Colocation**

```typescript
// ❌ BAD - State in parent causes all children to re-render
function ParentComponent() {
  const [expandedId, setExpandedId] = useState<string | null>(null);

  return (
    <div>
      {campaigns.map((campaign) => (
        <CampaignCard
          key={campaign.id}
          campaign={campaign}
          isExpanded={expandedId === campaign.id}
          onToggle={() => setExpandedId(campaign.id)}
        />
      ))}
    </div>
  );
}

// ✅ GOOD - State inside child, only that child re-renders
function CampaignCard({ campaign }: { campaign: Campaign }) {
  const [isExpanded, setIsExpanded] = useState(false);

  return (
    <div>
      <h3>{campaign.title}</h3>
      <button onClick={() => setIsExpanded(!isExpanded)}>
        {isExpanded ? 'Collapse' : 'Expand'}
      </button>
      {isExpanded && <CampaignDetails campaign={campaign} />}
    </div>
  );
}
```

**Strategy 2: Component Splitting**

```typescript
// ❌ BAD - Input state causes entire form to re-render
function DonationForm() {
  const [amount, setAmount] = useState(0);
  const [message, setMessage] = useState('');
  const [frequency, setFrequency] = useState('once');

  return (
    <form>
      {/* ExpensiveComponent re-renders on every keystroke */}
      <ExpensiveComponent />

      <input
        value={amount}
        onChange={(e) => setAmount(Number(e.target.value))}
      />
      <input
        value={message}
        onChange={(e) => setMessage(e.target.value)}
      />
    </form>
  );
}

// ✅ GOOD - Split into separate components
function DonationForm() {
  return (
    <form>
      {/* Doesn't re-render when inputs change */}
      <ExpensiveComponent />

      <AmountInput />
      <MessageInput />
    </form>
  );
}

function AmountInput() {
  const [amount, setAmount] = useState(0);
  return (
    <input
      value={amount}
      onChange={(e) => setAmount(Number(e.target.value))}
    />
  );
}
```

## 🏎️ Web Vitals

Core Web Vitals are Google's user experience metrics.

### Largest Contentful Paint (LCP)

**Target**: < 2.5 seconds

**What it measures**: Time until largest content element is visible.

**Optimization strategies**:

```typescript
// 1. Optimize images (see Image Optimization section)

// 2. Preload critical resources
export function Head() {
  return (
    <>
      <link
        rel="preload"
        href="/hero-image.webp"
        as="image"
        type="image/webp"
      />
      <link
        rel="preconnect"
        href="https://api.example.com"
      />
    </>
  );
}

// 3. Eliminate render-blocking resources
// Move non-critical CSS to end of <head>
// Use async/defer for scripts

// 4. Server-side rendering for initial content
// Use Next.js, Remix, or similar framework
```

### First Input Delay (FID) / Interaction to Next Paint (INP)

**Target**: < 100ms (FID), < 200ms (INP)

**What it measures**: Responsiveness to user interactions.

**Optimization strategies**:

```typescript
// 1. Break up long tasks
async function processLargeDonationBatch(donations: Donation[]) {
  // ❌ BAD - Blocks main thread
  donations.forEach((donation) => {
    processDonation(donation);
  });

  // ✅ GOOD - Yield to browser between chunks
  for (let i = 0; i < donations.length; i += 100) {
    const chunk = donations.slice(i, i + 100);
    chunk.forEach((donation) => processDonation(donation));

    // Yield to browser
    await new Promise((resolve) => setTimeout(resolve, 0));
  }
}

// 2. Use Web Workers for heavy computation
// worker.ts
self.addEventListener('message', (event) => {
  const { assets, rates } = event.data;
  const zakatAmount = calculateComplexZakat(assets, rates);
  self.postMessage({ zakatAmount });
});

// main.ts
const worker = new Worker(new URL('./worker.ts', import.meta.url));

worker.postMessage({ assets, rates });
worker.addEventListener('message', (event) => {
  const { zakatAmount } = event.data;
  setZakatAmount(zakatAmount);
});

// 3. Use useTransition for non-urgent updates (React 18)
import { useState, useTransition } from 'react';

function CampaignSearch() {
  const [query, setQuery] = useState('');
  const [results, setResults] = useState<Campaign[]>([]);
  const [isPending, startTransition] = useTransition();

  const handleSearch = (newQuery: string) => {
    setQuery(newQuery); // Urgent update

    startTransition(() => {
      // Non-urgent update - won't block input
      const filtered = campaigns.filter((c) =>
        c.title.toLowerCase().includes(newQuery.toLowerCase())
      );
      setResults(filtered);
    });
  };

  return (
    <div>
      <input
        value={query}
        onChange={(e) => handleSearch(e.target.value)}
        placeholder="Search campaigns..."
      />
      {isPending && <Spinner />}
      <ResultsList results={results} />
    </div>
  );
}
```

### Cumulative Layout Shift (CLS)

**Target**: < 0.1

**What it measures**: Visual stability (unexpected layout shifts).

**Optimization strategies**:

```typescript
// 1. Always specify image dimensions
<img
  src="/campaign.jpg"
  alt="Campaign banner"
  width={800}
  height={450}
  loading="lazy"
/>

// 2. Reserve space for dynamic content
function DonationCard({ campaignId }: { campaignId: string }) {
  const { data: campaign, isLoading } = useCampaign(campaignId);

  return (
    <div className="donation-card" style={{ minHeight: '200px' }}>
      {isLoading ? (
        <Skeleton height={200} />
      ) : (
        <CampaignDetails campaign={campaign} />
      )}
    </div>
  );
}

// 3. Avoid inserting content above existing content
// ❌ BAD - Notification pushes content down
function NotificationBanner() {
  const [show, setShow] = useState(false);

  return (
    <>
      {show && <div className="notification">New campaign!</div>}
      <MainContent />
    </>
  );
}

// ✅ GOOD - Notification overlays content
function NotificationBanner() {
  const [show, setShow] = useState(false);

  return (
    <>
      {show && (
        <div className="notification-overlay" style={{ position: 'fixed' }}>
          New campaign!
        </div>
      )}
      <MainContent />
    </>
  );
}

// 4. Use font-display: swap
// global.css
@font-face {
  font-family: 'CustomFont';
  src: url('/fonts/custom-font.woff2') format('woff2');
  font-display: swap; // Show fallback font immediately
}
```

## 🔮 Prefetching

Load resources before they're needed.

### Link Prefetching

```typescript
import { useEffect } from 'react';
import { useNavigate } from 'react-router-dom';

// Prefetch route on hover
export function CampaignCard({ campaign }: { campaign: Campaign }) {
  const navigate = useNavigate();

  const handleMouseEnter = () => {
    // Prefetch campaign details route
    const link = document.createElement('link');
    link.rel = 'prefetch';
    link.href = `/campaigns/${campaign.id}`;
    document.head.appendChild(link);
  };

  return (
    <div
      className="campaign-card"
      onMouseEnter={handleMouseEnter}
      onClick={() => navigate(`/campaigns/${campaign.id}`)}
    >
      <h3>{campaign.title}</h3>
    </div>
  );
}
```

### Data Prefetching

```typescript
import { useQueryClient } from '@tanstack/react-query';

// Prefetch data on hover
export function CampaignCard({ campaign }: { campaign: Campaign }) {
  const queryClient = useQueryClient();

  const handleMouseEnter = () => {
    // Prefetch campaign details
    queryClient.prefetchQuery({
      queryKey: ['campaigns', campaign.id],
      queryFn: () => fetchCampaign(campaign.id),
      staleTime: 60 * 1000, // Cache for 1 minute
    });
  };

  return (
    <div className="campaign-card" onMouseEnter={handleMouseEnter}>
      <h3>{campaign.title}</h3>
    </div>
  );
}
```

## 🔄 Service Workers

Enable offline support and caching strategies.

### Basic Service Worker

```typescript
// public/sw.js
const CACHE_NAME = "donation-platform-v1";
const urlsToCache = ["/", "/index.html", "/styles.css", "/main.js"];

// Install service worker
self.addEventListener("install", (event) => {
  event.waitUntil(
    caches.open(CACHE_NAME).then((cache) => {
      return cache.addAll(urlsToCache);
    }),
  );
});

// Fetch from cache first
self.addEventListener("fetch", (event) => {
  event.respondWith(
    caches.match(event.request).then((response) => {
      // Return cached version or fetch from network
      return response || fetch(event.request);
    }),
  );
});

// Update service worker
self.addEventListener("activate", (event) => {
  event.waitUntil(
    caches.keys().then((cacheNames) => {
      return Promise.all(
        cacheNames.map((cacheName) => {
          if (cacheName !== CACHE_NAME) {
            return caches.delete(cacheName);
          }
        }),
      );
    }),
  );
});
```

### Register Service Worker

```typescript
// src/registerServiceWorker.ts
export async function registerServiceWorker() {
  if ("serviceWorker" in navigator && import.meta.env.PROD) {
    try {
      const registration = await navigator.serviceWorker.register("/sw.js");
      console.log("Service Worker registered:", registration);

      // Check for updates
      registration.addEventListener("updatefound", () => {
        const newWorker = registration.installing;
        if (newWorker) {
          newWorker.addEventListener("statechange", () => {
            if (newWorker.state === "installed" && navigator.serviceWorker.controller) {
              // New version available
              showUpdateNotification();
            }
          });
        }
      });
    } catch (error) {
      console.error("Service Worker registration failed:", error);
    }
  }
}

// src/main.tsx
import { registerServiceWorker } from "./registerServiceWorker";

registerServiceWorker();
```

## ⚛️ Concurrent Features (React 18)

### useTransition

Mark state updates as non-urgent.

```typescript
import { useState, useTransition } from 'react';

export function CampaignFilter() {
  const [filter, setFilter] = useState('');
  const [isPending, startTransition] = useTransition();

  const handleFilterChange = (newFilter: string) => {
    // Urgent: Update input immediately
    setFilter(newFilter);

    // Non-urgent: Update results
    startTransition(() => {
      // Expensive filtering operation
      filterCampaigns(newFilter);
    });
  };

  return (
    <div>
      <input
        value={filter}
        onChange={(e) => handleFilterChange(e.target.value)}
        placeholder="Filter campaigns..."
      />
      {isPending && <Spinner />}
      <CampaignList filter={filter} />
    </div>
  );
}
```

### useDeferredValue

Defer expensive renders.

```typescript
import { useState, useDeferredValue, useMemo } from 'react';

export function CampaignSearch({ campaigns }: { campaigns: Campaign[] }) {
  const [searchQuery, setSearchQuery] = useState('');

  // Defer search query updates
  const deferredQuery = useDeferredValue(searchQuery);

  // Filter based on deferred value
  const filteredCampaigns = useMemo(() => {
    console.log('Filtering campaigns...');
    return campaigns.filter((c) =>
      c.title.toLowerCase().includes(deferredQuery.toLowerCase())
    );
  }, [campaigns, deferredQuery]);

  return (
    <div>
      <input
        value={searchQuery}
        onChange={(e) => setSearchQuery(e.target.value)}
        placeholder="Search campaigns..."
      />
      <p>Showing {filteredCampaigns.length} results</p>
      <CampaignList campaigns={filteredCampaigns} />
    </div>
  );
}
```

## 📊 Performance Budgets

Set and enforce performance limits.

### Define Budgets

```typescript
// performance-budget.json
{
  "budgets": [
    {
      "resourceType": "total",
      "budget": 500
    },
    {
      "resourceType": "script",
      "budget": 300
    },
    {
      "resourceType": "style",
      "budget": 50
    },
    {
      "resourceType": "image",
      "budget": 150
    }
  ],
  "metrics": {
    "fcp": 1800,
    "lcp": 2500,
    "fid": 100,
    "cls": 0.1
  }
}
```

### Enforce with CI/CD

```bash
# Install Lighthouse CI
npm install -D @lhci/cli

# .lighthouserc.js
module.exports = {
  ci: {
    collect: {
      staticDistDir: './dist',
    },
    assert: {
      preset: 'lighthouse:recommended',
      assertions: {
        'categories:performance': ['error', { minScore: 0.9 }],
        'first-contentful-paint': ['warn', { maxNumericValue: 2000 }],
        'largest-contentful-paint': ['error', { maxNumericValue: 2500 }],
        'interactive': ['error', { maxNumericValue: 3500 }],
        'cumulative-layout-shift': ['error', { maxNumericValue: 0.1 }],
      },
    },
    upload: {
      target: 'temporary-public-storage',
    },
  },
};

# Run in CI
lhci autorun
```

## 📚 References

**Performance Tools**:

- [React DevTools Profiler](https://react.dev/learn/react-developer-tools)
- [Chrome DevTools](https://developer.chrome.com/docs/devtools/)
- [Lighthouse](https://developers.google.com/web/tools/lighthouse)
- [WebPageTest](https://www.webpagetest.org/)

**Libraries**:

- [TanStack Virtual](https://tanstack.com/virtual/latest) - Virtualization
- [React Window](https://github.com/bvaughn/react-window) - Alternative virtualization
- [Workbox](https://developers.google.com/web/tools/workbox) - Service Worker toolkit

**Related Documentation**:

- [State Management](ex-so-plwe-fere__state-management.md) - Optimizing state updates
- [Best Practices](ex-so-plwe-fere__best-practices.md) - General React best practices
- [Testing](ex-so-plwe-fere__testing.md) - Performance testing strategies

---

This comprehensive guide covers React performance optimization from profiling to implementation. Always measure before optimizing, focus on user experience, and verify improvements with real metrics.
