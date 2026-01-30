---
title: React Observability
description: Monitoring and observability strategies for React applications, covering error tracking, performance monitoring, analytics, and debugging
category: explanation
tags:
  - react
  - observability
  - monitoring
  - error-tracking
  - performance
  - analytics
created: 2026-01-29
updated: 2026-01-29
---

# React Observability

Observability is the practice of understanding your application's internal state through external outputs. For React applications, this encompasses error tracking, performance monitoring, user analytics, and real-time debugging capabilities. Proper observability enables teams to identify issues before users report them, understand user behavior patterns, and continuously improve application quality.

## 🎯 Purpose

This document explains observability strategies and tools for React applications, providing conceptual understanding of monitoring approaches, their trade-offs, and how they apply to Sharia-compliant business systems.

## 📋 Core Concepts

### Observability vs Monitoring

**Monitoring** answers "Is the system working?" through predefined metrics and alerts. **Observability** answers "Why isn't it working?" by providing deep insights into system behavior through logs, metrics, and traces.

React applications require both:

- **Monitoring**: Uptime checks, error rates, performance budgets
- **Observability**: Error context, user sessions, performance traces

### The Three Pillars of Observability

1. **Logs**: Timestamped records of discrete events (errors, warnings, info)
2. **Metrics**: Numerical measurements over time (response times, error counts)
3. **Traces**: Request flows through distributed systems (API calls, component renders)

React applications primarily focus on client-side observability, requiring different strategies than server-side systems.

## 🔍 Error Tracking

Error tracking captures, aggregates, and analyzes runtime errors to identify patterns and prioritize fixes.

### Error Boundaries

React's Error Boundaries catch component tree errors and provide fallback UI:

```javascript
class ErrorBoundary extends React.Component {
  constructor(props) {
    super(props);
    this.state = { hasError: false };
  }

  static getDerivedStateFromError(error) {
    return { hasError: true };
  }

  componentDidCatch(error, errorInfo) {
    // Log to error tracking service
    logErrorToService(error, errorInfo);
  }

  render() {
    if (this.state.hasError) {
      return <ErrorFallback />;
    }
    return this.props.children;
  }
}
```

**Strategic placement**:

- Root level for global errors
- Around critical features (payment processing, Zakat calculations)
- Around third-party components

### Sentry Integration

Sentry provides comprehensive error tracking with React-specific features:

**Key capabilities**:

- Source map support for production debugging
- Component stack traces
- Breadcrumbs (user actions leading to error)
- Release tracking for regression identification
- Custom context (user ID, transaction details)

**Example for Zakat application**:

```javascript
Sentry.init({
  dsn: process.env.REACT_APP_SENTRY_DSN,
  environment: process.env.NODE_ENV,
  release: process.env.REACT_APP_VERSION,
  beforeSend(event, hint) {
    // Sanitize sensitive data
    if (event.user) {
      delete event.user.email;
    }
    return event;
  },
});

// Add custom context for Zakat calculation errors
Sentry.setContext("zakat", {
  calculationType: "nisab",
  amount: calculationAmount,
  date: calculationDate,
});
```

**Privacy considerations**: Sanitize personally identifiable information (PII) before sending to Sentry, especially for Sharia-compliant applications handling financial data.

### Error Reporting Best Practices

**Contextual information**:

- User actions leading to error
- Application state at error time
- Browser/device information
- Network conditions

**Error severity classification**:

- **Critical**: Payment failures, data loss, security issues
- **High**: Feature unavailable, incorrect calculations
- **Medium**: UI glitches, non-blocking errors
- **Low**: Cosmetic issues, minor inconsistencies

## 📊 Performance Monitoring

Performance monitoring tracks application speed, responsiveness, and resource usage to ensure good user experience.

### Web Vitals

Core Web Vitals are Google's metrics for user-centric performance:

**Largest Contentful Paint (LCP)**: Loading performance (target: <2.5s)

- Measures when main content becomes visible
- Affected by: Image optimization, server response time, render-blocking resources

**First Input Delay (FID)**: Interactivity (target: <100ms)

- Measures time from user interaction to browser response
- Affected by: JavaScript execution, main thread blocking

**Cumulative Layout Shift (CLS)**: Visual stability (target: <0.1)

- Measures unexpected layout shifts
- Affected by: Images without dimensions, dynamic content injection

**Implementation with web-vitals library**:

```javascript
import { getCLS, getFID, getLCP } from "web-vitals";

function sendToAnalytics({ name, delta, id }) {
  // Send to analytics service
  analytics.track("web_vital", {
    metric: name,
    value: delta,
    id: id,
  });
}

getCLS(sendToAnalytics);
getFID(sendToAnalytics);
getLCP(sendToAnalytics);
```

### React DevTools Profiler

React DevTools Profiler identifies rendering performance bottlenecks:

**Flame graph**: Visualizes component render times in hierarchical view

**Ranked chart**: Lists components by render duration

**Use cases**:

- Identify unnecessary re-renders
- Find slow components
- Optimize render paths
- Validate optimization efforts

**Profiling Zakat calculator example**:

- Profile calculation-heavy components
- Identify re-renders triggered by form inputs
- Optimize expensive calculations with useMemo
- Validate memoization effectiveness

### Application Performance Monitoring (APM)

APM tools provide end-to-end performance visibility:

**Key metrics**:

- Page load time
- API request duration
- Database query performance
- Client-side routing transitions
- Bundle size impact

**Tools**:

- **New Relic**: Full-stack APM with React support
- **Datadog**: Infrastructure and application monitoring
- **Dynatrace**: AI-powered performance insights

## 📈 Analytics

Analytics track user behavior, engagement patterns, and feature usage to inform product decisions.

### Event Tracking

**User actions to track**:

- Feature usage (Zakat calculator initiated, sadaqah donated)
- Conversion funnels (registration → profile → first transaction)
- User flows (navigation patterns, time on page)
- Engagement metrics (scroll depth, video plays)

**Implementation with custom hooks**:

```javascript
function useAnalytics() {
  const trackEvent = useCallback((eventName, properties) => {
    if (window.analytics) {
      window.analytics.track(eventName, {
        ...properties,
        timestamp: new Date().toISOString(),
        page: window.location.pathname,
      });
    }
  }, []);

  return { trackEvent };
}

// Usage in Zakat calculator
function ZakatCalculator() {
  const { trackEvent } = useAnalytics();

  const calculateZakat = (amount) => {
    const result = amount * 0.025;
    trackEvent("zakat_calculated", {
      amount: amount,
      result: result,
      calculationType: "standard",
    });
    return result;
  };
}
```

### Google Analytics 4 (GA4)

GA4 provides event-based analytics with enhanced privacy controls:

**Key features**:

- Automatic event tracking (page views, scrolls, outbound clicks)
- Custom event definitions
- User property tracking
- Conversion tracking
- Privacy-focused (cookieless options available)

**Considerations for Sharia-compliant apps**:

- Respect user privacy preferences
- Avoid tracking PII without consent
- Implement data retention policies
- Provide opt-out mechanisms

### Plausible Analytics

Privacy-focused alternative to Google Analytics:

**Benefits**:

- No cookies required
- GDPR/CCPA compliant by default
- Lightweight script (<1KB)
- Open-source self-hosting option

**Trade-offs**:

- Limited behavioral insights compared to GA4
- Simpler feature set
- Better for privacy-conscious applications

## 🎥 User Session Replay

Session replay tools record user interactions for debugging and UX improvement.

### LogRocket

LogRocket records user sessions with technical context:

**Captures**:

- DOM snapshots
- Network requests
- Console logs
- Redux state changes
- Performance metrics

**Use cases**:

- Reproduce hard-to-debug issues
- Understand user confusion points
- Validate bug reports
- Analyze conversion drop-off

**Privacy controls**:

```javascript
LogRocket.init("your-app/app-id", {
  // Sanitize sensitive inputs
  dom: {
    inputSanitizer: true,
    textSanitizer: true,
  },
  network: {
    // Sanitize request/response bodies
    requestSanitizer: (request) => {
      if (request.body) {
        request.body = "[SANITIZED]";
      }
      return request;
    },
  },
});
```

### FullStory

Similar to LogRocket with emphasis on user experience insights:

**Unique features**:

- Rage clicks detection
- Dead clicks tracking (clicks that don't respond)
- Frustration signals
- UX heatmaps

**Sharia compliance considerations**:

- Sanitize financial transaction details
- Exclude password fields
- Redact personal information
- Implement consent mechanisms

## 📝 Logging

Structured logging provides context-rich records for troubleshooting.

### Console Logging Levels

**Standard levels**:

- `console.error()` - Errors requiring immediate attention
- `console.warn()` - Potential issues or deprecated usage
- `console.info()` - Informational messages
- `console.debug()` - Detailed debugging information
- `console.log()` - General-purpose logging

**Best practices**:

- Use appropriate levels
- Include contextual information
- Avoid logging sensitive data
- Use structured format for easy parsing

### Structured Logging

```javascript
function createLogger(context) {
  return {
    info: (message, metadata = {}) => {
      console.info(
        JSON.stringify({
          level: "info",
          message,
          context,
          metadata,
          timestamp: new Date().toISOString(),
        }),
      );
    },
    error: (message, error, metadata = {}) => {
      console.error(
        JSON.stringify({
          level: "error",
          message,
          error: {
            message: error.message,
            stack: error.stack,
          },
          context,
          metadata,
          timestamp: new Date().toISOString(),
        }),
      );
    },
  };
}

// Usage
const logger = createLogger({ component: "ZakatCalculator" });
logger.info("Calculation initiated", { amount: 10000 });
```

### Log Aggregation

Client-side logs can be aggregated to centralized services:

**Options**:

- **Loggly**: Cloud log management
- **Papertrail**: Simple log aggregation
- **Elasticsearch + Kibana**: Self-hosted solution

**Implementation considerations**:

- Batch logs to reduce network requests
- Sample high-volume logs
- Implement log level filtering
- Set up log retention policies

## 🚀 Application Performance Monitoring (APM)

APM provides holistic view of application health and performance.

### Key Metrics

**Frontend metrics**:

- Time to First Byte (TTFB)
- First Contentful Paint (FCP)
- Time to Interactive (TTI)
- Total Blocking Time (TBT)

**Backend metrics** (for API calls):

- Request duration
- Error rate
- Throughput (requests/second)
- Database query time

### Distributed Tracing

Distributed tracing follows requests across multiple services:

**Benefits for React apps**:

- Trace API calls from frontend to backend
- Identify slow database queries
- Understand service dependencies
- Correlate frontend errors with backend issues

**Example trace**:

```
User clicks "Calculate Zakat"
  → React component renders (50ms)
  → API call to /api/zakat/calculate (200ms)
    → Database query (150ms)
    → Calculation logic (50ms)
  → Response received (250ms total)
  → UI updates (30ms)
```

## 📡 Real User Monitoring (RUM)

RUM collects performance data from actual users in production.

### RUM vs Synthetic Monitoring

**RUM** (Real User Monitoring):

- Measures actual user experiences
- Captures real network conditions
- Shows geographic variations
- Reflects device diversity

**Synthetic Monitoring**:

- Simulated tests from controlled environments
- Consistent baseline measurements
- Early warning for regressions
- Scheduled checks

### Performance Budgets

Performance budgets set thresholds for acceptable performance:

**Example budget**:

- Initial bundle size: <200KB gzipped
- LCP: <2.5s (75th percentile)
- FID: <100ms (95th percentile)
- CLS: <0.1 (75th percentile)
- API response time: <500ms (median)

**Enforcement**:

- CI/CD pipeline checks
- Build-time bundle analysis
- Automated performance testing
- Alerts on budget violations

## 🚨 Alerts and Notifications

Alerts notify teams of critical issues requiring immediate attention.

### Alert Configuration

**Key principles**:

- Alert on symptoms, not causes
- Set appropriate thresholds
- Reduce alert fatigue
- Include actionable context

**Example alert rules**:

- Error rate >5% for 5 minutes → PagerDuty notification
- LCP >4s for 10 minutes → Slack alert
- API error rate >10% → Email + SMS
- Payment failure rate >2% → Immediate escalation

### Uptime Monitoring

**Tools**:

- **Pingdom**: Simple uptime monitoring
- **UptimeRobot**: Free uptime checks
- **StatusCake**: Multi-location monitoring

**Monitoring strategy**:

- Check critical endpoints every 1-5 minutes
- Monitor from multiple geographic locations
- Test both availability and functionality
- Create public status page for transparency

## 🛠️ Feature Flags

Feature flags enable gradual rollouts, A/B testing, and instant rollbacks.

### LaunchDarkly

**Use cases**:

- Gradual feature rollouts (10% → 50% → 100%)
- User-targeted features
- Kill switches for problematic features
- Operational toggles for maintenance

**Implementation**:

```javascript
import { useLDClient } from "launchdarkly-react-client-sdk";

function ZakatCalculator() {
  const ldClient = useLDClient();
  const showNewCalculator = ldClient.variation("new-zakat-calculator", false);

  if (showNewCalculator) {
    return <NewZakatCalculator />;
  }
  return <LegacyZakatCalculator />;
}
```

### Split.io

Alternative to LaunchDarkly with focus on experimentation:

**Features**:

- A/B testing framework
- Feature flag management
- Impact analysis
- Rollout strategies

## 🧪 A/B Testing

A/B testing compares variations to optimize user experience and conversions.

### Testing Strategy

**Elements to test**:

- Call-to-action text and placement
- Form layouts and field ordering
- Donation amounts and suggestions
- Color schemes and visual hierarchy

**Statistical significance**:

- Define success metrics before testing
- Calculate required sample size
- Run tests until statistical significance
- Avoid early termination bias

**Example for donation platform**:

**Hypothesis**: Suggested donation amounts increase average donation

**Variation A** (Control): Open text field
**Variation B**: Preset amounts ($25, $50, $100, custom)

**Metrics**: Average donation amount, conversion rate

### Tools

**Google Optimize**: Free A/B testing integrated with GA4
**Optimizely**: Enterprise A/B testing platform
**VWO**: Visual editor for non-technical users

## 🐛 Debugging Tools

### React DevTools

**Components tab**:

- Inspect component props and state
- Modify values in real-time
- Navigate component tree
- Find component by DOM element

**Profiler tab**:

- Record render performance
- Identify unnecessary re-renders
- Analyze flame graphs
- Export performance data

### Redux DevTools

**Features**:

- Time-travel debugging
- Action replay
- State diff visualization
- Action filtering

**Integration**:

```javascript
import { configureStore } from "@reduxjs/toolkit";
import { devToolsEnhancer } from "@redux-devtools/extension";

const store = configureStore({
  reducer: rootReducer,
  devTools: process.env.NODE_ENV !== "production",
});
```

### Browser DevTools

**Network tab**: Analyze API requests, response times, payload sizes

**Performance tab**: Record runtime performance, analyze main thread activity

**Memory tab**: Detect memory leaks, analyze heap snapshots

**Lighthouse**: Automated audits for performance, accessibility, SEO

## 🕌 Sharia-Compliant Application Examples

### Monitoring Zakat Calculations

**Error tracking**:

- Track calculation errors with input context
- Monitor for incorrect nisab threshold applications
- Alert on discrepancies between calculation methods

**Performance monitoring**:

- Measure calculation performance for large portfolios
- Track form completion times
- Monitor API response times for real-time nisab rates

**Analytics**:

- Track calculation method preferences
- Measure calculator completion rates
- Analyze drop-off points in multi-step forms

### Tracking Donation Conversions

**Funnel analysis**:

1. Landing page view
2. Donation page visit
3. Amount selection
4. Payment method selection
5. Transaction completion

**Key metrics**:

- Conversion rate at each step
- Average donation amount
- Payment method distribution
- Completion time
- Error rates by step

**Session replay**:

- Understand confusion points in donation flow
- Identify form validation issues
- Analyze payment failures

### Monitoring Compliance Features

**Audit logging**:

- Track access to sensitive data
- Log compliance-related actions
- Monitor data export requests

**Performance budgets**:

- Ensure fast load times for critical compliance forms
- Monitor report generation performance
- Track audit trail query times

## 🔗 Related Documentation

- [React State Management](ex-so-plwe-fere__state-management.md) - Managing application state with observability context
- [React Testing](ex-so-plwe-fere__testing.md) - Testing strategies complementing observability
- [Frontend Architecture](../../../architecture/ex-ar-__frontend-architecture.md) - Architectural patterns for observable systems
- [Privacy and Security](../../../../governance/principles/security/ex-pr-se__privacy-first.md) - Privacy considerations for monitoring

## 📚 References

**Tools**:

- [Sentry](https://sentry.io/) - Error tracking and performance monitoring
- [LogRocket](https://logrocket.com/) - Session replay and debugging
- [Google Analytics 4](https://analytics.google.com/) - Web analytics
- [Plausible](https://plausible.io/) - Privacy-focused analytics
- [LaunchDarkly](https://launchdarkly.com/) - Feature flag management

**Standards**:

- [Web Vitals](https://web.dev/vitals/) - Core performance metrics
- [WCAG 2.1](https://www.w3.org/WAI/WCAG21/quickref/) - Accessibility guidelines
- [GDPR](https://gdpr.eu/) - Data protection regulation

**Best Practices**:

- [React Error Boundaries](https://react.dev/reference/react/Component#catching-rendering-errors-with-an-error-boundary)
- [Web Performance Working Group](https://www.w3.org/webperf/)
- [Observability Engineering (Honeycomb)](https://www.honeycomb.io/observability-engineering)
