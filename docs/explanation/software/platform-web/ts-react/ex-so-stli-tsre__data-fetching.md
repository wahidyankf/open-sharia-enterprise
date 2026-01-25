---
title: "React Data Fetching"
description: Data fetching patterns and strategies for React applications
category: explanation
subcategory: stack-libs
tags:
  - react
  - data-fetching
  - api
  - react-query
  - swr
  - typescript
related:
  - ./ex-so-stli-tsre__idioms.md
  - ./ex-so-stli-tsre__hooks.md
principles:
  - explicit-over-implicit
last_updated: 2026-01-25
---

# React Data Fetching

## Quick Reference

**Navigation**: [Stack Libraries](../README.md) > [TypeScript React](./README.md) > Data Fetching

**Related Guides**:

- [Idioms](./ex-so-stli-tsre__idioms.md) - useEffect patterns
- [Hooks](./ex-so-stli-tsre__hooks.md) - Data fetching hooks
- [State Management](./ex-so-stli-tsre__state-management.md) - Server state

## Overview

Data fetching is essential for React applications. This guide covers fetch API, TanStack Query (React Query), SWR, loading states, error handling, and caching strategies.

**Target Audience**: Developers building data-driven React applications, particularly Islamic finance platforms with API integration and real-time data requirements.

**React Version**: React 18.2+ with TypeScript 5+

## Basic Fetch with useEffect

### Simple Data Fetching

```typescript
interface UseFetchReturn<T> {
  data: T | null;
  loading: boolean;
  error: Error | null;
}

export const DonationsList: React.FC = () => {
  const [donations, setDonations] = useState<Donation[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<Error | null>(null);

  useEffect(() => {
    const fetchDonations = async () => {
      try {
        const response = await fetch('/api/donations');
        if (!response.ok) throw new Error('Failed to fetch');

        const data = await response.json();
        setDonations(data);
      } catch (err) {
        setError(err as Error);
      } finally {
        setLoading(false);
      }
    };

    fetchDonations();
  }, []);

  if (loading) return <LoadingSpinner />;
  if (error) return <ErrorMessage error={error} />;

  return (
    <ul>
      {donations.map(d => (
        <li key={d.id}>{d.campaignName}: {d.amount}</li>
      ))}
    </ul>
  );
};
```

### Fetch with Cleanup

```typescript
export const CampaignDetails: React.FC<{ id: string }> = ({ id }) => {
  const [campaign, setCampaign] = useState<Campaign | null>(null);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    let cancelled = false;

    const fetchCampaign = async () => {
      setLoading(true);

      try {
        const data = await campaignApi.getById(id);

        if (!cancelled) {
          setCampaign(data);
        }
      } catch (error) {
        if (!cancelled) {
          console.error('Fetch error:', error);
        }
      } finally {
        if (!cancelled) {
          setLoading(false);
        }
      }
    };

    fetchCampaign();

    return () => {
      cancelled = true;
    };
  }, [id]);

  if (loading) return <LoadingSpinner />;
  return <div>{campaign?.name}</div>;
};
```

## TanStack Query (React Query)

### Query Basics

```typescript
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';

export const DonationsList: React.FC = () => {
  const { data, isLoading, error, refetch } = useQuery({
    queryKey: ['donations'],
    queryFn: async () => {
      const response = await fetch('/api/donations');
      if (!response.ok) throw new Error('Failed to fetch');
      return response.json() as Promise<Donation[]>;
    },
    staleTime: 5 * 60 * 1000, // 5 minutes
    cacheTime: 10 * 60 * 1000, // 10 minutes
  });

  if (isLoading) return <LoadingSpinner />;
  if (error) return <ErrorMessage error={error as Error} />;

  return (
    <div>
      <button onClick={() => refetch()}>Refresh</button>
      <ul>
        {data?.map(d => (
          <li key={d.id}>{d.campaignName}</li>
        ))}
      </ul>
    </div>
  );
};
```

### Mutations

```typescript
export const CreateDonationForm: React.FC = () => {
  const queryClient = useQueryClient();

  const mutation = useMutation({
    mutationFn: (donation: NewDonation) => donationApi.create(donation),

    onSuccess: (newDonation) => {
      // Invalidate and refetch
      queryClient.invalidateQueries({ queryKey: ['donations'] });

      // Or optimistically update
      queryClient.setQueryData<Donation[]>(['donations'], (old = []) => [
        ...old,
        newDonation,
      ]);
    },

    onError: (error) => {
      console.error('Mutation failed:', error);
    },
  });

  const handleSubmit = (data: NewDonation) => {
    mutation.mutate(data);
  };

  return (
    <form onSubmit={e => {
      e.preventDefault();
      handleSubmit(/* form data */);
    }}>
      {/* Form fields */}
      <button type="submit" disabled={mutation.isPending}>
        {mutation.isPending ? 'Submitting...' : 'Submit'}
      </button>
      {mutation.isError && <p>Error: {mutation.error.message}</p>}
      {mutation.isSuccess && <p>Donation created!</p>}
    </form>
  );
};
```

### Pagination

```typescript
import { useQuery } from '@tanstack/react-query';
import { useState } from 'react';

export const PaginatedDonations: React.FC = () => {
  const [page, setPage] = useState(0);

  const { data, isLoading, isPreviousData } = useQuery({
    queryKey: ['donations', page],
    queryFn: () => donationApi.getPage(page, 20),
    keepPreviousData: true, // Keep previous data while loading next page
  });

  return (
    <div>
      {isLoading ? (
        <LoadingSpinner />
      ) : (
        <ul>
          {data?.donations.map(d => (
            <li key={d.id}>{d.campaignName}</li>
          ))}
        </ul>
      )}

      <div className="pagination">
        <button
          onClick={() => setPage(prev => Math.max(0, prev - 1))}
          disabled={page === 0}
        >
          Previous
        </button>

        <span>Page {page + 1}</span>

        <button
          onClick={() => setPage(prev => prev + 1)}
          disabled={isPreviousData || !data?.hasMore}
        >
          Next
        </button>
      </div>
    </div>
  );
};
```

## Related Documentation

- **[Idioms](./ex-so-stli-tsre__idioms.md)** - useEffect patterns
- **[Hooks](./ex-so-stli-tsre__hooks.md)** - Data fetching hooks
- **[State Management](./ex-so-stli-tsre__state-management.md)** - Server state management

---

**Last Updated**: 2026-01-25
