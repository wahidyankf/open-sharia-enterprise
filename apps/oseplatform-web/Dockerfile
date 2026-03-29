FROM node:24-alpine AS deps
WORKDIR /workspace
COPY ../../package.json ../../package-lock.json ./
COPY apps/oseplatform-fs/package.json apps/oseplatform-fs/
COPY libs/ts-ui/package.json libs/ts-ui/
COPY libs/ts-ui-tokens/package.json libs/ts-ui-tokens/
RUN npm ci --ignore-scripts

FROM node:24-alpine AS builder
WORKDIR /workspace
COPY --from=deps /workspace/node_modules ./node_modules
COPY apps/oseplatform-fs/ apps/oseplatform-fs/
COPY libs/ts-ui/ libs/ts-ui/
COPY libs/ts-ui-tokens/ libs/ts-ui-tokens/
COPY libs/ts-ui/src/ ./node_modules/@open-sharia-enterprise/ts-ui/src/
COPY libs/ts-ui/package.json ./node_modules/@open-sharia-enterprise/ts-ui/
COPY libs/ts-ui-tokens/src/ ./node_modules/@open-sharia-enterprise/ts-ui-tokens/src/
COPY libs/ts-ui-tokens/package.json ./node_modules/@open-sharia-enterprise/ts-ui-tokens/
WORKDIR /workspace/apps/oseplatform-fs
RUN npx tsx src/scripts/generate-search-data.ts
RUN npm run build

FROM node:24-alpine AS runner
WORKDIR /workspace
ENV NODE_ENV=production
ENV NEXT_TELEMETRY_DISABLED=1
ENV HOSTNAME=0.0.0.0
ENV PORT=3100
RUN addgroup --system --gid 1001 nodejs && \
    adduser --system --uid 1001 nextjs
COPY --from=builder --chown=nextjs:nodejs /workspace/apps/oseplatform-fs/.next/standalone ./
COPY --from=builder --chown=nextjs:nodejs /workspace/apps/oseplatform-fs/.next/static apps/oseplatform-fs/.next/static
COPY --from=builder --chown=nextjs:nodejs /workspace/apps/oseplatform-fs/public apps/oseplatform-fs/public
COPY --from=builder --chown=nextjs:nodejs /workspace/apps/oseplatform-fs/content apps/oseplatform-fs/content
COPY --from=builder --chown=nextjs:nodejs /workspace/apps/oseplatform-fs/generated apps/oseplatform-fs/generated
USER nextjs
EXPOSE 3100
CMD ["node", "apps/oseplatform-fs/server.js"]
