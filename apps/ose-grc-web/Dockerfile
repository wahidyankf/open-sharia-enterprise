# Build context: workspace root (../../.. from apps/ose-grc-web/)
FROM node:24-alpine AS build
WORKDIR /app

# Copy app package.json and strip workspace dependency before install
COPY apps/ose-grc-web/package.json ./
RUN node -e " \
  const pkg = JSON.parse(require('fs').readFileSync('package.json','utf8')); \
  delete pkg.dependencies['@open-sharia-enterprise/web-ui']; \
  delete pkg.dependencies['@open-sharia-enterprise/web-ui-token']; \
  require('fs').writeFileSync('package.json', JSON.stringify(pkg, null, 2)); \
" && npm install --ignore-scripts --legacy-peer-deps

# Install web-ui transitive dependencies
RUN npm install --save class-variance-authority@^0.7.0 clsx@^2.1.1 lucide-react@^0.447.0 radix-ui@^1.0.0 tailwind-merge@^2.5.3 --ignore-scripts --legacy-peer-deps

# Copy workspace libs as resolvable node_modules packages
COPY libs/web-ui/src/ ./node_modules/@open-sharia-enterprise/web-ui/src/
COPY libs/web-ui/package.json ./node_modules/@open-sharia-enterprise/web-ui/
COPY libs/web-ui-token/src/ ./node_modules/@open-sharia-enterprise/web-ui-token/src/
COPY libs/web-ui-token/package.json ./node_modules/@open-sharia-enterprise/web-ui-token/

# Copy app source
COPY apps/ose-grc-web/src/ ./src/
COPY apps/ose-grc-web/package.json apps/ose-grc-web/tsconfig.json apps/ose-grc-web/next.config.ts apps/ose-grc-web/postcss.config.mjs ./

# Generate contract types (when spec exists)
# COPY specs/apps/ose-grc/containers/contracts/generated/ ./specs/apps/ose-grc/containers/contracts/generated/
# RUN npx @hey-api/openapi-ts -i specs/apps/ose-grc/containers/contracts/generated/openapi-bundled.yaml -o src/generated-contracts -s

ENV NEXT_TELEMETRY_DISABLED=1
ARG OSE_GRC_BE_URL=http://ose-grc-be:8202
ENV OSE_GRC_BE_URL=${OSE_GRC_BE_URL}
RUN npx next build

FROM node:24-alpine

LABEL org.opencontainers.image.source="https://github.com/wahidyankf/ose-public"
LABEL org.opencontainers.image.description="OSE GRC frontend built with Next.js"

RUN addgroup -S app && adduser -S app -G app
WORKDIR /app
COPY --from=build --chown=app:app /app/.next/standalone ./
COPY --from=build --chown=app:app /app/.next/static ./.next/static
USER app
EXPOSE 3300
ENV PORT=3300 HOSTNAME="0.0.0.0" NODE_ENV=production NEXT_TELEMETRY_DISABLED=1
HEALTHCHECK --interval=30s --timeout=5s --start-period=10s --retries=3 \
  CMD wget --no-verbose --tries=1 --spider http://localhost:3300/ || exit 1

CMD ["node", "server.js"]
