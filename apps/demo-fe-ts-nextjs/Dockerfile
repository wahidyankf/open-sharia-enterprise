FROM node:24-alpine AS deps
WORKDIR /app
COPY package.json ./
RUN npm install --ignore-scripts

FROM node:24-alpine AS build
WORKDIR /app
COPY --from=deps /app/node_modules ./node_modules
COPY . .
ARG BACKEND_URL=http://demo-be:8201
ENV BACKEND_URL=${BACKEND_URL} NEXT_TELEMETRY_DISABLED=1
RUN npx next build

FROM node:24-alpine
RUN addgroup -S app && adduser -S app -G app
WORKDIR /app
COPY --from=build --chown=app:app /app/.next/standalone ./
COPY --from=build --chown=app:app /app/.next/static ./.next/static
USER app
EXPOSE 3301
ENV PORT=3301 HOSTNAME="0.0.0.0" NODE_ENV=production NEXT_TELEMETRY_DISABLED=1
CMD ["node", "server.js"]
