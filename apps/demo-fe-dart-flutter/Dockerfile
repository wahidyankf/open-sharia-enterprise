FROM ghcr.io/cirruslabs/flutter:3.41.2 AS build
WORKDIR /app
COPY . .
RUN flutter pub get
RUN flutter build web --release

FROM nginx:alpine
COPY --from=build /app/build/web /usr/share/nginx/html
COPY <<EOF /etc/nginx/conf.d/default.conf
server {
    listen 3301;
    root /usr/share/nginx/html;
    index index.html;
    location / {
        try_files \$uri \$uri/ /index.html;
    }
    location /api/ {
        proxy_pass http://demo-be:8201;
    }
    location /health {
        proxy_pass http://demo-be:8201;
    }
    location /.well-known/ {
        proxy_pass http://demo-be:8201;
    }
}
EOF
EXPOSE 3301
