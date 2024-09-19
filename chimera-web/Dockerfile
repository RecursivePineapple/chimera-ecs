FROM node:lts-alpine3.17

WORKDIR /build

RUN corepack enable && corepack prepare yarn@3.6.0 --activate

COPY . /build/

RUN yarn && yarn parcel build

FROM nginx

COPY --from=0 /build/dist/ /usr/share/nginx/html
