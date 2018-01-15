FROM node:9.4

RUN apt update -y \
 && apt install -yy ocaml

WORKDIR /app

COPY package.json /app
COPY package-lock.json /app

RUN npm install
