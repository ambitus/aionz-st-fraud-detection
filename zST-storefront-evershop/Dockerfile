FROM node

WORKDIR /app

RUN npm install -g npm@9

COPY . .

ENV PKG_CONFIG_PATH=/usr/lib/s390x-linux-gnu/pkgconfig/:/usr/share/pkgconfig/

RUN /bin/bash -c 'apt-get update'
RUN /bin/bash -c 'apt install -y libvips-dev'
RUN pkg-config --modversion vips-cpp
RUN npm install -g node-gyp
RUN npm install --build-from-source --unsafe-perm sharp@0.29.0

RUN npm install
RUN npm run build

EXPOSE 80

CMD ["sh", "startup.sh"]