FROM docker.io/library/postgres:15
RUN localedef -i fi_FI -c -f UTF-8 -A /usr/share/locale/locale.alias fi_FI.UTF-8
ENV LANG fi_FI.utf8
ENV POSTGRES_PASSWORD=app
ENV POSTGRES_USER=app
ENV POSTGRES_DB=valintalaskenta