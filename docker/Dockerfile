# Build stage 0
FROM erlang:21.3.8.22-alpine

# Set working directory
RUN mkdir /buildroot
WORKDIR /buildroot

# Copy our Erlang nodes
COPY lesser_evil_agent lesser_evil_agent
COPY lesser_evil lesser_evil

# And build the releases
WORKDIR lesser_evil_agent/test/le_test_app
RUN rm -rf _build
RUN rebar3 as prod release

WORKDIR /buildroot/lesser_evil
RUN rm -rf _build
RUN rebar3 as prod release

# Build stage 1
FROM alpine

# Install some libs
RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
    apk add util-linux pciutils usbutils coreutils binutils findutils grep procps

# Install the released application
COPY --from=0 ./buildroot/lesser_evil_agent/docker/start.sh /start.sh
RUN chmod +x /start.sh

COPY --from=0 /buildroot/lesser_evil/mem2.sh /mem.sh
RUN chmod +x /mem.sh
RUN mkdir /measurements

COPY --from=0 /buildroot/lesser_evil_agent/test/le_test_app/_build/prod/rel/le_test_app /le_test_app
COPY --from=0 /buildroot/lesser_evil/_build/prod/rel/lesser_evil /lesser_evil

# Expose relevant ports
EXPOSE 8888

CMD ["/start.sh"]