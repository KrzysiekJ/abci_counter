FROM erlang:20.1.2

WORKDIR /app

ADD . /app

RUN make

EXPOSE 46658

CMD /app/_rel/abci_counter_release/bin/abci_counter_release foreground
