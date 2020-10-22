FROM alpine/bundle:2.7.2
COPY . /apps
RUN bundle install
ENTRYPOINT ["bundle"]
