FROM alpine/bundle:2.7.2

COPY Gemfile /apps
RUN bundle install
COPY . /apps

ENTRYPOINT ["bundle"]
