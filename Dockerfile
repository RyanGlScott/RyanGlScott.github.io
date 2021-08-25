FROM alpine/bundle:2.7.2

COPY Gemfile /apps
RUN bundle update && bundle install
COPY . /apps

ENTRYPOINT ["bundle"]
