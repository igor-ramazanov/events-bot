FROM hseeberger/scala-sbt:11.0.7_1.3.10_2.13.2
RUN curl https://download.docker.com/linux/static/stable/x86_64/{docker-19.03.9.tgz} -s -o '#1' && \
    tar -xzf docker-19.03.9.tgz && \
    rm -rf docker-19.03.9.tgz && \
    mv docker/docker /usr/local/bin/ && \
    usermod -u 1000 sbtuser && \
    usermod -aG sbt sbtuser
USER sbtuser
WORKDIR /home/sbtuser
CMD ["sbt", "docker:publishLocal"]