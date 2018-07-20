FROM ubuntu

ADD . /config

WORKDIR /config

RUN apt update && apt install -y emacs25 git ispell

RUN ./install.sh

CMD ["emacs"]
