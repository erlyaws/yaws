# $Header: $

EAPI="2"

inherit eutils

DESCRIPTION="Yaws is a high performance HTTP 1.1 web server."
HOMEPAGE="http://yaws.hyber.org/"
SRC_URI="http://yaws.hyber.org/download/${P}.tar.gz"
LICENSE="BSD"
SLOT="0"
KEYWORDS="~x86 ~ppc ~sparc ~amd64"
IUSE=""

DEPEND="dev-lang/erlang"

PROVIDE="virtual/httpd-basic virtual/httpd-cgi"

src_install() {
	make DESTDIR=${D} install || edie
	keepdir /var/log/yaws
	rmdir ${D}var/lib/log/yaws
	rmdir ${D}var/lib/log
	# We need to keep these directories so that the example yaws.conf works
	# properly
	keepdir /usr/lib/yaws/examples/ebin
	keepdir /usr/lib/yaws/examples/include
	dodoc ChangeLog LICENSE README
}

pkg_postinst() {
	einfo "An example YAWS configuration has been setup to run on"
	einfo "Please edit /etc/yaws/yaws.conf to suit your needs."
}
