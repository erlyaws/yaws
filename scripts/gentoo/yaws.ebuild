# $Header$


DESCRIPTION="Yaws, a dynamic content, fast, erlang based webserver "
HOMEPAGE="http://yaws.hyber.org/"
SRC_URI="http://yaws.hyber.org/download/${P}.tar.gz"
LICENSE="BSD"
SLOT="0"
KEYWORDS="x86 ppc sparc alpha arm"
IUSE="ssl"

DEPEND=">=dev-lang/erlang-r8
	ssl?	( >=dev-libs/openssl-0.9.6d )"


 src_compile() {

    ./configure                    \
        --prefix=${D}/usr/         \
	--sysconfdir=${D}/etc/     \
	--localstatedir=${D}/var   || die
    
    make || die
}

src_install() {
    make install || die
}

